####  LIBRARY IMPORT  ####
library(rJava)
library(rjags) # install.packages(pkgs = c("rjags","coda", "runjags"),repos = "http://cran.fhcrc.org/")
library(runjags)
library(coda)
library(mcmcplots)
library(sqldf)
library(XLConnect) # install.packages("XLConnect", dependencies=TRUE) to avoid xlxs issue
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(scales)
library(grid)
library(vegan)

# CHECK INPUTS -----------------
args = commandArgs(trailingOnly=TRUE)
if (exists("choice"))
  args <- choice

if (length(args) > 0)
{
  if (!exists("run_dic"))
    run_dic <- TRUE
  if (any(args == "--run_dic"))
    run_dic <- TRUE
  if (any(args == "--no_dic"))
    run_dic <- FALSE

  if (!exists("use_wish"))
    use_wish <- TRUE
  if (any(args == "--use_wish"))
    use_wish <- TRUE
  if (any(args == "--no_wish"))
    use_wish <- FALSE

  norm_mean <- 0
  norm_sd   <- 1
  if (any(args == "--mean_0"))
  {
    norm_mean <- 0
    norm_sd   <- 1
  }
  if (any(args == "--no_mean_0"))
  {
    norm_mean <- 500
    norm_sd   <- 100
  }

  args <- args[!grepl("--", args)]
} else {
  if (!exists("run_dic"))
    run_dic <- TRUE
  if (!exists("use_wish"))
    use_wish <- TRUE
  if (!exists("norm_mean"))
  {
    norm_mean <- 0
    norm_sd   <- 1
  }
}

####  DATA IMPORT  ####
read.file <- function(basefile, terroot, ttrroot)
{
  terfile <- paste0(terroot, ".R")
  ttrfile <- paste0(ttrroot, ".R")
  base <- readChar(basefile, file.info(basefile)$size)
  ter <- readChar(terfile, file.info(terfile)$size)
  ttr <- readChar(ttrfile, file.info(ttrfile)$size)
  sprintf(base, ter, ttr)
}

# NAME MODELS ---------------
ter.0 <- "ter.one"
ter.models <- c("ter.species", "ter.species.mean", "ter.species.mean.e",
                "ter.species.mean.t", "ter.species.mean.et")
if (use_wish)
{
  ttr.0 <- c("ttr.wish.one", "ttr.wish.onev")
  ttr.models <- c("ttr.wish.species", "ttr.wish.speciesv") #, "ttr.wish.species.mean")
} else {
  ttr.0 <- "ttr.one"
  ttr.models <- c("ttr.species", "ttr.species.mean", "ttr.species.mean.t")
}

# BUILD MODELS ----------------
models<- list()
# Null model
for (ttr in ttr.0)
  models[[ttr]] <- read.file("base.R", ter.0, ttr)

# Models with null TTR
for (ter in ter.models)
  for (ttr in ttr.0)
    models[[paste(ter, ttr, sep="_")]] <- read.file("base.ter.R", ter, ttr)

# Models with null TER
for (ttr in ttr.models)
  models[[ttr]] <- read.file("base.ttr.R", ter.0, ttr)

# Models with species-specific TER and TTR
for (ter in ter.models)
  for (ttr in ttr.models)
    models[[paste(ter, ttr, sep="_")]] <- read.file("base.ter.ttr.R", ter, ttr)

# CREATING NEW DATABASE -------------
db <- dbConnect(SQLite())

# IMPORTING EXCEL DATA ---------------
wb <- loadWorkbook("data.xlsx")
setMissingValue(wb, value = "NA")

Tables <- readWorksheet(wb, sheet = getSheets(wb))

names(Tables) <- c("PLOT", "SPECIES", "TRAIT")    # Change the names of the data frames
str(Tables)      # structure of Tables
names(Tables)    # Names of the elements of Tables

# INSERTING TABLES INTO DATABASE ---------------
with(Tables, {
  dbWriteTable(conn = db, name = "PLOT",    value = PLOT,
               row.names = FALSE, overwrite = TRUE)
  dbWriteTable(conn = db, name = "SPECIES", value = SPECIES,
               row.names = FALSE, overwrite = TRUE)
  dbWriteTable(conn = db, name = "TRAIT",   value = TRAIT,
               row.names = FALSE, overwrite = TRUE)
})

## individual species plot wise ----------------
tree <- dbGetQuery(db,"SELECT PLOT_TRAIT, TRAIT.SPECIES_TRAIT, PLOT.NH4, PLOT.P,
                       PLOT.K, PLOT.SALINITY, PLOT.SILT, PLOT.PH, PLOT.URP,
                       PLOT.HH, TRAIT.HEIGHT, TRAIT.SLA, TRAIT.WD, TRAIT.SC

                       FROM PLOT JOIN TRAIT ON PLOT.PLOT_PLOT = TRAIT.PLOT_TRAIT

                       WHERE TRAIT.SLA IS NOT NULL AND TRAIT.WD IS NOT NULL AND
                       TRAIT.SC IS NOT NULL AND
                       SPECIES_TRAIT IN ('AMUR','BAEN',
                                         'GEWA','GORAN','KAKRA','KEORA',
                                         'POSUR','SINGRA','SUNDRI')")

n <- length(tree[,1]) # Sample size

f <- factor(tree$SPECIES_TRAIT)
species <- as.integer(f)
ns <- max(species)

HEIGHT <- (tree$HEIGHT - mean(tree$HEIGHT)) / sd(tree$HEIGHT)
SLA    <- (tree$SLA    - mean(tree$SLA))    / sd(tree$SLA)
WD     <- (tree$WD     - mean(tree$WD))     / sd(tree$WD)
SC     <- (tree$SC     - mean(tree$SC))     / sd(tree$SC)

traits <- as.matrix(cbind(HEIGHT,SLA,WD,SC)) * norm_sd + norm_mean
nt <- ncol(traits) # Number of traits to be considered

NH4      <- (tree$NH4      - mean(tree$NH4))       / sd(tree$NH4)
P        <- (tree$P        - mean(tree$P))         / sd(tree$P)
K        <- (tree$K        - mean(tree$K))         / sd(tree$K)
SALINITY <- (tree$SALINITY - mean(tree$SALINITY))  / sd(tree$SALINITY)
SILT     <- (tree$SILT     - mean(tree$SILT))      / sd(tree$SILT)
PH       <- (tree$PH       - mean(tree$PH))        / sd(tree$PH)
URP      <- (tree$URP      - mean(tree$URP))       / sd(tree$URP)
HH       <- (tree$HH       - mean(tree$HH))        / sd(tree$HH)

envir <- as.matrix(cbind(NH4, P, K, SALINITY, SILT, PH, URP, HH)) * norm_sd +
  norm_mean
ne <- ncol(envir) + 1

# RUN MODELS IN JAGS -----------------
# Do we want to do something specific? Or test everything?
if ((length(args) == 0) && (exists("model")))
  args <- as.character(model)

if (length(args) > 0)
{
  print(paste("I'm going to run", length(args), "model(s)."))
  print(paste("I", (if (run_dic) "will" else "won't"), "run DIC."))
  for (arg in args)
  {
    model.num <- as.integer(arg)
    print(paste0("Running model ", arg, ": ", names(models)[model.num]))
    cat(models[[model.num]])
    init <- run.jags(models[[model.num]], n.chains=2,
                     burnin=4000, sample=10000,
                     modules=c("glm","dic"))
    extend <- extend.jags(init, sample=20000)
    if (run_dic)
    {
      # DIC
      dic <- extract(extend, what='dic') # DIC, pD
      print(dic)
      write.csv(data.frame(model=arg,
                           name=names(models)[model.num],
                           dic=sum(dic$deviance+dic$penalty)),
                paste0(arg, ".csv"))
    }
    s <- add.summary(extend)
  }
} else {
  print("Testing all models!")
  for (name in names(models))
  {
    print(name)
    run.jags(models[[name]], n.chains = 2, adapt = 2, burnin = 2, sample = 2,
             modules = c("glm", "dic"))
  }
}
