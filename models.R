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

# BUILD MODELS
models<- list()

# Null model
ter.0 <- "ter.one"
ttr.0 <- "ttr.one"
models[["0"]] <- read.file("base.R", ter.0, ttr.0)

# Models with null TTR
for (ter in c("ter.species", "ter.species.mean", "ter.species.mean.e",
              "ter.species.mean.t", "ter.species.mean.et"))
  models[[ter]] <- read.file("base.ter.R", ter, ttr.0)

# Models with null TER
for (ttr in c("ttr.species", "ttr.species.mean", "ttr.species.mean.t"))
  models[[ttr]] <- read.file("base.ttr.R", ter.0, ttr)

# Models with species-specific TER and TTR
for (ter in c("ter.species", "ter.species.mean", "ter.species.mean.e",
              "ter.species.mean.t", "ter.species.mean.et"))
  for (ttr in c("ttr.species", "ttr.species.mean", "ttr.species.mean.t"))
    models[[paste(ter, ttr, sep="_")]] <- read.file("base.ter.ttr.R", ter, ttr)

# CREATING NEW DATABASE
db <- dbConnect(SQLite())

# IMPORTING EXCEL DATA
wb <- loadWorkbook("data.xlsx")
setMissingValue(wb, value = "NA")

Tables <- readWorksheet(wb, sheet = getSheets(wb))

names(Tables) <- c("PLOT", "SPECIES", "TRAIT")    # Change the names of the data frames
str(Tables)      # structure of Tables
names(Tables)    # Names of the elements of Tables

# INSERTING TABLES INTO DATABASE
with(Tables, {
  dbWriteTable(conn = db, name = "PLOT",    value = PLOT,
               row.names = FALSE, overwrite = TRUE)
  dbWriteTable(conn = db, name = "SPECIES", value = SPECIES,
               row.names = FALSE, overwrite = TRUE)
  dbWriteTable(conn = db, name = "TRAIT",   value = TRAIT,
               row.names = FALSE, overwrite = TRUE)
})

## individual species plot wise
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

HEIGHT <- ((tree$HEIGHT - mean(tree$HEIGHT))/sd(tree$HEIGHT))*100 + 500
SLA    <- ((tree$SLA    - mean(tree$SLA   ))/sd(tree$SLA   ))*100 + 500
WD     <- ((tree$WD     - mean(tree$WD    ))/sd(tree$WD    ))*100 + 500
SC     <- ((tree$SC     - mean(tree$SC    ))/sd(tree$SC    ))*100 + 500

traits <- as.matrix(cbind(HEIGHT,SLA,WD,SC))
nt <- ncol(traits) # Number of traits to be considered

NH4      <- ((tree$NH4      - mean(tree$NH4     ))/sd(tree$NH4     ))*100 + 500
P        <- ((tree$P        - mean(tree$P       ))/sd(tree$P       ))*100 + 500
K        <- ((tree$K        - mean(tree$K       ))/sd(tree$K       ))*100 + 500
SALINITY <- ((tree$SALINITY - mean(tree$SALINITY))/sd(tree$SALINITY))*100 + 500
SILT     <- ((tree$SILT     - mean(tree$SILT    ))/sd(tree$SILT    ))*100 + 500
PH       <- ((tree$PH       - mean(tree$PH      ))/sd(tree$PH      ))*100 + 500
URP      <- ((tree$URP      - mean(tree$URP     ))/sd(tree$URP     ))*100 + 500
HH       <- ((tree$HH       - mean(tree$HH      ))/sd(tree$HH      ))*100 + 500

envir <- as.matrix(cbind(NH4, P, K, SALINITY, SILT, PH, URP, HH))
ne <- ncol(envir) + 1

# Do we want to do something specific? Or test everything?
args = commandArgs(trailingOnly=TRUE)

if ((length(args) == 0) && (exists("model")))
  args <- as.character(model)

if (length(args) > 0)
{
  print(paste("I'm going to run", length(args), "model(s)."))
  for (arg in args)
  {
    model.num <- as.integer(arg)
    print(paste0("Running model ", arg, ": ", names(models)[model.num]))
    cat(models[[model.num]])
    init <- run.jags(models[[model.num]], n.chains=2,
                     burnin=4000, sample=10000,
                     modules=c("glm","dic"))
    extend <- extend.jags(init, sample=20000)
    # DIC
    dic <- extract(extend, what='dic') # DIC, pD
    print(dic)
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
