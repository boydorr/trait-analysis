choice <- 7
run_dic <- TRUE
source("models.R")

s7 <- s
s7$summary$statistics[grepl("avsigma", rownames(s7$summary$statistics)),"Mean"]
dic7 <- dic

choice <- 19
source("models.R")

s19 <- s
s19$summary$statistics[grepl("avsigma", rownames(s19$summary$statistics)),"Mean"]
dic19 <- dic
sum(dic19$deviance+dic19$penalty)

choice <- 8
run_dic <- FALSE
source("models.R")

s8 <- s
s8$summary$statistics[grepl("dof", rownames(s8$summary$statistics)),]
s8$summary$statistics[grepl("avsigma", rownames(s8$summary$statistics)),"Mean"]

for (i in 1:nt)
  for (j in 1:nt)
    ttrI[i,j] <- s8$summaries[grepl(paste0("avsigma\\[",i,",",j,"\\]"), rownames(s8$summary$statistics)),"Mean"]

choice <- 20
run_dic <- TRUE
source("modelf.R")
sp <- s
sp$summaries[grepl("dof", rownames(sp$summary$statistics)),]
sp$summaries[grepl("ve", rownames(sp$summary$statistics)),]
sp$summaries[grepl("sigma", rownames(sp$summary$statistics)),"Mean"]
dicp <- dic
sum(dicp$deviance+dicp$penalty)
# Mean deviance:  4625
# penalty 320.7
# Penalized deviance: 4946

st <- sp$summary$statistics
ttrs <- array(NA,c(9,4,4))
ttrmean <- matrix(0,4,4)
for (k in 1:9)
{
  for (i in 1:nt)
    for (j in 1:nt)
      ttrs[k,i,j] <- st[grepl(paste0("sigma\\[",k,",",i,",",j,"\\]"),
                              rownames(st)),"Mean"]
    ttrmean <- ttrmean + ttrs[k,,] / 9
}

# Mean deviance:  4592
# penalty 333
# Penalized deviance: 4925.215

# No ve
source("models.R")
s20 <- s
s20$summary$statistics[grepl("dof", rownames(s20$summary$statistics)),]
s20$summary$statistics[grepl("sigma", rownames(s20$summary$statistics)),"Mean"]
dic20 <- dic
sum(dic20$deviance+dic20$penalty)

st <- s20$summary$statistics
ttrs20 <- array(NA,c(9,4,4))
ttrmean20 <- matrix(0,4,4)
for (k in 1:9)
{
  for (i in 1:nt)
    for (j in 1:nt)
      ttrs20[k,i,j] <- st[grepl(paste0("sigma\\[",k,",",i,",",j,"\\]"),
                                rownames(st)),"Mean"]
    ttrmean20 <- ttrmean20 + ttrs20[k,,] / 9
}

# Mean deviance:  4604
# penalty 328.7
# Penalized deviance: 4932.58

# With ve
choice <- 20
run_dic <- TRUE
source("models.R")
s20ve <- s
s20ve$summary$statistics[grepl("dof", rownames(s20ve$summary$statistics)),]
s20ve$summary$statistics[grepl("ve", rownames(s20ve$summary$statistics)),]
s20ve$summary$statistics[grepl("sigma", rownames(s20ve$summary$statistics)),"Mean"]
dic20ve <- dic
sum(dic20ve$deviance+dic20ve$penalty)
dic20ve
# Mean deviance:  4603
# penalty 328.7
# Penalized deviance: 4932.03

choice <- 19
run_dic <- TRUE
source("models.R")
s19 <- s
s19$summaries[grepl("dof", rownames(s19$summary$statistics)),]
s19$summary$statistics[grepl("sigma", rownames(s19$summary$statistics)),"Mean"]
dic19 <- dic
sum(dic19$deviance+dic19$penalty)
dic19
# Mean deviance:  4679
# penalty 316.9
# Penalized deviance: 4995.49


st <- s20ve$summary$statistics
ttrs20ve <- array(NA,c(9,4,4))
ttrmean20ve <- matrix(0,4,4)
for (k in 1:9)
{
  for (i in 1:nt)
    for (j in 1:nt)
      ttrs20ve[k,i,j] <- st[grepl(paste0("sigma\\[",k,",",i,",",j,"\\]"),
                                rownames(st)),"Mean"]
    ttrmean20ve <- ttrmean20ve + ttrs20ve[k,,] / 9
}
ttrmean20ve

choice <- 8
run_dic <- FALSE
source("modelf.R")

so <- s
so$summary$statistics[grepl("dof", rownames(so$summary$statistics)),]
so$summary$statistics[grepl("avsigma", rownames(so$summary$statistics)),"Mean"]

st <- so$summary$statistics
ttrO <- matrix(NA,4,4)
for (i in 1:nt)
  for (j in 1:nt)
    ttrO[i,j] <- st[grepl(paste0("avsigma\\[",i,",",j,"\\]"),
                          rownames(st)),"Mean"]
