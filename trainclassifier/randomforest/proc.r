#!/usr/bin/Rscript

getwd()

# setwd("/home/docker")
#source("loadData.r", chdir = TRUE)

args = commandArgs(trailingOnly=TRUE)

#print(args[0]) ## loadData.r
#print(args[1]) ## arg 1

ls()

##################################
# --------------------
# "Include" files


# --------------------

load("data.RData")

# random forest  ###################################################
library(randomForest)
n <- names(r.train)
f <- as.formula(paste("Right ~", paste(n[!n %in% "Right"], collapse = " + ")))

i <- as.numeric(args[1])
j  <- as.numeric(args[2])

############################### RFC --> R-Side######

ntree <- 20*i
output.forest <- randomForest(f, 
                              data = r.train, 
                              ntree = ntree, 
                              mtry = 2*j, 
                              test = r.test,
                              do.trace = T
)

save(output.forest, file=paste("Data/outF_i_",i, "_j_",j, ".RData"))


