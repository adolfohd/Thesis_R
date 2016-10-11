#!/usr/bin/Rscript

getwd()
args = commandArgs(trailingOnly=TRUE)
load("data.RData")

setwd("~/code/thesis_R"); i=1; j=1
load("data/data.RData")

# random forest  ###################################################
library(randomForest)
n <- names(l.train)
f <- as.formula(paste("Left ~", paste(n[!n %in% "Left"], collapse = " + ")))

i <- as.numeric(args[1])
j  <- as.numeric(args[2])
############################### RFC --> L-Side######
ntree <- 5*i
output.forest <- randomForest(f, 
                              data = l.train, 
                              ntree = ntree, 
                              mtry = 2*j, 
                              test = l.test,
                              do.trace = T
)
save(output.forest, file=paste("Data/outLeft_ntree_",ntree, "_mtry_",2*j, ".RData"))


