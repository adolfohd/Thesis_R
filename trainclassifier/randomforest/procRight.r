#!/usr/bin/Rscript

getwd()
args = commandArgs(trailingOnly=TRUE)
load("data.RData")

# random forest  ###################################################
library(randomForest)
n <- names(r.train)
f <- as.formula(paste("Right ~", paste(n[!n %in% "Right"], collapse = " + ")))

i <- as.numeric(args[1])
j  <- as.numeric(args[2])
############################### RFC --> R-Side######
ntree <- 5*i
output.forest <- randomForest(f, 
                              data = r.train, 
                              ntree = ntree, 
                              mtry = 2*j, 
                              test = r.test,
                              do.trace = T
)
save(output.forest, file=paste("Data/outF_i_",i, "_j_",j, ".RData", sep=""))


