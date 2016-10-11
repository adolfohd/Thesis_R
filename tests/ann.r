install.packages("neuralnet")

# ANN2 - nnet  ###################################################

# ANN - neuralnet  ###################################################
# rm(rTrainNN,rTestNN,lTrainNN,lTestNN)
rm(list = ls())
setwd("~/code/thesis_R")
load("data/data.RData")

source("lib/allnumeric.r")

library(neuralnet)
rTrainNN <- allnumeric(r.train)
rTestNN <- allnumeric(r.test)
lTrainNN <- allnumeric(l.train)
lTestNN <- allnumeric(l.test)

n <- names(rTrainNN)
f <- as.formula(paste("Right ~", paste(n[!n %in% "Right"], collapse = " + ")))
nn <- neuralnet(f,data=rTrainNN,hidden=5,linear.output=F)
pr.nn <- compute(nn,rTrainNN[,-ncol(rTrainNN)])

plot(rTrainNN$Right,pr.nn$net.result, col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)