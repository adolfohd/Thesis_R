# install.packages("neuralnet")
install.packages(NeuralNetTools)
library(NeuralNetTools)
# ANN2 - nnet  ###################################################

# ANN - neuralnet  ###################################################
# rm(rTrainNN,rTestNN,lTrainNN,lTestNN)
rm(list = ls())
setwd("~/code/thesis_R")
load("data/data.RData")

source("lib/allnumeric.r")

# all features are numeric

library(neuralnet)
rTrainNN <- allnumeric(r.train)
rTestNN <- allnumeric(r.test)
lTrainNN <- allnumeric(l.train)
lTestNN <- allnumeric(l.test)

# separate each output level into separate outputs
# L-side

source("tests/separateoutput.r")

r.train.nn <- separate.output(rTrainNN, gait.phases)
summary(r.train.nn)

n <- names(r.train.nn)
f <- as.formula(paste( paste(gait.phases, collapse = " + ")   ," ~", paste(n[!n %in% gait.phases], collapse = " + ")))
nn <- neuralnet(f,data=r.train.nn,hidden=100, stepmax = 1000)
plot(nn)

pr.nn <- compute(nn,rTrainNN[,-ncol(rTrainNN)])


plot(rTrainNN$Right,pr.nn$net.result, col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)