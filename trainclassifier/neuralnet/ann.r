
is.this.cluster = TRUE

# ANN2 - nnet  ###################################################

# https://beckmw.wordpress.com/tag/nnet/

# ANN - neuralnet  ###################################################

# install.packages("neuralnet")
# install.packages("NeuralNetTools")


# https://beckmw.wordpress.com/tag/neural-network/
# 


# library(NeuralNetTools)

if (is.this.cluster){
  data.folder = ""
  lib.folder =  ""
  print(data.folder)
  args = commandArgs(trailingOnly=TRUE)
  i <- as.numeric(args[1])
  j  <- as.numeric(args[2])
  output.folder <- "data/"
}else{
  setwd("~/code/thesis_R")
  data.folder = "data/"
  lib.folder =  "lib/"
}

rm(list = ls())

load(  paste(data.folder, "data.RData"  , sep = ""))
source(paste( lib.folder, "allnumeric.r", sep = ""))

# all features are numeric

library(neuralnet)
rTrainNN <- allnumeric(r.train)
rTestNN <- allnumeric(r.test)
lTrainNN <- allnumeric(l.train)
lTestNN <- allnumeric(l.test)

# separate each output level into separate outputs
# L-side

source(paste(lib.folder,"separateoutput.r", sep = ""))

r.train.nn <- separate.output(rTrainNN, gait.phases)
summary(r.train.nn)

n <- names(r.train.nn)
f <- as.formula(paste( paste(gait.phases, collapse = " + ")   ," ~", paste(n[!n %in% gait.phases], collapse = " + ")))
nn <- neuralnet(f,data=r.train.nn,hidden=200)
# plot(nn)

pr.nn <- compute(nn,rTrainNN[,-ncol(rTrainNN)])


# plot(rTrainNN$Right,pr.nn$net.result, col='red',main='Real vs predicted NN',pch=18,cex=0.7)
# abline(0,1,lwd=2)

output.file.path <- paste(
  output.folder,
  "out",2*j, ".RData", sep = "")

print(output.file.path)

save(nn, file= output.file.path)
