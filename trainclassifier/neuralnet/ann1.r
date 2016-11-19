rm(list=ls())
Sys.time()


is.this.cluster = FALSE
# ANN2 - nnet  ###################################################
# https://beckmw.wordpress.com/tag/nnet/
# ANN - neuralnet  ###################################################
# install.packages("neuralnet")
# install.packages("NeuralNetTools")
# https://beckmw.wordpress.com/tag/neural-network/

# library(NeuralNetTools)


if (is.this.cluster){
  data.folder = ""
  lib.folder =  ""
  print("this is the cluster, right?")
	print(data.folder)
  args = commandArgs(trailingOnly=TRUE)
  i <- as.numeric(args[1])
  j  <- as.numeric(args[2])
  output.folder <- "data/"
}else{
  i <- 100
  j <- 1
  setwd("~/code/thesis_R")
  data.folder = "data/"
  lib.folder =  "lib/"
}
# rm(list = ls())
print("data directory:")
print(data.folder)
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

print(data.folder)

n <- names(r.train.nn)
f <- as.formula(paste( paste(gait.phases, collapse = " + ")   ," ~", paste(n[!n %in% gait.phases], collapse = " + ")))
print(f)
hidden = 30# i*10
print(paste("one layer, ", hidden, "neurons"))
nn <- neuralnet(f,data=r.train.nn,
                hidden=hidden, 
                algorithm = 'backprop', learningrate = 0.2,
                threshold = 0.01, stepmax = 1e+05)

# algorithm = 'sag'

# plot(nn)


# plot(rTrainNN$Right,pr.nn$net.result, col='red',main='Real vs predicted NN',pch=18,cex=0.7)
# abline(0,1,lwd=2)

output.file.path <- paste(
  output.folder,
  "oneLayer",hidden, "neurons.RData", sep = "")

print(paste("saving trained neural network in",output.file.path))
save(nn, file= output.file.path)

pr.nn <- compute(nn,rTrainNN[,-ncol(rTrainNN)])