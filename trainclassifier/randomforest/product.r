rm(list = ls())
setwd("~/code/thesis_R/trainclassifier/randomforest")

# install.packages("randomForest")
library(randomForest)

data.folder = "results/20161007primeraviernes/data/"
files1 = list.files(data.folder)
    
forests <- list()
error <- matrix()
for (i in 1:length(files1)){
  load(paste(data.folder,files1[i], sep=""))
  print(output.forest)
  
  forests[i] = output.forest
}
