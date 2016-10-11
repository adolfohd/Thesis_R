#!/usr/bin/Rscript

is.this.cluster = TRUE

if (is.this.cluster){
  getwd()
  args = commandArgs(trailingOnly=TRUE)
  load("data.RData")
  i <- as.numeric(args[1])
  j  <- as.numeric(args[2])
  output.folder <- "Data/"
}else{
  setwd("~/code/thesis_R"); i=1; j=1
  load("data/data.RData")
  output.folder <- "tests/"
}

# random forest  ###################################################
library(randomForest)
n <- names(l.train)
f <- as.formula(paste("Left ~", paste(n[!n %in% "Left"], collapse = " + ")))


############################### RFC --> L-Side######
ntree <- 5*i
output.forest <- randomForest(f, 
                              data = l.train, 
                              ntree = ntree, 
                              mtry = 2*j, 
                              test = l.test,
                              do.trace = T
)

output.file.path <- paste(
  output.folder,
  "outLeft_ntree_",ntree, "_mtry_",2*j, ".RData", sep = "")

save(output.forest, file= output.file.path)


