# --- Dependencies --- #
# install.packages("randomForest")
install.packages("plot3D")
# install.packages("plotly")
# -------------------- #

setwd("~/code/thesis_R/processResults/rfc")
rm(list = ls())
source("bestrfc.r")



setwd("~/code/thesis_R/trainclassifier/randomforest")


library(randomForest)

data.folder = "results/20161007primeraviernes/data/"
files1 = list.files(data.folder)

best.forest.info <- best.rfc(files1, data.folder)

library(plot)

x <- 1:nrow(best.forest.info$errors)
y <- 1:ncol(best.forest.info$errors)
z <- best.forest.info$errors

plot3D::persp3D(x,y,z, theta = 135, phi = 40, clab = "Out-of-bag error")
