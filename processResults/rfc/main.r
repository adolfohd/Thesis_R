# --- Dependencies --- #
# install.packages("randomForest")
# install.packages("plot3D")
# install.packages("plotly")
# -------------------- #

# setwd("~/code/thesis_R/processResults/rfc")
setwd("~/code/thesis_R/")
rm(list = ls())
source("lib/bestrfc.r")

# setwd("~/code/thesis_R/trainclassifier/randomforest")

library(randomForest)

data.folder.left = "data/results/20161012/left/"
files.left = list.files(data.folder.left)
best.forest.info.left <- best.rfc(files.left, data.folder.left)

data.folder.right = "data/results/20161012/right/"
files.right = list.files(data.folder.right)
best.forest.info.right <- best.rfc(files.right, data.folder.right)

# library(plot)

x <- 1:nrow(best.forest.info.left$errors)
y <- 1:ncol(best.forest.info.left$errors)
z <- best.forest.info.left$errors

# plot3D::persp3D(x,y,z, theta = 135, phi = 40, clab = "Out-of-bag error - LEFT")
# plot3D::persp3D(x,y,z,phi = 45, theta = 45,clab = "LEFT", xlab = 'ntrees', ylab = 'mtry', zlab = 'OOB error', axes = TRUE, ticktype = 'detailed', expand=.8)
plot3D::persp3D(x,y,z,phi = 45, theta = 45,clab = "LEFT", axes = TRUE, ticktype = 'detailed', expand=.8)
