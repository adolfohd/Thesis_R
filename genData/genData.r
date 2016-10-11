#!/usr/bin/Rscript

# setwd("/home/docker")
setwd("~/code/thesis_R")
rawData <- read.csv(file="data/gaitData.csv", header=TRUE, sep=";")
rawData$Gender<-factor(rawData$Gender)

rawData_ <- rawData[4:169]

# gaitData <- rawData_[,!(grepl("ilter",names(rawData_)))]

gaitData <- rawData_[,     !(grepl("ilter",names(rawData_)) # ignore filtered data since it will be re-computed
                              |
                              ( 
                                grepl("nkle",names(rawData_)) & grepl("ngle",names(rawData_))
                              )
                              |
                              (
                                 grepl("positions",names(rawData_)) & grepl("3",names(rawData_))
                              )
                            )
                     ] # ignore ankles angles??

rLabel <- rawData[171]
lLabel <- rawData[170]

# separete datasets for both sides (data is common, with different labels)
rData <- cbind(gaitData, rLabel)
rData$Right <- factor(rData$Right)

lData <- cbind(gaitData, lLabel)
lData$Left <- factor(lData$Left)

source("subsam.r")
# Correct class imbalance ###################################################
rsub.index <- subsam(rData)
rSub <- rData[rsub.index,]

lsub.index <- subsam(lData)
lSub <- lData[lsub.index,]

# Kalman filter ###################################################

source("myKF.r")
source("fdataset.r")
fSub.r <- fdataset(rSub)
fSub.l <- fdataset(lSub)

# # Normalization ###################################################
source("ndataset.r")
normalization.l <- ndataset(fSub.l)
normalization.r <- ndataset(fSub.r)

fNSub.l <- normalization.l$ndata
b.l <- normalization.l$b
m.l <- normalization.l$m

fNSub.r <- normalization.r$ndata
b.r <- normalization.r$b
m.r <- normalization.r$m


# # Feature selection ###################################################
n.features <- 30

print("Computing feature selection for R side")
library(varSelRF)
sel.r <- varSelRF(fNSub.r[,-ncol(fNSub.r)], fNSub.r[,ncol(fNSub.r)], ntree = 200, ntreeIterat = 100, vars.drop.frac = 0.2)
plot(sel.r)
sel.vars.r <- sel.r$selected.vars
print("Finished feature selection for R side")

print("Computing feature selection for L side")
sel.l <- varSelRF(fNSub.l[,-ncol(fNSub.l)], fNSub.l[,ncol(fNSub.l)], ntree = 200, ntreeIterat = 100, vars.drop.frac = 0.2)
plot(sel.l)
sel.vars.l <- sel.l$selected.vars
print("Finished feature selection for L side")

sel.vars <- intersect(sel.vars.l,sel.vars.r)[1:n.features] # common features to both sides
sel.vars.diff.r <- setdiff(sel.vars.r, sel.vars.l) # features in R that are not in L
sel.vars.diff.l <- setdiff(sel.vars.l, sel.vars.r) # features in L that are not in R

fNSub.l_ <- cbind(fNSub.l[sel.vars], fNSub.l[ncol(fNSub.l)])
fNSub.r_ <- cbind(fNSub.r[sel.vars], fNSub.r[ncol(fNSub.r)])

mm.r <- m.r[sel.vars]
bb.r <- b.r[sel.vars]
mm.l <- m.l[sel.vars]
bb.l <- b.l[sel.vars]

# Separate training and testing sets  ###################################################

trainingFactor <- 0.6
index.train <-  sample(1:nrow(fNSub.r_),floor(trainingFactor*nrow(fNSub.r_)))
r.train <- fNSub.r_[index.train,]
r.test  <- fNSub.r_[-index.train,]

index.train <- 1:floor(trainingFactor*nrow(fNSub.l_))
l.train <- fNSub.l_[index.train,]
l.test  <- fNSub.l_[-index.train,]
index.train

print("data saved on projectRoot/genData/data.RData")
save.image("genData/data.RData")




