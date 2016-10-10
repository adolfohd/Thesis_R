
# n = c(2, 3, 5)
# s = c("aa", "bb", "cc")
# b = c(TRUE, FALSE, TRUE)
# df = data.frame(n, s, b)       # df is a data frame

# PREAMBLE ###################################################
#### http://rdotnet.codeplex.com/documentation
#### https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
setwd("~/code/thesis_R")
# ---------------------------
load("data.RData")
#         ###################################################

#     Install Packages ###################################################
# install.packages("mldr") # multilabel
# install.packages("neuralnet")
# install.packages("mlbench")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("nnet")
# install.packages("varSelRF")
#----------------------------

rm(list = ls())
rm(list= ( setdiff(ls(), 
                   c(
                     "best.forest.r",
                     "best.forest.l",
                     "colNames", 
                     "ndatapoint",
                     "mm.r",
                     "mm.l",
                     "bb.r",
                     "bb.l"
                   ))))

# --------------------
# "Include" files
source("lib/subsam.r")
source("lib/subML.r")

# --------------------

rawData <- read.csv(file="gaitData.csv", header=TRUE, sep=";")
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

# Correct class imbalance ###################################################
rsub.index <- subsam(rData)
rSub <- rData[rsub.index,]

lsub.index <- subsam(lData)
lSub <- rData[lsub.index,]

# Kalman filter ###################################################

source("lib/myKF.r")
source("lib/fdataset.r")
fSub.r <- fdataset(rSub)
fSub.l <- fdataset(lSub)

# # Normalization ###################################################
source("lib/ndataset.r")
normalization.l <- ndataset(fSub.l)
normalization.r <- ndataset(fSub.r)

fNSub.l <- normalization.l$ndata
b.l <- normalization.l$b
m.l <- normalization.l$m

fNSub.r <- normalization.r$ndata
b.r <- normalization.r$b
m.r <- normalization.r$m


# # Feature selection ###################################################
# library(mlbench)
# library(caret)
# library(randomForest)
# control <- rfeControl(functions=rfFuncs, method="cv", number=100)
# run the RFE algorithm
# results <- rfe(rTrainNN[,-ncol(rTrainNN)], rTrainNN[,ncol(rTrainNN)], sizes=c(1:(ncol(rTrainNN)-1)), rfeControl=control)

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

# ANN2 - nnet  ###################################################

# # ANN - neuralnet  ###################################################
# rm(rTrainNN,rTestNN,lTrainNN,lTestNN)
# library(neuralnet)
# source("allnumeric.r")
# 
# rTrainNN <- allnumeric(r.train)
# rTestNN <- allnumeric(r.test)
# lTrainNN <- allnumeric(l.train)
# lTestNN <- allnumeric(l.test)
# 
# n <- names(rTrainNN)
# f <- as.formula(paste("Right ~", paste(n[!n %in% "Right"], collapse = " + ")))
# nn <- neuralnet(f,data=rTrainNN,hidden=c(20),linear.output=T)
# pr.nn <- compute(nn,rTrainNN[-ncol(rTrainNN)])
# plot(rTrainNN$Right,pr.nn$net.result, col='red',main='Real vs predicted NN',pch=18,cex=0.7)
# abline(0,1,lwd=2)

# random forest  ###################################################
library(randomForest)
n <- names(r.train)
f <- as.formula(paste("Right ~", paste(n[!n %in% "Right"], collapse = " + ")))
iter.trees <- 10
iter.bags  <- 20

############################### RFC --> R-Side######
error <- matrix(0,iter.trees,iter.bags)
for (i in 1:iter.trees){
  for (j in 1:iter.bags){
    ntree <- 100*i
    output.forest <- randomForest(f, 
                                  data = r.train, 
                                  ntree = ntree, 
                                  mtry = 2*j, 
                                  test = r.test,
                                  do.trace = T
    )
    error[i,j] <- output.forest$err.rate[ntree,"OOB"]
    if (i==1 && j==1){
      best.forest <- output.forest
      mine <- error[i,j]
    }
    #confmat     <- output.forest$confusion[,-9]
    #class.error <- output.forest$confusion[, 9]
    #errors <- rowSums(confmat)
    #error <- (class.error %*% errors)/sum(confmat)
    
    if (mine > error[i,j]){
      mine <- error[i,j]
      best.forest.r <- output.forest
    }
  }
}

############################### RFC --> L-Side######
library(randomForest)
n <- names(l.train)
error <- matrix(0,iter.trees,iter.bags)
for (i in 1:iter.trees){
  for (j in 1:iter.bags){
    ntree <- 100*i
    output.forest <- randomForest(f, 
                                  data = l.train, 
                                  ntree = ntree, 
                                  mtry = 2*j, 
                                  test = l.test,
                                  do.trace = T
    )
    error[i,j] <- output.forest$err.rate[ntree,"OOB"]
    if (i==1 && j==1){
      best.forest <- output.forest
      mine <- error[i,j]
    }
    #confmat     <- output.forest$confusion[,-9]
    #class.error <- output.forest$confusion[, 9]
    #errors <- rowSums(confmat)
    #error <- (class.error %*% errors)/sum(confmat)
    
    if (mine > error[i,j]){
      mine <- error[i,j]
      best.forest.l <- output.forest
    }
  }
}

# normalize datapoint

source("lib/ndatapoint.R")
ndatapoint(r.test[1,-31], mm.r, bb.r)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(output.forest,type = 2)) 
varImpPlot(output.forest)

# Start the clock!
ptm <- proc.time()
Prediction <- predict(best.forest, r.test[-31], OOB=TRUE, type = "response") # whole test data set
Prediction <- predict(best.forest, r.test[200,-(n.features+1)], OOB=TRUE, type = "response") # a single data point
Prediction <- predict(best.forest, as.numeric(r.test[100,-(n.features+1)]), OOB=TRUE, type = "response") # a single data point
# Stop the clock
proc.time() - ptm


write.table(r.test[1,-51], file = "foo.csv", sep = ",", col.names = NA, qmethod = "double")


# 
# 
# 
# 
# # Multi-label classification
# mlDataRaw <- cbind(gaitData, rLabel, lLabel)
# mlData <- mldr::mldr_from_dataframe(mlDataRaw, labelIndices = c(167,168), name="multilabeldata")
# 
# mlSRaw <- subML(mlDataRaw)
# mlSub <- mldr::mldr_from_dataframe(mlSRaw, labelIndices = c(167,168), name="subsampledmultilabeldata")



