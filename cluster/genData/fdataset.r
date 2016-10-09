fdataset <- function(dataset){
  # out <- dataset[-c(ncol(dataset), ncol(dataset)-1)] # do not include gender and label 
  defaultx = integer(3) # zeros
  defaultP <- diag(1,3) # 3x3 identity
  Ts = 0.03 # assume constant sampling time
  nombres <- colnames(dataset)
  print("Starting Kalman Filtering")
  for (i in 1:(ncol(dataset)-2)){ # ignore gender and label
    name <- nombres[i]
    data <- dataset[,i]
    
    x     <- defaultx
    P     <- defaultP
    R     <- 0.01
    sigma <- 1
    fil <- x
    print(paste(round(i/ncol(dataset)*100,digits = 0), "% progress"))
    for (j in 1: nrow(dataset)){
      z <- data[j]
      kfout <- myKF(Ts, z, x, P, R, sigma)
      x <- kfout$x
      P <- kfout$P
      fil <- rbind(fil, t(x))
    }
    fil <- fil[-1,]
    filData <- data.frame(fil)
    colNames <- paste(name, c("_x", "_v", "_a"), sep = "")
    colnames(filData) <- colNames
    if (i == 1){
      out <- filData
    }else{out <- cbind(out,filData)}
  }
  out <- cbind(out, dataset$Gender, dataset[,ncol(dataset)])
  colnames(out)[ncol(out)-1]<- colnames(dataset)[ncol(dataset)-1]
  colnames(out)[ncol(out)  ]<- colnames(dataset)[ncol(dataset)]
  return(out)
  print("Finisehd Kalman filtering")
}