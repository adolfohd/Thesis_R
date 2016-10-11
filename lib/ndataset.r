ndataset <- function(dataset){

  # normalize dataset
  
  data <- dataset[-c(ncol(dataset)-1, ncol(dataset))] # ignore gender and Right ... assuming Gender and //side//
  
  ndata <- data*0
  
 
  
  # ndata <- as.data.frame(scale(data)) 
  mm <- numeric(ncol(data))
  bb <- mm
  for (i in 1:ncol(data)){
    m <- min(data[,i])
    M <- max(data[,i])
    
    sh <- (m+M)/2 # shift
    sc <- abs(m-M)/2
    
    ndata[,i] <- as.data.frame((data[,i]-sh)/sc)
    mm[i] <- sc
    bb[i] <- sh
  }
  names(mm) <- names(data)
  names(bb) <- names(data)
  
  ndata[c(ncol(dataset)-1, ncol(dataset))] <- dataset[c(ncol(dataset)-1, ncol(dataset))] # reappend Gender and Right
  
  output <- list(
    ndata= ndata,
    m = mm,
    b = bb
  )
  
  return(output)
  
}