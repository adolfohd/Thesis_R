allnumeric <- function(dataset){
  out <- dataset
  # out$Gender <- as.numeric(as.character(dataset$Gender))
  out[ncol(dataset)] <- as.numeric(as.character(dataset[,ncol(dataset)]))
  return(out)
}