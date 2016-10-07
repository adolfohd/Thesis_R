ndatapoint <- function(datapoint, m, b){
  
  output <- numeric(ncol(datapoint))
  names(output) <- names(datapoint)
  for (i in 1:ncol(datapoint)){
    current.name <- names(datapoint)[i]
    # print(current.name)
    # print(datapoint[current.name])
    # print(b[current.name])
    # print(m[current.name])
    output[current.name] <- 
      (datapoint[current.name] - b[current.name])/m[current.name]
    # print("output")
    # print(output[current.name])
  }
  
  return(output)
  
}