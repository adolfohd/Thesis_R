separate.output <- function(dataset, gait.phases){
  
  data <- dataset[-ncol(dataset)]
  labels <- dataset[ncol(dataset)]
  for (i in 0:7){
    # column.name <- paste(
    #   names(dataset[ncol(dataset)]), ".",
    #   gait.phases[i], sep = "")
    column.name = gait.phases[i+1]
    data[column.name] = as.numeric(labels == i)
  }
  return(data)
}