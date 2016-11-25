


best.rfc <- function(list.of.files, data.folder){ 
  i_max = 8
  j_max = 5
  forests <- list()
  min.error = 1;
  error <- matrix(NA,i_max,j_max)
  for (i in 1:i_max){
    for (j in 1:j_max){
      current.file.name <- list.of.files[   (i-1)*j_max+j   ]
      current.file.path <- paste(data.folder,current.file.name, sep="")
      print(current.file.path)
      file.data <- file.info(current.file.path)
      print(file.data$size)
      if (file.data['size'] == 0)
      {
        warning(paste("WARNING! Empty file: ", current.file.path))
      }
      else
      {
        load(current.file.path)
        # print(output.forest)
        error[i,j] <- output.forest$err.rate[output.forest$ntree, "OOB"]
        if (error[i,j] < min.error){ 
          min.error <- error[i,j]
          best.forest <- output.forest  
        }
      }
    }
  }
  output <- list(best.forest = best.forest, errors = error)
  
  return(output)
}