subML <- function(dataSet) {
    # function [subData, subLabels] = subSampleDataset(dataSet, labels)
	# subData = zeros(1,size(dataSet,2));
	# subLabels = 0;
	i <- 1;
	first <- TRUE
	lR <- dataSet[ncol(dataSet)]
  lL <- dataSet[ncol(dataSet)-1]
	
	while (i <= nrow(dataSet)){
	  print(i)
	  if (lR[i,1] == 1 || lL[i,1] == 1 ){
	    if (first){
	      index <- seq(i-5,i)
	      first <- FALSE	      		
	    }else{
	      index <- c(index, (i-5):i)
	    }
	    i<-i+1
	    while ( (lR[i,1] != 0) && (lL[i,1] != 0)) {
	      index <- c(index, i)
	      # subLabels = [subLabels; labels(i)];
	      i<-i+1
	    }
	    index <- c(index, i:(i+5))
	    i <- i + 5;
	  }
	  i <- i+1;
	}
	index
	return(dataSet[index,])
}