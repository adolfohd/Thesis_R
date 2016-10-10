subsam <- function(dataSet) {
    # function [subData, subLabels] = subSampleDataset(dataSet, labels)
	# subData = zeros(1,size(dataSet,2));
	# subLabels = 0;
	i <- 1;
	first <- TRUE
	labels <- dataSet[ncol(dataSet)]
  
	print("Sub-sampling")
	while (i <= nrow(dataSet)){
	  print(paste(floor(i/nrow(dataSet)*100), "% progress"))
	  if (labels[i,1] == 1){
	    if (first){
	      index <- seq(i-5,i)
	      first <- FALSE	      		
	    }else{
	      index <- c(index, (i-5):i)
	    }
	    i<-i+1
	    while ( (!is.nan(labels[i,1])) & (labels[i,1] != 0)) {
	      index <- c(index, i)
	      # subLabels = [subLabels; labels(i)];
	      i<-i+1
	    }
	    index <- c(index, i:(i+5))
	    i <- i + 5;
	  }
	  i <- i+1;
	}
	  
# 	while (i <= nrow(dataSet)){
# 	    print(i)
# 	   	if (labels[i,1] == 1){
# 	    	  if (first){
#   	      		subData <- dataSet[(i-5:i),];
#   	      		first = FALSE	      		
#       		}else{
#       			  subData <- rbind(subData, dataSet[i-5:i,]);
#       		}
# 	      	i<-i+1
# 	      	while ( (!is.nan(labels[i,1])) & (labels[i,1] != 0)) {
#   	          subData <- rbind(subData, dataSet[i,])
#   	          # subLabels = [subLabels; labels(i)];
#   	          i<-i+1
# 	      	}
# 	      	for (j in seq (i,i+5)){
# 	          	subData = rbind(subData, dataSet[j,])
# 	          	# subLabels = [subLabels; labels(j)]
# 	      	}
# 	      	i <- i + 5;
# 	   	}
# 	   	i <- i+1;
# 	}
	print("Sub-sampling complete")
	return(index)
}

