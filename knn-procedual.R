#### from knn-procedual-createFolds.R get the bucketList
medianAndDeviation <- data.frame()
##############################
#### getting the median 
##############################
getMedian <- function(numCol){
	numCol <- sort(numCol)
	colLen <- length(numCol)

	if(colLen %% 2 == 1){
		numCol <- numCol[((length(numCol) + 1)/2)]
	}else{	
		v1 <- numCol[((length(numCol) + 1)/2)]
		v2 <- numCol[((length(numCol) + 1)/2)+1]
		numCol <- (v1 + v2) / 2.0
	}
	
}

################################
#### getting the absolute standard deviation
################################
getAbsoluteStandardDeviation <- function(numCol, median){
	sum = 0
	for (item in numCol){
		sum <- (sum + abs(item - median))
	}
	return(sum / length(numCol))
}

################################
#### Normalize a column
################################
normalizeColumn <- function(colNumber, set){
	col <- set[,colNumber]
	median <- getMedian(col)
	asd <- getAbsoluteStandardDeviation(col, median)
	medianAndDeviation <<- rbind(
			medianAndDeviation , cbind(median, asd))
	
	for (i in 1:length(col)){
		col[i] <- (col[i]-median)/asd
	}
	return(col)
}


#################################
#### Normalize a vector
#################################
normalizeVector <- function(v){
	responseVector <- vector()
	y <- 1
	for (i in v[,c(-1, -length(v))]){
		median <- medianAndDeviation[y,]$median
		asd <- medianAndDeviation[y,]$asd
		responseVector <- cbind(responseVector, (i-median)/asd)
		
		y <- 1 + y
	}
	v[c(-1, -length(v))] <- responseVector
	return(v)
}

#################################
#### calculate the manhattan distance
#################################
manhattanDistance <- function(vector1, vector2){
	dist <- data.frame()
	for (j in 1:nrow(vector2)){
			dist <- rbind(dist, c(j, sum(abs(vector1 - vector2[j,]))))
	
	}
	return(dist)
}


#####################################
#### get the nearest neighbor
#####################################
nearestNeighbor <- function(itemVector, id){
	
	distanceVector <- data.frame()
	distanceVector <- 
		rbind(distanceVector, 
			manhattanDistance(itemVector, 
				cycleDataFrame[c(-1, -length(cycleDataFrame))]))
	
	neighbor <- distanceVector[distanceVector[,2] == min(distanceVector[,2]), ]
	class <- cycleDataFrame[neighbor[,1],][length(cycleDataFrame)]
	classFrame <- data.frame(class, neighbor[,1], neighbor[,2], id)
	names(classFrame) <- c("newClass", "key", "distance", "name", "origClass")
	return (classFrame)
}


################################
#### classify instances
################################
classify <- function(itemVector, normalizedBuckets){
	#### return (nearestNeighbor(normalizeVector(itemVector), normalizedBuckets))
	return (nearestNeighbor(itemVector, normalizedBuckets))
}


################################
#### initiate cross validation
################################
cycleBuckets <- function(testBucket, bucketList){
	bucketList[testBucket] <- NULL
	frame <- do.call(rbind, bucketList)
	numericalCols <- frame[c(-1, -length(frame))]
	for (column in 1:length(numericalCols)){
		numericalCols[,column] <- normalizeColumn(column, numericalCols)
	}
	frame[c(-1, -length(frame))] <- numericalCols
	return(frame)
	

}




###############################
#### putting it all together
###############################
#### the outter loop makes identifies the bucket that will 
#### be used for testing

#### The method is indeed mechanical by this time
#### as we try to explain each step
#### There is no optimization at all
#### thus the algorithm runs for a few minutes :)

cycleDataFrame <- data.frame()
vecDists <- data.frame()
for (testBucket in names(bucketList)){
	medianAndDeviation <- data.frame()
	
	#### reserving the data frame for testing
	testFrame <- bucketList[[testBucket]]
	
	#### we skip the testBucket and
	#### return the cycle's data frame
	cycleDataFrame <- cycleBuckets(testBucket, bucketList)
	
	#### normalize the testFrame
	vec <- normalizeVector(testFrame)
	#### clusifying
	for (i in 1:nrow(vec)){
		vecDists <- rbind(vecDists, nearestNeighbor(vec[i,c(-1,-length(vec))], vec[i,c(1,length(vec))]))
	}	
	
}


nrow(vecDists)
names(vecDists)

#### get the confusion matrix [although we are using a table :) serves the purpose]
confusionMatrix <- table(vecDists$newClass, vecDists$origClass)

#### get the number of correctly classified instances 
correct <- diag(confusionMatrix)

#### Check the Accuracy 
(sum(correct) / nrow(vecDists))*100


