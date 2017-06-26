############################################
#### We refer to folds as buckets, just more illustrative
#### This is for the purpose of cross validation

numberOfBuckets <- 10
cnames <- c("comment", "num", "num", 
	"num", "num", "num", "num", "num", "class")
medianAndDeviation <- data.frame()
fileName <- "../book-machine learning/chapter 8/mpg.txt"
bucketName <- "bucket"
seperator <- ","

#### retrieve the file
fileData <<- read.delim(fileName, 
	sep=seperator, head=FALSE)
colnames(fileData) <- cnames
classColumn <- length(fileData)


#### shuffle the data
fileData <- fileData[sample(nrow(fileData)), ]

#### create buckets in a list with names
bucketKeys <- vector()
for (i in 1:numberOfBuckets){
	bucketKeys <- c(bucketKeys, paste(bucketName, i, sep=""))
}
bucketList <- vector(mode="list", length=length(bucketKeys))
names(bucketList) <- bucketKeys


#################
### list of buckets
bucketKeys <- vector()
for (i in 1:numberOfBuckets){
	bucketKeys <- c(bucketKeys , paste(bucketName, i, sep=""))
}
bucketList <- vector(mode="list", length=length(bucketKeys))
names(bucketList) <- bucketKeys 


#################
#################
#### list of dataframes class by class

### first convert the class to factor, its just convenient 
fileData$class <- as.factor(fileData$class)
numberOfClasses = length(levels(fileData$class))
#### creating names for each list of dataframes
ckeys <- vector()
for (i in 1:numberOfClasses){
	ckeys <- c(ckeys, paste("class", i, sep=""))
}
clist <- vector(mode="list", length=length(ckeys))
names(clist) <- ckeys

################
#### populate the populate each class' list
#### with its own instances 

classValue = 1
for (i in names(clist)){
	if(paste(i) == paste("class", classValue, sep="")){
		clist[[i]] <- fileData[fileData$class == classValue, ]
	}
	classValue <- classValue + 1
}
#### How the list list looks now 
clist







################
#### populate the buckets

#### get the class largest number
maxClass <- max(as.data.frame(lapply(clist, function(x) nrow(x))))

#### new data frame generated from the 
newFile <- data.frame()

#### populate te dataframe
for (i in 1:maxClass){
	# pick the ith value from each class
	combined <- lapply(clist, function(class) class[i,])
	temp <- data.frame()
	# attach complete cases (instances) to temp dataframe
	for (value in combined){
		temp <- rbind(temp, value[complete.cases(value), ])
	}
	newFile <- rbind(newFile, temp)
}


#### imagine a conveyor belt or any chain setting where buckets 
#### are supporsed to be filled with a product. Say ice cream, 
#### but we want the colors in a particular order, in 10 buckets.
#### For this case, the colors at the source are already stacked in order, 
#### therefore the buckets should arrive in the order that will have 
#### the color equally distributed {[red, blue, white], [red, blue, white],...}
#### 3 colors into first, next 3 colors, ...,
#### However sometimes because of a shortage, the sequence of colors may break
#### So we just pick from the source in the same fashion irrespective 
#### of the color {[red, white, red], [white, red, white], [white, white, white],...}
#### 2 colors for 3 into next,..., 1 color for 3,..., until the source is empty
 
bucketWheel <- function(frame, bn){
	if((bn %% 11) == 0 || bn == 0){
		bn <- 1
	}
	# filling the (bn)th with color
	bucketList[[bn]] <<- rbind(bucketList[[bn]], frame[complete.cases(frame), ])
	
	bn <- (bn + 1) %% 11
	return(bn)
}

#### keep a copy of newFile dataframe
v <- newFile 
#### keep track of the bucketList
bn <- 1
#### the range of rows that will be put into each bucket
range <- nrow(v[1:3,])
#### the number of iterations in the loop
loop <- nrow(v)
while (loop){
	bn <- bucketWheel(v[1:range, ], bn)
	# update the number of iterations
	v <- v[-c(1:range), ]
	loop <- nrow(v)
}

#### check the number of rows in the buckets if similar to the original
sum(as.data.frame(lapply(bucketList, function(x) nrow(x))))








