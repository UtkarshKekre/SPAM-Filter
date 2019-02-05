### Load the data into R
data1 <- read.csv(file.choose(),header = FALSE, sep = ";")
## Choose file SPAM_Data.csv
names <- read.csv(file.choose(),header = F, sep = ";")
## Choose file names.csv
names(data1) <- sapply((1:nrow(names)),function(i)toString(names[i,1]))

data1$y <- as.factor(data1$y)

sample <- dataset[sample(nrow(data1),1000),]

### Build a SPAM filter with R
install.packages("caret")

require(caret)

install.packages("kernlab")

require(kernlab)

install.packages("domc")

require(doMC)

trainIndex <- createDataPartition(sample$y,p=.8,list=FALSE, times =1 )

dataTrain <- sample[trainIndex,]

dataTest <- sample[-trainIndex,]

registerDoMC(cores=5)

### finding optimal value of a tuning parameter
sigDist <- sigest(y ~ ., data = dataTrain, frac = 1)
### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))

### Train the SVM

x <- train(y ~ .,
     data = dataTrain,
     method = "svmRadial",
     preProc = c("center", "scale"),
     tuneGrid = svmTuneGrid,
     trControl = trainControl(method = "repeatedcv", repeats = 5, 
     classProbs =  TRUE))

### Evaluation

pred <- predict(x,dataTest[,1:57])

acc <- confusionMatrix(pred,dataTest$y)
