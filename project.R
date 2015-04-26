library(caret)
library(dplyr)

setwd("U:/My Documents/R/MachineLearningProject")

loadData<-function() {
  trainData<<-read.csv(file = 'pml-training.csv', header = TRUE)
  testData<<-read.csv(file = 'pml-testing.csv', header = TRUE)
}

cleanUp<-function(data, nzv) {
  data<-dplyr::select(data , -one_of(nzv), -(X:num_window))
  if (exists("classe", data)) {
    data$classe<-as.factor(data$classe)
  }
  return(data)
}

run<-function() {
  loadData()
  nzv<-names(trainData)[nearZeroVar(trainData,saveMetrics=FALSE)]
  nzv<-c(nzv,names(testData)[nearZeroVar(testData,saveMetrics=FALSE)])
  trainData<-cleanUp(trainData, nzv)
  testData<-cleanUp(testData, nzv)
  
  inTrain<-createDataPartition(y=trainData$classe, p=0.7, list=FALSE)
  StrainData<-trainData[inTrain,]
  cvTestData<-trainData[-inTrain,]
  
  
  fitControl <- trainControl(method = "none")
  fitControl <- trainControl(method = "cv", number = 4)
  fitControl <- trainControl(method = "repeatedcv", number = 10,  repeats = 10)

  #rpart accuracy
  fit<-train(classe~., data=trainData, method="rpart", trControl = fitControl, tuneGrid=tgrid)
  
  
  #gbm accuracy: 0.9737028
  tgrid <- expand.grid(n.trees=c(200), interaction.depth=c(3), shrinkage = c(0.3))
  fit<-train(classe~., data=trainData, method="gbm", trControl = fitControl, tuneGrid=tgrid)

  #random forest acc: 0.9906228
  tgrid <- expand.grid(mtry=c(6))
  fit<-train(classe~., data=trainData, method="rf", trControl = fitControl, tuneGrid=tgrid)

  tgrid <- expand.grid(size=c(12), decay=c(0.3))
  fit<-train(classe~., data=trainData, method="nnet", maxit=10000, trControl = fitControl, tuneGrid=tgrid)
  
  tgrid <- expand.grid(size=c(12, 23,45))
  fit<-train(classe~., data=sample_n(StrainData,1000), method="mlp", trControl = fitControl, tuneGrid=tgrid)
  
  fit<-train(classe~., data=sample_n(StrainData,1000), method="neuralnet", trControl = fitControl, tuneGrid=tgrid)
  
  fit<-train(classe~., data=sample_n(StrainData,1000), method="pcaNNet", trControl = fitControl)
  
  tgrid <- expand.grid(.decay = c(0.3, 0.1), .size = c(4,5), .bag=c(T,F))
  fit<-train(classe~. , data=sample_n(StrainData,1000), method="avNNet", trControl = fitControl, tuneGrid=tgrid)
  
}

other<-function() {
#split data
inTrain<-createDataPartition(y=allTrainingData$classe, p=0.7, list=FALSE)
training<-allTrainingData[inTrain,]
testing<-allTrainingData[-inTrain,]

#featurePlot(x=training[,-160], y=training$classe)
featurePlot(x=training[,10:11], y=training$classe, plot='pairs')


#PCA
preProc <- preProcess(training[,-c(1:7,106)],method=c("knnImpute","pca"), thresh = 0.8)
trainingPC <- predict(preProc,training[,-c(1:7,106)])
}

makeSubmission <- function() {
  predictions<-predict(fit, testData)
  for (i in 1:20) {
    s <- as.character(predictions[i])
    write(s, file = paste("prediction_",i,".txt", sep=""))
  }
}