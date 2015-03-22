#Setting the local working dir
setwd("D:/Google Drive/Dottorato/Anno I/Corsi/Practical Machine Learning/Progetto")

#This package works only on Windows OS
#install.packages("doParallel")

library(caret)
library(kernlab)
library(randomForest)
#Trying to give boost to the operations using parallel computations
library(doParallel)
registerDoParallel(cores = 4)

#Reading the pml-training csv file for training and
#cleaning the data by removing columns with NAs etc
training<-read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
#The same to the pml-testing data
testing<-read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

#removing the first five columns
training<-training[,-c(1:5)]

#Calculating mean for each integer and numerics columns
#assigning means to NAs
for (colName in names(training)){
  
  if(class(training[[colName]]) %in% c("integer", "numeric")){
    m<-mean(training[[colName]], na.rm = TRUE)
    training[[colName]][is.na(training[[colName]])]<-m
  }
  
}

training<-training[,-nearZeroVar(training)]

index <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
trainingData <- training[index, ]
testingData <- training[-index, ]

train_control <- trainControl(method="cv", number=3)
model <- train(classe~., data=trainingData, method="rf", trControl = train_control)

answers <- predict(model, trainingData, type="raw")
c<-confusionMatrix(answers, trainingData$classe)
print("Calculated in sample error rate")
print(1-c[["overall"]][["Accuracy"]])

answers <- predict(model, testingData, type="raw")
c<-confusionMatrix(answers, testingData$classe)
print("Calculated out of sample error rate")
print(1-c[["overall"]][["Accuracy"]])

#generate data for submitting
answers <- predict(model, testing, type="raw")

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers)