library(ROCR)
library(klaR)
library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(pROC)

data <- read.csv(file="data.3_labels.csv")

data$sex <- as.factor(data$sex)
data$fbs <- as.factor(data$fbs)
data$exang <- as.factor(data$exang)
data$cp <- as.factor(data$cp)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)
data$restecg <- as.factor(data$restecg)
data$class <- as.factor(data$class)

#== RUN TO CREATE SUBSET 1 & 2 ==
#data$ca <- NULL

#== RUN TO CREATE SUBSET 2 ==
#data$trestbps <- NULL
#data$chol <- NULL
#data$fbs <- NULL
#data$restecg <- NULL
#data$sex <- NULL
#data$thalach <- NULL


#Create 10-folds
set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- kfold(datarandom, k=10, by=datarandom$class) #make sure each class is distributed in each fold

#Initiate data structures to store metrics for each model
accuracy <- vector()
precision <- vector()
precision0 <- vector()
precision1 <- vector()
precision2 <- vector()
recall <- vector()
recall0 <- vector()
recall1 <- vector()
recall2 <- vector()
fmeasure <- vector()
fmeasure0 <- vector()
fmeasure1 <- vector()
fmeasure2 <- vector()
auc <- vector()

for (i in 1:10)
{
  #Get one fold for testing
  testIndexes <- which(folds==i,arr.ind=TRUE) 
  #Get a different fold for training
  trainIndexes <- which(folds!=i,arr.ind=TRUE) 
  #Use the test and train data partitions 
  data_all.test <- datarandom[testIndexes, ] 
  data_all.train <- datarandom[trainIndexes, ] 
  
  #Train data
  set.seed(3)
  #data_train = tree(data_all.train$class~.,data_all.train)
  #data_train <- randomForest(data_all.train$class~., data = data_all.train) 
  data_train <- naiveBayes(data_all.train$class~., data=data_all.train)
  #data_train <- svm(data_all.train$class~.,data_all.train, kernel = "linear") 
  prediction <- predict(data_train, data_all.test, type="class")
  
  #Generate confusion matrix
  table <- table(prediction, data_all.test$class)
  table
  
  #Store metrics
  accuracy <- c(accuracy,(table[1,1]+table[2,2]+table[3,3])/sum(sum(table)))
  
  precision0 <- c(precision0,(table[1,1]/(table[1,1] + table[1,2] + table[1,3])))
  precision1 <- c(precision1,(table[2,2]/(table[2,1] + table[2,2] + table[2,3])))
  precision2 <- c(precision2,(table[3,3]/(table[3,1] + table[3,2] + table[3,3])))
  
  recall0 <- c(recall0, (table[1,1]/(table[1,1] + table[2,1] + table[3,1])))
  recall1 <- c(recall1, (table[2,2]/(table[1,2] + table[2,2] + table[3,2])))
  recall2 <- c(recall2, (table[3,3]/(table[1,3] + table[2,3] + table[3,3])))
  
  precision0[is.na(precision0)] <- 0
  precision1[is.na(precision1)] <- 0
  precision2[is.na(precision2)] <- 0  
  
  recall0[is.na(recall0)] <- 0
  recall1[is.na(recall1)] <- 0  
  recall2[is.na(recall2)] <- 0  
  
  fmeasure0 <- c(fmeasure0, (2 * tail(precision0, n=1) * tail(recall0, n=1))/(tail(precision0, n=1) + tail(recall0, n=1)))
  fmeasure1 <- c(fmeasure1, (2 * tail(precision1, n=1) * tail(recall1, n=1))/(tail(precision1, n=1) + tail(recall1, n=1)))
  fmeasure2 <- c(fmeasure2, (2 * tail(precision2, n=1) * tail(recall2, n=1))/(tail(precision2, n=1) + tail(recall2, n=1)))
  
  fmeasure0[is.na(fmeasure0)] <- 0
  fmeasure1[is.na(fmeasure1)] <- 0
  fmeasure2[is.na(fmeasure2)] <- 0
  
  aucs = c()
  lvls = levels(data_all.test$class)
  for (type.id in 1:3) 
  {
    type = as.factor(data_all.test$class == lvls[type.id])
    
    score = as.numeric(prediction)
    actual.class = data_all.test$class == lvls[type.id]
    
    pred = prediction(score, actual.class)
    perf = performance(pred, "tpr", "fpr")
    
    nbauc = performance(pred, "auc")
    nbauc = unlist(slot(nbauc, "y.values"))
    aucs[type.id] = nbauc
  }
  auc <- c(auc, mean(aucs))
}

#accuracy
accuracyaverage = mean(accuracy)


#precision
precision <- c(precision, mean(precision0, na.rm=TRUE))
precision <- c(precision, mean(precision1, na.rm=TRUE))
precision <- c(precision, mean(precision2, na.rm=TRUE))
precision
precisionaverage <- mean(precision, na.rm=TRUE)


#recall
recall <- c(recall, mean(recall0, na.rm=TRUE))
recall <- c(recall, mean(recall1, na.rm=TRUE))
recall <- c(recall, mean(recall2, na.rm=TRUE))
recall
recallaverage <- mean(recall, na.rm=TRUE)


#fmeasure
fmeasure <- c(fmeasure, mean(fmeasure0, na.rm=TRUE))
fmeasure <- c(fmeasure, mean(fmeasure1, na.rm=TRUE))
fmeasure <- c(fmeasure, mean(fmeasure2, na.rm=TRUE))
fmeasure
fmeasureaverage <- mean(fmeasure, na.rm=TRUE)

#aucaverage
aucaverage <- mean (auc, na.rm = TRUE)

accuracyaverage
precisionaverage
recallaverage
fmeasureaverage
aucaverage
