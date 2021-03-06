install.packages(c('tree','ISLR','randomForest', 'e1071'))
install.packages('pROC')
install.packages('ROCR')
install.packages('klaR')
install.packages('nnet')
library(ROCR)
library(klaR)
library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(pROC)
library(nnet)

data <- read.csv(file="data.5_labels.csv")

#Define categorical attribute
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

set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- kfold(datarandom, k=10, by=datarandom$class) #make sure each class is distributed in each fold

#--------- Random Forest or Decision Trees ---------
#Initiate data structures to store metrics for each model
accuracy <- vector()
precision <- vector()
precision0 <- vector()
precision1 <- vector()
precision2 <- vector()
precision3 <- vector()
precision4 <- vector()
recall <- vector()
recall0 <- vector()
recall1 <- vector()
recall2 <- vector()
recall3 <- vector()
recall4 <- vector()
fmeasure <- vector()
fmeasure0 <- vector()
fmeasure1 <- vector()
fmeasure2 <- vector()
fmeasure3 <- vector()
fmeasure4 <- vector()
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
  #== CHOOSE CLASSIFIER ==
  #data_train = tree(data_all.train$class~.,data_all.train)
  #data_train <- randomForest(data_all.train$class~., data = data_all.train) 
  data_train <- naiveBayes(data_all.train$class~., data=data_all.train)
  #data_train <- svm(data_all.train$class~.,data_all.train, kernel = "linear")
  #data_train <- nnet(data_all.train$class~., data = data_all.train, size = 20, rang = 0.1, decay = 5e-4, maxit = 200)
  
  prediction <- predict(data_train, data_all.test, type="class")
  
  #Generate confusion matrix
  table <- table(prediction, data_all.test$class)
  table
  
  #Store metrics
  accuracy <- c(accuracy,(table[1,1]+table[2,2]+table[3,3]+table[4,4]+table[5,5])/sum(sum(table)))
  
  precision0 <- c(precision0,(table[1,1]/(table[1,1] + table[1,2] + table[1,3] + table[1,4] + table[1,5])))
  precision1 <- c(precision1,(table[2,2]/(table[2,1] + table[2,2] + table[2,3] + table[2,4] + table[2,5])))
  precision2 <- c(precision2,(table[3,3]/(table[3,1] + table[3,2] + table[3,3] + table[3,4] + table[3,5])))
  precision3 <- c(precision3,(table[4,4]/(table[4,1] + table[4,2] + table[4,3] + table[4,4] + table[4,5])))
  precision4 <- c(precision4,(table[5,5]/(table[5,1] + table[5,2] + table[5,3] + table[5,4] + table[5,5])))
  
  recall0 <- c(recall0, (table[1,1]/(table[1,1] + table[2,1] + table[3,1] + table[4,1] + table[5,1])))
  recall1 <- c(recall1, (table[2,2]/(table[1,2] + table[2,2] + table[3,2] + table[4,2] + table[5,2])))
  recall2 <- c(recall2, (table[3,3]/(table[1,3] + table[2,3] + table[3,3] + table[4,3] + table[5,3])))
  recall3 <- c(recall3, (table[4,4]/(table[1,4] + table[2,4] + table[3,4] + table[4,4] + table[5,4])))
  recall4 <- c(recall4, (table[5,5]/(table[1,5] + table[2,5] + table[3,5] + table[4,5] + table[5,5])))
  
  precision0[is.na(precision0)] <- 0
  precision1[is.na(precision1)] <- 0
  precision2[is.na(precision2)] <- 0  
  precision3[is.na(precision3)] <- 0  
  precision4[is.na(precision4)] <- 0  
  
  recall0[is.na(recall0)] <- 0
  recall1[is.na(recall1)] <- 0  
  recall2[is.na(recall2)] <- 0  
  recall3[is.na(recall3)] <- 0  
  recall4[is.na(recall4)] <- 0
  
  fmeasure0 <- c(fmeasure0, (2 * precision0 * recall0)/(precision0 + recall0))
  fmeasure1 <- c(fmeasure1, (2 * precision1 * recall1)/(precision1 + recall1))
  fmeasure2 <- c(fmeasure2, (2 * precision2 * recall2)/(precision2 + recall2))
  fmeasure3 <- c(fmeasure3, (2 * precision3 * recall3)/(precision3 + recall3))
  fmeasure4 <- c(fmeasure4, (2 * precision4 * recall4)/(precision4 + recall4))
  
  fmeasure0[is.na(fmeasure0)] <- 0
  fmeasure1[is.na(fmeasure1)] <- 0
  fmeasure2[is.na(fmeasure2)] <- 0
  fmeasure3[is.na(fmeasure3)] <- 0
  fmeasure4[is.na(fmeasure4)] <- 0
  
  aucs = c()
  lvls = levels(data_all.test$class)
  for (type.id in 1:5) 
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
precision <- c(precision, mean(precision3, na.rm=TRUE))
precision <- c(precision, mean(precision4, na.rm=TRUE))
precision
precisionaverage <- mean(precision, na.rm=TRUE)


#recall
recall <- c(recall, mean(recall0, na.rm=TRUE))
recall <- c(recall, mean(recall1, na.rm=TRUE))
recall <- c(recall, mean(recall2, na.rm=TRUE))
recall <- c(recall, mean(recall3, na.rm=TRUE))
recall <- c(recall, mean(recall4, na.rm=TRUE))
recall
recallaverage <- mean(recall, na.rm=TRUE)


#fmeasure
fmeasure <- c(fmeasure, mean(fmeasure0, na.rm=TRUE))
fmeasure <- c(fmeasure, mean(fmeasure1, na.rm=TRUE))
fmeasure <- c(fmeasure, mean(fmeasure2, na.rm=TRUE))
fmeasure <- c(fmeasure, mean(fmeasure3, na.rm=TRUE))
fmeasure <- c(fmeasure, mean(fmeasure4, na.rm=TRUE))
fmeasure
fmeasureaverage <- mean(fmeasure, na.rm=TRUE)

#aucaverage
aucaverage <- mean (auc, na.rm = TRUE)

accuracyaverage
precisionaverage
recallaverage
fmeasureaverage
aucaverage


