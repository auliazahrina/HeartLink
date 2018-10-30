install.packages("SuperLearner")
install.packages("caret")
install.packages("ranger")
install.packages("kernlab")
install.packages("dplyr")
#install.packages('bartMachine')
#install.packages("dismo")
#install.packages('pROC')
#install.packages('ROCR')
library(SuperLearner)
library(caret)
library(bartMachine)
library(ranger)
library(kernlab)
library(dismo)
library(dplyr)
library(pROC)
library(ROCR)

data <-read.csv(file="data.2_labels.csv")

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

#Create tune function for ranger classifier
SL.ranger.tune <- function(...){
  SL.ranger(..., num.trees=1000, mtry=2)
}

#Create 10-folds
set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- kfold(datarandom, k=10, by=datarandom$class) #make sure each class is distributed in each fold

#Initiate data structures to store metrics for each model
accuracy <- vector()
precision <- vector()
recall <- vector()
fmeasure <- vector()
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
  
  #Prepare data
  y <- as.numeric(data_all.train[,ncol(data_all.train)])-1
  ytest <- as.numeric(data_all.test[,ncol(data_all.test)])-1
  x <- data.frame(data_all.train[,1:(ncol(data_all.train)-1)])
  xtest <- data.frame(data_all.test[,1:(ncol(data_all.test)-1)])
  
  #Train data
  set.seed(150)
  
  model <- SuperLearner(y,
                        x,
                        family = binomial(),
                        SL.library=list("SL.ksvm",
                                        "SL.bartMachine",
                                        "SL.ranger.tune"))

  predictions <- predict.SuperLearner(model, newdata=xtest)
  
  #Recode probabilities (dplyr package)
  conv.preds <- ifelse(predictions$pred>=0.5,1,0)
  preds <- conv.preds[,1]
  #Create confusion matrix (caret package)
  cm <- confusionMatrix(as.factor(preds), as.factor(ytest))
  
  table <- as.table(cm)
  table
  
  #Store metrics
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
  precision <- c(precision,(table[2,2]/(table[2,2]+table[2,1])))
  recall <- c(recall,(table[2,2]/(table[2,2]+table[1,2])))
  
  fmeasure <- c(fmeasure, (2 * (table[2,2]/(table[2,2]+table[2,1])) * (table[2,2]/(table[2,2]+table[1,2])))/((table[2,2]/(table[2,2]+table[2,1])) + (table[2,2]/(table[2,2]+table[1,2]))))
  
  roc_obj <- roc(as.factor(ytest), preds)
  auc <- c(auc,auc(roc_obj))
}
#accuracy
accuracyaverage = mean(accuracy)

#precision
precision
precisionaverage <- mean(precision, na.rm=TRUE)


#recall
recall
recallaverage <- mean(recall, na.rm=TRUE)


#fmeasure
fmeasure
fmeasureaverage <- mean(fmeasure, na.rm=TRUE)

#aucaverage
auc
aucaverage <- mean (auc, na.rm = TRUE)

accuracyaverage
precisionaverage
recallaverage
fmeasureaverage
aucaverage