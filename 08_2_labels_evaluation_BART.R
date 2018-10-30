install.packages('dismo')
install.packages('bartMachine')
library(pROC)
require(dismo)
library(bartMachine)
options(java.parameters = "-Xmx5g")

data <- read.csv(file="data.2_labels.csv")

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
  
  #Train data
  set.seed(3)

  #normal bartMachine
  #data_train = bartMachine(data_all.train[,1:(ncol(data_all.train)-1)], data_all.train$class)
  #tuned bartMachine
  data_train = bartMachine(data_all.train[,1:(ncol(data_all.train)-1)], data_all.train$class, num_trees = 50, alpha = 0.9, beta = 1, k=2)
  
  prediction <- predict(data_train, data_all.test[,1:(ncol(data_all.test)-1)], type="class")
  
  #Generate confusion matrix
  table <- table(prediction, data_all.test$class)
  table
  
  #Store metrics
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
  precision <- c(precision,(table[2,2]/(table[2,2]+table[2,1])))
  recall <- c(recall,(table[2,2]/(table[2,2]+table[1,2])))
  
  fmeasure <- c(fmeasure, (2 * (table[2,2]/(table[2,2]+table[2,1])) * (table[2,2]/(table[2,2]+table[1,2])))/((table[2,2]/(table[2,2]+table[2,1])) + (table[2,2]/(table[2,2]+table[1,2]))))

  roc_obj <- roc(data_all.test$class, as.numeric(prediction))
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