#load libraries
install.packages(c('ROCR', 'e1071', 'pROC', 'klaR', 'dismo'))
library(ROCR)
library(e1071)
library(pROC)
library(klaR)
require(dismo)

data_train <- read.csv(file="data_train.csv")
View(data_train)
str(data_train)
summary(data_train)

#convert categorical variables
data_train$sex <- as.factor(data_train$sex)
data_train$cp <- as.factor(data_train$cp)
data_train$fbs <- as.factor(data_train$fbs)
data_train$exang <- as.factor(data_train$exang)
data_train$slope <- as.factor(data_train$slope)
data_train$ca <- as.factor(data_train$ca)
data_train$thal <- as.factor(data_train$thal)
data_train$class <- as.factor(data_train$class)

#testing data subsets 
data_train_1 <- data_train
data_train_2 = data_train[complete.cases(data_train[, -12]), -12]
data_train_3 = data_train[complete.cases(data_train[, -c(4, 5, 6, 7, 12)]), -c(4, 5, 6, 7, 12)]
data_train_4 = data_train[complete.cases(data_train[, -c(1, 2, 4, 5, 6, 7, 8, 9, 11, 12)]), -c(1, 2, 4, 5, 6, 7, 8, 9, 11, 12)]

#create empty vectors
clearVectors <- function(){
  accuracy <- vector()
  
  precision0 <- vector()
  precision1 <- vector()
  precision2 <- vector()
  precision3 <- vector()
  precision4 <- vector()
  
  recall0 <- vector()
  recall1 <- vector()
  recall2 <- vector()
  recall3 <- vector()
  recall4 <- vector()
  
  aucs <- c()
}

#SVM Function, where x is the data set and y is the kernel
#Calculates evaluation metrics accuracy, precision, recall, f-measure
svmAndEvalMetrics <- function(x, y){
  
  set.seed(3)
  datarandom <- x[sample(nrow(x)),]
  folds <- cut(seq(1,nrow(x)),breaks=10,labels=FALSE)
  
  for(i in 1:10) {
    
    testIndexes <- which(folds==i, arr.ind=TRUE)
    trainIndexes <- which(folds!=i,arr.ind=TRUE)
    
    data_all.test <- datarandom[testIndexes, ]
    data_all.train <- datarandom[trainIndexes, ]
    
    set.seed(3)
    svm.model <- svm(data_all.train$class~ ., data = data_all.train, kernel = y)
    prediction <- predict(svm.model, data_all.test)
    table <- table(prediction, data_all.test$class)
    
    #calculate evaluation metrics
    accuracy <- c(accuracy,(table[1,1]+table[2,2]+table[3,3]+table[4,4]+table[5,5])/sum(sum(table)))
    
    precision0 <- c(precision0,(table[1,1])/(table[1,1] + table[1,2] + table[1,3] + table[1,4] + table[1,5]))
    precision1 <- c(precision1,(table[2,2]/(table[2,1] + table[2,2] + table[2,3] + table[2,4] + table[2,5])))
    precision2 <- c(precision2,(table[3,3]/(table[3,1] + table[3,2] + table[3,3] + table[3,4] + table[3,5])))
    precision3 <- c(precision3,(table[4,4]/(table[4,1] + table[4,2] + table[4,3] + table[4,4] + table[4,5])))
    precision4 <- c(precision4,(table[5,5]/(table[5,1] + table[5,2] + table[5,3] + table[5,4] + table[5,5])))
    
    precision0[is.na(precision0)] <- 0
    precision1[is.na(precision1)] <- 0
    precision2[is.na(precision2)] <- 0  
    precision3[is.na(precision3)] <- 0  
    precision4[is.na(precision4)] <- 0 
    
    recall0 <- c(recall0, (table[1,1]/(table[1,1] + table[2,1] + table[3,1] + table[4,1] + table[5,1])))
    recall1 <- c(recall1, (table[2,2]/(table[1,2] + table[2,2] + table[3,2] + table[4,2] + table[5,2])))
    recall2 <- c(recall2, (table[3,3]/(table[1,3] + table[2,3] + table[3,3] + table[4,3] + table[5,3])))
    recall3 <- c(recall3, (table[4,4]/(table[1,4] + table[2,4] + table[3,4] + table[4,4] + table[5,4])))
    recall4 <- c(recall4, (table[5,5]/(table[1,5] + table[2,5] + table[3,5] + table[4,5] + table[5,5])))
    
    recall0[is.na(recall0)] <- 0
    recall1[is.na(recall1)] <- 0  
    recall2[is.na(recall2)] <- 0  
    recall3[is.na(recall3)] <- 0  
    recall4[is.na(recall4)] <- 0

    lvls = levels(data_train$class)
    for (type.id in 1:5) {
      type = as.factor(data_all.train$class == lvls[type.id])
      
      score = as.numeric(prediction)
      actual.class = data_all.test$class == lvls[type.id]
      
      pred = prediction(score, actual.class)
      perf = performance(pred, "tpr", "fpr")
      
      nbauc = performance(pred, "auc")
      nbauc = unlist(slot(nbauc, "y.values"))
      aucs[type.id] = nbauc
    }
    
  }
  
  #print confusion matrix
  print (table)
  
  #print evaluation metrics
  print (mean(accuracy))
  
  precisionAverage = (mean(mean(precision0), mean(precision1), mean(precision2), mean(precision3), mean(precision4)))
  print(precisionAverage)
  
  recallAverage = (mean(mean(recall0), mean(recall1), mean(recall2), mean(recall3), mean(recall4)))
  print(recallAverage)
  
  #calculate and print fmeasure
  print (2*recallAverage*precisionAverage/(recallAverage+precisionAverage))
  
  print (mean(aucs))
  
}

#Run SVM function as appropriate and clear vectors between runs
svmAndEvalMetrics(data_train_1, "linear")
clearVectors()

svmAndEvalMetrics(data_train_1, "polynomial")
clearVectors()

svmAndEvalMetrics(data_train_1, "radial")
clearVectors()

svmAndEvalMetrics(data_train_2, "linear")
clearVectors()

svmAndEvalMetrics(data_train_2, "polynomial")
clearVectors()

svmAndEvalMetrics(data_train_2, "radial")
clearVectors()

svmAndEvalMetrics(data_train_3, "linear")
clearVectors()

svmAndEvalMetrics(data_train_3, "polynomial")
clearVectors()

svmAndEvalMetrics(data_train_3, "radial")
clearVectors()

svmAndEvalMetrics(data_train_4, "linear")
clearVectors()

svmAndEvalMetrics(data_train_4, "polynomial")
clearVectors()

svmAndEvalMetrics(data_train_4, "radial")
clearVectors()
