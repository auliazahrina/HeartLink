#load libraries
install.packages(c('tree'))
library(tree)

data_train <- read.csv(file="data_train.csv")
data_train = data_train[complete.cases(data_train[, -12]), -12]

#convert categorical variables of training data
data_train$sex <- as.factor(data_train$sex)
data_train$cp <- as.factor(data_train$cp)
data_train$fbs <- as.factor(data_train$fbs)
data_train$exang <- as.factor(data_train$exang)
data_train$slope <- as.factor(data_train$slope)
data_train$ca <- as.factor(data_train$ca)
data_train$thal <- as.factor(data_train$thal)
data_train$class <- as.factor(data_train$class)

#train on training data
set.seed(3)
tree.data_train=tree(data_train$class~.,data.train)

summary(tree.data_train)
plot(tree.data_train)
text(tree.data_train ,pretty = 0)

data_test <- read.csv(file= "data_test.csv")

#convert categorical variables of testing data
data_test$sex <- as.factor(data_test$sex)
data_test$cp <- as.factor(data_test$cp)
data_test$fbs <- as.factor(data_test$fbs)
data_test$exang <- as.factor(data_test$exang)
data_test$slope <- as.factor(data_test$slope)
data_test$ca <- as.factor(data_test$ca)
data_test$thal <- as.factor(data_test$thal)
data_test$class <- as.factor(data_test$class)

#test decision tree using testing data
#generate confusion matrix
prediction <- predict(tree.data_train,data_test,type = "class")
table <-table(prediction, data_test$class)
table

#create empty vectors and generate evaluation metrics
accuracy <- vector()
precision <- vector()
recall <- vector()

accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
accuracy

precision <- c(precision,(table[2,2])/(table[2,2] + table[2,1]))
precision

recall <- c(recall, (table[2,2]/(table[2,2] + table[1,2])))
recall

fmeasure<-2*precision*recall/(precision+recall)
fmeasure