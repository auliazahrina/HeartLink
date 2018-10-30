# load the library
library(caret)

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

# train BART model to tune parameter
set.seed(7)
model <- train(class~., data=data, method='bartMachine')
# summarize the model, get recommended alpha and beta
print(model)

# perform 10 folds cross validation to get ideal num_tree and k
cv <- build_bart_machine_cv(data[,1:(ncol(data)-1)], data$class,
                            num_tree_cvs = c(50,200), k_cvs = c(2,3,5),
                            nu_q_cvs = NULL, k_folds = 10, alpha = 0.9, beta = 1)
