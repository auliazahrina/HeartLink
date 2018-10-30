set.seed(3)
#load data
model.data <-read.csv(file="data.2_labels.csv")

model.data$ca <- NULL

model.data$sex <- as.factor(model.data$sex)
model.data$fbs <- as.factor(model.data$fbs)
model.data$exang <- as.factor(model.data$exang)
model.data$cp <- as.factor(model.data$cp)
model.data$slope <- as.factor(model.data$slope)
model.data$thal <- as.factor(model.data$thal)
model.data$restecg <- as.factor(model.data$restecg)
model.data$class <- as.factor(model.data$class)

#create model
model <- bartMachine(model.data[,1:(ncol(model.data)-1)], model.data$class, num_trees = 50, alpha = 0.9, beta = 1, k=2, serialize = TRUE)
model

#save model as an external file
save(model , file = 'BARTModel.rda')