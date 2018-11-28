install.packages(c('corrplot', 'cluster', 'fpc'))
library(corrplot)

#Define normalize function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#==============================================================
# CLEVELAND DATA 
#==============================================================
#Open Cleveland Data
clevelanddata <- read.csv(file="processed.cleveland.csv", 
                          na.strings = "?", 
                          sep ="", 
                          col.names = c("age","sex", "cp", "trestbps", "chol", 
                                        "fbs", "restecg", "thalach", "exang", 
                                        "oldpeak", "slope", "ca", "thal", "class"))



#Fill in N/A with median of each column if the number of NA in each column is less than half of the data
for(i in 1:ncol(clevelanddata)){
  na <- sum(is.na(clevelanddata[,i]))
  halfdata <- (length(clevelanddata[,i]))/2
  
  if(na < halfdata) {
    clevelanddata[is.na(clevelanddata[,i]), i] <- median(clevelanddata[,i], na.rm = TRUE)}
}

#Normalize cleveland data
datanorm <- as.data.frame(lapply(clevelanddata, normalize))

#Create Pearson correlation coefficient
cors <- cor(datanorm, method = "pearson")
#Show Pearson correlation coefficient for attribute "class"
cors[,14]
#Show correlation plot
corrplot(cors)

#testing contributor modifcation

#==============================================================
# ALL 5 LABELS DATA 
#==============================================================
data <- read.csv(file="data.5_labels.csv")
#Normalize all data
alldatanorm <- as.data.frame(lapply(data, normalize))
#Create Pearson correlation coefficient
allcors <- cor(alldatanorm, method = "pearson")
#Show Pearson correlation coefficient for attribute "class"
allcors[,14]
#Show correlation plot
corrplot(allcors)
