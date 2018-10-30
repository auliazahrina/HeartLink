#==============================================================
# OPEN DATA & FILL IN EMPTY DATA BASED ON EACH SOURCE'S MEDIAN
#==============================================================
#NOTE: empty data is filled in IF the number of missing data is 
#less than half of the total number of data in that column

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

#Open Switzerland Data
switzerlanddata <- read.csv(file="processed.switzerland.csv", 
                            na.strings = "?",
                            col.names = c("age","sex", "cp", "trestbps", "chol", 
                                          "fbs", "restecg", "thalach", "exang", 
                                          "oldpeak", "slope", "ca", "thal", "class"))

#Fill in N/A with median of each column if the number of NA in each column is less than half of the data
#(except fbs and ca) 
for(i in 1:ncol(switzerlanddata)){
  na <- sum(is.na(switzerlanddata[,i]))
  halfdata <- (length(switzerlanddata[,i]))/2
  
  if(na < halfdata) {
    switzerlanddata[is.na(switzerlanddata[,i]), i] <- median(switzerlanddata[,i], na.rm = TRUE)}
}

#Open Va Data
vadata <- read.csv(file="processed.va.csv", 
                   na.strings = "?",
                   col.names = c("age","sex", "cp", "trestbps", "chol", 
                                 "fbs", "restecg", "thalach", "exang", 
                                 "oldpeak", "slope", "ca", "thal", "class"))

#Fill in N/A with median of each column if the number of NA in each column is less than half of the data
#(except slope, ca, and thal) 
for(i in 1:ncol(vadata)){
  na <- sum(is.na(vadata[,i]))
  halfdata <- (length(vadata[,i]))/2
  
  if(na < halfdata) {
    vadata[is.na(vadata[,i]), i] <- median(vadata[,i], na.rm = TRUE)}
}

#Open Hungarian Data 
hungariandata <- read.csv(file="reprocessed.hungarian.csv", 
                          na.strings = "?", 
                          col.names = c("age","sex", "cp", "trestbps", "chol", 
                                        "fbs", "restecg", "thalach", "exang", 
                                        "oldpeak", "slope", "ca", "thal", "class"))

#Fill in N/A with median of each column if the number of NA in each column is less than half of the data
#(except slope, ca, and thal) 
for(i in 1:ncol(hungariandata)){
  na <- sum(is.na(hungariandata[,i]))
  halfdata <- (length(hungariandata[,i]))/2
  
  if(na < halfdata) {
    hungariandata[is.na(hungariandata[,i]), i] <- median(hungariandata[,i], na.rm = TRUE)}
}


#==============================================================
# ADD SOURCE COLUMN & MERGE ALL DATA 
#==============================================================
# Add source column
#clevelanddata$source   <- "cleveland"
#switzerlanddata$source <- "switzerland"
#vadata$source          <- "va"
#hungariandata$source   <- "hungarian"

# Merge data sets
data <- rbind(clevelanddata, switzerlanddata, vadata, hungariandata)


#==============================================================
# IMPUTE MISSING DATA USING MICE
#==============================================================
#Install mice
install.packages("mice")
library(mice)
library(lattice)

impdata <- as.data.frame(data)
# Do a non-imputation
imp <- mice(data, m=1, maxit = 5)

# Find the methods that mice chooses on each column
imp$method

# Look at the imputation matrix
imp$predictorMatrix

#Fill in 1 because thal has 0 on thalach column
imp$predictorMatrix["thal","thalach"] <- 1

#Generate custom functions for columns fbs, slope, ca, and thal 
mice.impute.pmm_fbs <- 
  function (y, ry, x, donors = 5, type = 1, ridge = 1e-05, version = "", 
            ...) 
  {
    repeat{
      vals <- mice.impute.pmm(y, ry, x, donors = 5, type = 1, ridge = 1e-05,
                              version = "", ...)
      if (all(vals == 0 | vals == 1)){
        break
      }
    }
    return(vals)
  }

mice.impute.pmm_slope <- 
  function (y, ry, x, donors = 5, type = 1, ridge = 1e-05, version = "", 
            ...) 
  {
    repeat{
      vals <- mice.impute.pmm(y, ry, x, donors = 5, type = 1, ridge = 1e-05,
                              version = "", ...)
      if (all(vals >= 1 && vals <= 3)){
        break
      }
    }
    return(vals)
  }

mice.impute.pmm_ca <- 
  function (y, ry, x, donors = 5, type = 1, ridge = 1e-05, version = "", 
            ...) 
  {
    repeat{
      vals <- mice.impute.pmm(y, ry, x, donors = 5, type = 1, ridge = 1e-05,
                              version = "", ...)
      if (all(vals >= 0 && vals <= 3)){
        break
      }
    }
    return(vals)
  }

mice.impute.pmm_thal <- 
  function (y, ry, x, donors = 5, type = 1, ridge = 1e-05, version = "", 
            ...) 
  {
    repeat{
      vals <- mice.impute.pmm(y, ry, x, donors = 5, type = 1, ridge = 1e-05,
                              version = "", ...)
      if (all(vals == 3 | vals == 6 | vals == 7)){
        break
      }
    }
    return(vals)
  }

#Assign impute functions to each column
imp$method["fbs"] <- "pmm_fbs"
imp$method["slope"] <- "pmm_slope"
imp$method["ca"] <- "pmm_ca"
imp$method["thal"] <- "pmm_thal"

#Impute data
imp_ds <- 
  mice(impdata, 
       method = imp$method, 
       predictorMatrix = imp$predictorMatrix)

#Get imputed data
fin_imp_data <- as.data.frame(complete(imp_ds, action = 3))

#THE NUMBER OF 0 VALUE IN:
#trestbps: 1
#chol: 172


#==============================================================
# SHUFFLE DATA AND SAVE ALL 5 LABELS DATA (0,1,2,3,4)
#==============================================================
#Shuffle the data
set.seed(3)
datarandom<-fin_imp_data[sample(nrow(fin_imp_data)),]

#Save all data 
write.csv(datarandom, 'data.5_labels.csv', row.names = FALSE)


#==============================================================
# CONVERT CLASS 4 TO 3 AND SAVE ALL 4 LABELS DATA (0,1,2,3)
#==============================================================
for(i in 1:nrow(datarandom))
{
  if(datarandom[i,14] == 4)
    datarandom[i,14] = 3
}
#Save all data 
write.csv(datarandom, 'data.4_labels.csv', row.names = FALSE)

#==============================================================================
# CONVERT CLASS 2 TO 1, THEN CLASS 3 TO 2 AND SAVE ALL 3 LABELS DATA (0,1,2)
#==============================================================================
#Convert class 2 to 1
for(i in 1:nrow(datarandom))
{
  if(datarandom[i,14] == 2)
    datarandom[i,14] = 1
}
#Convert class 3 to 2
for(i in 1:nrow(datarandom))
{
  if(datarandom[i,14] == 3)
    datarandom[i,14] = 2
}
#Save all data 
write.csv(datarandom, 'data.3_labels.csv', row.names = FALSE)

#==============================================================
# CONVERT CLASS 2 TO 1 AND SAVE ALL 2 LABELS DATA (0,1)
#==============================================================
for(i in 1:nrow(datarandom))
{
  if(datarandom[i,14] == 2)
    datarandom[i,14] = 1
}
#Save all data 
write.csv(datarandom, 'data.2_labels.csv', row.names = FALSE)