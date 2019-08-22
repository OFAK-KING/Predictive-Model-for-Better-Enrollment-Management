# Load needed packages

library(tidyverse)
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(car)
library(Hmisc)

# Importing data needed for the analysis.

Dataset <- read.csv("inq2015.csv", na.strings = c(NA, " "))

# imputing variables sex

Dataset$sex <- with(Dataset,impute(sex,max))

# Selecting the needed variable for anlyssis from the data that was imported.

Varia <- c("Enroll", "CAMPUS_VISIT", "Instate","TOTAL_CONTACTS", "hscrat", "sex", 
           "init_span", "int1rat", "int2rat", "interest", "mailq","premiere")
Data4Analysis <- Dataset[Varia]

# Checking the structure of the data about to be analysed.

str(Data4Analysis)

# Change some variables data type
Data4Analysis$Enroll <- factor(Data4Analysis$Enroll)
Data4Analysis$CAMPUS_VISIT <- factor(Data4Analysis$CAMPUS_VISIT)
Data4Analysis$Instate <- factor(Data4Analysis$Instate)
Data4Analysis$sex <- factor(Data4Analysis$sex)
Data4Analysis$mailq <- factor(Data4Analysis$mailq)
Data4Analysis$premiere <- factor(Data4Analysis$premiere)

# Transformation ofvery skewwed variables

hist(Data4Analysis$int1rat)
Data4Analysis$int1rat <- log10(Data4Analysis$int1rat)

hist(Data4Analysis$int2rat)
Data4Analysis$int2rat <- log10(Data4Analysis$int2rat)

hist(Data4Analysis$interest)
Data4Analysis$interest <- log10(Data4Analysis$interest + 1)

hist(Data4Analysis$hscrat)
Data4Analysis$hscrat <- log10(Data4Analysis$hscrat)

#To remove the rows with +/-Inf and NA
#First replace inf with NA and then replace Na with means
is.na(Data4Analysis) <- sapply(Data4Analysis, is.infinite)

Data4Analysis$int1rat <- with(Data4Analysis,impute(int1rat,mean))
Data4Analysis$int2rat <- with(Data4Analysis,impute(int2rat,mean))
Data4Analysis$interest <- with(Data4Analysis,impute(interest,mean))

# Creating the repeatability seed 

set.seed(123)

# Data partition

DataSplit <- sample.split(Data4Analysis$Enroll, SplitRatio = 0.7)

# Creating Training Dataset
TrainSet = subset(Data4Analysis, DataSplit == TRUE)

# Create Testing Dataseet
TestSet = subset(Data4Analysis, DataSplit == FALSE)

# Building a Logistic REgression model 

LogModel <- glm(formula = Enroll ~ . , family = binomial(link='logit'), data = TrainSet)

# Print out the result
summary(LogModel)

# Check possibility of multicollinearity

vif(LogModel)

# Based on the VIF results the the contact variables are going to taken out retaining only the 
# total contact to represent contact variables.


#Evaluating model performance using the test dataset
#Predicting the default probabilities based on the test dataset
PredProb <- predict(LogModel, newdata = TestSet, type = 'response')

#Turn the default probabilities to binary
PredProbResult <- ifelse(PredProb > 0.5,1,0)

# Creating confusion matrix
print("The confusion matrix is:")
print(table(PredProbResult, TestSet$Enroll))

# Creating ROC curve
pred <- prediction(PredProb,TestSet$Enroll)
perf <- performance(pred, "tpr", "fpr")

par(mar = rep(2, 4))
plot(perf)
abline(a=0,b=1)

# Calculating and print AUC value
auc <- performance(pred, measure="auc")
auc <- auc@y.values[[1]]
print(paste("AUC for the Logistic regression model is:", auc))


#=======================================================================

# Building a Decision Tree model

DTreeModel <- rpart(Enroll ~., method="class", data = TrainSet)

# Print out the result
printcp(DTreeModel)

# Display decision tree plot
prp(DTreeModel, type = 2, extra = "auto", cex = 0.8,
    main = "Decision Tree Diagram for prospective students 
    who would most likely enroll as new freshmen in the Fall 2015 semester")

#Evaluating model performance using the test dataset
#Predicting the default probabilities based on the test dataset
PredProba <- predict(DTreeModel,TestSet)
PredProba <- as.data.frame(PredProba)

#Turn the default probabilities to binary of threshold 0.5
ConvertProb2Binary <- function(DTreeProb){
  if (DTreeProb >= 0.5){
    return('Yes')
  }else{
    return("No")
  }
}

PredProba$Result <- sapply(PredProba$`1`,ConvertProb2Binary)

# Creating the confusion matrix
print("The confusion matrix is:")
print(table(PredProba$Result,TestSet$Enroll))

# Creating the ROC curve
pred <- prediction(PredProba$`1`,TestSet$Enroll)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(a=0,b=1)

# Calculating and printing AUC value
auc <- performance(pred, measure="auc")
auc <- auc@y.values[[1]]
print(paste("AUC for the Decision tree model is:", auc))

