# Building the Models(Logistic,Decision Tree, Support Vector Machine) for Education System
# Dependent variable is admit and independent variables are rest of all
# Data Understanding and Exploration
ed <- Project.1_Dataset
# Checking the dataset for NA's if Yellow occur then we have NA's otherwise clean data
library(Amelia)
missmap(ed,main = "Missing Map",col=c("yellow","black"),legend = FALSE)
any(is.na(ed))
str(ed)
ed$rank<- factor(ed$rank)
ed$ses <- factor(ed$ses)
ed$Race <- factor(ed$Race)
ed$Gender_Male<-factor(ed$Gender_Male)
ed$admit  <- factor(ed$admit)
colnames(ed)
attributes(ed)
summary(ed)

library(dplyr)
# Set the sample seed to 2
set.seed(2)
# Splitting the dataset into 80:20 ratio
train_obs <- floor(0.8*nrow(ed))
train_obs
train_ind <- sample(seq_len(nrow(ed)),size = train_obs)
train_ind
test = -train_ind
str(test)
# Train dataset for building the logistic model 
train_data <- ed[train_ind,]
str(train_data)
# TEst data for testing the dataset after building the successful model
test_data <- ed[-train_ind,]
str(test_data)
testing_high <- ed$admit[test]
testing_high
# Logistic regression model 
model <- glm(admit~., data = ed,family=binomial(link = "logit"))
summary(model)
# Logistic regression model with significance independent variable
model1 <- glm(admit~gre +gpa+rank , data = ed,family=binomial(link = "logit"))
summary(model1)
# probability  
prob <- predict(model1,test_data,type = "response")
prob
prob1 <- data.frame(prob)
results <- ifelse(prob1>0.5,1,0)
results 

# confusion matrix
table(testing_high,results)
(52+5)/(52+5+5+18)
# The accuracy of the model is 71.25%
final_data <- cbind(test_data,prob1)
View(final_data)
write.csv(final_data,"Log_model.csv")
# In above logistic model we got the accuracy percentage 71.25% which is considered as good model


#**************************************<<<Decision_Tree>>>*****************************************

# Decision Tree model
# Data Understanding and Exploration
ed <- Project.1_Dataset
library(Amelia)
missmap(ed,main = "Missing Map",col=c("yellow","black"),legend = FALSE)
any(is.na(ed))
str(ed)
ed$rank<- factor(ed$rank)
ed$ses <- factor(ed$ses)
ed$Race <- as.integer(ed$Race)
ed$Gender_Male<-factor(ed$Gender_Male)
ed$admit <- factor(ed$admit)
colnames(ed)
attributes(ed)
summary(ed)

library(dplyr)
#Splitting the dataset into the ratio of 70:30
library(caTools)
set.seed(2)

sample <- sample.split(ed$admit,SplitRatio = 0.70)
summary(sample)
#Split the dataset into train and testing data
train <- subset(ed,sample==TRUE)
str(train)
test <- subset(ed,sample==FALSE)
str(test)
#To build the decison tree model we need the library(rpart) package for visualization
library(rpart)
model <- rpart(admit ~., data = train, method = "class")
model
summary(model)
# To display it in figure
library(rattle)
library(rpart.plot)
fancyRpartPlot(model)
# Prediction of the model
pred <- predict(model,test,type = "class")
pred
pred1 <- data.frame(pred)
# confusion matrix is technique for summarizing the performance of a classificaton algorithm
table(test$admit,pred)
# Accuracy of the model is 73.34%
(73+15)/(73+15+9+23)
final_outcome <- cbind(test,pred)
View(final_outcome)
write.csv(final_outcome,"Decision_tree_model.csv")
# In this above Decison tree model we get the accuracy percentage 73.34% which is good model

#********************************************<<<Support_Vector_Machine>>>*****************************************
# Decision Tree model
# Data Understanding and Exploration
ed <- Project.1_Dataset
library(Amelia)
missmap(ed,main = "Missing Map",col=c("yellow","black"),legend = FALSE)
any(is.na(ed))
str(ed)
ed$rank<- factor(ed$rank)
ed$ses <- factor(ed$ses)
ed$Race <- as.integer(ed$Race)
ed$Gender_Male<-factor(ed$Gender_Male)
ed$admit <- factor(ed$admit)
colnames(ed)
attributes(ed)
summary(ed)

library(dplyr)
#Splitting the dataset into the ratio of 70:30
library(caTools)
set.seed(2)

sample <- sample.split(ed$admit,SplitRatio = 0.70)
summary(sample)
#Split the dataset into train and testing data
train <- subset(ed,sample==TRUE)
str(train)
test <- subset(ed,sample==FALSE)
str(test)

# SVM model Building
# install.packages("ISLR")
# library(ISLR)
# install.packages("e1071")
# library(e1071)
model <- svm(admit~., data = train)
summary(model)
pred <-predict(model,test)
table(test$admit,pred)
# Calculating the percentage of the model
(82)/(82+38)
# The accuracy of the SVM model is 68.34%
final_output <- cbind(test,pred)
write.csv(final_output,"SVM_model.csv")
# In the above SVM model got accuracy percentage 68%

# For the Education  system we build the three model to find out the best model for the Education system
# The first model is logistic model which has the accuracy percentage 71.34%
# The second model is DEcision Tree which has the accuract percentage 73.34%
# The third model is Support Vector Machine has the accuracy precentage 68.34%
# So from the above result we stated that the champion model among three model 
# is DECISION TREE MODEL with the precentage 73.34%


#************************************ DESCRIPTIVE ***********************************************

library(ggplot2)
library(dplyr)
df <- cut(test_data$gpa,breaks = c(2,2.7,3.4,4),labels = c("LOW","MEDIUM","HIGH"))
tail(df)
prob <- predict(model1,test_data,type = "response")
pl <- ggplot(test_data,aes(df,prob )) + geom_point(col="green") 
pl + xlab("GRADE POINT AVERAGE") + ylab("Probability Percentage") + scale_y_continuous()

#**********************************************************************************************
# cross grid admission variables with categorized gre
df <- cut(test_data$gre,breaks = c(0,440,580,800),labels = c("LOW","MEDIUM","HIGH"))
str(ed)
ed$Race <- factor(ed$Race)
ed$gre <- factor(df)
TTabular_View <- data.frame(ed$admit,ed$gre,ed$ses,ed$Race,ed$Gender_Male,ed$rank,ed$gpa)
View(Tabular_View)
# *********************************Naives Bayes *****************************************************

# Decision Tree model
# Data Understanding and Exploration
ed <- Project.1_Dataset
library(Amelia)
missmap(ed,main = "Missing Map",col=c("yellow","black"),legend = FALSE)
any(is.na(ed))
str(ed)
ed$rank<- factor(ed$rank)
ed$ses <- factor(ed$ses)
ed$Race <- as.integer(ed$Race)
ed$Gender_Male<-factor(ed$Gender_Male)
ed$admit <- factor(ed$admit)
str(ed)
colnames(ed)
attributes(ed)
summary(ed)

library(dplyr)
#Splitting the dataset into the ratio of 70:30
library(caTools)
set.seed(2)

sample <- sample.split(ed$admit,SplitRatio = 0.70)
summary(sample)
#Split the dataset into train and testing data
train <- subset(ed,sample==TRUE)
str(train)
test <- subset(ed,sample==FALSE)
str(test)
# Packages required to build naive bayes model 
install.packages("e1071","caret","mlbench")
# Building the Naive Baiyes Model 

model <- naiveBayes(admit~., data = train)
model
# Predict the values and form the confusion matrix
pred <- predict(model,test)
pred
table(pred,test$admit)
(72+11)/(72+27+10+11)
