install.packages("dplyr")
install.packages("caTools")

library("dplyr")
library("caTools")

# load the dataset 
titanic_dataset <- read.csv("C:\\Users\\Janhavi\\OneDrive\\Desktop\\Fergusson\\SEM 2\\ML\\Assignments\\Titanic-Dataset.csv")

# view the first few rows of the file
head(titanic_dataset)

# Data Preprocessing
# select relevent features
titanic_data <- titanic_dataset%>%select(Survived,Pclass,Sex,Age,Fare)

# Printing the number of missing values
print(colSums(is.na(titanic_data)))

# Handling missing values
titanic_data$Age[is.na(titanic_data$Age)]<- mean(titanic_data$Age,na.rm = TRUE)

# Converting Sex and Survived into numerical values
titanic_data$Sex<- as.factor(titanic_data$Sex)
titanic_data$Survived<- as.factor(titanic_data$Survived)

# view data structure
str(titanic_data)

# Splitting the data into train test split
set.seed(42)
split<-sample.split(titanic_data$Survived,SplitRatio = 0.7)
train_data<-subset(titanic_data,split==TRUE)
test_data<-subset(titanic_data,split==FALSE)

# Train the model
logistic_model<-glm(Survived ~ Pclass+Age+Sex+Fare, family = binomial,data = train_data)

# Summery of the model
summary(logistic_model)

# predict the probabilities for the test set
predicted_probabilities<-predict(logistic_model,newdata = test_data,type = "response")

# Convert the probabilities to binary predictions
predicted_class<- ifelse(predicted_probabilities>0.5,1,0)

# View first few predictions
head(predicted_class)

actual_class<-as.numeric(test_data$Survived)-1

# Evaluate the model
# create confusion matrix
confusion_matrix<-table(Predicted=predicted_class, Actual=actual_class)

# Calculate Accuracy
accuracy<- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(paste("Accuracy:",round(accuracy*100,2),"%"))

# Roc and AUC
install.packages("pROC")
library(pROC)

roc_curve<- roc(actual_class,predicted_class)
plot(roc_curve,col = "blue",main="ROC Curve - Logistic regression")

auc_curve<- auc(roc_curve)
print(paste("Auc:",auc_curve))
