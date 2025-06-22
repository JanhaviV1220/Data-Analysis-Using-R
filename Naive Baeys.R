# Import libraries
install.packages('mlbench') # contains various datasets
install.packages('caret') # for evaluating predictive models
install.packages('e1071') # for training a Naive Baeys classifier
library('mlbench')
library('caret')
library('e1071')

# Reading the dataset
# We will use the sonar dataset containing 208 rows and 61 variables
# It was collected by reflecting sonar waves off a metal cylinder and a rock at various angles and situations.
data(Sonar)
head(Sonar)

# Train Test Split
splitindex <- createDataPartition(Sonar$Class,p=0.7,list = FALSE)
train_data <- Sonar[splitindex,]
test_data <- Sonar[-splitindex,]

# Training the model
model <- naiveBayes(Class~.,data = train_data)

# Make Predictions
predictions <- predict(model, newdata = test_data)

# Check accuracy
confusion_matrix <- table(predictions, test_data$Class)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(confusion_matrix)
cat("Accuracy:",round(accuracy*100,2),"%\n")