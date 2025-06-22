# Load the libraries
install.packages('rpart')
library('rpart')

# Load the Dataset
data(iris)
head(iris)

# Split Data into Train and Testing Sets:
set.seed(42)
train_indices <- sample(1:nrow(iris),0.7*nrow(iris)) #To take 70% of the data as training data 
train_data <- iris[train_indices,]
test_data <- iris[-train_indices,]

# Build the decision tree model
model <- rpart(Species~.,data = train_data)

# Visualize the model
plot(model, uniform=TRUE, margin=0.1)
text(model, use.n=TRUE, all=TRUE, cex=0.8)

# Make predictions
predictions <- predict(model,newdata = test_data, type = 'class')

# Evaluate model
table(predictions, test_data$Species)
accuracy <- sum(predictions == test_data$Species)/nrow(test_data)
print(paste("Accuracy:",accuracy))