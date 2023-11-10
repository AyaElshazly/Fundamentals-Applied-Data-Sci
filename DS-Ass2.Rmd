# # Assignment 2
# needed packages
install.packages("lubridate")
install.packages("e1071")
# Install or update the 'keras' package
install.packages("keras")

library(caret)
library(dplyr)
library(MASS)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(e1071)
library(rpart)
library(keras)
# # Part 1: Classification
# Load the data set
data <- read.csv("MedicalCentre.csv")

# ------------------------------------------------------------------------------------------------------------
# A. Feature Engineering 

#   1. Prepare the data for downstream processes, e.g., dealing with missing values
head(data)
# Remove PatientId column
data <- subset(data,select = -PatientId)
# Remove AppointmentID column
data <- subset(data,select = -AppointmentID)

# count total missing values in each column of data frame
sapply(data, function(x) sum(is.na(x))) #The ‘Age’ column has 3 missing values.
#identify locations of missing values in 'assists' column
which(is.na(data$Age))
# Create a data frame with missing values
# Remove rows with missing values
cleaned_df <- na.omit(data)
# check if missing values are removed
sum(is.na(cleaned_df)) # ) 0: indicates that there is no missing values
# ------------------------------------------------------------------------------------------------------------
# 2. Initialize a function to plot all features within the dataset to visualize for outliers
# Display types for each attribute
sapply(cleaned_df, class)
# choose only the numeric columns
library(dplyr)
numeric_cols <- sapply(cleaned_df, is.numeric)
numeric_df <- cleaned_df[, numeric_cols]
numeric_df <- glimpse(numeric_df)
# function to plot numeric features
numeric_outliers <- function(numeric_df) {
  # Get the names of all features in the dataset
  feature_names <- names(numeric_df)
  # Loop through each feature and create a boxplot
  for (feature in feature_names) {
    # Create a boxplot for the current feature
    
    p <- boxplot(numeric_df,main="Boxplots for Numerical Features", aes(y = feature), col="blue") +
      geom_boxplot() + 
      labs(title = paste("Boxplot of", feature),
           y = feature)
  }
}
numeric_outliers(numeric_df)
# plot distribution of Neighbourhood
cleaned_df$Neighbourhood <- with(cleaned_df, reorder(Neighbourhood, Neighbourhood, function(x) length(x)))
ggplot(data = cleaned_df) +
  ggtitle("Distribution of the Neighbourhood") +
  geom_bar(aes(x= Neighbourhood),fill = "blue") + coord_flip()+theme_minimal()
# plot distribution of Gender 
cleaned_df$Gender <- with(cleaned_df, reorder(Gender, Gender, function(x) length(x)))
ggplot(data = cleaned_df) +
  ggtitle("Distribution of the Gender") +
  geom_bar(aes(x= Gender),fill = "blue")+theme_minimal()
# plot distribution of No-show 
cleaned_df$No.show <- with(cleaned_df, reorder(No.show, No.show, function(x) length(x)))
ggplot(data = cleaned_df) +
  ggtitle("Distribution of the No-show") +
  geom_bar(aes(x= No.show),fill = "blue")+theme_minimal()
# ------------------------------------------------------------------------------------------------------------
# 3. Count the frequency of negative Age feature observations, and remove them
sum(cleaned_df$Age < 0)
cleaned_df <- cleaned_df[cleaned_df$Age >= 0, ]
# check if they are removed
sum(cleaned_df$Age < 0)
# ------------------------------------------------------------------------------------------------------------
# 4. The values within AwaitingTime are negative, transform them into positive values

cleaned_df$ScheduledDay <- ymd_hms(cleaned_df$ScheduledDay)
cleaned_df$AppointmentDay <- ymd_hms(cleaned_df$AppointmentDay)
cleaned_df$AwaitingTime <- difftime(cleaned_df$AppointmentDay, cleaned_df$ScheduledDay, units = "secs")
head(cleaned_df)
sum(cleaned_df$AwaitingTime < 0)
# Convert the columns to datetime objects
cleaned_df$AwaitingTime <- abs(cleaned_df$AwaitingTime)
sum(cleaned_df$AwaitingTime < 0)
# ------------------------------------------------------------------------------------------------------------
# 5. ML algorithm requires the variables to be coded into its equivalent integer codes. Encode the string 
# categorical values into an integer code
#get unique values in No.show
unique_Noshow <- unique(cleaned_df$No.show)
unique_Noshow
cleaned_df$No.show <- as.integer(factor(cleaned_df$No.show, levels = c("Yes", "No"))) #"Yes" will be 1, and "No" will be 2.
#get unique values in Gender
unique_Gender <- unique(cleaned_df$Gender)
unique_Gender
cleaned_df$Gender <- as.integer(factor(cleaned_df$Gender, levels = c("M", "F"))) #"M" will be 1, and "F" will be 2.
#get unique values in Neighbourhood
unique_neighbourhoods <- unique(cleaned_df$Neighbourhood)
print(unique_neighbourhoods)
cleaned_df$Neighbourhood <- as.integer(factor(cleaned_df$Neighbourhood))
# display the first 6 rows in cleaned_df dataframe
head(cleaned_df)
# ------------------------------------------------------------------------------------------------------------
# 6. Separate the date features into date components
# extracts the year, month, and day components from ScheduledDay and AppointmentDay columns
cleaned_df$ScheduledDay <- as.POSIXct(cleaned_df$ScheduledDay, format="%Y-%m-%dT%H:%M:%SZ")
cleaned_df$AppointmentDay <- as.POSIXct(cleaned_df$AppointmentDay, format="%Y-%m-%dT%H:%M:%SZ")
cleaned_df$ScheduledDay_Year <- format(cleaned_df$ScheduledDay, "%Y")
cleaned_df$ScheduledDay_Month <- format(cleaned_df$ScheduledDay, "%m")
cleaned_df$ScheduledDay_Day <- format(cleaned_df$ScheduledDay, "%d")
cleaned_df$AppointmentDay_Year <- format(cleaned_df$AppointmentDay, "%Y")
cleaned_df$AppointmentDay_Month <- format(cleaned_df$AppointmentDay, "%m")
cleaned_df$AppointmentDay_Day <- format(cleaned_df$AppointmentDay, "%d")
# display random samples of data to show the date components
sample_df <- cleaned_df %>% sample_n(10)
sample_df
# ------------------------------------------------------------------------------------------------------------
# 7. ML algorithms work best when the input data are scaled to a narrow range around zero. Rescale the 
# age feature with a normalizing (e.g., min_max normalization) or standardization (e.g., z_score standardization) function.
# apply min_max normalization 
cleaned_df$Age <- (cleaned_df$Age - min(cleaned_df$Age)) / (max(cleaned_df$Age) - min(cleaned_df$Age))
head(cleaned_df$Age)
# ------------------------------------------------------------------------------------------------------------
# 8. Conduct variability comparison between features using a correlation matrix & drop correlated features
# Display types for each attribute
sapply(cleaned_df, class)
numeric_columns <- sapply(cleaned_df, is.numeric)  # Identify numeric columns
numeric_columns
cor_matrix <- cor(cleaned_df[, numeric_columns])  # Calculate correlation matrix
heatmap(cor_matrix,col = colorRampPalette(c("blue", "white", "red"))(65))
# Drop correlated features if any exist
# find attributes that are highly corrected (ideally > 0.5)
highlyCorrelated <- findCorrelation(cor_matrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
hc = sort(highlyCorrelated)
hc
reduced_Data = cleaned_df[ ,-c(hc)]
print (reduced_Data)
head(cleaned_df)
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# B. Model Development I 
class(cleaned_df)
cleaned_df <- cleaned_df %>% mutate_if(is.character, as.integer)
# Display types for each attribute
sapply(cleaned_df, class)
# Delete unwanted columns
cleaned_df <- subset(cleaned_df,select = -ScheduledDay)
cleaned_df <- subset(cleaned_df,select = -AppointmentDay)
cleaned_df <- subset(cleaned_df,select = -AwaitingTime)
sapply(cleaned_df, class)
head(cleaned_df)

#   Develop a SVM and Decision Tree classifier to predict the outcome of the test. The performance of the 
# classifier should be evaluated by partitioning the dataset into a train dataset (70%) and test dataset (30%). 
# Use the train dataset to build the models and the test dataset to evaluate how well the model generalizes to 
# future results.
cleaned_df$No.show <- as.factor(cleaned_df$No.show)
levels(cleaned_df$No.show) <- c("Yes", "No")
# Display types for each attribute
sapply(cleaned_df, class)
# Split dataset into train and test sets
set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(cleaned_df), replace=TRUE, prob=c(0.7,0.3))
train  <- cleaned_df[sample, ]
test   <- cleaned_df[!sample, ]

dim(cleaned_df)

# Check class of 'train' object
class(train)
#view dimensions of training set
print("dimensions of training set")
dim(train)
#view dimensions of test set
print("dimensions of test set")
dim(test)

# Train SVM model
library(keras)
svm_model <- svm(No.show~., data = train, kernel = "linear", cost = 1)
svm_predict <- predict(svm_model, newdata = test)
# Train Decision Tree model
dt_model <- rpart(No.show~., data = train, method = "class")
dt_predict <- predict(dt_model, newdata = test, type = "class")
# Calculate the accuracy of the model on the testing dataset
actual_labels <- test$No.show
accuracy_svm <- sum(svm_predict == actual_labels) / length(actual_labels)
# Print the accuracy of the model on the testing dataset
print(paste("Accuracy of the SVM model on the testing dataset is:", accuracy_svm))
accuracy_dt <- sum(dt_predict == actual_labels) / length(actual_labels)
# Print the accuracy of the model on the testing dataset
print(paste("Accuracy of the Decision Tree model on the testing dataset is:", accuracy_dt))
# Evaluate performance of classifiers
print("Confusion Matrix for SVM")
confusionMatrix(data = svm_predict, reference = test$No.show)
print("Confusion Matrix for Decision Tree")
confusionMatrix(data = dt_predict, reference = test$No.show)
# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# C. Model Development II 
#   Train a Deep Neural Network using Keras (Ensure to determine the best configuration that provides the 
#best accuracy). Try changing the activation function or dropout rate. What effects does any of these have on 
# the result?
# Install and load the 'reticulate' package
install.packages("reticulate")
library(reticulate)

# Install TensorFlow
install_tensorflow()
# Train DNN model

train$No.show <- as.factor(train$No.show)
levels(train$No.show) <- c("Yes", "No")
train$No.show <- ifelse(train$No.show == "Yes", 1, 0)
test$No.show <- as.factor(test$No.show)
levels(test$No.show) <- c("Yes", "No")
test$No.show <- ifelse(test$No.show == "Yes", 1, 0)
train$Neighbourhood <- as.integer(train$Neighbourhood)
train$Age <- as.integer(train$Age)
train$No.show <- as.integer(train$No.show)
test$Neighbourhood <-as.integer(test$Neighbourhood )
test$Age <- as.integer(test$Age)
test$No.show <- as.integer(test$No.show)

head(train)
head(test$No.show)
library(keras)
# Separate features and target variable
features_train <- train[, !(colnames(train) %in% c("No.show"))]

features_test <- test[, !(colnames(test) %in% c("No.show"))]

x_input_train <- array_reshape(features_train, c(nrow(features_train)[1], prod(dim(features_train)[-1]))) / max(features_train)
x_input_test <- array_reshape(features_test, c(nrow(features_test)[1], prod(dim(features_test)[-1]))) / max(features_test)


y_input_train <- to_categorical(train$No.show, 2)
y_input_test <- to_categorical(test$No.show, 2)


model <- keras_model_sequential() 
model %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(train) - 1) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 2, activation = "sigmoid")

summary(model)

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)
model %>% fit(
  x_input_train,
  y_input_train,
  epochs = 10,
  batch_size = 32,
  validation_data  = list(x_input_test,y_input_test) 
)
#Evaluating model on the cross validation dataset
loss_and_metrics <- model %>% evaluate(x_input_test, y_input_test, batch_size = 128)
loss_and_metrics[2]


#   D. Model Evaluation & Comparison 
#   1) Write a Function to detect the Model’s Accuracy by applying the trained model on a testing dataset to 
# find the predicted labels of Status. Was there overfitting? 
# Function to calculate model accuracy
install.packages("MASS")
library(klaR)
library(e1071)
detect_accuracy <- function(model, test_data) {
  
  # Predict the labels of No-show using the trained model
  predicted_probs <- predict(model, x_input_test)
  predicted_labels <- ifelse(predicted_probs > 0.5, 1, 0)

  # Compare predicted labels with actual labels to calculate accuracy
  actual_labels <- test_data$No.show
  accuracy <- sum(predicted_labels == actual_labels) / length(actual_labels)
  return(accuracy)
}

# calculate accuracy of model neural network
accuracy_SVM <- detect_accuracy(svm_model, test)
print(paste("Accuracy of the  SVM model on the testing dataset is:", accuracy_SVM))

accuracy_DT <- detect_accuracy(dt_model, test)
print(paste("Accuracy of the Decision Tree model on the testing dataset is:", accuracy_DT))

accuracy_NN <- detect_accuracy(model, test)
print(paste("Accuracy of the Neural Network model on the testing dataset is:", accuracy_NN))

# Check for overfitting by comparing the training and testing accuracies
train_accuracy <- calculateAccuracy(train$No.show, predict(model, train))
test_accuracy <- dnn_accuracy
overfitting <- ifelse(train_accuracy > test_accuracy, "Yes", "No")
print(paste("Overfitting:", overfitting))

#   2) Tune the model using GridSearchCV 
# Perform Grid Search for SVM
library(caret)
svm_grid <- expand.grid(C = c(0.1, 1, 10), gamma = c(0.1, 1, 10))
svm_tuned <- train(No.show ~ ., data = train, method = "svmRadial", tuneGrid = svm_grid)
svm_tuned_predicted <- predict(svm_tuned, test)
svm_tuned_accuracy <- calculateAccuracy(test$No.show, svm_tuned_predicted)

# Perform Grid Search for Decision Tree
dt_grid <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))
dt_tuned <- train(No.show ~ ., data = train, method = "rpart", tuneGrid = dt_grid)
dt_tuned_predicted <- predict(dt_tuned, test)
dt_tuned_accuracy <- calculateAccuracy(test$No.show, dt_tuned_predicted)

# Print tuned accuracies
print(paste("Tuned SVM Accuracy:", svm_tuned_accuracy))
print(paste("Tuned Decision Tree Accuracy:", dt_tuned_accuracy))

# 3) Evaluate the performance of the SVM, Decision tree and Deep Neural Network classifier on the 
# dataset based on the following criteria: Accuracy, Sensitivity and Specificity. Identify the model that 
# performed best and worst according to each criterion.
# Evaluate the performance of the SVM classifier
svm_confusion <- confusionMatrix(data = svm_predict, reference = test$No.show)
svm_accuracy <- svm_confusion$overall["Accuracy"]
svm_sensitivity <- svm_confusion$byClass["Sensitivity"]
svm_specificity <- svm_confusion$byClass["Specificity"]

# Evaluate the performance of the decision tree classifier
dt_confusion <- confusionMatrix(data = dt_predict, reference = test$No.show)
dt_accuracy <- dt_confusion$overall["Accuracy"]
dt_sensitivity <- dt_confusion$byClass["Sensitivity"]
dt_specificity <- dt_confusion$byClass["Specificity"]

# Evaluate the performance of the deep neural network classifier
dnn_predicted_probs <- predict(model, x_input_test)
dnn_predicted <- ifelse(dnn_predicted_probs > 0.5, 1, 0)
dnn_confusion <- confusionMatrix(dnn_predicted, test$No.show)
dnn_accuracy <- dnn_confusion$overall["Accuracy"]
dnn_sensitivity <- dnn_confusion$byClass["Sensitivity"]
dnn_specificity <- dnn_confusion$byClass["Specificity"]

# Print the evaluation results
print("Accuracy:")
print(paste("SVM:", svm_accuracy))
print(paste("Decision Tree:", dt_accuracy))
print(paste("Deep Neural Network:", dnn_accuracy))
cat("\n")
print("Sensitivity:")
print(paste("SVM:", svm_sensitivity))
print(paste("Decision Tree:", dt_sensitivity))
print(paste("Deep Neural Network:", dnn_sensitivity))
cat("\n")
print("Specificity:")
print(paste("SVM:", svm_specificity))
print(paste("Decision Tree:", dt_specificity))
print(paste("Deep Neural Network:", dnn_specificity))

# ------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------
# 4) Carry out a ROC analysis to compare the performance of the SVM model with the Decision Tree 
# model. Plot the ROC graph of the models.

library(ROCR)

# Predict the class probabilities for SVM and decision tree models
svm_probabilities <- predict(svm_model, test, type = "prob")[, 2]
dt_probabilities <- predict(dt_model, test, type = "prob")[, 2]

# Create prediction objects for SVM and decision tree models
svm_prediction <- prediction(svm_probabilities, test$No.show)
dt_prediction <- prediction(dt_probabilities, test$No.show)

# Calculate the performance metrics for SVM and decision tree models
svm_performance <- performance(svm_prediction, "tpr", "fpr")
dt_performance <- performance(dt_prediction, "tpr", "fpr")

# Plot the ROC curves for SVM and decision tree models
plot(svm_performance, col = "blue", lwd = 2, main = "ROC Curve Comparison")
plot(dt_performance, col = "red", lwd = 2, add = TRUE)

# Add a legend to the plot
legend("bottomright", legend = c("SVM", "Decision Tree"), col = c("blue", "red"), lwd = 2)

# #   part2: Clustering
framingham <- read.csv("framingham.csv")
framingham_subset <- data.frame(male = framingham$male,
                                age = scale(framingham$age))

install.packages("cluster")
install.packages("factoextra")
install.packages("ggsignif")
install.packages("rstatix")
install.packages("backports")

set.seed(123) # to ensure reproducibility
kmeans_fit <- kmeans(framingham_subset, centers = 4, nstart = 25)

library(backports)
library(cluster)
library(factoextra)
library(ggsignif)
library(rstatix)
# 1) Plot the clusters
library(cluster)
fviz_cluster(kmeans_fit, geom = "point", data = framingham_subset,stand = FALSE) +
  labs(title = "K-means Clustering of Framingham Data (Sex and Age)")+scale_x_discrete(labels=c('female','male'),limits=c(0,1))

elbow <- fviz_nbclust(framingham_subset, kmeans, method = "wss")

# 2) Plot the elbow curve
elbow

# calculate the Silhouette Coefficient for each point
silhouette_score <- silhouette(kmeans_fit$cluster, dist(framingham_subset))

# 3) Plot the Silhouette Coefficient
fviz_silhouette(silhouette_score)



