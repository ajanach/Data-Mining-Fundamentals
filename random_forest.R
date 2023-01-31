## ZAD 3 Ansambli - slučajne šume

# requirements: 
# install.packages("randomForest")
library(randomForest)
library(caret)
library(ggplot2)

# loading the dataset:
data <- read.csv("diabetes.csv")
data$Outcome <- as.factor(data$Outcome)

# splitting dataset into testing and training: 
set.seed(123)
split_index <- createDataPartition(data$Outcome, p = 0.7, list = FALSE)
train_data <- data[split_index, ]
test_data <- data[-split_index, ]

# set the number of trees in the ensemble
ntrees <- 100

# create the control object
control1 <- trainControl(method = "cv", number = 10)
control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# train the ensemble model
ensemble_model1 <- train(Outcome ~ ., data = train_data, method = "rf", trControl = control1, ntree = ntrees)
ensemble_model2 <- train(Outcome ~ ., data = train_data, method = "rf", trControl = control2, ntree = ntrees)

plot(ensemble_model1)
plot(ensemble_model2)

# make predictions on new data
predictions1 <- predict(ensemble_model1, test_data)
predictions2 <- predict(ensemble_model2, test_data)

# calculate the calibration error:
ce(predictions1, test_data$Outcome)
ce(predictions2, test_data$Outcome)

# create confusion matrix
confusionMatrix(predictions1, test_data$Outcome)
confusionMatrix(predictions2, test_data$Outcome)


# calculate accuracy values
acc1 <- confusionMatrix(predictions1, test_data$Outcome)$overall[1]
acc2 <- confusionMatrix(predictions2, test_data$Outcome)$overall[1]

# create data frame with accuracy values
acc_data <- data.frame(method = c("control1_cv", "control2_repeated_cv"),
                       accuracy = c(acc1, acc2))

# plot the graph
ggplot(acc_data, aes(x = method, y = accuracy)) +
  geom_bar(stat = "identity") +
  xlab("Method") +
  ylab("Accuracy") +
  ggtitle("Accuracy of Ensemble Models")
