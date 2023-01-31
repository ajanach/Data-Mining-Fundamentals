## ZAD 2 KLASIFIKACIJA KNN

# requirements:
# install.packages("Metrics")
# install.packages("caret")
library("caret")
library("Metrics")
library(ggplot2)

# loading the dataset:
data <- read.csv("diabetes.csv")
data$Outcome <- as.factor(data$Outcome)
str(data)

# splitting dataset into testing and training: 
set.seed(123)
split_index <- createDataPartition(data$Outcome, p = 0.7, list = FALSE)
train_data <- data[split_index, ]
test_data <- data[-split_index, ]

# train control:
control1 <- trainControl(method = "repeatedcv", number = 5) #k-fold
control2 <- trainControl(method = "repeatedcv", number = 10, repeats = 100) # repeated k-fold
control3 <- trainControl(method = "boot", number = 100) #bootstrap
control4 <- trainControl(method = "LOOCV")

# model training:
knn_model1 <- train(Outcome ~ ., data = train_data, method = "knn", trControl = control1)
knn_model2 <- train(Outcome ~ ., data = train_data, method = "knn", trControl = control2)
knn_model3 <- train(Outcome ~ ., data = train_data, method = "knn", trControl = control3)
knn_model4 <- train(Outcome ~ ., data = train_data, method = "knn", trControl = control4)

# prediction:
test_predictions1 <- predict(knn_model1, test_data)
test_predictions2 <- predict(knn_model2, test_data)
test_predictions3 <- predict(knn_model3, test_data)
test_predictions4 <- predict(knn_model4, test_data)

# calculate the calibration error:
ce(test_predictions1, test_data$Outcome)
ce(test_predictions2, test_data$Outcome)
ce(test_predictions3, test_data$Outcome)
ce(test_predictions4, test_data$Outcome)

# confusion matrics:
confusionMatrix(test_predictions1, test_data$Outcome)
confusionMatrix(test_predictions2, test_data$Outcome)
confusionMatrix(test_predictions3, test_data$Outcome)
confusionMatrix(test_predictions4, test_data$Outcome)


# Graphical visualization of accuracy for each model:
models <- list(knn_model1, knn_model2, knn_model3, knn_model4)
names(models) <- c("knn_k-fold", "knn_repeated_k-fold", "knn_bootstrap", "knn_LOOCV")

# calculate accuracy of each model:
accuracies <- sapply(models, function(x) {
  preds <- predict(x, test_data)
  cm <- confusionMatrix(preds, test_data$Outcome)
  cm$overall["Accuracy"]
})

# convert to data frame for plotting:
acc_df <- data.frame(model = names(accuracies), accuracy = as.numeric(accuracies))

# plot accuracy of each model:
ggplot(acc_df, aes(x = model, y = accuracy)) +
  geom_col(fill = "steelblue") +
  labs(x = "Model", y = "Accuracy", title = "Accuracies of each KNN Model") +
  theme_classic()

