library(rpart)
library(rpart.plot)
Hotel <- read.csv(file.choose(), header = TRUE)
cols_to_factor <- c("reservation_status", "hotel", "country", "meal",
                 "market_segment", "distribution_channel", "reserved_room_type")
Hotel[cols_to_factor] <- lapply(Hotel[cols_to_factor], as.factor)
set.seed(123)
train_indices <- sample(1:nrow(Hotel), 0.7 * nrow(Hotel))
train_data <- Hotel[train_indices, ]
test_data <- Hotel[-train_indices,]
tree_model <- rpart(reservation_status ~ .,
                    data = train_data,
                    method = "class",
                    control = rpart.control(cp = 0.001, minsplit = 10, maxdepth = 30))
rpart.plot(tree_model,
           type = 5,           
           extra = 104,       
           fallen.leaves = TRUE,
           cex = 0.7)         
predictions <- predict(tree_model, test_data, type = "class")
confusion_matrix <- table(Predicted = predictions, Actual = test_data$reservation_status)
print(confusion_matrix)
accuracy <- sum(predictions == test_data$reservation_status) / nrow(test_data)
cat("Accuracy:", round(accuracy * 100, 2), "%")
