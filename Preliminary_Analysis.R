library(boot)
library(caret)
library(rpart)
library(tidyverse)
set.seed(1)

data = read.csv("C:\\Users\\kevin\\Desktop\\data_clean.csv")
camry_data = data[data$make_second == "Camry", ]

# First, try a 80% and 20% split into training and test data.
train_index = sample(nrow(camry_data), floor(0.8*nrow(camry_data)))
train = camry_data[train_index, ]
test = camry_data[-train_index, ]


# Ordinary linear regression
lr_model = glm(listed_price~mileage+year+Accident+Past.Owners+is_fleet_use, data=train)
lr_train_pred = predict(lr_model, train, type="response")
lr_test_pred = predict(lr_model, test, type="response")
summary(lr_model)

# Check the RMSE and MAE on training set for linear regression model.
lr_train_RMSE = sqrt(mean((train$listed_price - lr_train_pred)^2))
lr_train_MAE = mean(abs(train$listed_price - lr_train_pred))

# Check the RMSE and MAE on test set for linear regression model.
lr_test_RMSE = sqrt(mean((test$listed_price - lr_test_pred)^2))
lr_test_MAE = mean(abs(test$listed_price - lr_test_pred))


# Regression tree
regtree_model = rpart(listed_price~mileage+year+Accident+Past.Owners+is_fleet_use, 
                      data = train, method = "anova")
regtree_train_pred = predict(regtree_model, train, type = "vector")
regtree_test_pred = predict(regtree_model, test, type = "vector")

# Check the RMSE and MAE on training set for regression tree model.
regtree_train_RMSE = sqrt(mean((train$listed_price - regtree_train_pred)^2))
regtree_train_MAE = mean(abs(train$listed_price - regtree_train_pred))

# Check the RMSE and MAE on test set for regression tree model.
regtree_test_RMSE = sqrt(mean((test$listed_price - regtree_test_pred)^2))
regtree_test_MAE = mean(abs(test$listed_price - regtree_test_pred))

# Plot the regression tree.
plot(regtree_model, uniform=TRUE, main="Regression Tree Model")
text(regtree_model, use.n=FALSE, all=TRUE, cex=0.9)

# 5-fold cross validation using linear regression.
train_control = trainControl(method="cv", number=5)
glm_cv5_model = train(listed_price~mileage+year+Accident+Past.Owners+is_fleet_use, 
                  data=camry_data, trControl=train_control, method="glm")
