## Code for linear and gradient boosting regression

## This code is a mixture of own knowledge, websites such as github and
# stack exchange, and previous code used for other regression models

## Packages

library(DALEX)
library(DALEXtra)
library(pdp)
library(caret)
library(Metrics)
library(gridExtra)
library(ggplot2)
library(xgboost)
library(readr)
library(tidyverse)  
library(car)        
library(MASS)       


### Linear Regression 

## This code is only for the Wyorks 2015 model, the code is repeatable for the
# other models by changng the data set from WYorks to WMids and the predictor
# variable from RightW15 to RightW19 or vice versa


# WYorks 2015 

WY15 = lm(formula = RightW15 ~ Pctwhite + PctFE + Pctprimarysector + PctEngnot1stlang + 
          Pctover60 + Pctselfemp + Pctunder10yearsinUK + PctAsian + 
          PctChristian + Pcthomeownership + Pcttop3NSSeC, data = WYorkscsv)
summary(WY15)

# Calculate VIF for each predictor variable
vif <- vif(WY15)

# Print VIF values
print(vif)



### Gradient Boosting Regression

## West Yorkshire local elections (code can be adapted for either 2015
# or 2019 elections)
## This code shows an example for one of the models, the only change needed 
# is to change the data set from Wyorks to Wmids and the response variable
# to RightW19 from RightW15 or vice versa



## Read the data using readr package

WYorks <- read.csv("WYorkscsv.csv")

# Define the predictor and response variables - 
predictors <- c("Pctwhite", "PctFE", "Pctprimarysector", "PctEngnot1stlang", "Pctover60", 
                "Pcttop3NSSeC", "Pctselfemp", "Pctunder10yearsinUK", 
                "PctAsian", "PctChristian", "Pcthomeownership")
response <- "RightW15"

set.seed(123) # for reproducibility
trainIndex <- createDataPartition(WYorks$RightW15, p = 0.7, list = FALSE)
trainData <- WYorks[trainIndex, ]
testData <- WYorks[-trainIndex, ]

# The data set created was not in the correct structure required for the 
# xgboost package, this code converts it into a matrix
trainX <- as.matrix(trainData[,predictors])
trainY <- trainData[,response]
testX <- as.matrix(testData[,predictors])
testY <- testData[,response]

# Fit the gradient boosting regression model, whilst there where many examples of
# how to produce the code for this, the code below was the most simple and efficient
set.seed(123) # for reproducibility
model <- xgboost(data = trainX, label = trainY, max_depth = 6, eta = 0.1, nrounds = 100  , objective = "reg:squarederror")

## Understanding the results

# Error values
test_pred <- predict(model, testX)
rmse <- rmse(test_pred, testY)
rmse
mae <- mean(abs(testY - test_pred))
mse <- mean((testY - test_pred)^2)
cat("Mean absolute error:", mae, "\n")
cat("Mean squared error:", mse, "\n")


# calculate feature importance scores
importance <- xgb.importance(feature_names = colnames(trainX), model = model)
# plot feature importance
xgb.plot.importance(importance_matrix = importance)

# Plots
plot(test_pred, testY)

# Create a list of PDP plots for each predictor variable
pdp_plots <- lapply(predictors, function(var) {
  pdp::partial(model, pred.var = var, plot = TRUE, train = trainX)
})

# Combine the PDP plots into a single output
output <- do.call(grid.arrange, c(pdp_plots, ncol = 4))

# Display the output
print(output)


WYorks <- read.csv("WYorkscsv.csv")

# Define the predictor and response variables
predictors <- c("Pctwhite", "PctFE", "Pctprimarysector", "PctEngnot1stlang", "Pctover60", 
                "Pcttop3NSSeC", "Pctselfemp", "Pctunder10yearsinUK", 
                 "PctChristian")
response <- "RightW15"
WYorks <- na.omit(WYorks)
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(WYorks$RightW15, p = 0.7, list = FALSE)
trainData <- WYorks[trainIndex, ]
testData <- WYorks[-trainIndex, ]

# Convert the data to the required format for xgboost
trainX <- as.matrix(trainData[,predictors])
trainY <- trainData[,response]
testX <- as.matrix(testData[,predictors])
testY <- testData[,response]
# Fit the gradient boosting regression model
set.seed(123) # for reproducibility
model <- xgboost(data = trainX, label = trainY, max_depth = 6, eta = 0.1, nrounds = 100  , objective = "reg:squarederror")

## Understanding the results
## Error values
test_pred <- predict(model, testX)
rmse <- rmse(test_pred, testY)
rmse
mae <- mean(abs(testY - test_pred))
mse <- mean((testY - test_pred)^2)
cat("Mean absolute error:", mae, "\n")
cat("Mean squared error:", mse, "\n")


# calculate feature importance scores
importance <- xgb.importance(feature_names = colnames(trainX), model = model)
# plot feature importance
xgb.plot.importance(importance_matrix = importance)

# Plots
plot(test_pred, testY)

# Create a list of PDP plots for each predictor variable
pdp_plots <- lapply(predictors, function(var) {
  pdp::partial(model, pred.var = var, plot = TRUE, train = trainX)
})

# Combine the PDP plots into a single output
output <- do.call(grid.arrange, c(pdp_plots, ncol = 4))

# Display the output
print(output)










