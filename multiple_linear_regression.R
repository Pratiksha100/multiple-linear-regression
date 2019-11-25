# Data Preprocessing

# Importing datset
dataset = read.csv('50_Startups.csv')
#dataset = dataset[, 2:3]

#encoding categorical variables
dataset$State = factor(dataset$State, 
                         levels = c('New York', 'California', 'Florida'), 
                         labels = c(1, 2, 3))

#splitting the data into training set and testing set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to Training Set
regressor = lm(formula = Profit ~ ., data = training_set)
#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)

# Building optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset) #dataset<-->training_set optional(to determine which statistical state is significant and which is not)
summary(regressor)


regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset) 
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset) 
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset) 
summary(regressor)


# #if you are also interested in an automatic implementation of 
# #Backward Elimination in R, here it is:
# backwardElimination <- function(x, sl) {
#   numVars = length(x)
#   for (i in c(1:numVars)){
#     regressor = lm(formula = Profit ~ ., data = x)
#     maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
#     if (maxVar > sl){
#       j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
#       x = x[, -j]
#     }
#     numVars = numVars - 1
#   }
#   return(summary(regressor))
# }
# 
# SL = 0.05
# dataset = dataset[, c(1,2,3,4,5)]
# backwardElimination(training_set, SL)