vowel.train <- read.csv("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/vowel.train")
vowel.test <- read.csv("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/vowel.test")
head(vowel.train)
str(vowel.test)

# Q1 ----------------------------------------------------------------------

# Set the variable y to be a factor variable in both the training and test set. 
# Then set the seed to 33833. Fit (1) a random forest predictor relating the factor 
# variable y to the remaining variables and (2) a boosted predictor using the "gbm" 
# method. Fit these both with the train() command in the caret package.
# 
# What are the accuracies for the two approaches on the test data set? 
# What is the accuracy among the test set samples where the two methods agree?

vowel.test$y <- factor(vowel.test$y)
str(vowel.test)
vowel.train$y <- factor(vowel.train$y)
str(vowel.train)
set.seed(33833)
#fit random forest
library(caret)
rf.model <- train(y~.,data = vowel.train,method ="rf",prox=TRUE)
gbm.model <- train(y~., data=vowel.train, method = "gbm", verbose = FALSE)

#predict on test data
pred.rf <- predict(rf.model,vowel.test)
pred.gbm <- predict(gbm.model,vowel.test)

confusionMatrix(vowel.test$y,pred.gbm)$overall['Accuracy']
confusionMatrix(vowel.test$y,pred.rf)$overall['Accuracy']

pred.comm <- pred.rf ==pred.gbm
confusionMatrix(vowel.test$y[pred.comm],pred.rf[pred.comm])$overall['Accuracy']


# Q2 ----------------------------------------------------------------------

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

# Set the seed to 62433 and predict diagnosis with all the other variables using 
# a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis 
# ("lda") model. Stack the predictions together using random forests ("rf"). 
# What is the resulting accuracy on the test set? Is it better or worse than each 
# of the individual predictions?

set.seed(62433)
Al.pred.rf <- train(diagnosis ~., data = training,method = "rf", prox = TRUE)
Al.pred.gbm <- train(diagnosis ~., data=training,method = "gbm",verbose = FALSE)
Al.pred.lda <- train(diagnosis~., data = training, method = "lda")

Al.pred.test.rf <- predict(Al.pred.rf,testing)
Al.pred.test.gbm <- predict(Al.pred.gbm,testing)
Al.pred.test.lda <- predict(Al.pred.lda,testing)

qplot(Al.pred.test.gbm,Al.pred.lda, color = diagnosis, data=testing)

Al.pred.comb <- data.frame(Al.pred.test.gbm,Al.pred.test.lda,Al.pred.test.rf,diagnosis=testing$diagnosis)

comb.model <- train(diagnosis ~.,data=Al.pred.comb,method = "rf")
comb.Pred <- predict(comb.model,Al.pred.comb)

confusionMatrix(testing$diagnosis,Al.pred.test.rf)$overall["Accuracy"]
confusionMatrix(testing$diagnosis,Al.pred.test.gbm)$overall["Accuracy"]
confusionMatrix(testing$diagnosis,Al.pred.test.lda)$overall["Accuracy"]
confusionMatrix(testing$diagnosis,comb.Pred)$overall["Accuracy"]


# Q3 ----------------------------------------------------------------------

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
Which variable is the last coefficient to be set to zero as the penalty increases? 
  (Hint: it may be useful to look up ?plot.enet).

set.seed(233)
mod_lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
library(elasticnet)
plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)


# # Q4 ----------------------------------------------------------------------
# Fit a model using the bats() function in the forecast package to the training
# time series. Then forecast this model for the remaining time points. 
# For how many of the testing points is the true value within the 95% 
# prediction interval bounds?
  
  
URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"

library(lubridate) # For year() function below

dat = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)
library(forecast)
fit <- bats(tstrain)
fcast <- forecast(fit, nrow(testing), level = c(95))
plot(fcast)
points(dat$visitsTumblr)
print(sum(fcast$lower <= testing$visitsTumblr & testing$visitsTumblr <= fcast$upper) / dim(testing)[1])


# Q5 ----------------------------------------------------------------------
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

# Set the seed to 325 and fit a support vector machine using the e1071 package 
# to predict Compressive Strength using the default settings. 
# Predict on the testing set. What is the RMSE?

set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)
