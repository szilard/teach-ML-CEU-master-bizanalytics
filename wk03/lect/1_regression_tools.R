# install.packages('ggplot2')
# install.packages('lubridate')
# install.packages('ddply')
# install.packages('data.table')
# install.packages('caret')
# install.packages('party')
# install.packages('glmnet')
# install.packages('doMC')

library(ggplot2)
library(lubridate)
library(plyr)
library(data.table)
library(caret)
library(party)
library(doMC)


registerDoMC(cores = 4)

GetBikeData <- function(filePath) {
  dt <- fread(filePath)
  dt$quarter    <- factor(dt$season, labels = c("Q1", "Q2", "Q3", "Q4"))
  dt[weather == 4, weather := 3]  # Remove Very Bad weather, since we have only one instance
  dt$weather    <- factor(dt$weather, levels = 1:3, labels = c("Good", "Normal", "Bad"))
  dt$hour       <- hour(ymd_hms(dt$datetime))
  dt$hourFactor <- factor(dt$hour)
  dt$times      <- as.POSIXct(strftime(ymd_hms(dt$datetime), format="%H:%M:%S"), format="%H:%M:%S")
  dt$weekday    <- wday(ymd_hms(dt$datetime))
  return(dt)
}

RootMeanSquaredError <- function(real_value, predicted_value) {
  return(sqrt(mean((real_value - predicted_value) ^ 2)))
}

bikeTrain <- GetBikeData("data/bike_rental/bike_rental_train.csv")
bikeTrain$weather

bikeTest <- GetBikeData("data/bike_rental/bike_rental_test.csv")
bikeTest

ggplot(bikeTrain, aes(x=count)) + geom_histogram() 

##################################################
# 1 Linear Models
##################################################

#   1.a Factor vs integer
##########################

lmSeasonModel <- lm(count~season, bikeTrain)
summary(lmSeasonModel)
postResample(bikeTest$count, predict(lmSeasonModel, bikeTest))

# Remember: quarter <- factor(season)
lmQuarterModel <- lm(count~quarter, bikeTrain)
summary(lmQuarterModel)
postResample(bikeTest$count, predict(lmQuarterModel, bikeTest))

# Remember: lmQuarterModel is the same as our ModelByQuarter manual model was (see RMSE)
ModelByQuarter <- function(train, test) {
  new_test = copy(test)
  for (q in unique(train$quarter)) {
    new_test[quarter == q, prediction := mean(train[quarter == q]$count) ]
  }
  return(new_test$prediction)
}
postResample(bikeTest$count, ModelByQuarter(bikeTrain, bikeTest))

#   1.b Let's build a more complex model
#########################################
lmComplex <- lm(count~quarter+temp+atemp+weather+hour+holiday+workingday+windspeed, bikeTrain)
summary(lmComplex)
postResample(bikeTest$count, predict(lmComplex, bikeTest))

lmHourFactorComplex <- lm(count~quarter+temp+atemp+weather+hourFactor+holiday+workingday+windspeed, bikeTrain)
summary(lmHourFactorComplex)
postResample(bikeTest$count, predict(lmHourFactorComplex, bikeTest))

#   Question: why is lmComplex different from lmHourFactorComplex?

#   1.c Let's introduce caret
#########################################
trctrl <- trainControl(method = "none")
lmCaret <- train(count~quarter+temp+atemp+weather+hour+holiday+workingday+windspeed, 
                  data = bikeTrain, 
                  method = "lm",
                  trControl=trctrl
)
lmCaret
postResample(bikeTest$count, predict(lmCaret, bikeTest))

#   1.d Lasso, Ridge and Elastic Net
#########################################

# Remember: Ridge, when alpha to 0. Lasso, when alpha to 1
set.seed(123)
trctrl <- trainControl(method = "cv")
lmElasticNetCaret <- train(count~quarter+temp+atemp+weather+hour+holiday+workingday+windspeed, 
                 data = bikeTrain, 
                 method = "glmnet",
                 trControl=trctrl,
                 tuneLength=5
                 #tuneGrid = data.frame(lambda=seq(0.1,1,0.1), alpha=rep(1, 10))
)
plot(lmElasticNetCaret)
lmElasticNetCaret
coef(lmElasticNetCaret$finalModel, lmElasticNetCaret$bestTune$lambda)

postResample(bikeTest$count, predict(lmElasticNetCaret, bikeTest))

# Remember: Interpret the scale and the sign of coefficients


##################################################
# 2 Nearest Neighbors
##################################################


# Exercise 2: 
#   - find additional features (to season, weekday), and find a good combination for best prediction
#   - find a good crossvalidation size (number)
#   - does repeat have an impact?
#   - does preProcessing improve results?
#   - can you improve resulst by setting k values manually?
set.seed(123)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnModel <- train(count~season+workingday, data = bikeTrain, method = "knn",
                 trControl=trctrl,
                 preProcess=c("center", "scale"),
                 tuneLength=10
                 #tuneGrid = data.frame(k=c(2:8))
                 )
knnModel
postResample(bikeTest$count, predict(knnModel, bikeTest))


##################################################
# 3 Trees
##################################################

# Exercise 3/A: 
#   - Find good input features 
#   - find maxdepth
#   - find minsplit
library(rpart)

treeSimpleModel <- ctree(count~season+weekday, data=bikeTrain,
                         controls = ctree_control())
RootMeanSquaredError(bikeTest$count, predict(treeSimpleModel, bikeTest))

treeSimpleModel <- ctree(count~season+holiday+workingday+temp+hour, data=bikeTrain,
                         controls = ctree_control()) # maxdepth = 10, minsplit=10, mincriterion=0.99
postResample(bikeTest$count, predict(treeSimpleModel, bikeTest))
plot(treeSimpleModel)

# Exercise 3/B:
#   - cp (minimum R^2 improvement) parameter is new here, can you find the best values for it?
#   - would rpart2 (using maxdepth) improve results? 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
treeCPModel <- train(count~quarter+holiday+workingday+temp+hour, data = bikeTrain, method = "rpart",
                  trControl=trctrl,
                  tuneLength = 20
                  #tuneGrid = data.frame(cp=seq(0.000001, 0.001, 0.0001))
                  )
treeCPModel
postResample(bikeTest$count, predict(treeCPModel, bikeTest))

treeMDModel <- train(count~quarter+holiday+workingday+temp+hour, data = bikeTrain, method = "rpart2",
                     trControl=trctrl,
                     tuneLength = 20
                     #tuneGrid = data.frame(maxdepth=seq(1,10))
)
treeMDModel
postResample(bikeTest$count, predict(treeMDModel, bikeTest))


##################################################
# 4 Support Vector Machine
##################################################

#   4.a Linear model
#################################################
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(123)
svmLinearModel <- train(count~quarter+holiday+workingday+temp+hour, 
                    data = bikeTrain, 
                    method = "svmLinear",
                    trControl=trctrl,
                    #preProcess = c("center", "scale"),
                    tuneLength = 10)
plot(svmLinearModel)
postResample(bikeTest$count, predict(svmLinearModel, bikeTest))

#   4.b Non-Linear model
#################################################

set.seed(123)
svmRadialModel <- train(count~quarter+temp+hour, 
                        data = bikeTrain, 
                        method = "svmRadial",
                        trControl=trctrl,
                        tuneLength = 10)
svmRadialModel
plot(svmRadialModel)
postResample(bikeTest$count, predict(svmRadialModel, bikeTest))  # 129.02

