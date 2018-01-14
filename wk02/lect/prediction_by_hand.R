# install.packages('ggplot2')
# install.packages('lubridate')
# install.packages('ddply')
# install.packages('data.table')
# install.packages('caret')

library(ggplot2)
library(lubridate)
library(plyr)
library(data.table)
library(caret)

GetBikeData <- function(filePath) {
  dt <- fread(filePath)
  dt$quarter <- factor(dt$season, labels = c("Q1", "Q2", "Q3", "Q4"))
  dt$weather <- factor(mapvalues(dt$weather, from = c(1, 2, 3, 4), to = c("Good", "Normal", "Bad", "Very Bad")))
  dt$hour    <- factor(hour(ymd_hms(dt$datetime)))
  dt$times   <- as.POSIXct(strftime(ymd_hms(dt$datetime), format="%H:%M:%S"), format="%H:%M:%S")
  dt$weekday <- wday(ymd_hms(dt$datetime))
  return(dt)
}

# GetBikeData("wk01/lect/data/bike_rental_train.csv")
# train_df <- bikeData[datetime <= as.Date('2011-12-31')]
# test_df <- bikeData[datetime > as.Date('2011-12-31')]
# write.table(train_df, "data/bike_rental/bike_rental_train.csv", row.names=FALSE)
# write.table(test_df, "data/bike_rental/bike_rental_test.csv", row.names=FALSE)

bikeTrain <- GetBikeData("data/bike_rental/bike_rental_train.csv")
bikeTrain


ggplot(bikeTrain, aes(x=count)) + geom_histogram() 

RootMeanSquaredError <- function(real_value, predicted_value) {
  return(sqrt(mean((real_value - predicted_value) ^ 2)))
}

bikeTrain$prediction_model_zero <- 0
RootMeanSquaredError(bikeTrain$count, bikeTrain$prediction_model_zero)

bikeTrain$prediction_model_200 <- 200
RootMeanSquaredError(bikeTrain$count, bikeTrain$prediction_model_200)

bikeTrain$prediction_model_median <- median(bikeTrain$count)
RootMeanSquaredError(bikeTrain$count, bikeTrain$prediction_model_median)

bikeTrain$prediction_model_mean <- mean(bikeTrain$count)
RootMeanSquaredError(bikeTrain$count, bikeTrain$prediction_model_mean)

bikeTrain[, .(count = mean(count)), by=.(quarter)]
bikeTrain[quarter == "Q1", prediction_model_by_season := 71.90552]
bikeTrain[quarter == "Q2", prediction_model_by_season := 160.94075]
bikeTrain[quarter == "Q3", prediction_model_by_season := 186.99487]
bikeTrain[quarter == "Q4", prediction_model_by_season := 154.78713]
RootMeanSquaredError(bikeTrain$count, bikeTrain$prediction_model_by_season)

# Stages of the previous block: idea, model fit and model evaluation
ModelBySeason <- function(train, test) {
  new_test = copy(test)
  for (q in unique(train$quarter)) {
    new_test[quarter == q, prediction := mean(train[quarter == q]$count) ]
  }
  return(new_test$prediction)
}

RootMeanSquaredError(bikeTrain$count, ModelBySeason(bikeTrain, bikeTrain))

ModelByHour <- function(train, test) {
  new_test = copy(test)
  new_test$model_prediction <- mean(train$count)
  for (h in unique(train$hour)) {
    new_test[hour == h, prediction := mean(train[hour == h]$count) ]
  }
  return(new_test$prediction)
}

RootMeanSquaredError(bikeTrain$count, ModelByHour(bikeTrain, bikeTrain))

ModelByHourSeason <- function(train, test) {
  new_test = copy(test)
  new_test$model_prediction <- mean(train$count)

  for (h in unique(train$hour)) {
    for (q in unique(train$quarter)) {
      train_subset = train[quarter == q & hour == h]
      if (nrow(train_subset) > 0) {
        new_test[quarter == q & hour == h, 
               model_prediction := mean(train_subset$count) ]
      }
    }
  }
  return(new_test$model_prediction)
}
RootMeanSquaredError(bikeTrain$count, ModelByHourSeason(bikeTrain, bikeTrain))

ModelByHourSeasonTemp <- function(train, test) {
  new_test = copy(test)
  new_test$model_prediction <- mean(train$count)

  for (h in unique(train$hour)) {
    for (q in unique(train$quarter)) {
      for (t in unique(round(train$temp)))
      {
        train_subset = train[quarter == q & hour == h & round(temp) == t]
        if (nrow(train_subset) > 0) {
          new_test[quarter == q & hour == h & round(temp) == t,
                 model_prediction := mean(train_subset$count) ]
        }
      }
    }
  }
  return(new_test$model_prediction)
}
RootMeanSquaredError(bikeTrain$count, ModelByHourSeasonTemp(bikeTrain, bikeTrain))

ModelByHourSeasonHumidity <- function(train, test) {
  new_test = copy(test)
  new_test$model_prediction <- mean(train$count)

  for (h in unique(train$hour)) {
    for (q in unique(train$quarter)) {
      for (hum in unique(train$humidity))
      {
        train_subset = train[quarter == q & hour == h & humidity == hum]
        if (nrow(train_subset) > 0) {
          new_test[quarter == q & hour == h & humidity == hum,
                 model_prediction := mean(train_subset$count) ]
        }
      }
    }
  }
  return(new_test$model_prediction)
}

RootMeanSquaredError(bikeTrain$count, ModelByHourSeasonHumidity(bikeTrain, bikeTrain))

# What is the 

unique(bikeTrain$season)
unique(bikeTrain$hour)
unique(bikeTrain$temp)
unique(bikeTrain$humidity)

bikeTest <- GetBikeData("data/bike_rental/bike_rental_test.csv")

RootMeanSquaredError(bikeTest$count, ModelBySeason(bikeTrain, bikeTest))
RootMeanSquaredError(bikeTest$count, ModelByHour(bikeTrain, bikeTest))
RootMeanSquaredError(bikeTest$count, ModelByHourSeason(bikeTrain, bikeTest))
RootMeanSquaredError(bikeTest$count, ModelByHourSeasonTemp(bikeTrain, bikeTest))
RootMeanSquaredError(bikeTest$count, ModelByHourSeasonHumidity(bikeTrain, bikeTest))

outOfSampleError <- data.table(method=factor(1:5, labels = c("Season", "Hour", "HourSeason", "HourTemp", "HourHumidity")),
                               in_sample_error=c(126.2283, 91.8762, 75.13035, 57.09002, 44.85029),
                               out_of_sample_error=c(223.825, 173.2504, 163.4772, 169.7447, 189.6957)
           )
outOfSampleError$method <- factor(outOfSampleError$method)
outOfSampleError <- melt(outOfSampleError, id=c("method"))

ggplot(outOfSampleError, aes(x=method, y=value, color=variable)) + geom_point(size=5)
