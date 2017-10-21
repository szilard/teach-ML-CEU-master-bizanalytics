library(dplyr)
library(readr)
library(purrr)

library(rpart)
library(ggplot2)

set.seed(1234)

# DATA, CREATE TRAIN/TEST SPLIT

# https://www.kaggle.com/harlfoxem/housesalesprediction/data
data <- read_csv("kc_house_data.csv")

test_ratio <- 0.5
data_train <- data %>% sample_frac(test_ratio)
data_test <- anti_join(data, data_train, by = "id")

#

RMSE <- function(x, true_x) sum((x - true_x)^2)

regularization_parameters <- c(1, 2, 5, 10, 20, 30, 40, 50) # minbucket

results <- map_df(
    regularization_parameters,
    ~ {
        param <- .
        model <- rpart(
            formula = log(price) ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + sqft_above + sqft_basement + yr_built + yr_renovated + zipcode + lat + long + sqft_living15 + sqft_lot15,
            data = data_train,
            control = rpart.control(xval = 0, cp = 0.00001, minbucket = param),
            method = "anova")

        train_error <- RMSE(predict(model, data_train), log(data_train[["price"]]))
        test_error <- RMSE(predict(model, data_test), log(data_test[["price"]]))
        data_frame("error_type" = c("train", "test"),
                   "error_value" = c(train_error, test_error),
                   "parameter" = param)
    }
) %>% rbind()

ggplot(results) + geom_line(aes(x = parameter, y = error_value, color = error_type))
