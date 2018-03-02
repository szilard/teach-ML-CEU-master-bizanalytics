
## install lightgbm
##
## install.packages("devtools")
## devtools::install_github("Microsoft/LightGBM", subdir = "R-package")
##
## might need cmake: 
## sudo apt-get install cmake

library(readr)
library(lightgbm)
library(ROCR)


d <- read_csv("wk09/lect/data/airline100K.csv")

set.seed(123)
N <- nrow(d)
idx <- sample(1:N, 0.6*N)
d_train <- d[idx,]
d_test <- d[-idx,]


X <- Matrix::sparse.model.matrix(dep_delayed_15min ~ . - 1, data = d)
X_train <- X[idx,]
X_test <- X[-idx,]

dlgb_train <- lgb.Dataset(data = X_train, label = ifelse(d_train$dep_delayed_15min=='Y',1,0))

system.time({
  md <- lgb.train(data = dlgb_train, objective = "binary", 
                  nrounds = 100, num_leaves = 512, learning_rate = 0.1)
})
  

phat <- predict(md, data = X_test)

rocr_pred <- prediction(phat, d_test$dep_delayed_15min)
performance(rocr_pred, "auc")@y.values[[1]]

