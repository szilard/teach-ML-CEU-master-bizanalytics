library(h2o)
h2o.init(nthreads=-1)

dx_train <- h2o.importFile("https://s3.amazonaws.com/benchm-ml--main/train-0.1m.csv")
dx_test <- h2o.importFile("https://s3.amazonaws.com/benchm-ml--main/test.csv")
md <- h2o.randomForest(x = 1:8, y = 9, training_frame = dx_train, ntrees = 50)  
h2o.performance(md, dx_test)@metrics$AUC