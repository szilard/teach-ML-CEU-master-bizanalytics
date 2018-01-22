# install.packages('ggplot2')
# install.packages('lubridate')
# install.packages('ddply')
# install.packages('data.table')
# install.packages('caret')
# install.packages('party')
# install.packages('glmnet')
# install.packages('doMC')
# install.packages('e1071')
# install.packages('pROC')

library(ggplot2)
library(lubridate)
library(plyr)
library(data.table)
library(caret)
library(party)
library(doMC)
library(e1071)
library(pROC)


# data: https://archive.ics.uci.edu/ml/datasets/Spambase

# # from remote:
# data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
# data_localfile <- "wk-05-ML/data/spam.csv"
# download.file(data_url, data_localfile)
# #
# data_meta <- "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.names"
# col_names <- readLines(data_meta)[34:90]
# col_names <- unlist(lapply(strsplit(col_names,":"), `[[`, 1))
# col_names <- gsub(";","semicolon",col_names, fixed = TRUE) 
# col_names <- gsub("(","parenth",col_names, fixed = TRUE)  
# col_names <- gsub("[","brack",col_names, fixed = TRUE)   ## quick & dirty
# col_names <- gsub("!","excl",col_names, fixed = TRUE)
# col_names <- gsub("$","dollar",col_names, fixed = TRUE)
# col_names <- gsub("#","hash",col_names, fixed = TRUE)
# col_names <- c(col_names, "spam")
# # dput(col_names)


registerDoMC(cores = 4)

GetSpamData <- function(filePath) {
  col_names <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", 
                 "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet", 
                 "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", 
                 "word_freq_people", "word_freq_report", "word_freq_addresses", 
                 "word_freq_free", "word_freq_business", "word_freq_email", "word_freq_you", 
                 "word_freq_credit", "word_freq_your", "word_freq_font", "word_freq_000", 
                 "word_freq_money", "word_freq_hp", "word_freq_hpl", "word_freq_george", 
                 "word_freq_650", "word_freq_lab", "word_freq_labs", "word_freq_telnet", 
                 "word_freq_857", "word_freq_data", "word_freq_415", "word_freq_85", 
                 "word_freq_technology", "word_freq_1999", "word_freq_parts", 
                 "word_freq_pm", "word_freq_direct", "word_freq_cs", "word_freq_meeting", 
                 "word_freq_original", "word_freq_project", "word_freq_re", "word_freq_edu", 
                 "word_freq_table", "word_freq_conference", "char_freq_semicolon", 
                 "char_freq_parenth", "char_freq_brack", "char_freq_excl", "char_freq_dollar", 
                 "char_freq_hash", "capital_run_length_average", "capital_run_length_longest", 
                 "capital_run_length_total", "spam")
  
  d <- fread(filePath, header = FALSE, col.names = col_names)
  d$spam <- factor(d$spam, levels = 0:1, labels = c("NotSpam", "Spam"))
  return(d)
}
SpamAll <- GetSpamData("data/spam/spam.csv")
nrow(SpamAll)

# https://topepo.github.io/caret/data-splitting.html
trainIndex <- createDataPartition(SpamAll$spam, p = .8, list = FALSE, times = 1)
SpamTrain <- SpamAll[trainIndex, ]
SpamTest <- SpamAll[-trainIndex, ]

sum((as.numeric(SpamTrain$spam))-1)/nrow(SpamTrain)

sum((as.numeric(SpamTest$spam)-1)/nrow(SpamTest))

##################################################
# 1 Linear Models
##################################################

#   1.a Factor vs integer
##########################

lmSeasonModel <- lm(spam~., SpamTrain)
summary(lmSeasonModel)
postResample(SpamTest$count, predict(lmSeasonModel, SpamTest))

predict(lmSeasonModel, SpamTest)

# Remember: quarter <- factor(season)
lmQuarterModel <- lm(count~quarter, SpamTrain)
summary(lmQuarterModel)
postResample(SpamTest$count, predict(lmQuarterModel, SpamTest))


#   1.c Let's introduce caret
#########################################
trctrl <- trainControl(method = "cv")
lmCaret <- train(spam~., 
                  data = SpamTrain, 
                  method = "glm",
                  trControl=trctrl
)
lmCaret
postResample(SpamTest$spam, predict(lmCaret, SpamTest))

colnames(SpamTrain, 2)

#   1.d Lasso, Ridge and Elastic Net
#########################################
SpamTrain
# Remember: Ridge, when alpha to 0. Lasso, when alpha to 1
set.seed(123)
trctrl <- trainControl(method = "cv", classProbs=TRUE, summaryFunction=twoClassSummary)
lmElasticNetCaret <- train(spam~., 
                 data = SpamTrain, 
                 method = "glmnet",
                 trControl=trctrl,
                 tuneLength=5,
                 metric='AUC'
                 #tuneGrid = data.frame(lambda=seq(0.1,1,0.1), alpha=rep(1, 10))
)

lmElasticNetCaret
lmElasticNetCaretRoc <- roc(predictor = predict(lmElasticNetCaret, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
lmElasticNetCaretRoc
plot(lmElasticNetCaretRoc)

confusionMatrix(SpamTest$spam, predict(lmElasticNetCaret, SpamTest, decision.values=T))

plot(lmElasticNetCaret)

coef(lmElasticNetCaret$finalModel, lmElasticNetCaret$bestTune$lambda)
postResample(SpamTest$spam, predict(lmElasticNetCaret, SpamTest))

# Remember: Interpret the scale and the sign of coefficients


##################################################
# 2 Nearest Neighbors
##################################################


# Exercise 2: 
#   - find a good crossvalidation size (number)
#   - does repeat have an impact?
#   - does preProcessing improve results?
#   - can you improve resulst by setting k values manually?
set.seed(123)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnModel <- train(spam~., data = SpamTrain, method = "knn",
                 trControl=trctrl,
                 preProcess=c("center", "scale"),
                 tuneLength=20
                 #tuneGrid = data.frame(k=c(2:8))
                 )
knnModel

lmElasticNetCaret
knnModelRoc <- roc(predictor = predict(knnModel, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
knnModelRoc
plot(knnModelRoc)

postResample(SpamTest$count, predict(knnModel, SpamTest))


##################################################
# 3 Trees
##################################################

# Exercise 3/A: 
#   - Find good input features 
#   - find maxdepth
#   - find minsplit
library(rpart)

treeSimpleModel <- ctree(spam~., data=SpamTrain,
                         controls = ctree_control())
plot(treeSimpleModel)
RootMeanSquaredError(SpamTest$count, predict(treeSimpleModel, SpamTest))

treeSimpleModel <- ctree(count~season+holiday+workingday+temp+hour, data=SpamTrain,
                         controls = ctree_control()) # maxdepth = 10, minsplit=10, mincriterion=0.99
postResample(SpamTest$count, predict(treeSimpleModel, SpamTest))
plot(treeSimpleModel)

# Exercise 3/B:
#   - cp (minimum R^2 improvement) parameter is new here, can you find the best values for it?
#   - would rpart2 (using maxdepth) improve results? 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
treeCPModel <- train(spam~., data = SpamTrain, method = "rpart",
                  trControl=trctrl,
                  tuneLength = 20
                  #tuneGrid = data.frame(cp=seq(0.000001, 0.001, 0.0001))
                  )
treeCPModel

treeCPModelRoc <- roc(predictor = predict(treeCPModel, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
treeCPModelRoc
plot(treeCPModelRoc)


treeMDModel <- train(spam~., data = SpamTrain, method = "rpart2",
                     trControl=trctrl,
                     tuneLength = 20
                     #tuneGrid = data.frame(maxdepth=seq(1,10))
)
treeMDModel
treeMDModelRoc <- roc(predictor = predict(treeMDModel, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
treeMDModelRoc
plot(treeMDModelRoc)


##################################################
# 4 Support Vector Machine
##################################################

#   4.a Linear model
#################################################
trctrl <- trainControl(method = "repeatedcv", classProbs=TRUE, number = 10, repeats = 3)

set.seed(123)
svmLinearModel <- train(spam~., 
                    data = SpamTrain, 
                    method = "svmLinear",
                    trControl=trctrl,
                    #preProcess = c("center", "scale"),
                    tuneLength = 10)
svmLinearModel
svmLinearModelRoc <- roc(predictor = predict(svmLinearModel, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
svmLinearModelRoc
plot(svmLinearModelRoc)

#   4.b Non-Linear model
#################################################

set.seed(123)
svmRadialModel <- train(spam~., 
                        data = SpamTrain, 
                        method = "svmRadial",
                        trControl=trctrl,
                        tuneLength = 10)
svmRadialModel
svmRadialModelRoc <- roc(predictor = predict(svmRadialModel, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
svmRadialModelRoc
plot(svmRadialModel)

plot(svmRadialModel)
postResample(SpamTest$count, predict(svmRadialModel, SpamTest))  # 129.02

