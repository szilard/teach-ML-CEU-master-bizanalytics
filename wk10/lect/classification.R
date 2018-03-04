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
# install.packages('randomForest')


library(ggplot2)
library(lubridate)
library(plyr)
library(data.table)
library(caret)
library(party)
library(doMC)
library(e1071)
library(pROC)
library(randomForest)
library(lightgbm)


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
set.seed(123)
trainIndex <- createDataPartition(SpamAll$spam, p = .8, list = FALSE, times = 1)
SpamTrain <- SpamAll[trainIndex, ]
SpamTest <- SpamAll[-trainIndex, ]

sum((as.numeric(SpamTrain$spam))-1)/nrow(SpamTrain)

sum((as.numeric(SpamTest$spam)-1)/nrow(SpamTest))

expand.grid(mtry=seq(1,15,5), ntree=c(100, 150, 200))

set.seed(123)
trctrlRF <- trainControl(method = "repeatedcv", classProbs=TRUE, number = 10, repeats = 1)
rfModel <- train(spam~., 
                 data = SpamTrain, 
                 method = "rf",
                 preProcess = c("center", "scale"),
                 trControl=trctrlRF,
                 ntree=100,
                 tuneLength = 10
                 )
rfModel
rfModelRoc <- roc(predictor = predict(rfModel, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
rfModelRoc
plot(rfModel)


set.seed(123)
trctrlXgbTree <- trainControl(method = "repeatedcv", classProbs=TRUE, number = 10, repeats = 3)
xgbTreeModel <- train(spam~., 
                 data = SpamTrain, 
                 method = "gbm",
                 preProcess = c("center", "scale"),
                 trControl=trctrlXgbTree,
                 tuneLength = 10
)
rfModel
rfModelRoc <- roc(predictor = predict(rfModel, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
rfModelRoc
plot(rfModel)


set.seed(123)
trctrl <- trainControl(method = "repeatedcv", classProbs=TRUE, number = 10, repeats = 1,
                       preProcOptions = list(thresh=0.6))
svmRadialModel <- train(spam~., 
                        data = SpamTrain, 
                        method = "svmRadial",
                        preProcess = c("center", "scale", "pca"),
                        trControl=trctrl,
                        tuneLength = 10)
svmRadialModel
svmRadialModelRoc <- roc(predictor = predict(svmRadialModel, SpamTest, type='prob', decision.values=T)$Spam, response = SpamTest$spam)
svmRadialModelRoc
plot(svmRadialModel)




