# Content from: David Huang's Data Vizualization kaggle kernel: https://www.kaggle.com/h19881812/data-vizualization/code

# install.packages('ggplot2')
# install.packages('lubridate')
# install.packages('ddply')


library(ggplot2)
library(lubridate)
library(plyr)

data_localfile <- "wk01/lect/data/bike_rental_train.csv"
train <- read.csv(data_localfile)

train$season  <- factor(train$season, labels = c("Spring", "Summer", "Fall", "Winter"))
train$weather <- factor(train$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
train$hour    <- factor(hour(ymd_hms(train$datetime)))
train$times   <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$Weekday <- wday(ymd_hms(train$datetime), label=TRUE)

## Few example plots
ggplot(train, aes(x=count))+geom_density() 
ggplot(train, aes(x=season, y=windspeed))+geom_boxplot() # easy to understand
ggplot(train, aes(x=windspeed)) + geom_density() + facet_grid(. ~ season) # difficult to understand

ggplot(train, aes(x=temp)) + geom_density() + facet_grid(. ~ season)

##################################

season_summary <- ddply(train,.(season,hour),
                        summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more in Fall, and much less in Spring.\n") + 
  theme(plot.title=element_text(size=18))

weather_summary <- ddply(train,.(weather,hour),
                         summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more when the weather is Good.\n") + 
  theme(plot.title=element_text(size=18))


day_summary <- ddply(train,.(Weekday,hour),
                     summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = Weekday)) +
  geom_point(data = day_summary, aes(group=Weekday)) +
  geom_line(data = day_summary, aes(group=Weekday)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes for morning/evening commutes on weekdays,
          and daytime rides on weekends\n")


weather_prob <- ddply(train,.(season, hour),
                      summarise, Good = mean(weather == "Good"),
                      Normal = mean(weather == "Normal"),
                      Bad = mean(weather == "Bad"),
                      Very_bad = mean(weather == "Very Bad"))


ggplot(train, aes(x = hour, y = Good, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Good") +
  theme_minimal() +
  ggtitle("The probability of Good weather is higher in Fall. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Normal, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Normal") +
  theme_minimal() +
  ggtitle("The probability of Normal weather is higher in Spring. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Bad, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Bad") +
  theme_minimal() +
  ggtitle("The probability of Bad weather is higher in Summer and Winter. \n") + 
  theme(plot.title=element_text(size=18))


