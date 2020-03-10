library(tidyverse)
library(lubridate)
library(stringr)
library(caret)
library(readr)
library(gridExtra)
library(xgboost)
library(Metrics)
library(ggplot2)


# Data Fields
# datetime - hourly date + timestamp  
# season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
# holiday - whether the day is considered a holiday
# workingday - whether the day is neither a weekend nor holiday
# weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
# temp - temperature in Celsius
# atemp - "feels like" temperature in Celsius
# humidity - relative humidity
# windspeed - wind speed
# casual - number of non-registered user rentals initiated
# registered - number of registered user rentals initiated
# count - number of total rentals


train_set <- read_csv("Bike_Sharing_Demand/train.csv")
test_set <- read_csv("Bike_Sharing_Demand/test.csv")
submission <- read_csv("Bike_Sharing_Demand/sampleSubmission.csv")

# remove casual registered
train_set <- train_set %>% 
  select(-casual, -registered) %>%  
  mutate(
    year = year(datetime),
    month = month(datetime),
    hour = hour(datetime),
    wday = wday(datetime))


test_set <- test_set %>% 
  mutate(
    year = year(datetime),
    month = month(datetime),
    hour = hour(datetime),
    wday = wday(datetime))


str(train_set)
summary(train_set)


# Data Visualization
# The count vs temperature plot shows that rental count increases as the temperature increases.
train_set_vis <- train_set

train_set_vis$season  <- factor(train_set_vis$season, labels = c("Spring", "Summer", "Fall", "Winter"))
train_set_vis$weather <- factor(train_set_vis$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
train_set_vis$holiday <- factor(train_set_vis$holiday)
train_set_vis$workingday <- factor(train_set_vis$workingday)
train_set_vis$year <- factor(train_set_vis$year)
train_set_vis$month <- factor(train_set_vis$month)
# train_set_vis$hour <- factor(train_set_vis$hour)
train_set_vis$wday <- factor(train_set_vis$wday, labels = c("Sun","Mon", "Tue","Wed","Thu","Fir","Sat"))


non_hour_list <- (colnames(train_set_vis) != "count")%>% 
  which()

lst <- map(non_hour_list, function(i) {
  df_list <- colnames(train_set_vis)[i]
  
  train_set_vis %>% 
    select(df_list, count) %>% 
    rename(aa = df_list) %>% 
    ggplot(aes(aa,count)) +
    geom_point(alpha=.2,color = "#008ABC") +
    labs(title = paste0(df_list," vs count"), x = df_list, y = "",color=df_list) +
    theme_bw() +  
    theme(legend.position = "bottom")
})

do.call(grid.arrange, lst)





factor_list <- sapply(train_set_vis, is.factor) %>% 
  which()

lst <- lapply(factor_list, function(i) {
  df_list <- colnames(train_set_vis)[i]
  
  train_set_vis %>% 
    rename(aa = df_list) %>% 
    group_by(aa, hour) %>% 
    summarise(count = sum(count)) %>% 
    ggplot(aes(x = hour, y = count, group = aa, colour = aa)) +
    labs(title = paste0("Count by ",df_list), x = "Hour", y = "Count", color = df_list) + 
    theme_bw() +
    geom_line()
})

do.call(grid.arrange, lst)


################################################################################
################################################################################

## Building Second Model with more features
# Model that attempts to predict count based off of the following features :-



## Important Finding
# This sort of model doesn’t work well given our seasonal and time series data.
# We need a model that can account for this type of trend. 
# We will get thrown off with the growth of our dataset accidentaly attributing to the winter season instead of realizing it’s just overall demand growing.


##########################################################################################
##########################################################################################
colnames(X_train)

train_set$count = log1p(train_set$count)

X_train <- train_set %>%
  select(-count, - datetime, -holiday, -weather, -atemp) %>%
  as.matrix()

y_train <- train_set$count

X_test = test_set %>% 
  select(- datetime, -holiday, -weather, -atemp) %>% 
  as.matrix()


dtrain = xgb.DMatrix(X_train, label = y_train)



#####
#####
## cv grid search
searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6), 
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = c(3:10),
                                min_child = seq(1), 
                                eta = c(0.1)
)



ntrees <- 150

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,                            
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse,currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))})
)


output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames

head(output)
tail(output)



#####
#####


model = xgb.train(data = dtrain, 
                  nround = 150, 
                  max_depth = 10, 
                  eta = 0.1, 
                  subsample = 0.6, 
                  min_child_weight = 1)


xgb.importance(feature_names = colnames(X_train), model) %>% 
  xgb.plot.importance()






preds = predict(model, X_test)
preds = expm1(preds)


solution = data.frame(datetime = test_set$datetime, count = preds)


write.csv(solution, "Bike_Sharing_Demand/solution.csv", row.names = FALSE)

