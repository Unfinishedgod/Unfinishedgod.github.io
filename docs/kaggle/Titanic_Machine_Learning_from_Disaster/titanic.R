library(tidyverse)
library(lubridate)
library(stringr)
library(caret)
library(readr)

train_set <- read_csv("/home/owen/Unfinishedgod.github.io/docs/kaggle/Titanic_Machine_Learning_from_Disaster/train.csv")
test_set <- read_csv("/home/owen/Unfinishedgod.github.io/docs/kaggle/Titanic_Machine_Learning_from_Disaster/test.csv")

submission <- read_csv("gender_submission.csv")

colSums(is.na(train_set))

str(train_set)
