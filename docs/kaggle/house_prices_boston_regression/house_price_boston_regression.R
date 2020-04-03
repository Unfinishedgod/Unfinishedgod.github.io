library(readr)
library(tidyverse)
getwd()


train_set <- read_csv("./docs/kaggle/house_prices_boston_regression/train.csv")
test_set <- read_csv("./docs/kaggle/house_prices_boston_regression/test.csv")

str(train_set)

dim(train_set)
dim(test_set)
