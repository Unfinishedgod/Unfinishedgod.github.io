library(readr)
library(tidyverse)


train_set <- read_csv("./docs/kaggle/house_prices_boston_regression/train.csv")
test_set <- read_csv("./docs/kaggle/house_prices_boston_regression/test.csv")

str(train_set)

dim(train_set)
dim(test_set)

sum(is.na(train_set))
sum(is.na(test_set))

colSums(is.na(train_set))
colSums(is.na(test_set))
