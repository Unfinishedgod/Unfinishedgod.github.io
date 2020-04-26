library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(pROC)
library(h2o)
library(caret)
library(aws.s3)

# localH2O = h2o.init(nthreads=-1)

h2o.init()
# getwd()

train_set <- s3read_using(FUN = read_csv, object = "Porto_Seguro_Safe_Driver_Prediction/train.csv", bucket = "owentest")
test_set <- s3read_using(FUN = read_csv, object = "Porto_Seguro_Safe_Driver_Prediction/test_set", bucket = "owentest")

#Setting Missing values to NA. Converting _cat variables to categorical and creating dummy variable for those.
# Set missing values to NA 
train_set[train_set == -1] <- NA
test_set[test_set == -1] <- NA
# collect the categorical variable names
cat_vars <- names(train_set)[grepl('_cat$', names(train_set))]

# convert categorical features to factors
train_set <- train_set %>%
  mutate_at(.vars = cat_vars, .funs = as.factor)

test_set <- test_set %>%
  mutate_at(.vars = cat_vars, .funs = as.factor)

#One hot encode the factor variables
#train_set <- model.matrix(~ . - 1, data = train_set)

h2o.init()

#Splitting Datasets into train and validation. I am taking 75% data. 
#I will use cross validation in next version.
set.seed(32)


# index <- createDataPartition(train_set[,"target"], p = 0.75, list = FALSE)

index <- sample(1:nrow(train_set), nrow(train_set) * 0.7)

tiny_train <- train_set[index, ]
train_val <- train_set[-index, ]

tiny_train.hex  <- as.h2o(tiny_train)
train_val.hex  <- as.h2o(train_val)
test.hex <- as.h2o(test_set)


rm(train, tiny_train, train_val)
gc()

# Preparing Target and predictors.
target <- "target"
predictors <- setdiff(names(tiny_train.hex), target)

# Let's run automl now.
automl_h2o_models <- h2o.automl(
  x = predictors, 
  y = target,
  training_frame    = tiny_train.hex,
  leaderboard_frame = train_val.hex,
  max_runtime_secs = 3000
)

automl_leader <- automl_h2o_models@leader

# Predict on test set
pred_conversion <- h2o.predict(object = automl_leader, newdata = test.hex)

pred_conversion <- as.data.frame(pred_conversion)
Submission <- cbind(test$id, pred_conversion)
colnames(Submission) <- c("id", "target")
write.csv(Submission, "Submission_AutoML.csv", row.names = F)

# Any results you write to the current directory are saved as output.