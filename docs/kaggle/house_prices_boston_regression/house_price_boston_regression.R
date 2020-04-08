library(readr)
library(tidyverse)


train_set <- read_csv("./docs/kaggle/house_prices_boston_regression/train.csv")
test_set <- read_csv("./docs/kaggle/house_prices_boston_regression/test.csv")

colnames(train_set) %>% 
  data.frame()

dim(train_set)
dim(test_set)

sum(is.na(train_set))
sum(is.na(test_set))

colSums(is.na(train_set)) 
colSums(is.na(test_set))



set.seed(2018)
quick_RF <- randomForest(x=all[1:1460,-79], y=all$SalePrice[1:1460], ntree=100, importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")



all %>% 
  replace_na(0)

which(colnames(all) == "SalePrice")

colSums(is.na(all))
all$SalePrice