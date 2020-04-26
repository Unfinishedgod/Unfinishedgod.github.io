library(readr)
library(tidyverse)
library(randomForest)

library(ggplot2)
library(h2o)

library(DT)
library(magrittr)

localH2O = h2o.init(nthreads=-1)

h2o.init()


train_set <- read_csv("./docs/kaggle/house_prices_boston_regression/train.csv")
test_set <- read_csv("./docs/kaggle/house_prices_boston_regression/test.csv")


train.hex <- as.h2o(train_set)
test.hex <- as.h2o(test_set)


h2o.describe(train.hex)


train.hex[, "Id"] <- NULL

categoryFeatures <- c(
  "MSSubClass",
  "OverallQual", 
  "OverallCond",
  "YearBuilt",
  "YearRemodAdd",
  "BsmtFullBath",
  "BsmtHalfBath",
  "FullBath",
  "HalfBath",
  "BedroomAbvGr",
  "KitchenAbvGr",
  "KitchenQual",
  "TotRmsAbvGrd",
  "Fireplaces",
  "GarageYrBlt",
  "GarageCars",
  "MoSold",
  "YrSold")

categoryFeatures
colnames(train_set)

train.hex[, categoryFeatures] <- as.factor(train.hex[, categoryFeatures])
test.hex[, categoryFeatures] <- as.factor(test.hex[, categoryFeatures])

# Table indicate that there are a lot of missing data and noninformative zeros so i'm getting rid of them. I'm assuming that columns with less then 20% of data will not improve the model ( to check )

withoutMissingColNames <- h2o.colnames(train.hex[, !as.numeric(
  as.character(
    h2o.describe(train.hex)$Missing
  )
) > (nrow(train.hex) * 0.8)])


filteredColNames <- h2o.colnames(train.hex[, !as.numeric(
  as.character(
    h2o.describe(train.hex)$Zeros
  )
) > (nrow(train.hex) * 0.8), withoutMissingColNames])


train.hex2 <- train.hex[, filteredColNames]
test.hex2 <- test.hex[, setdiff(filteredColNames, "SalePrice")]
ncol(train.hex2)



tmp <- h2o.group_by(train.hex2, 
                    by = c("YrSold", "MoSold"), 
                    nrow("SalePrice")) %>%
  as.data.frame()

ggplot(tmp, aes(MoSold, nrow_SalePrice)) +
  geom_bar(aes(fill = YrSold), stat = "identity") + 
  facet_grid(YrSold ~ .)

ggplot(tmp, aes(MoSold, MoSold)) +
  geom_bar(aes(fill = YrSold), stat = "identity") + 
  facet_grid(YrSold ~ .)



h2o.group_by(train.hex2, by = "YrSold", sum("SalePrice"))


train.hex3 <- train.hex2[train.hex2$YrSold != "2010", ]

train.hex3[, h2o.columns_by_type(train.hex3, coltype = "numeric")] %>%
  h2o.skewness()



train.hex3[, h2o.columns_by_type(train.hex3, coltype = "numeric")] %>%
  h2o.log1p() %>%
  h2o.skewness()


skewedTmp <- train.hex3[, h2o.columns_by_type(train.hex3, coltype = "numeric")] %>%
  h2o.log1p() %>%
  h2o.skewness() %>%
  `if`(. > 0.5 | . > -0.5, TRUE, FALSE)

banedColumns <- train.hex3[, h2o.columns_by_type(train.hex3, coltype = "numeric")[!skewedTmp]] %>%
  h2o.colnames()

banedColumns


train.hex4 <- train.hex3[, h2o.columns_by_type(train.hex3, coltype = "numeric")] %>%
  h2o.colnames() %>%
  setdiff(., banedColumns) %>%
  { h2o.log1p(train.hex3[, .]) }

head(train.hex4)


train.hex5 <- h2o.cbind(train.hex3[, h2o.columns_by_type(train.hex3, coltype = "categorical")], train.hex4)

head(train.hex5, 5)



glm <- h2o.glm(y = "C8",
               x = setdiff(h2o.colnames(train.hex5), "C8"),
               training_frame = train.hex5, 
               nfolds = 5,
               solver = "L_BFGS"
)



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