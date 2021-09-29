setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/Models2.5")
library(tidyverse)
library(xgboost)
library(DMwR)
library(caret)
dfx <- readRDS("dfx_cln_oldR.rds")
features <- read.csv("mutualInfo_features.csv")
features$X <- levels(features$X)[features$X]
sel_feat <- features[1:40, "X"]
# dfx <- dfx %>% select(sel_feat, alc_status) old version gives stupid error!
# match(sel_feat, names(dfx)) will find index in list

dfx <- dfx %>% select(c(match(sel_feat, names(dfx)), ncol(dfx)))
str(dfx)

set.seed(1234)

splitIndex <- createDataPartition(dfx$alc_status, p = c(0.8, 0.2), list = FALSE)

train <- dfx[splitIndex,]
valid <- dfx[-splitIndex,]

t <- Sys.time()
smote_train <- SMOTE(alc_status ~ ., data = train, perc.over = 700, perc.under = 100)
#7 samples are added to each in the minority population
# in order to balance the odds of .12 / 1 in the original imbalanced data
print("time spent:")
print(Sys.time() - t)

table(smote_train$alc_status)

      # 0       1 
 # 921193 1052792 
 
saveRDS(smote_train, "../smote_data/smote_train.rds")
saveRDS(valid, "../smote_data/test.rds")