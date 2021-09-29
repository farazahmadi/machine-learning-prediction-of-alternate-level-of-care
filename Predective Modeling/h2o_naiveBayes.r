setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/Models2.5")
library(tidyverse)
library(h2o)
library(inspectdf)
tabna <- function(x){table(x, useNA = "ifany")}
dfx <- readRDS("./dfx_v2.5.rds")

h2o.init()
dfx %>% inspect_na() %>% print(n=10)
dfx <- drop_na(dfx)


####

features <- read.csv("mutualInfo_features.csv")
features$X <- levels(features$X)[features$X]
names(features)[1:2] <- c("predictor", "mutInf")
features[grep("^flag", features$predictor), "is_ed"] <- FALSE
features[grep("^patserv", features$predictor), "is_ed"] <- FALSE
features[
which(features$predictor %in% c("acutelos", "scu", "prior_hosp_90d",
"prior_hosp_365d", "readm_90d")), "is_ed"] <- FALSE
features[is.na(features$is_ed), "is_ed"] <- TRUE

# sum(features$is_ed)
# [1] 37
# sum(features$is_ed[1:40])
# [1] 21

sel_feat_ed_small <- features[1:40,] %>% filter(is_ed) %>% .$predictor
sel_feat_ed_big <- features %>% filter(is_ed) %>% .$predictor
sel_feat <- features[1:40,] %>% .$predictor

####

df.hex  <- as.h2o(dfx)
# Now that we have our tuned model and parameters, we train the models on the train sample and test
# on the hold-out test sample
df.split <- h2o.splitFrame(df.hex, ratios = c(0.8), seed = 1234)
train <- df.split[[1]]
test <- df.split[[2]] #unseen data

df.split_valid <- h2o.splitFrame(train, ratios = c(0.8), seed = 1234)


####
#NAIVE BAYES####
####
t <- Sys.time()
naive1 <- h2o.naiveBayes(
model_id = "naiveBayes_1",
x = c( "main_PhysInj_1L" ,   "admambul"  ),
y = "alc_status",
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
balance_classes = TRUE,
laplace = 0,
seed = 1234)

cat("time elapsed for model training: ",  difftime(Sys.time(), t, unit = "hour"), "hours \n")



svm1 <- h2o.psvm(
model_id = "SVM_1",
x = sel_feat_ed_small,
y = "alc_status",
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
gamma = 0.01,
rank_ratio = 0.1,
seed = 1234)
