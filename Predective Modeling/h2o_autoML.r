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
#AUTO ML
####
t <- Sys.time()
aml <- h2o.automl(
x = sel_feat_ed_big,
y = "alc_status",
max_models = 50,
training_frame = train,
leaderboard_frame = test,
# validation_frame = df.split_valid[[2]],
balance_classes = TRUE,
nfolds = 5,
stopping_rounds = 5,
stopping_tolerance = 1e-3,
stopping_metric = "AUCPR",
sort_metric = "AUCPR",
seed = 1234)

cat("time elapsed for AutoML: ", Sys.time() - t, "\n")
print(aml@leaderboard, n = nrow(aml@leaderboard))
aml@leaderboard %>% as.data.frame()%>% write.csv("./autoML_EDonly.csv")

t <- Sys.time()
aml_2 <- h2o.automl(
x = sel_feat,
y = "alc_status",
max_models = 50,
training_frame = train,
leaderboard_frame = test,
# validation_frame = df.split_valid[[2]],
balance_classes = TRUE,
nfolds = 5,
stopping_rounds = 5,
stopping_tolerance = 1e-3,
stopping_metric = "AUCPR",
sort_metric = "AUCPR",
seed = 1234)


cat("time elapsed for AutoML: ",  difftime(Sys.time(), t, unit = "hour"), "hours \n")
#Took 27 hours
print(aml_2@leaderboard, n = nrow(aml_2@leaderboard))
aml_2@leaderboard %>% as.data.frame()%>% write.csv("./autoML_EDCIHI.csv")

modH <- new_resultFrame()
modH <- add_cv_results(h2o.getModel(aml_2@leader@model_id), modH)


                                                # Model Hit_Ratio
# 1 StackedEnsemble_BestOfFamily_AutoML_20210705_150745     0.815
  # Mean_PerC_Error LogLoss   AUC AUCPR Recall Precision Specificity max_F1
# 1            0.29   0.303 0.814 0.371  0.572     0.338       0.936  0.425
  # trshold training_AUC training_AUCPR training_Recall training_Precision
# 1   0.159        0.819          0.415           0.555              0.382
  # training_Spec
# 1         0.935
# >

##DOES NOT get better than this #