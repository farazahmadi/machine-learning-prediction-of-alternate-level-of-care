setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/Models2.5")
library(tidyverse)
library(h2o)
library(inspectdf)
tabna <- function(x){table(x, useNA = "ifany")}
##adding cihi vars
dfx <- readRDS("./dfx_v2.5.rds")

h2o.init()
dfx %>% inspect_na() %>% print(n=10)
dfx <- drop_na(dfx)

###backward compatibility save 
# saveRDS(dfx, "./dfx_cln_oldR.rds", version = 2)
###

#############Train model on first 30 vars
#DATA IS READY TO USE
########################
 

new_resultFrame <-  function(){

return(data.frame(Model = character(),
Hit_Ratio = numeric(),
Mean_PerC_Error = numeric(),
LogLoss = numeric(),
AUC = numeric(),
AUCPR = numeric(),
Recall = numeric(),
Precision = numeric(),
Specificity = numeric(),
max_F1 = numeric(),
trshold = numeric(), 
training_AUC = numeric(),
training_AUCPR = numeric(),
training_Recall = numeric(),
training_Precision = numeric(),
training_Spec = numeric(),
stringsAsFactors = FALSE))
}

add_cv_results <- function (myModel, modH){
# for tuning hyperparameters, using cross validation as 
	idx <- nrow(modH) + 1
	tab2 <- myModel@model$cross_validation_metrics@metrics$cm$table
	tab1 <- myModel@model$training_metrics@metrics$cm$table


	modH[idx, 1] <- myModel@model_id
	modH[idx, 2] <- round(1- myModel@model$cross_validation_metrics@metrics$cm$table$Error[3], digits=3)
	modH[idx, 3] <- round(myModel@model$cross_validation_metrics@metrics$mean_per_class_error, digits=3)
	modH[idx, 4] <- round(myModel@model$cross_validation_metrics@metrics$logloss, digits=3)
	modH[idx, 5] <- round(myModel@model$cross_validation_metrics@metrics$AUC, digits = 3)
	modH[idx, 6] <- round(myModel@model$cross_validation_metrics@metrics$pr_auc, digits = 3)
	modH[idx, 7] <- round(tab2[2,2] / (tab2[2,2] + tab2[2,1]),digits=3)#recall
	modH[idx, 8] <- round(tab2[2,2] / (tab2[2,2] + tab2[1,2]),digits=3)# precision
	modH[idx, 9] <- round(tab2[1,1] / (tab2[1,1] + tab2[2,1]),digits=3)# Specificity
	modH[idx, 10] <- round(myModel@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores$value[1], digits=3)
	modH[idx, 11] <- round(myModel@model$cross_validation_metrics@metrics$max_criteria_and_metric_scores$threshold[1], digits=3)
	modH[idx, 12] <- round(myModel@model$training_metrics@metrics$AUC, digits = 3)
	modH[idx, 13] <- round(myModel@model$training_metrics@metrics$pr_auc, digits = 3)
	modH[idx, 14] <- round(tab1[2,2] / (tab1[2,2] + tab1[2,1]),digits=3)#recall
	modH[idx, 15] <- round(tab1[2,2] / (tab1[2,2] + tab1[1,2]),digits=3)# precision
	modH[idx, 16] <- round(tab1[1,1] / (tab1[1,1] + tab1[2,1]),digits=3)# Specificity

	return(modH)
}

add_results <- function (myModel, modH){
	idx <- nrow(modH) + 1
	tab2 <- myModel@model$validation_metrics@metrics$cm$table
	tab1 <- myModel@model$training_metrics@metrics$cm$table


	modH[idx, 1] <- myModel@model_id
	modH[idx, 2] <- round(1- myModel@model$validation_metrics@metrics$cm$table$Error[3], digits=3)
	modH[idx, 3] <- round(myModel@model$validation_metrics@metrics$mean_per_class_error, digits=3)
	modH[idx, 4] <- round(myModel@model$validation_metrics@metrics$logloss, digits=3)
	modH[idx, 5] <- round(myModel@model$validation_metrics@metrics$AUC, digits = 3)
	modH[idx, 6] <- round(myModel@model$validation_metrics@metrics$pr_auc, digits = 3)
	modH[idx, 7] <- round(tab2[2,2] / (tab2[2,2] + tab2[2,1]),digits=3)#recall
	modH[idx, 8] <- round(tab2[2,2] / (tab2[2,2] + tab2[1,2]),digits=3)# precision
	modH[idx, 9] <- round(tab2[1,1] / (tab2[1,1] + tab2[2,1]),digits=3)# Specificity
	modH[idx, 10] <- round(myModel@model$validation_metrics@metrics$max_criteria_and_metric_scores$value[1], digits=3)
	modH[idx, 11] <- round(myModel@model$validation_metrics@metrics$max_criteria_and_metric_scores$threshold[1], digits=3)
	modH[idx, 12] <- round(myModel@model$training_metrics@metrics$AUC, digits = 3)
	modH[idx, 13] <- round(myModel@model$training_metrics@metrics$pr_auc, digits = 3)
	modH[idx, 14] <- round(tab1[2,2] / (tab1[2,2] + tab1[2,1]),digits=3)#recall
	modH[idx, 15] <- round(tab1[2,2] / (tab1[2,2] + tab1[1,2]),digits=3)# precision
	modH[idx, 16] <- round(tab1[1,1] / (tab1[1,1] + tab1[2,1]),digits=3)# Specificity

	return(modH)
}

add_test_results <- function (perf, modH){
	idx <- nrow(modH) + 1
	tab2 <- perf@metrics$cm$table


	modH[idx, 1] <- perf@metrics$model$name
	modH[idx, 2] <- round(1- perf@metrics$cm$table$Error[3], digits=3)
	modH[idx, 3] <- round(perf@metrics$mean_per_class_error, digits=3)
	modH[idx, 4] <- round(perf@metrics$logloss, digits=3)
	modH[idx, 5] <- round(perf@metrics$AUC, digits = 3)
	modH[idx, 6] <- round(perf@metrics$pr_auc, digits = 3)
	modH[idx, 7] <- round(tab2[2,2] / (tab2[2,2] + tab2[2,1]),digits=3)#recall
	modH[idx, 8] <- round(tab2[2,2] / (tab2[2,2] + tab2[1,2]),digits=3)# precision
	modH[idx, 9] <- round(tab2[1,1] / (tab2[1,1] + tab2[2,1]),digits=3)# Specificity
	modH[idx, 10] <- round(perf@metrics$max_criteria_and_metric_scores$value[1], digits=3)
	modH[idx, 11] <- round(perf@metrics$max_criteria_and_metric_scores$threshold[1], digits=3)


	return(modH)
}



#not using scu and patserv yet!
# varx <- 5:42 
vary <- "alc_status"

df.hex  <- as.h2o(dfx)

df.split <- h2o.splitFrame(df.hex, ratios = c(0.8), seed = 1234)
train <- df.split[[1]]
test <- df.split[[2]] #not using this until best hyperparameters are chosen

sel_feat <- rownames(feat_rank)[1:30]

df.hex_gbmNew <- h2o.gbm(
training_frame = train,
x = sel_feat,
y = vary,
model_id = "gbm_first30MutINF",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
#score_each_iteration = T, #wrong if used with above argument (I guess!)
seed = 1234
)

# modH <- new_resultFrame()
modH <- add_cv_results(df.hex_gbmNew, modH)

sel_feat <- rownames(feat_rank)[1:40]

df.hex_gbmNew2 <- h2o.gbm(
training_frame = train,
x = sel_feat,
y = vary,
model_id = "gbm_first40",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_cv_results(df.hex_gbmNew2, modH)

##added

sel_feat <- rownames(feat_rank)[1:45]

df.hex_gbmNew22 <- h2o.gbm(
training_frame = train,
x = sel_feat,
y = vary,
model_id = "gbm_first45",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_cv_results(df.hex_gbmNew22, modH)
###

sel_feat <- rownames(feat_rank)[1:50]

df.hex_gbmNew3 <- h2o.gbm(
training_frame = train,
x = sel_feat,
y = vary,
model_id = "gbm_first50",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_cv_results(df.hex_gbmNew3, modH)

sel_feat <- rownames(feat_rank)[1:60]

df.hex_gbmNew4 <- h2o.gbm(
training_frame = train,
x = sel_feat,
y = vary,
model_id = "gbm_first60",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_cv_results(df.hex_gbmNew4, modH)

sel_feat <- rownames(feat_rank)[1:68]

df.hex_gbmNew5 <- h2o.gbm(
training_frame = train,
x = sel_feat,
y = vary,
model_id = "gbm_first70",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_cv_results(df.hex_gbmNew5, modH)

################
# Using other algorithms, Random Forest etc., 

sel_feat <- rownames(feat_rank)[1:40] 

df_rf1 <- h2o.randomForest(
training_frame = train,
x = sel_feat,
y = vary,
model_id = "rf_first40",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_results(df_rf1, modH)

sel_feat <- rownames(feat_rank)[1:50] 

df_rf2 <- h2o.randomForest(
training_frame = train,
validation_frame = test,
x = sel_feat,
y = vary,
model_id = "rf_first50",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_results(df_rf2, modH)

####Playing with sample per class a little bit
#Up sample the minrotity (probably the default but let's see!)
sel_feat <- rownames(feat_rank)[1:40]

df.hex_gbmNew2_2 <- h2o.gbm(
training_frame = train,
validation_frame = test,
x = sel_feat,
y = vary,
model_id = "gbm_first40_overSample",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
class_sampling_factors = c(1, 8.3),
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_results(df.hex_gbmNew2_2, modH)

df.hex_gbmNew2_3 <- h2o.gbm(
training_frame = train,
validation_frame = test,
x = sel_feat,
y = vary,
model_id = "gbm_first40_UnderSample",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
class_sampling_factors = c(0.12, 1),
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modH <- add_results(df.hex_gbmNew2_3, modH)

##Not much difference between their results!----> use H2o balance_classes like before

