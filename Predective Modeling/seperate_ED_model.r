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

df.hex  <- as.h2o(dfx)
# Now that we have our tuned model and parameters, we train the models on the train sample and test
# on the hold-out test sample
df.split <- h2o.splitFrame(df.hex, ratios = c(0.8), seed = 1234)
train <- df.split[[1]]
test <- df.split[[2]] 

##Loading top 5 GBM models
gbm_models <- c()
path <- "./hypeTune_results/top5_gbm_models_cv"
model_names <- grep("^GBM_model", list.files(path), value=T)
for(model in model_names){
	gbm_models <- c(gbm_models, h2o.loadModel(paste(path, model, sep = "/")))
}
#Now we have 5 models that we can use their "hyperTuned" parameters
gbm_models[[1]]@parameters

# $model_id
# [1] "GBM_model_R_1623451639824_53840"

# $nfolds
# [1] 5

# $score_tree_interval
# [1] 10

# $fold_assignment
# [1] "Stratified"

# $balance_classes
# [1] TRUE

# $ntrees
# [1] 356

# $max_depth
# [1] 12

# $min_rows
# [1] 256

# $nbins_cats
# [1] 16

# $stopping_metric
# [1] "AUCPR"

# $stopping_tolerance
# [1] 1e-04

# $seed
# [1] 1234

# $learn_rate
# [1] 0.05

# $learn_rate_annealing
# [1] 0.99

# $distribution
# [1] "bernoulli"

# $sample_rate_per_class
# [1] 0.0948 0.7900

# $col_sample_rate
# [1] 0.99

# $col_sample_rate_change_per_level
# [1] 1.09

# $col_sample_rate_per_tree
# [1] 0.7

# $min_split_improvement
# [1] 0

# $histogram_type
# [1] "RoundRobin"

# $categorical_encoding
# [1] "Enum"

# $x
 # [1] "rural"              "female_flag"        "age_grp"            "triage"            
 # [5] "admambul"           "cacsit1_grp3"       "cacsitcnt1"         "cacsitcnt"         
 # [9] "prv_grp"            "prv_orthop"         "frail_grp"          "all_Cachx_3L"      
# [13] "acutelos"           "readm_90d"          "flag_cardiovers"    "flag_feeding_tb"   
# [17] "flag_mvent_ge96"    "flag_pa_nutrit"     "flag_radiother"     "flag_tracheost"    
# [21] "flag_vasc_accdv"    "scu"                "dementia_comorb"    "dementia_main"     
# [25] "paralysis"          "weightloss"         "psychoses"          "main_PhysInj_1L"   
# [29] "main_MentBehav_1L"  "main_Zfactors_1L"   "main_MusclSkelt_1L" "patserv_34"        
# [33] "patserv_72"         "patserv_38"         "patserv_17"         "patserv_64"        
# [37] "patserv_39"         "patserv_12"         "patserv_15"         "patserv_36"        

# $y
# [1] "alc_status"

res <- new_resultFrame()


for (i in c(1:5)){ 

gbm <- gbm_models[[i]]
newGBM <- do.call(h2o.gbm,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- gbm@parameters
		p$model_id = paste0("GBM_ed_small_", i)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$ntrees = 1000
		p$x = sel_feat_ed_small
		p$stopping_rounds = 5
		p$stopping_tolerance = 1e-4
		p$score_tree_interval = 10
		p$sample_rate = NULL
		p
	})
res <- add_results(newGBM, res)
}

###Bigger just ed features
for (i in c(1:5)){ 

gbm <- gbm_models[[i]]
newGBM <- do.call(h2o.gbm,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- gbm@parameters
		p$model_id = paste0("GBM_ed_big_", i)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$ntrees = 1000
		p$x = sel_feat_ed_big
		p$stopping_rounds = 5
		p$stopping_tolerance = 1e-4
		p$score_tree_interval = 10
		p$sample_rate = NULL
		p
	})
res <- add_results(newGBM, res)
}


###Now top mixed cihi and ED features
for (i in c(1:5)){ 

gbm <- gbm_models[[i]]
newGBM <- do.call(h2o.gbm,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- gbm@parameters
		p$model_id = paste0("GBM_ed_plusCIHI_", i)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$ntrees = 1000
		p$x = sel_feat
		p$stopping_rounds = 5
		p$stopping_tolerance = 1e-4
		p$score_tree_interval = 10
		p$sample_rate = NULL
		p
	})
res <- add_results(newGBM, res)
}

write.csv(res, "./Final Results/gbm_plusMinusCihi.csv", row.names=F)

######
# Now for the Random Forest models

rf_models <- c()
path <- "./hypeTune_results/top5_RF_models_cv"
model_names <- grep("^DRF_model", list.files(path), value=T)
for(model in model_names){
	rf_models <- c(rf_models, h2o.loadModel(paste(path, model, sep = "/")))
}
#Now we have 5 models that we can use their "hyperTuned" parameters

newRF <- c()

for (i in c(2:5)){ 

rf_model <- rf_models[[i]]
newRF<- c( newRF, do.call(h2o.randomForest,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- rf_model@parameters
		p$model_id = paste0("RF_ed_small_", i)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$ntrees = 1000
		p$x = sel_feat_ed_small
		p$stopping_rounds = 5
		p$stopping_tolerance = 1e-4
		p$score_tree_interval = 10
		p$sample_rate = NULL
		p$mtries <- as.integer(p$mtries / 40 * length(p$x)) #as the features sizes is changing, using mtries as a percentage
		p
	}))
res <- add_results(newRF[[i]], res)
}

###
for (i in c(1:5)){ 

rf_model <- rf_models[[i]]
newRF<- c( newRF, do.call(h2o.randomForest,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- rf_model@parameters
		p$model_id = paste0("RF_ed_big_", i)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$ntrees = 1000
		p$x = sel_feat_ed_big
		p$stopping_rounds = 5
		p$stopping_tolerance = 1e-4
		p$score_tree_interval = 10
		p$sample_rate = NULL
		p$mtries <- as.integer(p$mtries / 40 * length(p$x)) #as the features sizes is changing, using mtries as a percentage
		p
	}))
res <- add_results(newRF[[i+10]], res)
}

for (i in c(6:10)){
	res <- add_results(newRF[[i]], res)

}

for (i in c(1:5)){ 

rf_model <- rf_models[[i]]
newRF<- c( newRF, do.call(h2o.randomForest,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- rf_model@parameters
		p$model_id = paste0("RF_ed_plusCIHI_", i)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$ntrees = 1000
		p$x = sel_feat
		p$stopping_rounds = 5
		p$stopping_tolerance = 1e-4
		p$score_tree_interval = 10
		p$sample_rate = NULL
		p$mtries <- as.integer(p$mtries / 40 * length(p$x)) #as the features sizes is changing, using mtries as a percentage
		p
	}))
res <- add_results(newRF[[i]], res)
}

write.csv(res, "./Final Results/gbmANDrf_plusMinusCihi.csv", row.names=F)

