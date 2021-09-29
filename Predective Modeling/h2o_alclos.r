setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/Models2.5/")
library(tidyverse)
df <- read.csv("../datasets/New/bigCihi.csv")
tabna <- function(x){table(x, useNA = "ifany")}

##df is mostly big Cihi + alc_status

df %>% filter(alclos == 0) %>% select(alc_status) %>% tabna
# x
      # 0       1 
# 1300004     279 
df %>% filter(alclos > 0) %>% nrow()
# [1] 174002
tabna(df$alc_status)
# x
      # 0       1 
# 1300057  174228 


 df %>%
group_by(  entry )%>%
summarise(count = n(),
count_perc = count / nrow(df) * 100,
alc_perc = mean(as.numeric(as.character(alc_status))) * 100)

 df %>%
group_by(  admcat )%>%
summarise(count = n(),
count_perc = count / nrow(df) * 100,
alc_perc = mean(as.numeric(as.character(alc_status))) * 100)

########################
#########################New outcome variable
########################

df_alc <- df %>% select(ID, days_to_lefteddate, alclos)

dfx <- left_join(dfx, df_alc, by = c("ID", "days_to_lefteddate"))

dfx <- dfx %>% mutate(alc_30 = ifelse(alclos >= 30, 1, 0) %>% factor)
dfx <- dfx %>% mutate(alc_10 = ifelse(alclos >= 10, 1, 0) %>% factor)
dfx <- dfx %>% mutate(alc_5 = ifelse(alclos >= 5, 1, 0) %>% factor)


#######################
dfx <- drop_na(dfx)
df.hex  <- as.h2o(dfx)

df.split <- h2o.splitFrame(df.hex, ratios = c(0.8), seed = 1234)
train <- df.split[[1]]
test <- df.split
df.split_valid <- h2o.splitFrame(train, ratios = c(0.8), seed = 1234) #validation set to size of 5-fold

df.hex_gbmNew <- h2o.gbm(
training_frame = train,
x = sel_feat,
y = "alc_30",
model_id = "gbm_alc30_noParam",
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

modH <- new_resultFrame()
modH <- add_cv_results(df.hex_gbmNew, modH)
#very low AUCPR

#############Checking if important predictors are the same 
library(infotheo)
X <- dfx %>% select(-c(1:4), - alc_status, -alclos, -alc_30)
res <- X %>% sapply(function(x){
mutinformation(x, dfx$alc_status, method = "emp") / sqrt(entropy(x) * entropy(df$alc_status)) })
#SAME MOSTLY
# write.csv(res[order(-res)], "./mutualInfo_features.csv")
# feat_rank <- res[order(-res)] %>% as.data.frame()

#############Bringing some tuned models

##Loading top 5 GBM models
gbm_models <- c()
path <- "./hypeTune_results/top5_gbm_models_cv"
model_names <- grep("^GBM_model", list.files(path), value=T)
for(model in model_names){
	gbm_models <- c(gbm_models, h2o.loadModel(paste(path, model, sep = "/")))
}

res <- new_resultFrame()
newGBM <- c()
for (i in c(1:5)){ 

gbm <- gbm_models[[i]]
newGBM <- c(newGBM, do.call(h2o.gbm,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- gbm@parameters
		p$model_id = paste0("GBM_ed_small_", i)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$ntrees = 1000
		p$x = sel_feat
		p$y = "alc_30"
		p$stopping_rounds = 5
		p$stopping_tolerance = 1e-4
		p$score_tree_interval = 10
		p$sample_rate = NULL
		p
	}))
res <- add_results(newGBM[[i]], res)
}

#even worse results,
#Lets Tune gbm for alc_30 prediction


hyper_params = list(max_depth = c(4,6,8,12,16,20))
t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = list(strategy = "Cartesian"),
algorithm = "gbm",
grid_id = "depth_grid_gbm",
balance_classes = TRUE,
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = "alc_30",
ntrees = 10000, #just some big number
learn_rate = 0.05,
learn_rate_annealing = .99,
sample_rate = 0.8,
col_sample_rate = 0.8,
#sample_rate_per_class = c(1, 8.3), #For the resampling inside trees ---will igonre sample_rate
seed = 1234,
stopping_rounds = 5,
stopping_tolerance = 1e-4, ##early stop if AUCPR did not improve at least 0.01% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 10
)
print(Sys.time() - t)
#Sort base on validation perf

depth_res <- new_resultFrame()
sortedGrid <- h2o.getGrid("depth_grid_gbm", sort_by = "AUCPR", decreasing = TRUE)

depths <- sortedGrid@summary_table$max_depth[1:3]
max_depth <- max(depths) %>% as.integer
min_depth <- min(depths) %>% as.integer

##############Depth span is found, now going for the next hypTune step


hyper_params <- list(
	max_depth = seq(min_depth, max_depth, 1),
	# sample_rate = seq(0.2, 1, 0.01),
	sample_rate_per_class = lapply(seq(0.4, 1, 0.01), function(x){x * c(0.12, 1)}),
	col_sample_rate = seq(0.4, 1, 0.01),
	col_sample_rate_per_tree = seq(0.4, 1, 0.01),
	col_sample_rate_change_per_level = seq(0.9, 1.1, .01),
	min_rows = 2 ^ seq(0, log2(nrow(df.split_valid[[1]]))-1,1),
	# nbins = 2 ^ seq(1,6, 1),
	# nbins_cats = 2 ^ seq(1,6, 1),
	min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
	histogram_type = c("UniformAdaptive", "QuantilesGlobal", "RoundRobin")
)

search_criteria = list(
strategy = "RandomDiscrete", #random grid search
max_runtime_secs = 3600 * 1,
max_models = 500,
seed = 1234, 
stopping_rounds = 5,
stopping_metric = "AUCPR", #Try with AUC also
stopping_tolerance = 1e-4
)


t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = search_criteria,
algorithm = "gbm",
grid_id = "final_grid_gbm_alc_30",
# nfolds = 5,
# fold_assignment = "Stratified",
balance_classes = TRUE,
x = sel_feat,
y = "alc_30",
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
nbins_cats = 16, # instead of 1024 default 
ntrees = 10000,
learn_rate = 0.05,
learn_rate_annealing = 0.99,
max_runtime_secs = 3600, #for each model
stopping_rounds = 5,
stopping_metric = "AUCPR", #Try with AUC also
stopping_tolerance = 1e-4,
score_tree_interval = 10,
seed = 1234
)
print(Sys.time() - t) 
sortedGrid <- h2o.getGrid("final_grid_gbm_alc_30", sort_by = "AUCPR", decreasing = TRUE)

###Rerrun overnight


################
#What if we used alc_10 or alc_5

df.hex_gbmNew2 <- h2o.gbm(
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = "alc_10",
model_id = "gbm_alc10_noParam",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
#score_each_iteration = T, #wrong if used with above argument (I guess!)
seed = 1234
)

modH <- add_results(df.hex_gbmNew2, modH)

df.hex_gbmNew3 <- h2o.gbm(
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = "alc_5",
model_id = "gbm_alc5_noParam",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
#score_each_iteration = T, #wrong if used with above argument (I guess!)
seed = 1234
)

modH <- add_results(df.hex_gbmNew3, modH)


              # Model Hit_Ratio Mean_PerC_Error LogLoss   AUC AUCPR Recall
# 1 gbm_alc30_noParam     0.970           0.381   0.058 0.865 0.116  0.258
# 2 gbm_alc10_noParam     0.908           0.330   0.148 0.836 0.214  0.407
# 3  gbm_alc5_noParam     0.852           0.303   0.219 0.822 0.285  0.513
  # Precision Specificity max_F1 trshold training_AUC training_AUCPR
# 1     0.156       0.989  0.195   0.094        0.879          0.868
# 2     0.220       0.971  0.286   0.136        0.841          0.823
# 3     0.263       0.956  0.348   0.169        0.827          0.804
  # training_Recall training_Precision training_Spec
# 1           0.874              0.749         0.849
# 2           0.877              0.706         0.837
# 3           0.871              0.693         0.827