###addition to previous file "h2o_ML.R"
###Hyper parameter tuning of the 40features model
varx <- sel_feat
vary <- ncol(dfx)

hyper_params = list(max_depth = c(4,6,8,12,16,20))
t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = list(strategy = "Cartesian"),
algorithm = "gbm",
grid_id = "depth_grid_gbm",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
training_frame = train,
# validation_frame = test,
x = varx,
y = vary,
ntrees = 10000, #just some big number
learn_rate = 0.05,
learn_rate_annealing = .99,
sample_rate = 0.8,
col_sample_rate = 0.8,
#sample_rate_per_class = c(1, 8.3), #For the resampling inside trees ---will igonre sample_rate
seed = 1234,
stopping_rounds = 5,
stopping_tolerance = 1e-4, ##early stop if AUCPR did not improve at least 0.01% over 5 consecutive turns
stopping_metric = "auc",
score_tree_interval = 10
)
print(Sys.time() - t)
#Sort base on validation perf
h2o.saveGrid("./hypeTune_results/depth_grid_gbm/", grid_id = "depth_grid_gbm")

depth_res <- new_resultFrame()
sortedGrid <- h2o.getGrid("depth_grid_gbm", sort_by = "AUCPR", decreasing = TRUE)
sortedGrid <- h2o.getGrid("depth_grid_gbm", sort_by = "auc", decreasing = TRUE)

gbm <- h2o.getModel(sortedGrid@model_ids[[1]])

depth_res <- add_results(gbm, depth_res)

depth_res <-h2o.getModel("depth_grid_gbm_model_1") %>% add_results(depth_res)
depth_res <-h2o.getModel("depth_grid_gbm_model_2") %>% add_results(depth_res)
depth_res <-h2o.getModel("depth_grid_gbm_model_3") %>% add_results(depth_res)
depth_res <-h2o.getModel("depth_grid_gbm_model_4") %>% add_results(depth_res)
depth_res <-h2o.getModel("depth_grid_gbm_model_5") %>% add_results(depth_res)

# Based on the top 3 well-performing models:
#--->>>depth min  = 6 and depth max = 12
#Now we tune other parameters

#h2o.removeAll() #remove h2o objects to make space

hyper_params <- list(
	max_depth = seq(6,12,1),
	# sample_rate = seq(0.2, 1, 0.01),
	sample_rate_per_class = lapply(seq(0.2, 1, 0.1), function(x){x * c(0.12, 1)}),
	col_sample_rate = seq(0.2, 1, 0.1),
	col_sample_rate_per_tree = seq(0.2, 1, 0.1),
	col_sample_rate_change_per_level = seq(0.9, 1.1, .01),
	min_rows = 2 ^ seq(0, log2(nrow(train))-1,1),
	# nbins = 2 ^ seq(1,6, 1),
	# nbins_cats = 2 ^ seq(1,6, 1),
	min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
	histogram_type = c("UniformAdaptive", "QuantilesGlobal", "RoundRobin")
)

search_criteria = list(
strategy = "RandomDiscrete", #random grid search
max_runtime_secs = 3600 * 9,
max_models = 200,
seed = 1234, 
stopping_rounds = 5,
stopping_metric = "AUCPR", #Try with AUC also
stopping_tolerance = 1e-3
)
t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = search_criteria,
algorithm = "gbm",
grid_id = "final_grid_gbm_9_6",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
x = varx,
y = vary,
training_frame = train,
# validation_frame = test,
nbins_cats = 16, # instead of 1024 default 
ntrees = 10000,
learn_rate = 0.05,
learn_rate_annealing = 0.99,
max_runtime_secs = 3600, #for each model
stopping_rounds = 5,
stopping_metric = "AUCPR", #Try with AUC also
stopping_tolerance = 1e-3,
score_tree_interval = 10,
seed = 1234
)
print(Sys.time() - t) 
h2o.saveGrid("./hypeTune_results/final_grid_gbm_9_6", grid_id = "final_grid_gbm_9_6")

sortedGrid <- h2o.getGrid("final_grid_gbm_9_6", sort_by = "AUCPR", decreasing = TRUE)

sortedGrid@summary_table %>% as.data.frame %>% write.csv("./hypeTune_results/final_grid_gbm_12hrs/grid_results.csv", row.names=F)
sortedGrid@summary_table %>% as.data.frame %>% write.csv("./hypeTune_results/grid_results_9_6.csv", row.names=F)

# top <- h2o.getModel(sortedGrid@model_ids[[1]])
# depth_res <- add_results(top, depth_res)

grid_res_cv <- new_resultFrame()

for (i in sortedGrid@model_ids){
	grid_res_cv <- add_cv_results(h2o.getModel(i), grid_res_cv)
}



##########################
###Training some models with hyper parameters
hpRes <- new_resultFrame()
sel_feat <- feat_rank[1:40]

df_gbm_hp1 <- h2o.gbm(
training_frame = train,
validation_frame = test,
x = sel_feat,
y = vary,
model_id = "gbm_hp2",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 5000,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
max_depth = 11,
min_rows = 128,
min_split_improvement = 1e-4,
col_sample_rate = 0.6,
col_sample_rate_change_per_level = 1.1,
col_sample_rate_per_tree = 0.7,
seed = 1234
)

hpRes <- add_results(df_gbm_hp1, hpRes)

df_gbm_hp3 <- h2o.gbm(
training_frame = train,
validation_frame = test,
x = sel_feat,
y = vary,
model_id = "gbm_hp3",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 5000,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
max_depth = 11,
min_rows = 128,
min_split_improvement = 1e-4,
col_sample_rate = 0.6,
col_sample_rate_change_per_level = 1.1,
col_sample_rate_per_tree = 0.7,
sample_rate_per_class = c(0.12, 1),
seed = 1234
)

hpRes <- add_results(df_gbm_hp3, hpRes)

###Don't use sample_rate_per_class

df_gbm_hp4 <- h2o.gbm(
training_frame = train,
validation_frame = test,
x = sel_feat,
y = vary,
model_id = "gbm_hp4_learnRate",
nfolds = 5,
fold_assignment = "Stratified",
balance_classes = TRUE,
nbins_cats = 16, # instead of 1024 default 
ntrees = 10000,
learn_rate = 0.05,
learn_rate_annealing = 0.99,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
max_depth = 11,
min_rows = 128,
min_split_improvement = 1e-4,
col_sample_rate = 0.6,
col_sample_rate_change_per_level = 1.1,
col_sample_rate_per_tree = 0.7,
seed = 1234
)

hpRes <- add_results(df_gbm_hp4, hpRes)


df.split <- h2o.splitFrame(df.hex, ratios = c(0.8), seed = 6541)
perf <- h2o.performance(df_gbm_hp4, newdata = df.split[[2]])



##########################################################
######################
###TUNING AGAIN FOR A LONGER PERIOD OF TIME
####################

hyper_params <- list(
	max_depth = seq(6,12,1),
	# sample_rate = seq(0.2, 1, 0.01),
	sample_rate_per_class = lapply(seq(0.4, 1, 0.01), function(x){x * c(0.12, 1)}),
	col_sample_rate = seq(0.4, 1, 0.01),
	col_sample_rate_per_tree = seq(0.4, 1, 0.01),
	col_sample_rate_change_per_level = seq(0.9, 1.1, .01),
	min_rows = 2 ^ seq(0, log2(nrow(train))-1,1),
	# nbins = 2 ^ seq(1,6, 1),
	# nbins_cats = 2 ^ seq(1,6, 1),
	min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
	histogram_type = c("UniformAdaptive", "QuantilesGlobal", "RoundRobin")
)

search_criteria = list(
strategy = "RandomDiscrete", #random grid search
max_runtime_secs = 3600 * 40,
max_models = 500,
seed = 1234, 
stopping_rounds = 5,
stopping_metric = "AUCPR", #Try with AUC also
stopping_tolerance = 1e-4
)

df.split <- h2o.splitFrame(df.hex, ratios = c(0.8), seed = 1234)
train <- df.split[[1]]
test <- df.split[[2]]
df.split_valid <- h2o.splitFrame(train, ratios = c(0.8), seed = 1234) #validation set to size of 5-fold

t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = search_criteria,
algorithm = "gbm",
grid_id = "final_grid_gbm_11_6",
# nfolds = 5,
# fold_assignment = "Stratified",
balance_classes = TRUE,
x = varx,
y = vary,
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

h2o.saveGrid("./hypeTune_results/final_grid_gbm_11_6", grid_id = "final_grid_gbm_11_6")

sortedGrid <- h2o.getGrid("final_grid_gbm_11_6", sort_by = "AUCPR", decreasing = TRUE)

gridtop5 <- new_resultFrame()
for (i in 1:10){
	gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
	gridtop5 <- add_results(gbm, gridtop5)

}
####Taking a closer look at top 5 models
##Doing a 5fold cv
gridtop5_cv <- new_resultFrame()
for (i in 1:5){
	gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
	cvgbm <- do.call(h2o.gbm,
	#update parameters of CV models
	{
		p <- gbm@parameters
		p$model_id = NULL
		p$training_frame = train
		p$validation_frame = NULL
		p$nfolds = 5
		p$fold_assignment = "Stratified"
		p
	})
	print(gbm@model_id)
	gridtop5_cv <- add_cv_results(cvgbm, gridtop5_cv)
	gridtop5_cv$Model[nrow(gridtop5_cv)] <- gbm@model_id
}
gridtop5_cv


                          # Model Hit_Ratio Mean_PerC_Error LogLoss   AUC AUCPR Recall
# 1 final_grid_gbm_11_6_model_294     0.812           0.290   0.539 0.813 0.365  0.575
# 2 final_grid_gbm_11_6_model_174     0.814           0.292   0.542 0.812 0.364  0.567
# 3 final_grid_gbm_11_6_model_197     0.810           0.289   0.534 0.812 0.365  0.580
# 4 final_grid_gbm_11_6_model_251     0.811           0.290   0.532 0.812 0.364  0.578
# 5 final_grid_gbm_11_6_model_198     0.814           0.292   0.542 0.813 0.365  0.569
  # Precision Specificity max_F1 trshold training_AUC training_AUCPR training_Recall
# 1     0.334       0.936  0.423   0.657        0.819          0.797           0.877
# 2     0.337       0.935  0.422   0.666        0.818          0.796           0.880
# 3     0.332       0.936  0.422   0.655        0.825          0.804           0.875
# 4     0.333       0.936  0.422   0.655        0.827          0.805           0.880
# 5     0.336       0.935  0.422   0.667        0.819          0.796           0.869
  # training_Precision training_Spec
# 1              0.679         0.826
# 2              0.676         0.828
# 3              0.686         0.827
# 4              0.684         0.832
# 5              0.683         0.820

# IDs for these models are as below:
# GBM_model_R_1623451639824_55493
top_id_list <- c(55493,55064,54650, 54264, 53840) %>% sapply(function(x){paste0("GBM_model_R_1623451639824_",x)})


###Saving top GBM cross validated models
myPath = "./hypeTune_results/top5_gbm_models_cv/"
for (i in top_id_list){
	h2o.saveModel(h2o.getModel(i), path = myPath)
}
#Sorted Grid for 500 model hyperTuning
sortedGrid@summary_table %>% as.data.frame %>% write.csv("./hypeTune_results/top5_gbm_models_cv/sortedGrid_477Models.csv", row.names=F)



########################################
df_stackEns <- h2o.stackedEnsemble(
x = varx,
y = vary,
training_frame = train,
base_models = top_id_list)

###ERROR: base models should have cross validation prediction = TRUE