###################################
###Random Forest 
modRF <- new_resultFrame()

df.split <- h2o.splitFrame(df.hex, ratios = c(0.8), seed = 1234)
train <- df.split[[1]]
test <- df.split[[2]]
df.split_valid <- h2o.splitFrame(train, ratios = c(0.8), seed = 1234) #validation set to size of 5-fold

sel_feat <- rownames(feat_rank)[1:40] 

#using prespecified validation for quicker results

df_rf1 <- h2o.randomForest(
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = vary,
model_id = "rf_first",
# nfolds = 5,
# fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5,
score_each_iteration = T,
seed = 1234
)

modRF <- add_results(df_rf1, modRF)

# modH <- add_cv_results(df_xgb, modH)

df_rf2 <- h2o.randomForest(
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = vary,
model_id = "rf_first",
# nfolds = 5,
# fold_assignment = "Stratified",
balance_classes = TRUE,
ntrees = 500,
binomial_double_trees = TRUE,
stopping_rounds = 5,
stopping_tolerance = 1e-4, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 10,
score_each_iteration = T,
seed = 1234
)

modRF <- add_results(df_rf2, modRF)


####Hypertune

hyper_params = list(max_depth = c(4,6,8,12,14,16,18,20))
t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = list(strategy = "Cartesian"),
algorithm = "randomForest",
grid_id = "depth_grid_rf",
balance_classes = TRUE,
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = vary,
ntrees = 10000, #just some big number
binomial_double_trees = TRUE,
seed = 1234,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.01% over 5 consecutive turns
stopping_metric = "auc",
score_tree_interval = 5
)
print(Sys.time() - t)

sortedGrid <- h2o.getGrid("depth_grid_rf", sort_by = "AUCPR", decreasing = TRUE)
sortedGrid <- h2o.getGrid("depth_grid_rf", sort_by = "auc", decreasing = TRUE)

# 16:20 for max depth

hyper_params = list(max_depth = c(16,18,20,22,24,26,28,30))
t <- Sys.time()
grid2 <- h2o.grid(
hyper_params = hyper_params,
search_criteria = list(strategy = "Cartesian"),
algorithm = "randomForest",
grid_id = "depth_grid_rf_2",
balance_classes = TRUE,
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = vary,
ntrees = 10000, #just some big number
binomial_double_trees = TRUE,
seed = 1234,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.01% over 5 consecutive turns
stopping_metric = "auc",
score_tree_interval = 5
)
print(Sys.time() - t)

sortedGrid <- h2o.getGrid("depth_grid_rf_2", sort_by = "AUCPR", decreasing = TRUE)
sortedGrid <- h2o.getGrid("depth_grid_rf_2", sort_by = "auc", decreasing = TRUE)

#higher than 20 gets overfitted
hyper_params <- list(
	max_depth = seq(16,20,1),
	# sample_rate = seq(0.2, 1, 0.01),
	sample_rate_per_class = lapply(seq(0.4, 1, 0.01), function(x){x * c(0.12, 1)}),
	mtries = c(-1,-2, seq(10,37,3)),
	min_rows = 2 ^ seq(0, log2(nrow(df.split_valid[[1]]))-1,1),
	min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
	histogram_type = c("UniformAdaptive", "QuantilesGlobal", "RoundRobin")
)

search_criteria = list(
strategy = "RandomDiscrete", #random grid search
max_runtime_secs = 3600 * 8,
max_models = 500,
seed = 1234, 
stopping_rounds = 5,
stopping_metric = "AUCPR", #Try with AUC also
stopping_tolerance = 1e-3
)
t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = search_criteria,
algorithm = "randomForest",
grid_id = "final_grid_rf",
balance_classes = TRUE,
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = vary,
ntrees = 10000, #just some big number
binomial_double_trees = TRUE,
seed = 1234,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.01% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5
)
print(Sys.time() - t) 

# sortedGrid <- h2o.getGrid("final_grid_rf", sort_by = "auc", decreasing = TRUE)
sortedGrid <- h2o.getGrid("final_grid_rf", sort_by = "AUCPR", decreasing = TRUE)

#Looking closer at the top 5 models
for (i in sortedGrid@model_ids[1:5]){
	modRF <- add_results(h2o.getModel(i), modRF)
}

                # Model Hit_Ratio Mean_PerC_Error LogLoss       AUC  AUCPR  Recall Precision
# 3                 rf_first     0.800           0.292   0.304 0.804 0.341  0.586     0.320
# 2   final_grid_rf_model_29     0.811           0.296   0.551 0.808 0.359  0.564     0.332
# 3.1  final_grid_rf_model_8     0.801           0.290   0.551 0.808 0.358  0.591     0.322
# 4   final_grid_rf_model_96     0.815           0.300   0.547 0.807 0.358  0.550     0.336
# 5   final_grid_rf_model_56     0.803           0.289   0.550 0.809 0.357  0.591     0.325
# 6   final_grid_rf_model_72     0.808           0.294   0.553 0.808 0.357  0.572     0.329
    # Specificity max_F1 trshold training_AUC training_AUCPR training_Recall
# 3         0.936  0.414   0.162        0.834          0.819           0.866
# 2         0.934  0.418   0.666        0.821          0.808           0.854
# 3.1       0.937  0.417   0.649        0.821          0.810           0.850
# 4         0.932  0.417   0.673        0.826          0.809           0.867
# 5         0.937  0.419   0.648        0.820          0.807           0.879
# 6         0.935  0.418   0.664        0.821          0.806           0.862
    # training_Precision training_Spec
# 3                0.698         0.823
# 2                0.690         0.809
# 3.1              0.686         0.809
# 4                0.689         0.820
# 5                0.674         0.826
# 6                0.686         0.815

###reTrain the top 5 models with CV on all train subset of data
RFtop5_cv <- new_resultFrame()

for (i in 1:5){
	rfModel <- h2o.getModel(sortedGrid@model_ids[[i]])
	cvRFModel <- do.call(h2o.randomForest,
	#update parameters of CV models
	{
		p <- rfModel@parameters
		p$model_id = NULL
		p$training_frame = train
		p$validation_frame = NULL
		p$nfolds = 5
		p$fold_assignment = "Stratified"
		p
	})
	print(rfModel@model_id)
	RFtop5_cv <- add_cv_results(cvRFModel, RFtop5_cv)
	RFtop5_cv$Model[nrow(RFtop5_cv)] <- paste("RF_top5_CV", i, sep="_")
}
RFtop5_cv

                            # Model Hit_Ratio Mean_PerC_Error LogLoss   AUC AUCPR Recall
# 1 DRF_model_R_1623451639824_61450     0.812           0.297   0.551 0.807 0.354  0.561
# 2                    RF_top5_CV_2     0.804           0.292   0.551 0.807 0.354  0.582
# 3                    RF_top5_CV_3     0.808           0.295   0.546 0.806 0.354  0.569
# 4                    RF_top5_CV_4     0.811           0.296   0.550 0.808 0.353  0.564
# 5                    RF_top5_CV_5     0.808           0.294   0.552 0.807 0.353  0.572
  # Precision Specificity max_F1 trshold training_AUC training_AUCPR training_Recall
# 1     0.331       0.934  0.416   0.667        0.821          0.808           0.869
# 2     0.324       0.936  0.416   0.654        0.821          0.808           0.869
# 3     0.327       0.935  0.415   0.659        0.825          0.808           0.868
# 4     0.331       0.934  0.417   0.661        0.820          0.808           0.861
# 5     0.327       0.935  0.416   0.662        0.821          0.805           0.875
  # training_Precision training_Spec
# 1              0.681         0.819
# 2              0.672         0.823
# 3              0.687         0.821
# 4              0.681         0.815
# 5              0.678         0.824

top_id_list <- c(61450,61732,61869,62138,62398) %>% sapply(function(x){paste0("DRF_model_R_1623451639824_",x)})

myPath = "./hypeTune_results/top5_RF_models_cv/"
for (i in top_id_list){
	h2o.saveModel(h2o.getModel(i), path = myPath)
}
#saving Sorted Grid for model hyperTuning
sortedGrid@summary_table %>% as.data.frame %>% write.csv("./hypeTune_results/top5_RF_models_cv/sortedGrid_rf.csv", row.names=F)


#############################
#############################
##########################################################
############################# Hyper parameter tuning for ed only model
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
#############################
modRF <- new_resultFrame()

# 16:20 for max depth

hyper_params = list(max_depth = c(16,18,20,22,24,26,28,30))
t <- Sys.time()
depthgrid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = list(strategy = "RandomDiscrete"),
algorithm = "randomForest",
grid_id = "depth_rf_ed",
balance_classes = TRUE,
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat_ed_big,
y = "alc_status",
ntrees = 10000, #just some big number
binomial_double_trees = TRUE,
seed = 1234,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.01% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5
)
print(Sys.time() - t)

sortedGrid <- h2o.getGrid("depth_rf_ed", sort_by = "AUCPR", decreasing = TRUE)
sortedGrid <- h2o.getGrid("depth_rf_ed", sort_by = "auc", decreasing = TRUE)

modRF <- add_results(h2o.getModel("depth_rf_ed_model_2"), modRF)
#16:20 seems the way to go

hyper_params <- list(
	max_depth = seq(16,20,1),
	# sample_rate = seq(0.2, 1, 0.01),
	# sample_rate_per_class = lapply(seq(0.4, 1, 0.01), function(x){x * c(0.12, 1)}),
	mtries = c(-1,-2, seq(10,length(sel_feat_ed_big),3)),
	min_rows = 2 ^ seq(0, log2(nrow(df.split_valid[[1]]))-1,1),
	min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
	histogram_type = c("UniformAdaptive", "QuantilesGlobal", "RoundRobin")
)

search_criteria = list(
strategy = "RandomDiscrete", #random grid search
max_runtime_secs = 3600 * 4,
max_models = 500,
seed = 1234, 
stopping_rounds = 5,
stopping_metric = "AUCPR", #Try with AUC also
stopping_tolerance = 1e-3
)
t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = search_criteria,
algorithm = "randomForest",
grid_id = "final_grid_rf_ED",
balance_classes = TRUE,
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
binomial_double_trees = TRUE, #build twice internal trees, for each class
x = sel_feat_ed_big,
y = "alc_status",
ntrees = 10000, #just some big number
binomial_double_trees = TRUE,
seed = 1234,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.01% over 5 consecutive turns
stopping_metric = "AUCPR",
score_tree_interval = 5
)
print(Sys.time() - t) 

# sortedGrid <- h2o.getGrid("final_grid_rf_ED", sort_by = "auc", decreasing = TRUE)
sortedGrid <- h2o.getGrid("final_grid_rf_ED", sort_by = "AUCPR", decreasing = TRUE)

top5 <- new_resultFrame()
for (i in sortedGrid@model_ids[1:5]){
	top5 <- add_results(h2o.getModel(i), top5)
}
top5
###Picking the first one, training it on all samples using cross validation
bestModel <- h2o.getModel(sortedGrid@model_ids[[1]])

bestModel_cv <- do.call(h2o.randomForest,
{
	p <- bestModel@parameters
	p$model_id = "rf_ed_best_cv"
	p$training_frame = train
	p$nfolds = 5
	p$fold_assignment = "Stratified"
	p$keep_cross_validation_prediction = TRUE#this was not runn in saved model
	p$keep_cross_validation_fold_assignment = TRUE
	p$stopping_rounds = 5
	p$stopping_tolerance = 1e-3
	p$score_tree_interval = 10
	p
})

perf <- h2o.performance(bestModel_cv, test)

top5 <- add_cv_results(bestModel_cv, top5)# careful here as the validation frame is null
#it was corrected using perf

top5 %>% write.csv("./hypeTune_results/top5_RF_ed_models_cv/top5_andBestCV.csv", row.names=F)
sortedGrid@summary_table %>%
 as.data.frame %>%
 write.csv("./hypeTune_results/top5_RF_ed_models_cv/sortedGrid_rf_ed.csv", row.names=F)
#saving the trained model also
bestModel_cv %>% h2o.saveModel("./hypeTune_results/top5_RF_ed_models_cv")

#try a little experiment---> will it actually help the positive predictions?
sample_rate_per_class = [0.1128, 0.94]

sampleRate_rf <- do.call(h2o.randomForest,
{
	p <- bestModel@parameters
	p$model_id = "sample_rate_best"
	training_frame = df.split_valid[[1]]
	validation_frame = df.split_valid[[2]]
	p$sample_rate_per_class = c(0.1128, 0.94)
	p$nfolds = NULL
	p$fold_assignment = NULL
	p
})

srcomp <- top5[1,]
srcomp <- add_results(sampleRate_rf, srcomp)
srcomp

################end of experiment---
####
rf_models <- c()
path <- "./hypeTune_results/top5_RF_models_cv"
model_names <- grep("^DRF_model", list.files(path), value=T)
for(model in model_names){
	rf_models <- c(rf_models, h2o.loadModel(paste(path, model, sep = "/")))
}
#Now we have 5 models that we can use their "hyperTuned" parameters
rf_models[[1]]@parameters

rf_cv_model <- do.call(h2o.randomForest,
{
	p <- rf_models[[1]]@parameters
	p$training_frame = train
	p$validation_frame = test
	p$nfolds = NULL
	p$fold_assignment = NULL
	p$sample_rate_per_class = NULL
	p$keep_cross_validation_prediction = TRUE
	p$keep_cross_validation_fold_assignment = TRUE
	p
})

newRF <- c(rf_cv_model)
modRFcihi <- new_resultFrame()
modRFcihi <- add_results(rf_cv_model, modRFcihi)
for (i in c(2:5)){ 

rf_model <- rf_models[[i]]
newRF<- c( newRF, do.call(h2o.randomForest,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- rf_model@parameters
		p$model_id = paste0("RF_EDCihi_", i)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$sample_rate_per_class = NULL
		p$keep_cross_validation_prediction = TRUE
		p$keep_cross_validation_fold_assignment = TRUE
		p$stopping_rounds = 5
		p$stopping_tolerance = 1e-3
		p$score_tree_interval = 10
		p
	}))
modRFcihi <- add_results(newRF[[i]], modRFcihi)
}
modRFcihi
modRFcihi %>% write.csv("./hypeTune_results/top5_RF_models_cv/updated_version/top5_tuned_test_results.csv")
###The 4th one seems like the one! It is saved
newRF[[4]]@parameters

newRF[[4]] %>% h2o.saveModel("./hypeTune_results/top5_RF_models_cv/updated_version")