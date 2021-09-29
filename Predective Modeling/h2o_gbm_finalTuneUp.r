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



##Loading top 5 GBM models
gbm_models <- c()
path <- "./hypeTune_results/top5_gbm_models_cv"
model_names <- grep("^GBM_model", list.files(path), value=T)
for(model in model_names){
	gbm_models <- c(gbm_models, h2o.loadModel(paste(path, model, sep = "/")))
}
#Now we have 5 models that we can use their "hyperTuned" parameters
gbm_models[[1]]@parameters

testRes <- new_resultFrame()
for(i in 1:5){
testRes <- add_test_results(h2o.performance(gbm_models[[i]], newdata=test), testRes)}


new_gbm <- do.call(h2o.gbm,

{
	p <- gbm_models[[1]]@parameters
	p$training_frame = train
	p$model_id <- "no_class_sr"
	p$sample_rate_per_class = NULL
	p
	})
	
testRes <- add_test_results(h2o.performance(new_gbm, newdata=test), testRes)

##We'll get back to this

gbm_cihi <- do.call(h2o.gbm,

{
	p <- gbm_models[[1]]@parameters
	p$model_id <- "gbm_cihi_cv_1"
	p$sample_rate = 0.79
	p$sample_rate_per_class = NULL
	p$training_frame = train
	p$validation_frame = test
	p$nfolds = 5
	p$fold_assignment = "Stratified"
	p$keep_cross_validation_prediction = TRUE
	p$keep_cross_validation_fold_assignment = TRUE
	p
	})
testRes <- add_test_results(h2o.performance(gbm_cihi, newdata=test), testRes)


bestModel <- gbm_cihi
bestModel %>% h2o.saveModel("./hypeTune_results/top_gbm_cihi_models_cv/")
sink("./hypeTune_results/top_gbm_cihi_models_cv/parameters.txt")
print(bestModel@parameters)
sink()

add_results(gbm_cihi, new_resultFrame()) %>% write.csv("./hypeTune_results/top_gbm_cihi_models_cv/test_train_results.csv")
bestModel@model$cross_validation_metrics@metrics$AUC
bestModel@model$validation_metrics@metrics$AUC
bestModel@model$training_metrics@metrics$AUC

###############
###############
# Tuning for ed-only model


hyper_params <- list(
	max_depth = seq(5,15,1),
	sample_rate = seq(0.4, 1, 0.01),
	# sample_rate_per_class = lapply(seq(0.4, 1, 0.01), function(x){x * c(0.12, 1)}),
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
max_runtime_secs = 3600 * 8,
max_models = 500,
seed = 1234, 
stopping_rounds = 5,
stopping_metric = "AUCPR", #Try with AUC also
stopping_tolerance = 1e-3
)

df.split_valid <- h2o.splitFrame(train, ratios = c(0.8), seed = 1234) #validation set to size of 5-fold

t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = search_criteria,
algorithm = "gbm",
grid_id = "gbm_cihi_grid",
# nfolds = 5,
# fold_assignment = "Stratified",
balance_classes = TRUE,
x = sel_feat_ed_big,
y = "alc_status",
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
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


sortedGrid <- h2o.getGrid("gbm_cihi_grid", sort_by = "AUCPR", decreasing = TRUE)
sortedGrid
gridtop5 <- new_resultFrame()
for (i in 1:5){
	gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
	gridtop5 <- add_results(gbm, gridtop5)

}

gbm_first <- do.call(h2o.gbm,
{
	p <- h2o.getModel(sortedGrid@model_ids[[1]])@parameters
	p$sample_rate = NULL
	p
})

gridtop5 <- add_results(gbm_first, gridtop5)

#sample rate actually helps here, higher training accuracy but slightly less test, so lets keep it!
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
		p$validation_frame = test
		p$nfolds = 5
		p$fold_assignment = "Stratified"
		p$keep_cross_validation_prediction = TRUE
		p$keep_cross_validation_fold_assignment = TRUE
		p
	})
	print(gbm@model_id)
	gridtop5_cv <- add_cv_results(cvgbm, gridtop5_cv) #its supposed to not use unseen test yet, only after coming up with the best model
	gridtop5_cv$Model[nrow(gridtop5_cv)] <- gbm@model_id
}
gridtop5_cv
> gridtop5_cv
                    # Model Hit_Ratio Mean_PerC_Error LogLoss   AUC AUCPR Recall Precision
# 1  gbm_cihi_grid_model_62     0.795           0.346   0.326 0.738 0.288  0.467     0.284
# 2 gbm_cihi_grid_model_159     0.803           0.350   0.326 0.738 0.288  0.449     0.290
# 3  gbm_cihi_grid_model_25     0.798           0.348   0.326 0.738 0.289  0.461     0.286
# 4 gbm_cihi_grid_model_139     0.797           0.347   0.326 0.737 0.288  0.464     0.285
# 5 gbm_cihi_grid_model_207     0.800           0.349   0.327 0.737 0.287  0.454     0.288
  # Specificity max_F1 trshold training_AUC training_AUCPR training_Recall
# 1       0.921  0.353   0.164        0.771          0.769           0.860
# 2       0.919  0.353   0.174        0.751          0.742           0.872
# 3       0.920  0.353   0.171        0.758          0.750           0.876
# 4       0.920  0.353   0.167        0.758          0.752           0.876
# 5       0.919  0.352   0.170        0.760          0.753           0.884
  # training_Precision training_Spec
# 1              0.624         0.775
# 2              0.603         0.769
# 3              0.608         0.778
# 4              0.606         0.776
# 5              0.605         0.785

bestModel <- cvgbm
bestModel %>% h2o.saveModel("./hypeTune_results/top_gbm_ed_models_cv/")
sink("./hypeTune_results/top_gbm_ed_models_cv/parameters.txt")
print(bestModel@parameters)
sink()

add_results(cvgbm, new_resultFrame()) %>% write.csv("./hypeTune_results/top_gbm_ed_models_cv/test_train_results.csv")
bestModel@model$cross_validation_metrics@metrics$AUC
bestModel@model$validation_metrics@metrics$AUC
bestModel@model$training_metrics@metrics$AUC

###Save the best and write hyptune parameters