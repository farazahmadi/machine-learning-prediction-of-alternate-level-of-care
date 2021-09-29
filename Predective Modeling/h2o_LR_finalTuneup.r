modLR <- new_resultFrame()

df.split_valid <- h2o.splitFrame(train, ratios = c(0.8), seed = 1234) #validation set to size of 5-fold


#####ED-only model


glm_noreg <- h2o.glm(
family = "binomial",
training_frame = train,
nfolds = 5,
fold_assignment = "Stratified",
x = sel_feat_ed_big,
y = "alc_status",
model_id = "glm_noreg",
lambda = 0, #no penalty
compute_p_values = TRUE,
remove_collinear_columns = TRUE,
stopping_rounds = 5,
stopping_tolerance = 1e-4, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_each_iteration = T,
seed = 1234
)

# glm_noreg@model$coefficients_table %>% as.data.frame() 

modLR <- add_cv_results(glm_noreg, modLR)
glm_noreg@model$cross_validation_metrics_summary
nrow(test)
# [1] 274428
glm_noreg %>% h2o.saveModel("./hypeTune_results/logistic_regression/LR_ED/")
modLR %>% write.csv("./hypeTune_results/logistic_regression/LR_ED/test_train_results.csv", row.names = F)

glm_noreg@model$cross_validation_metrics_summary %>% as.data.frame() %>%
write.csv("./hypeTune_results/logistic_regression/LR_ED/5fold_results.csv")

pred <- h2o.predict(glm_noreg, newdata = test)
pred %>% head()
df_test <- test["alc_status"] %>% as.data.frame
prob_test <- pred$p1 %>% as.data.frame()
LR_ED_ROC <- data.frame(prob = prob_test$p1, obs = df_test$alc_status)

LR_ED_ROC %>% saveRDS("./hypeTune_results/logistic_regression/LR_ED/LR_ED_ROC.rds")

###Now EDcihi


glm_noreg_edcihi <- h2o.glm(
family = "binomial",
training_frame = train,
nfolds = 5,
fold_assignment = "Stratified",
x = sel_feat,
y = "alc_status",
model_id = "glm_noreg_edcihi",
lambda = 0, #no penalty
compute_p_values = TRUE,
remove_collinear_columns = TRUE,
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_each_iteration = T,
seed = 1234
)

# glm_noreg@model$coefficients_table %>% as.data.frame() 

modLR <- add_cv_results(glm_noreg_edcihi, modLR)

glm_noreg_edcihi %>% h2o.saveModel("./hypeTune_results/logistic_regression/LR_EDCIHI/")
modLR[5,] %>% write.csv("./hypeTune_results/logistic_regression/LR_EDCIHI/test_train_results.csv", row.names = F)

glm_noreg_edcihi@model$cross_validation_metrics_summary %>% as.data.frame() %>%
write.csv("./hypeTune_results/logistic_regression/LR_EDCIHI/5fold_results.csv")

pred <- h2o.predict(glm_noreg_edcihi, newdata = test)
df_test <- test["alc_status"] %>% as.data.frame
prob_test <- pred$p1 %>% as.data.frame()
LR_EDCIHI_ROC <- data.frame(prob = prob_test$p1, obs = df_test$alc_status)

LR_EDCIHI_ROC %>% saveRDS("./hypeTune_results/logistic_regression/LR_EDCIHI/LR_EDCIHI_ROC.rds")


###########################################
###########################################ELASTIC NET TUNING
###########################################


hyper_params <- list(
lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0),
alpha = seq(0,1,0.01)
)

hyper_params_final <- list(
lambda =  c(0.00001),
alpha = c(0.11,0.12,0.39, 0.24)
)
search_criteria = list(
strategy = "RandomDiscrete", #random grid search
max_runtime_secs = 3600 * 1,
max_models = 100,
seed = 1234)

t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params_final,
search_criteria = search_criteria,
algorithm = "glm",
grid_id = "EL_grid",
x = sel_feat_ed_big,
y = "alc_status",
family = "binomial",
training_frame = train,
nfolds = 5,
fold_assignment = "Stratified",
seed = 1234
)
print("Time elapsed for glm hyperTuning: ", Sys.time() - t)

sortedGrid <- h2o.getGrid("EL_grid", sort_by = "AUCPR", decreasing = TRUE)

bestModel <- h2o.getModel(sortedGrid@model_ids[[1]])

testRes <- new_resultFrame()
testRes <- add_test_results(h2o.performance(bestModel, newdata=test), testRes)
testRes <- add_cv_results(bestModel, testRes)
testRes %>% write.csv("./hypeTune_results/logistic_regression/ElasticNet_ED/test_train_results.csv")
bestModel %>% h2o.saveModel("./hypeTune_results/logistic_regression/ElasticNet_ED/")
sink("./hypeTune_results/logistic_regression/ElasticNet_ED/parameters.txt")
print(bestModel@parameters)
sink()

bestModel@model$cross_validation_metrics_summary %>% as.data.frame() %>%
write.csv("./hypeTune_results/logistic_regression/ElasticNet_ED/5fold_results.csv")

pred <- h2o.predict(bestModel, newdata = test)
df_test <- test["alc_status"] %>% as.data.frame
prob_test <- pred$p1 %>% as.data.frame()
EL_ED_ROC <- data.frame(prob = prob_test$p1, obs = df_test$alc_status)

EL_ED_ROC %>% saveRDS("./hypeTune_results/logistic_regression/ElasticNet_ED/EL_ED_ROC.rds")



################################ELASTIC NET FOR ED CIHI


hyper_params <- list(
lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0),
alpha = seq(0,1,0.01)
)

hyper_params_final <- list(
lambda =  c(0.0001, 0.00001),
alpha = c(0.59,0.75,0.97, 0.25, 1) #tuned from prev files
)
search_criteria = list(
strategy = "RandomDiscrete", #random grid search
max_runtime_secs = 3600 * 1,
max_models = 200,
seed = 1234)

t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params_final,
search_criteria = search_criteria,
algorithm = "glm",
grid_id = "EL_grid_CIHI",
x = sel_feat,
y = "alc_status",
family = "binomial",
training_frame = train,
nfolds = 5,
fold_assignment = "Stratified",
seed = 1234
)
print("Time elapsed for glm hyperTuning: ", Sys.time() - t)

sortedGrid <- h2o.getGrid("EL_grid_CIHI", sort_by = "AUCPR", decreasing = TRUE)

bestModel <- h2o.getModel(sortedGrid@model_ids[[1]])


testRes <- add_test_results(h2o.performance(bestModel, newdata=test), testRes)
testRes <- add_cv_results(bestModel, testRes)
testRes %>% write.csv("./hypeTune_results/logistic_regression/ElasticNet_EDCIHI/test_train_results.csv")
bestModel %>% h2o.saveModel("./hypeTune_results/logistic_regression/ElasticNet_EDCIHI/")
sink("./hypeTune_results/logistic_regression/ElasticNet_EDCIHI/parameters.txt")
print(bestModel@parameters)
sink()

bestModel@model$cross_validation_metrics_summary %>% as.data.frame() %>%
write.csv("./hypeTune_results/logistic_regression/ElasticNet_EDCIHI/5fold_results.csv")

pred <- h2o.predict(bestModel, newdata = test)
df_test <- test["alc_status"] %>% as.data.frame
prob_test <- pred$p1 %>% as.data.frame()
EL_EDCIHI_ROC <- data.frame(prob = prob_test$p1, obs = df_test$alc_status)

EL_EDCIHI_ROC %>% saveRDS("./hypeTune_results/logistic_regression/ElasticNet_EDCIHI/EL_EDCIHI_ROC.rds")
