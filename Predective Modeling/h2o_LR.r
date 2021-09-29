modLR <- new_resultFrame()

df.split <- h2o.splitFrame(df.hex, ratios = c(0.8), seed = 1234)
train <- df.split[[1]]
test <- df.split[[2]]
df.split_valid <- h2o.splitFrame(train, ratios = c(0.8), seed = 1234) #validation set to size of 5-fold
#code for sel_feat is in seperate_ED_model.r file.
# sel_feat <- rownames(feat_rank)[1:40] 
vary = "alc_status"



df_glm_def <- h2o.glm(
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = vary,
model_id = "glm_1",
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_each_iteration = T,
seed = 1234
)

modLR <- add_results(df_glm_def, modLR)




df_glm_2 <- h2o.glm(
family = "binomial",
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = vary,
lambda_search = TRUE,
model_id = "glm_lambdaSearch",
# stopping_rounds = 5,
# stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
# stopping_metric = "AUCPR",
# score_each_iteration = T,
seed = 1234
)

modLR <- add_results(df_glm_2, modLR)

#Does not look good because have to disable early stopping
df_glm_3 <- h2o.glm(
family = "binomial",
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = sel_feat,
y = vary,
model_id = "glm_3",
lambda = 0, #no penalty
stopping_rounds = 5,
stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
stopping_metric = "AUCPR",
score_each_iteration = T,
seed = 1234
)

modLR <- add_results(df_glm_3, modLR)

#Removing SCU as it has too many levels (16)
df_glm_4 <- h2o.glm(
family = "binomial",
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
x = grep("scu", sel_feat, value= TRUE, invert = TRUE),
y = vary,
model_id = "glm_noSCU_no stopping",
lambda = 0, #no penalty
# stopping_rounds = 5,
# stopping_tolerance = 1e-3, ##early stop if AUCPR did not improve at least 0.1% over 5 consecutive turns
# stopping_metric = "AUCPR",
# score_each_iteration = T,
seed = 1234
)

modLR <- add_results(df_glm_4, modLR)



###Grid search over lamvbda

hyper_params <- list(
lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0),
alpha = seq(0,1,0.01)
)
search_criteria = list(
strategy = "RandomDiscrete", #random grid search
max_runtime_secs = 3600 * 1,
max_models = 200,
seed = 1234)

t <- Sys.time()
grid <- h2o.grid(
hyper_params = hyper_params,
search_criteria = search_criteria,
algorithm = "glm",
grid_id = "glm_grid",
x = grep("scu", sel_feat, value= TRUE, invert = TRUE),
y = vary,
family = "binomial",
training_frame = df.split_valid[[1]],
validation_frame = df.split_valid[[2]],
seed = 1234
)
print("Time elapsed for glm hyperTuning: ", Sys.time() - t)


sortedGrid <- h2o.getGrid("glm_grid", sort_by = "auc", decreasing = TRUE)
sortedGrid <- h2o.getGrid("glm_grid", sort_by = "AUCPR", decreasing = TRUE)

# H2O Grid Details
# ================

# Grid ID: glm_grid 
# Used hyper parameters: 
  # -  alpha 
  # -  lambda 
# Number of models: 200 
# Number of failed models: 0 

# Hyper-Parameter Search Summary: ordered by decreasing auc
   # alpha   lambda          model_ids                auc
# 1 [0.59] [1.0E-5] glm_grid_model_152 0.7886795460171198
# 2 [0.75] [1.0E-5]   glm_grid_model_9 0.7886746197066793
# 3 [0.97] [1.0E-5]  glm_grid_model_62 0.7886735717950397
# 4 [0.25] [1.0E-5]  glm_grid_model_34 0.7886733249166507
# 5  [1.0] [1.0E-5]  glm_grid_model_57 0.7886730009010012

# ---
     # alpha lambda         model_ids auc
# 195 [0.85]  [1.0] glm_grid_model_73 0.5
# 196 [0.29]  [0.5] glm_grid_model_75 0.5
# 197 [0.56]  [0.5] glm_grid_model_76 0.5
# 198 [0.87]  [0.5] glm_grid_model_88 0.5
# 199 [0.59]  [1.0] glm_grid_model_93 0.5
# 200 [0.98]  [1.0] glm_grid_model_97 0.5


lr <- h2o.getModel(sortedGrid@model_ids[[1]])
newLR <- do.call(h2o.glm,
	#update parameters models, original models were cross validated, no need for that now, also change predictors in here
	{
		p <- lr@parameters
		p$model_id = paste0("LR_ed_small_", 1)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$x = sel_feat_ed_small
		p
	})
modLR <- add_results(newLR, modLR)

newLR <- do.call(h2o.glm,
	{
		p <- lr@parameters
		p$model_id = paste0("LR_ed_big_", 1)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$x = sel_feat_ed_big
		p
	})
modLR <- add_results(newLR, modLR)

newLR <- do.call(h2o.glm,
	{
		p <- lr@parameters
		p$model_id = paste0("LR_ed_plusCIHI_", 1)
		p$training_frame = train
		p$validation_frame = test
		p$nfolds = NULL
		p$fold_assignment = NULL
		p$x = sel_feat
		p
	})
modLR <- add_results(newLR, modLR)

write.csv(modLR, "./results_GLM_unseenTest.csv")


                  # Model Hit_Ratio Mean_PerC_Error LogLoss   AUC AUCPR Recall
# 1                 glm_1     0.810           0.308   0.314 0.789 0.333  0.538
# 2      glm_lambdaSearch     0.762           0.342   0.349 0.725 0.271  0.521
# 3                 glm_3     0.812           0.309   0.313 0.789 0.334  0.531
# 4 glm_noSCU_no stopping     0.814           0.310   0.314 0.789 0.333  0.526
# 5         LR_ed_small_1     0.791           0.356   0.333 0.723 0.268  0.450
# 6           LR_ed_big_1     0.783           0.352   0.332 0.727 0.271  0.469
# 7      LR_ed_plusCIHI_1     0.799           0.302   0.314 0.788 0.332  0.565
  # Precision Specificity max_F1 trshold training_AUC training_AUCPR
# 1     0.325       0.930  0.405   0.169        0.789          0.330
# 2     0.259       0.924  0.346   0.126        0.721          0.268
# 3     0.328       0.930  0.405   0.173        0.789          0.331
# 4     0.329       0.929  0.405   0.174        0.788          0.330
# 5     0.276       0.918  0.342   0.162        0.724          0.269
# 6     0.270       0.919  0.343   0.160        0.729          0.272
# 7     0.314       0.933  0.404   0.160        0.789          0.332
  # training_Recall training_Precision training_Spec
# 1           0.568              0.311         0.934
# 2           0.517              0.256         0.924
# 3           0.539              0.322         0.931
# 4           0.546              0.318         0.932
# 5           0.450              0.275         0.918
# 6           0.447              0.279         0.918
# 7           0.541              0.322         0.931
