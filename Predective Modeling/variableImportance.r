

##feature importance

gbmcihi <- h2o.loadModel("./hypeTune_results/top_gbm_cihi_models_cv/gbm_cihi_cv_1")

h2o.varimp(gbmcihi)
h2o.varimp_plot(gbmcihi, num_of_features = 30)

png("./varImport/CIHI_varimp.png", width = 8, height = 10, units = "in", res = 300)
h2o.varimp(gbmcihi) %>%
as.data.frame() %>%
select(variable, scaled_importance) %>%
ggplot(aes(x = reorder(variable, scaled_importance),
 weight = scaled_importance)) +
geom_bar(fill = "dodgerblue1") +
#scale_fill_discrete(name = "variable name")+
theme_classic() + ggtitle("Variable Importance: GBM in-hospital model") +
xlab("") + ylab("scaled importance") +
coord_flip()
dev.off()


####top 20 version with variable names
 [1] "acutelos"          "admambul"          "main_PhysInj_1L"   "age_grp"          
 [5] "scu"               "cacsitcnt"         "cacsit1_grp3"      "triage"           
 [9] "main_Zfactors_1L"  "prv_grp"           "main_MentBehav_1L" "patserv_34"       
[13] "frail_grp"         "all_Cachx_3L"      "dementia_main"     "patserv_12"       
[17] "rural"             "patserv_17"        "readm_90d"         "weightloss"    

name_ <- c("Acute LOS", "ED arrival method", "Physical Inj. main diagnosis", "Age",
"Special care unit", "ED investigative technology count", "Investigative technology type", "Triage score",
"Z codes main diagnosis", "ED main health provider", "Mental/behavioural main diagnosis", "Orthopaedic surgery in hospital",
"Frailty index", "Cachexia diagnosis", "Dementia  main diagnosis", "Cardiology service in hosptial",
"Rural/urban", "Neurology service in hospital", "90-day readmission", "Weightloss comorbidity")

png("./varImport/Top20_CIHI_varimp.png", width = 8, height = 10, units = "in", res = 300)
h2o.varimp(gbmcihi) %>%
as.data.frame() %>%
select(variable, scaled_importance) %>%
head(20) %>%
mutate(varName = name_) %>%
ggplot(aes(x = reorder(varName, scaled_importance),
 weight = scaled_importance)) +
geom_bar(fill = "dodgerblue1") +
#scale_fill_discrete(name = "variable name")+
theme_classic() + ggtitle("Top 20 Predictors: GBM in-hospital model") +
xlab("") + ylab("scaled importance") +
coord_flip()
dev.off()


#################ED models

gbmed <- h2o.loadModel("./hypeTune_results/top_gbm_ed_models_cv/GBM_model_R_1626226288660_14594")

h2o.varimp(gbmed)
h2o.varimp_plot(gbmed, num_of_features = 30)

png("./varImport/ED_varimp.png", width = 8, height = 10, units = "in", res = 300)
h2o.varimp(gbmed) %>%
as.data.frame() %>%
select(variable, scaled_importance) %>%
ggplot(aes(x = reorder(variable, scaled_importance),
 weight = scaled_importance)) +
geom_bar(fill = "dodgerblue1") +
#scale_fill_discrete(name = "variable name")+
theme_classic() + ggtitle("Variable Importance: GBM Emergency Department model") +
xlab("") + ylab("scaled importance") +
coord_flip()
dev.off()


name__ <- c("ED arrival method", "Physical Inj. main diagnosis", "Age",
"ED investigative technology count", "Investigative technology type", "Triage score",
"Mental/behavioural main diagnosis", "Frailty index",
"Income quintiles", "Ethnic concentration quintiles", "Residential Instability quintiles",
"Z codes main diagnosis", "Charlson Index", "Waiting time from triage to first assessment",
"Dependency quintiles", "Time from first asses. to discharge from ED",
"Cachexia diagnosis", "Musculoskeletal diseases as main diagnosis", "Dementia main diagnosis",
"Weightloss comorbidity")


png("./varImport/Top20_ED_varimp.png", width = 8, height = 10, units = "in", res = 300)
h2o.varimp(gbmed) %>%
as.data.frame() %>%
select(variable, scaled_importance) %>%
head(20) %>%
mutate(varName = name__) %>%
ggplot(aes(x = reorder(varName, scaled_importance),
 weight = scaled_importance)) +
geom_bar(fill = "dodgerblue1") +
#scale_fill_discrete(name = "variable name")+
theme_classic() + ggtitle("Top 20 Predictors: GBM Emergency Department model") +
xlab("") + ylab("scaled importance") +
coord_flip()
dev.off()