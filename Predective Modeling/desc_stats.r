setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/Models2.5")
library(tidyverse)
library(h2o)
library(inspectdf)
library(gtsummary)
tabna <- function(x){table(x, useNA = "ifany")}
##adding cihi vars
dfx <- readRDS("./dfx_v2.5.rds")


dfx %>% group_by(rural, alc_status)%>%
summarise()

# "ID"                 "days_to_lefteddate" "indexfyear"         "dischdisp"         
 # [5] "rural"              "instability_q_da"   "dependency_q_da"    "ethniccon_q_da"    
 # [9] "incquint"           "female_flag"        "age_grp"            "triage"            
# [13] "btany"              "clidecunitflag"     "admambul"           "cacsanetech"       
# [17] "cacsit1_grp3"       "cacsitcnt1"         "cacsitcnt"          "cacspartition"     
# [21] "glscoma_bin"        "prv_grp"            "prv_orthop"         "X30day_ed_revisit" 
# [25] "frail_grp"          "all_Cachx_3L"       "time1_grp"          "time2_grp"         
# [29] "acutelos"           "prior_hosp_90d"     "prior_hosp_365d"    "readm_90d"         
# [33] "flag_cardiovers"    "flag_cell_saver"    "flag_chemother"     "flag_dialysis"     
# [37] "flag_feeding_tb"    "flag_heart_resu"    "flag_intv"          "flag_mvent_ge96"   
# [41] "flag_mvent_lt96"    "flag_pa_nutrit"     "flag_paracent"      "flag_pleurocent"   
# [45] "flag_radiother"     "flag_tracheost"     "flag_vasc_accdv"    "scu"               
# [49] "dementia_comorb"    "dementia_main"      "diabetes_comp"      "cerebvsclr"        
# [53] "paralysis"          "weightloss"         "psychoses"          "charl_index_grp"   
# [57] "main_PhysInj_1L"    "main_MentBehav_1L"  "main_Zfactors_1L"   "main_MusclSkelt_1L"
# [61] "patserv_34"         "patserv_72"         "patserv_38"         "patserv_17"        
# [65] "patserv_19"         "patserv_64"         "patserv_35"         "patserv_39"        
# [69] "patserv_55"         "patserv_12"         "patserv_15"         "patserv_36"        
# [73] "alc_status"        
# > 
 
dfx %>%
group_by(time1_grp
 )%>%
summarise(count = n(),
alc_perc = mean(as.numeric(as.character(alc_status))) * 100) %>%
filter(count > 100) %>% arrange(desc(alc_perc))%>%
print(n = 50)

dfx <- dfx %>% mutate(flag_intv = as.integer(flag_intv))
dfx %>% select(prv_orthop, main_Zfactors_1L, flag_intv, alc_status) %>%
tbl_summary(by = alc_status) %>%
add_p()

dfx %>%
select(female_flag, btany, admambul, dependency_q_da,
 main_Zfactors_1L, alc_status, cacsitcnt, flag_intv) %>%
tbl_summary(
by = alc_status,
statistic = list(all_continuous() ~ "{mean} ({sd})",
					all_categorical() ~ "{n} / {N} ({p}%)"),
digits = all_continuous() ~ 2,
missing = "ifany",
missing_text = "(missing)"
) %>%
add_p()


as_gt() %>% gt::gtsave(filename = "./desc_stats/all.rtf")

# as_flex_table() %>% flextable::save_as_docx(path = "./gttable.docx")

####Gettting all 70 variables in one table
dfx %>%
select(3:ncol(dfx)) %>%
tbl_summary(
by = alc_status,
statistic = list(all_continuous() ~ "{mean} ({sd})",
					all_categorical() ~ "{n} ({p}%)"),
digits = all_continuous() ~ 2,
missing = "ifany",
missing_text = "(missing)"
) %>%
modify_footnote(all_stat_cols() ~ "Frequency (%) or Median (IQR)") %>%
add_p() %>%
as_gt() %>% gt::gtsave(filename = "./desc_stats/all_vars.rtf")



#############################
#############################
###CV Line plot##############

x <- gbm_ed@model$cross_validation_metrics_summary %>% as.data.frame()

path <- "./hypeTune_results/cv_for_plot/ED"
table_names <- grep("^5fold_", list.files(path), value=T)
x_list <- c()
x_type <- c("EL", "GBM", "LR", "RF")
tab <- data.frame()
for(table in table_names){
	x <- read.csv(paste(path, table, sep = "/"))
	x <- x %>% rename(metric = X) %>%
		filter(metric %in% c( "auc", "aucpr", "recall", "precision",
		"f1","specificity", "accuracy"))%>%
		select(metric, mean, sd) %>%
		mutate(model = x_type[[match(table, table_names)]])
	tab <- rbind(tab,x)
}

tab <- tab %>% mutate(metric = factor(metric),
model = factor(model)) #for some reason it had 23 levels!, now reduced to 7

##Reading caret cv metrics, their format is different, needs some tweaking before being added to rest!

cart
       # X Model    AUC  PRAUC Recall Precision     F1   Spec Accuracy Threshold
# 1 result Fold1 0.6576 0.6457 0.6675    0.6162 0.6409 0.5843   0.6259       0.5
# 2 result Fold2 0.6604 0.6486 0.6692    0.6181 0.6426 0.5865   0.6279       0.5
# 3 result Fold3 0.6578 0.6462 0.6675    0.6170 0.6412 0.5856   0.6265       0.5
# 4 result Fold4 0.6576 0.6461 0.6108    0.6255 0.6181 0.6343   0.6226       0.5
# 5 result Fold5 0.6558 0.6452 0.6100    0.6238 0.6168 0.6321   0.6211       0.5

cart <- read.csv(paste(path, "CART_upSample_ED_TrainTest_RESULTS.csv", sep = "/"))
mu <- cart %>% summarise_at(vars(AUC:Accuracy), ~mean(.))
sd <- cart %>% summarise_at(vars(AUC:Accuracy), ~sd(.))

# > mu
      # AUC   PRAUC Recall Precision      F1    Spec Accuracy
# 1 0.65784 0.64636  0.645   0.62012 0.63192 0.60456   0.6248
#so that it match tab 
colnames(mu) <- c("auc", "aucpr", "recall", "precision", "f1", "specificity", "accuracy")
colnames(sd) <- c("auc", "aucpr", "recall", "precision", "f1", "specificity", "accuracy")

tab2 <- data.frame(metric = colnames(mu), mean = as.numeric(mu) , sd = as.numeric(sd), model=rep("CART",7))

bagg <- read.csv(paste(path, "BAGGING_dSample_ED_TrainTest_RESULTS.csv", sep = "/"))
mu <- bagg %>% summarise_at(vars(AUC:Accuracy), ~mean(.))
sd <- bagg %>% summarise_at(vars(AUC:Accuracy), ~sd(.))

# > mu
      # AUC   PRAUC Recall Precision      F1    Spec Accuracy
# 1 0.65784 0.64636  0.645   0.62012 0.63192 0.60456   0.6248
#so that it match tab 
colnames(mu) <- c("auc", "aucpr", "recall", "precision", "f1", "specificity", "accuracy")
colnames(sd) <- c("auc", "aucpr", "recall", "precision", "f1", "specificity", "accuracy")

tab3 <- data.frame(metric = colnames(mu), mean = as.numeric(mu) , sd = as.numeric(sd), model=rep("BAG-CART",7))

png("./desc_stats/5fold_ED2.png", width = 6, height = 8, units = "in", res = 300)
rbind(tab,tab2,tab3) %>% ggplot(aes(x = mean, y = model, colour = metric)) +
# geom_line() + 
geom_point(position = position_dodge2(w = 0.2, padding = 0.2), size = 2, shape = 15)+
xlim(0.25,1)+
labs(x = "Accuracy (95% CI)", y = "ML model", colour = "Metrics",
title = "5-fold hold-out results for ED models")+
geom_linerange(aes(xmin = mean - 1.96*sd, xmax = mean + 1.96*sd), position = position_dodge2(w = 0.2, padding = 0.2), size = 1) 
dev.off()



########################
#######Repeating all for EDCIHI


path <- "./hypeTune_results/cv_for_plot/EDCIHI"
table_names <- grep("^5fold_", list.files(path), value=T)
x_list <- c()
x_type <- c("EL", "GBM", "LR", "RF")
tab <- data.frame()
for(table in table_names){
	x <- read.csv(paste(path, table, sep = "/"))
	x <- x %>% rename(metric = X) %>%
		filter(metric %in% c( "auc", "aucpr", "recall", "precision",
		"f1","specificity", "accuracy"))%>%
		select(metric, mean, sd) %>%
		mutate(model = x_type[[match(table, table_names)]])
	tab <- rbind(tab,x)
}

tab <- tab %>% mutate(metric = factor(metric),
model = factor(model)) #for some reason it had 23 levels!, now reduced to 7

##Reading caret cv metrics, their format is different, needs some tweaking before being added to rest!


cart <- read.csv(paste(path, "CART_upSample_EDCIHI_TrainTest_RESULTS.csv", sep = "/"))
mu <- cart %>% summarise_at(vars(AUC:Accuracy), ~mean(.))
sd <- cart %>% summarise_at(vars(AUC:Accuracy), ~sd(.))

# > mu
      # AUC   PRAUC Recall Precision      F1    Spec Accuracy
# 1 0.65784 0.64636  0.645   0.62012 0.63192 0.60456   0.6248
#so that it match tab 
colnames(mu) <- c("auc", "aucpr", "recall", "precision", "f1", "specificity", "accuracy")
colnames(sd) <- c("auc", "aucpr", "recall", "precision", "f1", "specificity", "accuracy")

tab2 <- data.frame(metric = colnames(mu), mean = as.numeric(mu) , sd = as.numeric(sd), model=rep("CART",7))

bagg <- read.csv(paste(path, "BAGGING_dSample_EDCIHI_5foldcv.csv", sep = "/"))
mu <- bagg %>% summarise_at(vars(AUC:Accuracy), ~mean(.))
sd <- bagg %>% summarise_at(vars(AUC:Accuracy), ~sd(.))

# > mu
      # AUC   PRAUC Recall Precision      F1    Spec Accuracy
# 1 0.65784 0.64636  0.645   0.62012 0.63192 0.60456   0.6248
#so that it match tab 
colnames(mu) <- c("auc", "aucpr", "recall", "precision", "f1", "specificity", "accuracy")
colnames(sd) <- c("auc", "aucpr", "recall", "precision", "f1", "specificity", "accuracy")

tab3 <- data.frame(metric = colnames(mu), mean = as.numeric(mu) , sd = as.numeric(sd), model=rep("BAG-CART",7))

png("./desc_stats/5fold_EDCIHI.png", width = 6, height = 8, units = "in", res = 300)
rbind(tab,tab2,tab3) %>% ggplot(aes(x = mean, y = model, colour = metric)) +
# geom_line() + 
geom_point(position = position_dodge2(w = 0.2, padding = 0.2), size = 2, shape = 15)+
xlim(0.25,1)+
labs(x = "Accuracy (95% CI)", y = "ML model", colour = "Metrics",
title = "5-fold hold-out results for ED-CIHI models")+
geom_linerange(aes(xmin = mean - 1.96*sd, xmax = mean + 1.96*sd), position = position_dodge2(w = 0.2, padding = 0.2), size = 1) 
dev.off()