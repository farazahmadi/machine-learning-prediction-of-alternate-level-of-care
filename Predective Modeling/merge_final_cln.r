setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders")
library(tidyverse)
library(h2o)
tabna <- function(x){table(x, useNA = "ifany")}
##adding cihi vars
df <- readRDS("./Cihi-only Models/cihi_cln.rds")
##adding ed data set
dfx <- readRDS("./Models2.0/dfx_2.0.rds")
setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/Models2.5")



###PATSERV feature engineering
pat <- df %>%
group_by(patserv
 )%>%
summarise(count = n(),
alc_perc = mean(as.numeric(as.character(alc_status))) * 100) %>%
filter(count > 1000 & !patserv == 99) %>%  ##99 is ALC as main service
arrange(desc(alc_perc))

###TOP 5 ALC and TOP LOW ALC are flagged for prediction
pat_values <- c(head(pat) %>% .$patserv %>% as.character%>% as.numeric,
tail(pat) %>% .$patserv %>% as.character%>%as.numeric)

##Create flags using mutate
for (v in pat_values){
var_name <- paste0("patserv_", v)
df <- df %>% mutate(!!var_name := ifelse(patserv == v, 1, 0) %>% factor)
}

names(df)
 # [1] "ID"                 "days_to_lefteddate" "indexfyear"         "dischdisp"         
 # [5] "rural"              "instability_q_da"   "dependency_q_da"    "ethniccon_q_da"    
 # [9] "incquint"           "age_group"          "female_flag"        "acutelos"          
# [13] "prior_hosp_90d"     "prior_hosp_365d"    "readm_90d"          "flag_cardiovers"   
# [17] "flag_cell_saver"    "flag_chemother"     "flag_dialysis"      "flag_feeding_tb"   
# [21] "flag_heart_resu"    "flag_intv"          "flag_mvent_ge96"    "flag_mvent_lt96"   
# [25] "flag_pa_nutrit"     "flag_paracent"      "flag_pleurocent"    "flag_radiother"    
# [29] "flag_tracheost"     "flag_vasc_accdv"    "scu"                "patserv"           
# [33] "alc_status"         "age_grp"            "dementia_comorb"    "dementia_main"     
# [37] "diabetes_comp"      "cerebvsclr"         "paralysis"          "weightloss"        
# [41] "psychoses"          "charl_index_grp"    "main_PhysInj_1L"    "main_MentBehav_1L" 
# [45] "main_Zfactors_1L"   "main_MusclSkelt_1L" "patserv_34"         "patserv_72"        
# [49] "patserv_38"         "patserv_17"         "patserv_19"         "patserv_64"        
# [53] "patserv_35"         "patserv_39"         "patserv_55"         "patserv_12"        
# [57] "patserv_15"         "patserv_36"        


names(dfx)
# "IDm"                "age_grp"            "age_group"          "female_flag"       
 # [5] "rural"              "dependency_q_da"    "deprivation_q_da"   "ethniccon_q_da"    
 # [9] "instability_q_da"   "incquint"           "triage"             "btany"             
# [13] "clidecunitflag"     "admambul"           "cacsanetech"        "cacsit1_grp"       
# [17] "cacsit1_grp2"       "cacsit1_grp3"       "cacsitcnt1"         "cacsitcnt"         
# [21] "cacspartition"      "glscoma"            "glscoma_bin"        "prv_grp"           
# [25] "prv_orthop"         "X30day_ed_revisit"  "charl_index"        "charl_index_bin"   
# [29] "frailty_score"      "frail_grp"          "main_PhysInj_1L"    "main_MentBehav_1L" 
# [33] "main_Zfactors_1L"   "main_MusclSkelt_1L" "all_Cachx_3L"       "trg_to_ass"        
# [37] "time1_grp"          "ass_to_lef"         "time2_grp"          "alc_status"  


df <- df %>%
mutate(IDm = paste0(ID, "_", days_to_lefteddate))
#ONLY select the ED vars
dfx <- dfx %>%
 select(IDm, triage:X30day_ed_revisit, frailty_score, frail_grp, all_Cachx_3L:time2_grp)
 
dfx <- left_join(dfx, df, by = "IDm")

dfx <- dfx %>% select(-IDm, -patserv, -age_group, -cacsit1_grp, -cacsit1_grp2,
-glscoma, -trg_to_ass, -ass_to_left, -frailty_score)
##sort them out
dfx <- dfx %>% select(ID:female_flag, age_grp, triage:time2_grp,
acutelos:scu, dementia_comorb:ncol(dfx), alc_status )
############
str(dfx)
library(infotheo)
X <- dfx %>% select(-c(1:4), - alc_status)
res <- X %>% sapply(function(x){
mutinformation(x, dfx$alc_status, method = "emp") / sqrt(entropy(x) * entropy(df$alc_status)) })

write.csv(res[order(-res)], "./mutualInfo_features.csv")
feat_rank <- res[order(-res)] %>% as.data.frame()

sel_feat <- rownames(feat_rank)[1:40]
#################
# X <- X %>% select(-charl_index_grp) %>%
# mutate_if(is.factor(),~as.numeric(as.character(.)))
chisq.test(dfx$flag_cardiovers %>% as.numeric, dfx$flag_cell_saver %>% as.numeric(), correct = FALSE)
cor(dfx$flag_cardiovers %>% as.numeric, dfx$flag_cell_saver %>% as.numeric())

#################

saveRDS(dfx, "./dfx_v2.5.rds")



#################

dfx %>% select(33:60) %>% sapply(tabna)