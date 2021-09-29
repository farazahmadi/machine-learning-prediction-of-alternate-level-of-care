setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders")
library(tidyverse)
library(icd)
df <- read.csv("./datasets/New/NACRASCIHI_2_dupRemoved.csv")

tabna <- function(x){table(x, useNA = "ifany")}


icd <- df %>% select(ID, days_to_lefteddate, dx10code1:dx10code10, alc_status)%>%
mutate(IDm = paste(ID, days_to_lefteddate, sep="_"))

# library(data.table)
# long <- melt(setDT(icd), id.vars = c("ID", "days_to_lefteddate"), variable.name="code")

# long <- long %>% arrange(ID, days_to_lefteddate) %>%
# mutate(IDm = paste(ID, days_to_lefteddate, sep="_"))

ls("package:icd")

# "icd10_comorbid"          
# [31] "icd10_comorbid_ahrq"      "icd10_comorbid_ccs"      
# [33] "icd10_comorbid_charlson"  "icd10_comorbid_elix"     
# [35] "icd10_comorbid_hcc"       "icd10_comorbid_pccc_dx"  
# [37] "icd10_comorbid_pccc_pcs"  "icd10_comorbid_quan_deyo"
# [39] "icd10_comorbid_quan_elix" "icd10_filter_invalid"    
# [41] "icd10_filter_valid"       "icd10_map_ahrq"          
# [43] "icd10_map_ahrq_pcs"       "icd10_map_cc"            
# [45] "icd10_map_ccs"            "icd10_map_charlson"      
# [47] "icd10_map_elix"           "icd10_map_pccc_dx"       
# [49] "icd10_map_pccc_pcs"       "icd10_map_quan_deyo"     
# [51] "icd10_map_quan_elix"      "icd10_names_ccs"         

ic <- icd %>% select(IDm, dx10code1:dx10code10)
result = data.frame()
res <- icd10_comorbid_charlson(ic, return_df = TRUE)


#Calculate three different comorbidity index---> charlson, quan, van_walraven
charl_index <- charlson_from_comorbid(res, scoring_system = "charlson")
f <- data.frame("ID" = attributes(charl_index), "charl_index" = as.numeric(charl_index))
names(f)[1] <- "IDm"
f$quan_index <- as.numeric(charlson_from_comorbid(res, scoring_system = "quan"))
f$van_walraven <- as.numeric(van_walraven_from_comorbid(icd10_comorbid_quan_elix(ic, return_df = TRUE)))

ic <- ic %>% left_join(f)
ic <- ic %>% left_join(icd %>% select(IDm, alc_status))

sapply(f[,2:4], tabna)

ic %>% group_by(charl_index) %>%
 summarise (n = n(), tot_perc = n/nrow(ic),
 alc_perc = mean(as.numeric(alc_status))*100)
 
 ic %>% group_by(quan_index) %>%
 summarise (n = n(), tot_perc = n/nrow(ic),
 alc_perc = mean(as.numeric(alc_status))*100)
 
 ic %>% group_by(van_walraven) %>%
 summarise (n = n(), tot_perc = n/nrow(ic),
 alc_perc = mean(as.numeric(alc_status))*100) %>% print(n=50)
 
 
ic$charl_index <- factor(ic$charl_index)
ic$quan_index <- factor(ic$quan_index)

#the way convert numerical factor to numeric
ic <- ic %>% mutate(charl_index_grp = ifelse(as.numeric(levels(charl_index))[charl_index] > 3,
 "Oth", as.numeric(levels(charl_index))[charl_index]) ) %>% 
 mutate(charl_index_grp = factor(charl_index_grp))
ic <- ic %>% mutate(charl_index_bin = ifelse(charl_index %in% c(0,1) , "0and1", "More") ) %>%
mutate(charl_index_bin = factor(charl_index_bin))

ic <- ic %>% mutate(quan_index_grp = ifelse(as.numeric(levels(quan_index))[quan_index] > 3,
 "Oth", as.numeric(levels(quan_index))[quan_index]) ) %>% 
 mutate(quan_index_grp = factor(quan_index_grp))
ic <- ic %>% mutate(quan_index_bin = ifelse(quan_index %in% c(0,1) , "0and1", "More") ) %>%
mutate(quan_index_bin = factor(quan_index_bin))

str(ic)

result <- double() 
result <- add_result("charl_index" , ic, result)
result <- add_result("quan_index" , ic, result)
result <- add_result("van_walraven" , ic, result) #as numeric, too many levels, maybe join later
result <- add_result("charl_index_grp" , ic, result)
result <- add_result("charl_index_bin" , ic, result)
result <- add_result("quan_index_grp" , ic, result)
result <- add_result("quan_index_bin" , ic, result)
result
write.csv(result, "./ICD10 codes/comorb_index_univ_results.csv")



###############Engineer more Features

## Using 1L code of main ICD10 diagnosis
ic$mcode1L <- substr(ic$dx10code1, 1,1)

# S physical injury
# F mental, bahivoural,etc
# Z factors
# M musculoskeleton

ic <- ic %>% mutate(main_PhysInj_1L = ifelse(mcode1L == "S", 1, 0),
main_MentBehav_1L = ifelse(mcode1L == "F", 1, 0),
main_Zfactors_1L = ifelse(mcode1L == "Z", 1, 0),
main_MusclSkelt_1L = ifelse(mcode1L == "M", 1, 0))
#turn to factor
ic <- ic %>% mutate_at(vars(main_PhysInj_1L:main_MusclSkelt_1L), ~factor(.))

sapply(ic %>% select(main_PhysInj_1L:main_MusclSkelt_1L), tabna)
#Univariate odds
result <- double() 
result <- add_result("main_PhysInj_1L" , ic, result)
result <- add_result("main_MentBehav_1L" , ic, result)
result <- add_result("main_Zfactors_1L" , ic, result)
result <- add_result("main_MusclSkelt_1L" , ic, result)
result

## Using 1L code of main ICD10 diagnosis
ic$mcode3L <- substr(ic$dx10code1, 1,3)

#R64 Cachexia
#F05 Delirium-Frailty
#W01-06-10-18-19 FALL-Frailty
# Add more later?
fall_frial <- c("W01", "W06", "W10","W18", "W19")
ic %>% filter(mcode3L == "R64") %>% nrow()

ic <- ic %>% mutate(main_Cachx_3L = ifelse(mcode3L == "R64", 1, 0),
main_Frail_Del_3L = ifelse(mcode3L == "F05", 1, 0),
main_Frail_Fal_3L = ifelse(mcode3L %in% fall_frial, 1, 0))
#turn to factor
ic <- ic %>% mutate_at(vars(main_Cachx_3L:main_Frail_Fal_3L), ~factor(.))

result <- add_result("main_Cachx_3L" , ic, result)
result <- add_result("main_Frail_Del_3L" , ic, result)
result <- add_result("main_Frail_Fal_3L" , ic, result) # No W in data, wierd
result


###Analayzing W code in all other dx10codes

# ic %>% filter_at(vars(dx10code1:dx10code10), any_vars(str_starts(.,"W"))) %>%
# select(IDm , dx10code1:dx10code10) %>% head(50)

#number of people containing at least one W code in all 10 codes
# ic %>% filter_at(vars(dx10code1:dx10code10), any_vars(str_starts(.,"W"))) %>%
# select(IDm , dx10code1:dx10code10) %>% nrow()
#now cheking for frailty codes
# ic %>% filter_at(vars(dx10code1:dx10code10), any_vars(substr(.,1,3) %in% fall_frial)) %>%
# select(IDm , dx10code1:dx10code10) %>% head(50)

# ic %>% filter_at(vars(dx10code1:dx10code10), any_vars(substr(.,1,3) %in% fall_frial)) %>%
# select(IDm , dx10code1:dx10code10) %>% nrow()


#Using frailty codes on all 10 codes to construct the dummy variables
##FALL
fal_ls <- ic %>% filter_at(vars(dx10code1:dx10code10), any_vars(substr(.,1,3) %in% fall_frial)) %>%
select(IDm , dx10code1:dx10code10) %>% .$IDm

ic <- ic %>% mutate(all_Frail_Fal_3L = ifelse(IDm %in% fal_ls, 1, 0))
## Delirium
del_ls <- ic %>% filter_at(vars(dx10code1:dx10code10),
 any_vars(substr(.,1,3) == "F05")) %>%
select(IDm , dx10code1:dx10code10) %>% .$IDm 

ic <- ic %>% mutate(all_Frail_Del_3L = ifelse(IDm %in% del_ls, 1, 0))
## Cachexia
cach_ls <- ic %>% filter_at(vars(dx10code1:dx10code10),
 any_vars(substr(.,1,3) == "R64")) %>%
select(IDm , dx10code1:dx10code10) %>% .$IDm 

ic <- ic %>% mutate(all_Cachx_3L = ifelse(IDm %in% cach_ls, 1, 0))
#turn to factor
ic <- ic %>%
 mutate_at(vars(all_Frail_Fal_3L, all_Frail_Del_3L, all_Cachx_3L), ~factor(.))
 
###
result <- add_result("all_Cachx_3L" , ic, result)
result <- add_result("all_Frail_Del_3L" , ic, result)
result <- add_result("all_Frail_Fal_3L" , ic, result) 
result

write.csv(result, "./ICD10 codes/icd10_varExtract_univ_results.csv")

###Save created subsets
str(ic)

ic %>% select(IDm, charl_index, quan_index, charl_index_bin, charl_index_grp,
quan_index, quan_index_bin, quan_index_grp, main_PhysInj_1L:main_MusclSkelt_1L,
main_Cachx_3L:all_Cachx_3L) %>% write.csv("./ICD10 codes/icd10_vars.csv", row.names=F)


