setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders")
library(tidyverse)

df <- readRDS("main_features_cln.rds")
icd <- read.csv("./ICD10 codes/icd10_vars.csv")

str(icd)

df <- df %>% mutate(IDm = paste(ID, days_to_lefteddate, sep="_"))
df <- left_join(df, icd)

#main_Frail_Fal_3L has zero value
##################################
library(infotheo)

X <- df %>% select(-ID, -IDm, - alc_status)
res <- mutinformation(X, df$alc_status, method = "emp")

res <- X %>% sapply(function(x){
mutinformation(x, df$alc_status, method = "emp") / sqrt(entropy(x) * entropy(df$alc_status)) })

write.csv(res[order(-res)], "./mutualInfo_features.csv")


##################################
df %>% group_by(admambul) %>%
summarise(n = n(),
 n_alc_perc = round(mean(as.numeric(alc_status)) * 100,digits = 2))
 
 
 #####################
 df %>% select(-ID, -days_to_lefteddate, -IDm) %>% sapply(tabna)