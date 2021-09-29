setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders")
library(tidyverse)

df <- read.csv("./datasets/New/NACRASCIHI_2_dupRemoved.csv")

tabna <- function(x){table(x, useNA = "ifany")}

df<- df %>% select(ID, days_to_lefteddate,triage, female_flag, btany, clidecunitflag,
cacsanetech, cacsit1, cacsitcnt1, cacsitcnt, cacspartition, glscoma,
age_group, rural, dependency_q_da, deprivation_q_da, ethniccon_q_da,
instability_q_da, incquint, prvserv1, admambul, alc_status )

##Not added above are the ICD10 codes and other mutations of them
##Inst type if more information is found should be added
##complaint and consultserv 
##ANY TIME BASED variable is not added
str(df)
df$alc_status <- factor(df$alc_status)
df$female_flag <- as.factor(df$female_flag)

df[df$triage == 9, "triage"] <- NA
df$triage <- factor(df$triage)

df$btany[df$btany == ""] <- NA
df$btany <- factor(df$btany)

df$cacsanetech <- factor(df$cacsanetech, levels = c(8, 1, 2, 3, 4))

df$cacsanetech_bin <- ifelse(df$cacsanetech == 8, 0,1) #Anaestethic were used
df$cacsanetech_gen <- ifelse(df$cacsanetech == 1, 1, 0) 
df$cacsanetech_oth <- ifelse(df$cacsanetech == 2, 1, 0) 
df$cacsanetech_unm <- ifelse(df$cacsanetech == 3, 1, 0) 
df$cacsanetech_lcl <- ifelse(df$cacsanetech == 4, 1, 0) 


##Technology code
df$cacsit1_grp[df$cacsit1 %in% c(1:5)] <- "Cat_scan"
df$cacsit1_grp[df$cacsit1 == 6] <- "EEG"
df$cacsit1_grp[df$cacsit1 %in% c(7,20,21)] <- "Xray"
df$cacsit1_grp[df$cacsit1 == 8] <- "Mammography"
df$cacsit1_grp[df$cacsit1 %in% c(9:11)] <- "MRI"
df$cacsit1_grp[df$cacsit1 %in% c(12:15)] <- "Nuclear"
df$cacsit1_grp[df$cacsit1 == 16] <- "Stress_test"
df$cacsit1_grp[df$cacsit1 %in% c(17:19)] <- "Ultrasound"

df %>% group_by(cacsit1_grp) %>% summarise( n= n(),
perc = round(n/nrow(df) * 100,digits=2),
alc_perc_grp = mean(as.numeric(alc_status)) * 100) %>%
arrange(desc(alc_perc_grp))

##Based on above results, EEG, Mammo and Stress_test are combind together
##and rest will have their own title

df$cacsit1_grp[df$cacsitcnt == 0] <- "Notapp"
#Grouping some of the least populated categ.
df$cacsit1_grp[df$cacsit1_grp %in% c("EEG",
									 "Mammography",
									 "Stress_test")] <- "Others"
#factor it here!
df$cacsit1_grp <- factor(df$cacsit1_grp)
df$cacsit1_grp <- relevel(df$cacsit1_grp, "Notapp")
tabna(df$cacsit1_grp)

##
df$cacsitcnt1[df$cacsit1_grp == "Notapp"] <- "Notapp"
df$cacsitcnt1 <- factor(df$cacsitcnt1)
df$cacsitcnt1 <- relevel(df$cacsitcnt1, "Notapp")
levels(df$cacsitcnt1) <- c( "Notapp", "Just1","2orMore" )

##cacsitcnt is numerical

##
tabna(df$cacspartition)
df$cacspartition[df$cacspartition == ""] <- NA
df$cacspartition <- factor(df$cacspartition, levels = c("A", "D", "I"))

## Glascow Coma Scale ##
df$glscoma[df$glscoma %in% c(3:8)] <- "Severe"
df$glscoma[df$glscoma %in% c(9:13)] <- "Moderate"
df$glscoma[df$glscoma %in% c(14,15)] <- "Mild"
df$glscoma[df$glscoma %in% c(99)] <- NA
df$glscoma[df$glscoma %>% is.na()] <- "Notapp"
df$glscoma <- as.factor(df$glscoma)
tabna(df$glscoma)
df$glscoma <- relevel(df$glscoma, "Notapp")##changing order to chnage base of odds ratio in LR

df$glscoma_bin <- as.character(df$glscoma)
df$glscoma_bin[! df$glscoma == "Notapp" ] <- "Y"
tabna(df$glscoma_bin)
df$glscoma_bin   <- factor(df$glscoma_bin )

df$glscoma_grp <- as.character(df$glscoma)
df$glscoma_grp[ df$glscoma %in% c("Moderate", "Mild") ] <- "LessSevere"
df$glscoma_grp   <- factor(df$glscoma_grp )
df$glscoma_grp   <- relevel(df$glscoma_grp, "Notapp" )

## age_group, to be determined
df$age_group <- factor(df$age_group, levels = c(levels(df$age_group), "102"))
df[which(df$age_group == "102-110"), "age_group"] <- "102"
df$age_group <- as.numeric(as.character(df$age_group))

#Grouped based on literature or other...
df[which(df$age_group %in% c(65:67)), "age_grp"] <- 1
df[which(df$age_group %in% c(68:73)), "age_grp"] <- 2
df[which(df$age_group %in% c(74:80)), "age_grp"] <- 3
df[which(df$age_group >= 81), "age_grp"] <- 4
df$age_grp <- factor(df$age_grp)
## 
df$rural[df$rural == ""]<- NA
df$rural <- factor(df$rural)
df$dependency_q_da <- as.factor(df$dependency_q_da)
df$deprivation_q_da <- as.factor(df$deprivation_q_da)
df$ethniccon_q_da <- as.factor(df$ethniccon_q_da)
df$instability_q_da <- as.factor(df$instability_q_da)
df$incquint <- as.factor(df$incquint)

##ambulance
levels(df$admambul)<- c(levels(df$admambul), "Y")
df[df$admambul %in% c("G", "C", "A"), "admambul"] <- "Y"
df$admambul %>% tabna()
df$admambul <- factor(df$admambul)

# saveRDS(df, "main_features_cln_oldR.rds")

###########Inspect
library(inspectdf)
df %>% inspect_na %>% print(n=30)
df %>% inspect_na %>% show_plot()

#####Correlation

library(corrplot)
dfx <- df %>% select(-alc_status, -ID, -days_to_lefteddate, -prvserv1)
#couldnt be done for factors aka categorical variables
corrplot(dfx, method="circle")



############Select and export
saveRDS(df, "main_features_cln.rds")
df_sel1 <- df %>% select(ID, days_to_lefteddate,
 age_grp, female_flag, rural, dependency_q_da:instability_q_da, incquint, alc_status)
 
 
#####TRAINNG MODEL
library(h2o)
h2o.init()

df_sel1 <- df_sel1 %>% drop_na()

df.hex <- as.h2o(df_sel1)

df.split <- h2o.splitFrame(df.hex, ratios = c(0.7), seed = 1234)
train <- df.split[[1]]
test <- df.split[[2]]

pred <- names(df_sel1)[3:10]
target <- c("alc_status")

df.hex_rf <- h2o.randomForest(
training_frame = train,
validation_frame = test,
x = pred,
y = target,
model_id = "rf_demog",
ntrees = 500,
# nfolds = 10,
# fold_assignment = "Stratified",
score_each_iteration = T)

h2o.performance(df.hex_rf)
h2o.performance(df.hex_rf, newdata = test)
h2o.saveModel(df.hex_rf, "./Models/", force=TRUE)


####Adding more features
df_sel <- df %>% select(ID, days_to_lefteddate,
 age_grp, female_flag, rural, dependency_q_da:instability_q_da, incquint,
 triage, btany, clidecunitflag, admambul,alc_status)
 
df_sel <- df_sel %>% drop_na()

df.hex <- as.h2o(df_sel)

df.split <- h2o.splitFrame(df.hex, ratios = c(0.7), seed = 1234)
train <- df.split[[1]]
test <- df.split[[2]]

pred <- names(df_sel)[3:14]
target <- c("alc_status")

df.hex_rf <- h2o.randomForest(
training_frame = train,
validation_frame = test,
x = pred,
y = target,
model_id = "rf_demog_triage",
ntrees = 500,
# nfolds = 10,
# fold_assignment = "Stratified",
score_each_iteration = T)

h2o.performance(df.hex_rf)
h2o.performance(df.hex_rf, newdata = test)

h2o.saveModel(df.hex_rf, "./Models/", force=TRUE)
#h2o.loadModel("P:\\2019 0970 151 000\\User Data\\Faraz-Export IDAVE folders\\Models\\rf_demog_triage")


