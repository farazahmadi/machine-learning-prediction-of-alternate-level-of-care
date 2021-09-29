setwd("P:/2019 0970 151 000/User Data/Faraz")
library(tidyverse)
#data loading and preprocessing
df <- read.csv("NACRASCIHI.csv")

#before demo vars
vars <- names(df)[2:130]
rem_days <- grep("days", names(df), value=TRUE)
rem_time <- grep("time", names(df), value=TRUE)

glm_vars <- vars[! (vars %in% c(rem_days, rem_time))]

str(df[glm_vars])

tabna <- function(x){table(x, useNA = "ifany")}

##
df$alc_status <- as.factor(df$alc_status)
##

################################################LOG ODDS Tutorial

df$female_flag <- as.factor(df$female_flag)

glm.1 <- glm(data = df, alc_status ~ female_flag, family = 'binomial')
summary(glm.1)

exp(glm.1$coef)
# (Intercept) female_flag1 
#   0.1133606    1.3207381 
tabna(df$alc_status, df$female_flag)
#xtabs(alc_status ~ female_flag, data = df)

library(gmodels)
CrossTable(df$alc_status, df$female_flag)

# Coefficients:
              # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.177181   0.003869 -562.70   <2e-16 ***
# female_flag1  0.278191   0.005104   54.51   <2e-16 ***

#intercept coeff is the log odds for the male group (is reference here)
#meaning the log[p(male being alc) / p(male not being alc)]
#looking at the contingency table this could be confirmed
# 
#the coeficient for the female_flag1 (meaning being female) is 
# the log of odds ratio between the female group and the male group

exp(glm.1$coef)
# (Intercept) female_flag1 
#   0.1133606    1.3207381 

# meaning the odds of being alc is going to be 1.3 times more if the 
# subject is female

##################################################################################
###################
#Triage=1 is the most severe

df$triage <- as.factor(df$triage)

# glm.1 <- glm(data = df, alc_status ~ triage, family = 'binomial')
# summary(glm.1)

# output <- summary(glm.1)
# z<-output$coefficients[,1]/output$coefficients[,2]
# pv<-(1-pnorm(abs(z),0,1))*2

# CI_lower<-exp(output$coefficients[,1]-1.96*output$coefficients[,2])
# CI_upper<-exp(output$coefficients[,1]+1.96*output$coefficients[,2])


## blood transfused
	df$btany[df$btany == ""] <- NA
	df$btany <- factor(df$btany)

	# glm.1 <- glm(data = df, alc_status ~ btany, family = 'binomial')
	# summary(glm.1)

#########Adding to the result###############

add_result <- function(var, or_ls){
	formula <- as.formula(paste("alc_status ~ ", var))
	glm.var <- glm(data = df, formula, family = 'binomial')
	output <- summary(glm.var)
	z<-output$coefficients[,1]/output$coefficients[,2]
	pv<-(1-pnorm(abs(z),0,1))*2
	CI_lower<-exp(output$coefficients[,1]-1.96*output$coefficients[,2])
	CI_upper<-exp(output$coefficients[,1]+1.96*output$coefficients[,2])
	#profiling method for finding confidence interval is more accurate but too slow
	#or_ls <- rbind(or_ls, cbind(OR = exp(coef(glm.var)), confint(glm.var), pv)) 
	or_ls <- rbind(or_ls, cbind(OR = exp(coef(glm.var)), CI_lower, CI_upper, pv)) 
	return(or_ls)
}

result <- double()

result <- add_result("female_flag", result)
result <- add_result("triage", result)
result <- add_result("clidecunitflag", result)
result <- add_result("btany", result)
###saved to 17-3 file


###CACS vars###
df$cacsanetech <- factor(df$cacsanetech, levels = c(8, 1, 2, 3, 4))
# result <- add_result("cacsanetech", result) ##SOLID

##CACS branch---don't know variable meaning!
df$cacsbranch <- as.factor(df$cacsbranch) #too many levels 43!
# glm.1 <- glm(data = df, alc_status ~ cacsbranch, family = 'binomial')
# summary(glm.1)


## CACS Code ---Looks very good! ---Not significant p value --->needs more grouping and modification

#The null values are marked as not applicable
levels(df$cacscode) <- c(levels(df$cacscode), "Notapp")
df$cacscode[df$cacscode == ""] <- "Notapp"
df$cacscode <- factor(df$cacscode)
df$cacscode <- relevel(df$cacscode, "Notapp")##changing order to chnage base of odds ratio in LR

# glm.1 <- glm(data = df, alc_status ~ cacscode, family = 'binomial')
# summary(glm.1)

##CACScode binary vers.

df[df$cacscode == "Notapp", "cacscode_bin"] <- 0 # No code
df[! df$cacscode == "Notapp", "cacscode_bin"] <- 1 #any code
df$cacscode_bin <- factor(df$cacscode_bin)
glm.1 <- glm(data = df, alc_status ~ cacscode_bin, family = 'binomial')
summary(glm.1)

###Grouping all smaller freq into Other category
tbl <- tabna(df$cacscode)
tbl[order(-tbl)]

df$cacscode_grp <- df$cacscode
levels(df$cacscode_grp) <- c(levels(df$cacscode_grp), "Others")
df$cacscode_grp[! df$cacscode %in% c("B001","B002","B003","B004","B005", "Notapp")] <- "Others"
df$cacscode_grp <- factor(df$cacscode_grp)

glm.1 <- glm(data = df, alc_status ~ cacscode_grp, family = 'binomial')
summary(glm.1)

##Much better p values! in the modified cacscode!


# cacsintervention --> 99.9 NULL values ---> not used
# cacsit1 to 8
tabna(df$cacsit1)

#####################OLD VERSION OF DPLYR!!!!!! :////
# df <- within(df, {
	# cacsit1_fac <- dplyr::recode(cacsit1, `1` = "cat_scan",
	# `2` = "cat_scan",
	# `3` = "cat_scan",
	# `4` = "cat_scan",
	# `5` = "cat_scan",
	# `6` = "EEG",
	# `7` = "xray",
	# `20` = "xray",
	# `21` = "xray",
	# `8` = "Mammography",
	# `9` = "MRI",
	# `10` = "MRI",
	# `11` = "MRI",
	# `12` = "Nuclear",
	# `13` = "Nuclear",
	# `14` = "Nuclear",
	# `15` = "Nuclear",
	# `16` = "stress_test",
	# `17` = "ultrasound", 
	# `18` = "ultrasound",
	# `19` = "ultrasound"
	# )
# })

df$cacsit1_grp[df$cacsit1 %in% c(1:5)] <- "Cat_scan"
df$cacsit1_grp[df$cacsit1 == 6] <- "EEG"
df$cacsit1_grp[df$cacsit1 %in% c(7,20,21)] <- "Xray"
df$cacsit1_grp[df$cacsit1 == 8] <- "Mammography"
df$cacsit1_grp[df$cacsit1 %in% c(9:11)] <- "MRI"
df$cacsit1_grp[df$cacsit1 %in% c(12:15)] <- "Nuclear"
df$cacsit1_grp[df$cacsit1 == 16] <- "Stress_test"
df$cacsit1_grp[df$cacsit1 %in% c(17:19)] <- "Ultrasound"





###Turn NA to not applicable
actual_NA <- df[which(!df$cacsitcnt == 0 & is.na(df$cacsit1_grp)), c("cacsit1_grp", "cacsitcnt")]
nrow(actual_NA) #237 actual NAs with cacsitcnt of non zero

df$cacsit1_grp[df$cacsitcnt == 0] <- "Notapp"
#Grouping some of the least populated categ.
df$cacsit1_grp[df$cacsit1_grp %in% c("EEG",
									 "Mammography",
									 "Stress_test",
									 "Nuclear")] <- "Others"
#factor it here!
df$cacsit1_grp <- factor(df$cacsit1_grp)
df$cacsit1_grp <- relevel(df$cacsit1_grp, "Notapp")
tabna(df$cacsit1_grp)

glm.1 <- glm(data = df, alc_status ~ cacsit1_grp, family = 'binomial')
summary(glm.1)

result <- add_result("cacsit1_grp", result)

#CACS total count
tabna(df$cacsitcnt)
glm.1 <- glm(data = df, alc_status ~ cacsitcnt, family = 'binomial')
summary(glm.1)
#Shows increase in odds ratio as number goes higher




#CACS Category count 1 to 7 [values: 1,2 and Na]

df$cacsitcnt1[df$cacsit1_grp == "Notapp"] <- "Notapp"
df$cacsitcnt1 <- factor(df$cacsitcnt1)
df$cacsitcnt1 <- relevel(df$cacsitcnt1, "Notapp")
levels(df$cacsitcnt1) <- c( "Notapp", "Just1","2orMore" )
glm.1 <- glm(data = df, alc_status ~ cacsitcnt1, family = 'binomial')
summary(glm.1)

### CACS logic type
tabna(df$cacslogictype)
df$cacslogictype <- as.factor(df$cacslogictype)
# glm.1 <- glm(data = df, alc_status ~ cacslogictype, family = 'binomial')
# summary(glm.1)

### CACS Partition Type
tabna(df$cacspartition)
df$cacspartition[df$cacspartition == ""] <- NA
df$cacspartition <- factor(df$cacspartition, levels = c("A", "D", "I"))
glm.1 <- glm(data = df, alc_status ~ cacspartition, family = 'binomial')
summary(glm.1)

#############END of CACS vars

##Adding odds ratio and confint and p value to results
result <- add_result("cacsanetech", result) ##SOLID

result <- add_result("cacscode", result) ##Too many levels but for keeping in records
result <- add_result("cacscode_grp", result) ##SOLID
result <- add_result("cacscode_bin", result) ##SOLID

result <- add_result("cacsitcnt", result) ##SOLID
result <- add_result("cacsitcnt1", result) ## Questionable but seems SOLID ---
 # other numbers could be picked maybe?
result <- add_result("cacslogictype", result) 
result <- add_result("cacspartition", result) ##SOLID
result
##These CACS vars were not added!
# result <- add_result("cacsbranch", result) ##CI not converge
saveRDS(result, "./univar_result_CACS.rds")


################

## Glascow Coma Scale ##

df$glscoma[df$glscoma %in% c(3:8)] <- "Severe"
df$glscoma[df$glscoma %in% c(9:13)] <- "Moderate"
df$glscoma[df$glscoma %in% c(14,15)] <- "Mild"
df$glscoma[df$glscoma %in% c(99)] <- NA
df$glscoma[df$glscoma %>% is.na()] <- "Notapp"
df$glscoma <- as.factor(df$glscoma)
tabna(df$glscoma)
df$glscoma <- relevel(df$glscoma, "Notapp")##changing order to chnage base of odds ratio in LR

# glm.1 <- glm(data = df, alc_status ~ glscoma, family = 'binomial')
# summary(glm.1)
df$glscoma_bin <- as.character(df$glscoma)
df$glscoma_bin[! df$glscoma == "Notapp" ] <- "Y"
tabna(df$glscoma_bin)
df$glscoma_bin   <- factor(df$glscoma_bin )
# glm.1 <- glm(data = df, alc_status ~ glscoma_bin, family = 'binomial')
# summary(glm.1)

df$glscoma_grp <- as.character(df$glscoma)
df$glscoma_grp[ df$glscoma %in% c("Moderate", "Mild") ] <- "LessSevere"
df$glscoma_grp   <- factor(df$glscoma_grp )
df$glscoma_grp   <- relevel(df$glscoma_grp, "Notapp" )

## Patient complaint1 ##



tabna(df$complaint1) #173 levels

##Trying to look more into the 40% missing obs
df[which(is.na(df$days_to_ambarrdate)),] %>% select(complaint1) %>% is.na() %>% sum()
#They are not just patients who arrived by the ambulance

#Checking NA with year in data
comp.na <- df[which(is.na(df$complaint1)),] %>% select(complaint1, indexfyear) 

tabna(comp.na$indexfyear) ##Number of missing obs of compalint per year
tabna(comp.na$indexfyear) / tabna(df$indexfyear) * 100 ##percentage

#intresting find is that from 2014 to 2016 this variable has around 5 percent missing
#as the average for other years was around 40% !!!!
# But sizes are much smaller in these last thress years

#Check complaint1 and eddischdx1 (discharge diagnosis) 
df[c("complaint1", "eddischdx1")] %>% head()
#No way is discharge as it is 99 percent null itself
 

####This has a odds ratio reference problem but let's keep it for now

tbl <- tabna(df$complaint1)
tbl[order(-as.numeric(tbl))] #NA not shown
#CrossTable(df$complaint1, df$alc_status) #hard to get information this way!
###Lets group by highest freq categories

selected_cat <- names(tbl[order(-as.numeric(tbl))])[1:22]
# [1] NA    "651" "7"   "251" "3"   "999" "557" "409" "852" "401" "555" "257" "402" "8"  
# [15] "855" "260" "551" "5"   "4"   "653" "403" "407"
df$complaint1_grp <- df$complaint1
df$complaint1_grp[! df$complaint1_grp %in% selected_cat] <- "Others"
df$complaint1_grp <- factor(df$complaint1_grp)
#Now building the LR model
glm.1 <- glm(data = df, alc_status ~ complaint1_grp, family = 'binomial')
summary(glm.1)


# Some categories have insanely high odds ratio, lets take a closer look!
df$complaint1_555



## Consultserv1 ## ####This has a odds ratio reference problem but let's keep it for now

str(df$consultserv1) #not Factor
tabna(df$consultserv1)

tbl <- tabna(df$consultserv1)
tbl[order(-as.numeric(tbl))]
selected_cat <- names(tbl[order(-as.numeric(tbl))])[1:15]
# NA   "10" "30" "1"  "34" "12" "39" "17" "15" "18" "32" "55" "74" "16" "37"
df$consultserv_grp <- df$consultserv1
df$consultserv_grp[! df$consultserv_grp %in% selected_cat] <- "Others"
tabna(df$consultserv_grp)
#Now building the LR model
glm.1 <- glm(data = df, alc_status ~ consultserv_grp, family = 'binomial')
summary(glm.1)



##Percentage of missing in each year
consult.na <- df[which(is.na(df$consultserv1)),] %>% select(consultserv1, indexfyear) 

tabna(consult.na$indexfyear) ##Number of missing obs of compalint per year
tabna(consult.na$indexfyear) / tabna(df$indexfyear) * 100 ##percentage



#####Add to results
result <- add_result("glscoma", result) 
result <- add_result("glscoma_bin", result) 
result <- add_result("glscoma_grp", result) 

result <- add_result("complaint1_grp", result) 
result <- add_result("consultserv_grp", result) 

saveRDS(result, "./univar_result_consult.rds")

####Next are Mostly Cihi vars

cvar <- c("admcat", "age_group","dependency_q_da", "deprivation_q_da",
"dischdisp", "entry", "ethniccon_q_da", "female_flag", "incquint", "indexfyear",
"instability_q_da", "instftyp", "rural")

sapply(df[cvar], tabna)
str(df[cvar])


#####Demographic in cvar

##AGE --used as integer
	df$age_group <- factor(df$age_group, levels = c(levels(df$age_group), "102"))
	df[which(df$age_group == "102-110"), "age_group"] <- "102"
	df$age_group <- as.numeric(as.character(df$age_group))
	tabna(df$age_group)
	#Now building the LR model
	glm.1 <- glm(data = df, alc_status ~ age_group, family = 'binomial')
	summary(glm.1)
##Female Flag---> was used at top of file

##Rural
	tabna(df$rural)
	df$rural[df$rural == ""]<- NA
	df$rural <- factor(df$rural)
	#Now building the LR model
	glm.1 <- glm(data = df, alc_status ~ rural, family = 'binomial')
	summary(glm.1)

## "dependency_q_da"
	df$dependency_q_da <- as.factor(df$dependency_q_da)
	glm.1 <- glm(data = df, alc_status ~ dependency_q_da, family = 'binomial')
	summary(glm.1)
	
## deprivation_q_da
	df$deprivation_q_da <- as.factor(df$deprivation_q_da)
	glm.1 <- glm(data = df, alc_status ~ deprivation_q_da, family = 'binomial')
	summary(glm.1)
	
## "ethniccon_q_da"
	df$ethniccon_q_da <- as.factor(df$ethniccon_q_da)
	glm.1 <- glm(data = df, alc_status ~ ethniccon_q_da, family = 'binomial')
	summary(glm.1)
	
## instability_q_da
	df$instability_q_da <- as.factor(df$instability_q_da)
	glm.1 <- glm(data = df, alc_status ~ instability_q_da, family = 'binomial')
	summary(glm.1)
	
## Income quantile
	df$incquint <- as.factor(df$incquint)
	glm.1 <- glm(data = df, alc_status ~ incquint, family = 'binomial')
	summary(glm.1)

#####Add to results
result <- add_result("age_group", result) 
result <- add_result("rural", result) 
result <- add_result("dependency_q_da", result) 
result <- add_result("deprivation_q_da", result) 
result <- add_result("ethniccon_q_da", result) 
result <- add_result("instability_q_da", result) 
result <- add_result("incquint", result) 
saveRDS(result, "./univar_result_demo.rds")
write.csv(result, "./univar_result_final.csv")


