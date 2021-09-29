################################################
#############ICD10 Codes########################
setwd("P:/2019 0970 151 000/User Data/Faraz")
library(tidyverse)
#data loading and preprocessing
df <- read.csv("NACRASCIHI.csv")
tabna <- function(x){table(x, useNA = "ifany")}
##
df$alc_status <- as.factor(df$alc_status)
##

code_vars <- names(df)[89:118]
str(df[code_vars])

## NA AND Null analysis
sapply(code_vars, function(x){
df[x] %>% is.na() %>% sum() %>% length() 
})
# Mostly Null
sapply(code_vars, function(x){
df[df[x] == "",x]  %>% length() / nrow(df) * 100 %>% round(digits=2)
})


icd <- df["dx10code1"]
icd$alc <- as.numeric(df$alc_status) - 1
icd$c1 <- substr(icd$dx10code1, 1,1)

icd1 <- icd %>% group_by(c1) %>% summarise(n = n(),
 perc = round(n/nrow(df) * 100,digits=1), alc_perc_grp = round(mean(alc) * 100, digits=1) ) 
 
icd1 %>% arrange(desc(n)) %>% print(n=40)

##Category R is mostly symptoms---no particular summary
icd$complaint <- df$complaint1
icd %>% group_by(c1) %>% summarise( n= n(),
perc = round(n/nrow(df) * 100,digits=2),
alc_perc_grp = mean(alc) * 100,
no_complaint = sum(is.na(complaint)))%>% 
mutate(no_complaint_perc_of_NA = round(no_complaint/ sum(no_complaint) * 100, digits=2)) %>%
arrange(desc(n)) %>% print(n=25)

 
#############Create binary variables#####

##one letter code with descending freq order
code_1 <- icd1 %>% arrange(desc(n))  %>% .$c1 	

for(code in code_1[1:16]){
	icd[,code] <- factor(ifelse(icd$c1 == code, 1, 0))
}
##merging the least 5 togethers
icd$Oth <- factor(ifelse(icd$c1 %in% code_1[17:21], 1, 0))

############ Logistic Regression on binary vars

icd$alc <- factor(icd$alc)
vars <- names(icd)[5:21]
lapply(vars, function(var){
	formula <- as.formula(paste("alc ~ ", var))
	glm.var <- glm(data = icd, formula, family = 'binomial')
	summary(glm.var)
})

##all significant
###
add_result <- function(var, or_ls){
	formula <- as.formula(paste("alc ~ ", var))
	glm.var <- glm(data = icd, formula, family = 'binomial')
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
for (x in vars){
	result <- add_result(x, result)
}	
write.csv(result, "../Faraz-Export IDAVE folders/icd10_1L_univar_results.csv")


############# S,F and Z had biggest odds ratios


###Exploring more#######
icd$c3 <- substr(icd$dx10code1, 1,3)
#if categorized by 3 letters, have total groups of 1151
unique(icd$c3) %>% length()
icd$alc <- factor(icd$alc)

icd$alc_n <- as.numeric(icd$alc) - 1
icd %>% group_by(c3) %>% summarise( n= n(),
perc = round(n/nrow(df) * 100,digits=2),
alc_perc_grp = mean(alc_n) * 100) %>%
arrange(n) %>% print(n=1000)

#About 1000 of them have population less than 1000 which is too little

icd %>% group_by(c3) %>% summarise( n= n(),
perc = round(n/nrow(df) * 100,digits=2),
alc_perc_grp = mean(alc_n) * 100) %>%
arrange(desc(alc_perc_grp)) %>% print(n=25)

# have to filer by size

icd3_top <- icd %>% group_by(c3) %>% summarise( n= n(),
perc = round(n/nrow(df) * 100,digits=2),
alc_perc_grp = mean(alc_n) * 100) %>%
arrange(desc(alc_perc_grp)) %>% filter(n > 1000) %>% print(n=200)

icd3_top$alc_perc_grp %>% mean() #12%

code_3 <- icd3_top %>% filter(alc_perc_grp > mean(alc_perc_grp)) %>% .$c3

##apply logistic regression only on the above average codes (about 50 are here)

for(code in code_3){
	icd[,code] <- factor(ifelse(icd$c3 == code, "Y", "N"))
}

result_3 <- double()
for (x in code_3){
	result_3 <- add_result(x, result_3)
}	

write.csv(result_3, "../Faraz-Export IDAVE folders/icd10_3L_univar_results.csv")

######################################

#Diabetes# CODE starts with E 
# have to filer by size

icd3_top <- icd %>% group_by(c3) %>% summarise( n= n(),
perc = round(n/nrow(df) * 100,digits=2),
alc_perc_grp = mean(alc_n) * 100) %>%
arrange(desc(n)) %>% filter(str_detect(c3, "E")) %>% print(n=200)

# E11 being related to diabetes, had less alc_perc_grp than the mean,
# Therefore shouldn't have a high odds ratio!



##################Charlson Index

