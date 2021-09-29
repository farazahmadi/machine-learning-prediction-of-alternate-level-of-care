setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders")
library(tidyverse)

df <- read.csv("./datasets/New/NACRASCIHI_2_dupRemoved.csv")

tabna <- function(x){table(x, useNA = "ifany")}

####cleaned version -- earlier version of dfx used in model training
df_cln <- readRDS("main_features_cln.rds")
icd <- read.csv("./ICD10 codes/icd10_vars.csv")

df_cln <- df_cln %>% mutate(IDm = paste(ID, days_to_lefteddate, sep="_"))
df_cln <- left_join(df_cln, icd)

######################
    # "days_to_arrivaldate"       "days_to_ambarrdate"       
  # [4] "days_to_ambtransdate"      "arrival_time"              "ambarr_time"              
  # [7] "ambtrans_time"             "triage"                    "days_to_triagedate"       
 # [10] "triage_time"               "days_to_regdate"           "reg_time"                 
 # [13] "days_to_assessdate"        "assess_time"               "days_to_npassessdate"     
 # [16] "npassess_time"             "days_to_indate1"           "days_to_indate2"          
 # [19] "days_to_indate3"           "days_to_indate4"           "days_to_indate5"          
 # [22] "days_to_indate6"           "days_to_indate7"           "days_to_indate8"          
 # [25] "days_to_indate9"           "days_to_indate10"          "instart_time1"            
 # [28] "instart_time2"             "instart_time3"             "instart_time4"            
 # [31] "instart_time5"             "instart_time6"             "instart_time7"            
 # [34] "instart_time8"             "instart_time9"             "instart_time10"           
 # [37] "days_to_consultarrdate1"   "days_to_consultarrdate2"   "days_to_consultarrdate3"  
 # [40] "consultarr_time1"          "consultarr_time2"          "consultarr_time3"         
 # [43] "days_to_consultdate1"      "days_to_consultdate2"      "days_to_consultdate3"     
 # [46] "consult_time1"             "consult_time2"             "consult_time3"            
 # [49] "clidecunitflag"            "days_to_clidecunitindate"  "days_to_clidecunitoutdate"
 # [52] "clidecunitin_time"         "clidecunitout_time"        "days_to_referraldate"     
 # [55] "days_to_lefteddate"        "lefted_time"               "days_to_ddate"            
 # [58] "d_time"                    "aminst_enc"                "prvnum1_enc"              
 # [61] "prvnum2_enc"               "prvnum3_enc"               "prvnum4_enc"              
 # [64] "prvnum5_enc"               "days_to_bdate"             "btany"                    
 # [67] "cacsanetech"               "cacsbranch"                "cacscode"                 
 # [70] "cacsintervention"          "cacsit1"                   "cacsit2"                  
 # [73] "cacsit3"                   "cacsit4"                   "cacsit5"                  
 # [76] "cacsit6"                   "cacsit7"                   "cacsit8"                  
 # [79] "cacsitcnt"                 "cacsitcnt1"                "cacsitcnt2"               
 # [82] "cacsitcnt3"                "cacsitcnt4"                "cacsitcnt5"               
 # [85] "cacsitcnt6"                "cacsitcnt7"                "cacslogictype"            
 # [88] "cacspartition"             "incode1"                   "incode2"                  
 # [91] "incode3"                   "incode4"                   "incode5"                  
 # [94] "incode6"                   "incode7"                   "incode8"                  
 # [97] "incode9"                   "incode10"                  "inatloc1"                 
# [100] "inatloc2"                  "inatloc3"                  "inatloc4"                 
# [103] "inatloc5"                  "inatloc6"                  "inatloc7"                 
# [106] "inatloc8"                  "inatloc9"                  "inatloc10"                
# [109] "dx10code1"                 "dx10code2"                 "dx10code3"                
# [112] "dx10code4"                 "dx10code5"                 "dx10code6"                
# [115] "dx10code7"                 "dx10code8"                 "dx10code9"                
# [118] "dx10code10"                "eddischdx1"                "eddischdx2"               
# [121] "eddischdx3"                "glscoma"                   "complaint1"               
# [124] "complaint2"                "complaint3"                "consultserv1"             
# [127] "consultserv2"              "consultserv3"              "admambul"                 
# [130] "prvserv1"                  "prvserv2"                  "prvserv3"                 
# [133] "prvserv4"                  "prvserv5"                  "prvserv6"                 
# [136] "prvserv7"                  "prvserv8"                  "prvserv9"                 
# [139] "prvserv10"                 "from_type"                 "to_type"                  
# [142] "incquint"                  "rural"                     "instability_q_da"         
# [145] "deprivation_q_da"          "dependency_q_da"           "ethniccon_q_da"           
# [148] "ch1"                       "ch2"                       "ch3"                      
# [151] "ch4"                       "ch5"                       "ch6"                      
# [154] "ch7"                       "ch8"                       "ch9"                      
# [157] "ch10"                      "ch11"                      "ch12"                     
# [160] "ch13"                      "ch14"                      "ch15"                     
# [163] "ch16"                      "ch17"                      "charl"                    
# [166] "indexfyear"                "age_group"                 "female_flag"              
# [169] "acutelos"                  "admcat"                    "alclos"                   
# [172] "dischdisp"                 "entry"                     "instftyp"                 
# [175] "los"                       "scuhrs"                    "flag_chemother"           
# [178] "flag_dialysis"             "flag_feeding_tb"           "flag_heart_resu"          
# [181] "flag_mvent_ge96"           "flag_mvent_lt96"           "flag_pa_nutrit"           
# [184] "flag_paracent"             "flag_pleurocent"           "flag_radiother"           
# [187] "flag_tracheost"            "flag_vasc_accdv"           "flag_biopsy"              
# [190] "flag_endo"                 "alc_status"                "n_comorbidities"          
# [193] "prior_hosp_90d"            "prior_hosp_365d"           "readm_90d"     

# df %>% group_by(ID, days_to_arrivaldata)

vars <- names(df)[2:141]
rem_days <- grep("days", names(df), value=TRUE)
rem_time <- grep("time", names(df), value=TRUE)

time_vars <- vars[ (vars %in% c(rem_days, rem_time))]

dft <- df %>% select(ID, admambul, all_of(time_vars), alc_status)

# time(hour) vars are factors, need to change them to sth else

library(inspectdf)
inspect_na(dft) %>% show_plot()

dft %>% is.na() %>% colSums()

dft %>% sapply(function(x){ x == ""}) %>% colSums()

##hour,minute vars have NULL ("") values
## days vars have NA
dft %>% select(days_to_ambarrdate) %>% filter(is.na(.)) %>% nrow()
dft %>% select(days_to_ambarrdate) %>% filter(. == "")

dft <- dft %>% mutate_all(~na_if(.,"")) ##Use this instead of ~ifelse

png("./time_vars_missingPrev.png", width = 3000, height = 1360, res = 300)
inspect_na(dft) %>% show_plot()
dev.off()

dft %>% is.na() %>% colSums() %>% .[order(.)]
###
##np asses and normal asses
dft %>% select(assess_time, npassess_time) %>%
 filter(is.na(assess_time) & !is.na(npassess_time)) %>% nrow
#---> could be filled with about 10%


ts <- dft %>% select(days_to_triagedate, days_to_assessdate,
triage_time, assess_time, npassess_time) 
ts %>% head()

ts <- ts %>% mutate_at(vars(assess_time, triage_time), ~as.POSIXlt(.,format = "%H:%M"))
head(ts)
#the exact data is not important but the difference between times is.

ts <- ts %>% mutate(
 assess_time = assess_time + (days_to_assessdate - days_to_triagedate)*24*60*60)

ts <- ts %>% rowwise()  %>%
 mutate(triage_to_assess =
 difftime(assess_time, triage_time, units = "hours") %>% unclass %>% .[[1]])
 
ts$alc <- dft$alc_status
ts %>% ggplot(aes(x = triage_to_assess, fill = factor(alc))) +
 geom_histogram(binwidth= 0.5) + xlim(c(-5,15))
 
#not much info from graph, grouping time to quantiles

time_cut <- quantile(ts$triage_to_assess, seq(0,1,0.1), na.rm = TRUE)

ts_final <- ts %>% select(triage_to_assess, alc)
ts_final %>%
 mutate(timeCat = 
 cut(triage_to_assess, breaks = time_cut, labels = 	paste0("quant", c(1:10)) ))%>%
 group_by(timeCat) %>%
 summarise(quant_count = n(),
 ALC_perc = round(mean(alc) * 100, digits = 2)) 
 
 # Looks GOOOOOOD!----triage to assess calculated with ~5% MISSING, less than 1% NEGATIVE
 
####################################Now lets create more wait time vars

#arrival to triage
#There is two kind of arrival: by foot and by ambulance
levels(dft$admambul)<- c(levels(dft$admambul), "Y")
dft[dft$admambul %in% c("G", "C", "A"), "admambul"] <- "Y"
dft$admambul %>% tabna()
dft$admambul <- factor(dft$admambul)

arr <- dft %>% select(admambul, days_to_ambarrdate,ambarr_time, days_to_arrivaldate, arrival_time)
arr %>% is.na() %>% colSums()

####4 scenarios
# 1- Y, ambul time , NA arr time
# 2- Y, ambul time, arr time (with few more mintues)
# 3- N, NA ambul time, arr time
# 4- -, NA, NA

#how many all nas? --- 23% missing
arr %>% filter(is.na(ambarr_time) & is.na(arrival_time)) %>% nrow() / nrow(arr) * 100



################
#register to triage maybe?
na_count <- round(dft %>% is.na %>% colSums() / nrow(dft) * 100, digits = 2) %>% as.data.frame
write.csv(na_count,"./Feature Engineering/time_vars_naCount.csv")
reg <- dft %>%
 select(days_to_triagedate,triage_time, days_to_regdate, reg_time)
 
#register does not have much meaning as it is close to triage
#############
#calculate time from inital asssessment to left_ed_data
ts <- dft %>% select(ID, days_to_triagedate, days_to_assessdate, days_to_lefteddate,
triage_time, assess_time, lefted_time, alc_status) 
ts %>% head()

ts <- ts %>% mutate_at(vars(assess_time, triage_time), ~as.POSIXlt(.,format = "%H:%M"))
head(ts)
#the exact data is not important but the difference between times is.

ts <- ts %>% mutate(
 assess_time = assess_time + (days_to_triagedate - days_to_triagedate)*24*60*60)

ts <- ts %>% rowwise()  %>%
 mutate(trg_to_ass =
 difftime(assess_time, triage_time, units = "hours") %>% unclass %>% .[[1]])
 
ts1 <- ts %>% select(ID, days_to_lefteddate, trg_to_ass)%>% as.data.frame
#part2 
ts <- dft %>% select(ID, days_to_triagedate, days_to_assessdate, days_to_lefteddate,
triage_time, assess_time, lefted_time, alc_status) 
ts %>% head()

ts <- ts %>% mutate_at(vars(assess_time, lefted_time), ~as.POSIXlt(.,format = "%H:%M"))
head(ts)
#the exact data is not important but the difference between times is.

ts <- ts %>% mutate(
 lefted_time = lefted_time + (days_to_lefteddate - days_to_assessdate)*24*60*60)

ts <- ts %>% rowwise()  %>%
 mutate(ass_to_lef =
 difftime(lefted_time, assess_time, units = "hours") %>% unclass %>% .[[1]])
 
ts2 <- ts %>% select(ID, days_to_lefteddate, ass_to_lef) %>% as.data.frame


time_features <- inner_join(ts1,ts2, by = c("ID","days_to_lefteddate" ))
write.csv(time_features, "./Feature Engineering/time_features.csv", row.names=F)
####Negative values in triage_to_assess
ts_neg <- ts %>% filter(!is.na(trg_to_ass) & trg_to_ass < 0) %>% as.data.frame() %>%
inner_join(dft %>% select(ID, days_to_lefteddate, days_to_regdate, reg_time),
by = c("ID","days_to_lefteddate" )) %>% 
inner_join(df%>% select(ID, days_to_lefteddate, triage),
by = c("ID","days_to_lefteddate" ))

ts_neg %>% arrange(trg_to_ass) %>% write.csv("./res.csv", row.names = F)
################################################################################
###############REVISITS to ED###################################################

rev <- df %>% select(ID, days_to_lefteddate, days_to_triagedate, triage_time, triage, alc_status)

rev %>% arrange(ID, days_to_triagedate)
#count revisits
rev %>%
 group_by(ID) %>%
 summarise(visit_counts = n()) %>%
 ggplot(aes(visit_counts)) + geom_histogram(bins = 20) + xlim(0,30)
#########

rev <- rev %>% arrange(ID, days_to_triagedate)
#poeple with only one visit
once <- rev %>% group_by(ID) %>% tally() %>%
	filter(n == 1) %>% .$ID
	
rev$revisit <- ifelse(rev$ID %in% once, "No", NA)
#A very bad way to do this, can't think of anythin else right now!
for (i in c(1:nrow(rev))){
	if(is.na(rev[i, "revisit"])){
		if(! rev[i-1,"ID"] == rev[i,"ID"]){
			rev[i,"revisit"] <- "No"
		}
		else{
			rev[i,"revisit"] <- ifelse(
			rev[i,"days_to_triagedate"] - rev[i-1,"days_to_lefteddate"] < 31, "Yes", "No")
		
		}
	}
}
tabna(rev$revisit)
# some missing, find distance to reg_day , should have done this from the start
rev <- inner_join(rev, dft %>% select(ID,days_to_lefteddate, days_to_regdate),
by = c("ID","days_to_lefteddate" ))

for (i in which(is.na(rev$revisit))){
	if(! rev[i-1,"ID"] == rev[i,"ID"]){
			rev[i,"revisit"] <- "No"
		}
		else{
			rev[i,"revisit"] <- ifelse(
			rev[i,"days_to_regdate"] - rev[i-1,"days_to_lefteddate"] < 31, "Yes", "No")
		
		}
}

tabna(rev$revisit)

rev %>%
group_by(revisit) %>%
 summarise(count = n(),
 ALC_perc = round(mean(alc_status) * 100, digits = 2)) 
 
 
#save to file

saved <- rev %>% select(ID, days_to_lefteddate, revisit)
names(saved)[3] <- "30day_ed_revisit"
saved %>% write.csv("./Feature Engineering/30dayRevisit.csv")



#############
# Provider Type
#############
df %>% select(prvserv1:prvserv10) %>% is.na %>% colSums / nrow(df) * 100

prv <- df %>% select(ID, days_to_lefteddate, prvserv1, alc_status)
tab <- tabna(prv$prvserv1)
tab[-order(tab)]

saved <- prv %>%
group_by(prvserv1) %>%
 summarise(count = n(),
 count_perc = round(n() / nrow(prv)* 100, digits=2) ,
 ALC_perc = round(mean(alc_status) * 100, digits = 2),
 ALC_count = sum(alc_status)) %>%
 filter(ALC_count > 6)%>%
 arrange(desc(count)) 
 
 
 
saved %>% write.csv("./Feature Engineering/prvtype1_report.csv")

prv %>%
group_by(prvserv1) %>%
 summarise(count = n(),
 count_perc = round(n() / nrow(prv)* 100, digits=2) ,
 ALC_perc = round(mean(alc_status) * 100, digits = 2),
 ALC_count = sum(alc_status)) %>%
 filter(ALC_count > 6)%>%
arrange(desc(ALC_perc))  %>% print(n=10)

#select two most populatted groups +oth based on manaf
# selecto orthopedics surgeon as I believe it is useful! with second largest alc concentration and 5th population overall
prv <- prv %>%
mutate(prv_grp = ifelse(prvserv1 %in% c(3,1, NA), prvserv1, "oth")) %>%
mutate(prv_orthop = ifelse(prvserv1 %in% c(34), "Yes", "No")) #NAs are 205 that are being held in others category for now

prv %>% select(ID, days_to_lefteddate, prv_grp, prv_orthop) %>% 
write.csv("./Feature Engineering/prvtype_vars.csv", row.names = F)
######################### Group by hospital!
tab <- tabna(df$aminst_enc)
tab[order(-tab)]

#missing for consult
sum(is.na(df$consultserv1)) / nrow(df) * 100#33%
sum(is.na(df$complaint1)) / nrow(df) * 100 #39%

df %>%
group_by(aminst_enc)%>%
summarise(count = n(),
count_perc = round(n() / nrow(df)* 100, digits=2),
 ALC_perc = round(mean(alc_status) * 100, digits = 2),
 comp_miss = round(sum(is.na(complaint1))/ n() * 100, digits=2),
 consult_miss = round(sum(is.na(consultserv1))/ n() * 100, digits=2))%>%
arrange(desc(count))%>%
print(n=50)


########################## Main procedure
intv <- df %>% select(ID, days_to_lefteddate, incode1:incode10, alc_status) 
intv %>%is.na() %>%  colSums() / nrow(df) * 100

intv <- intv %>% mutate_at(vars(incode1:incode10), ~na_if(.,""))
intv %>%is.na() %>%  colSums() / nrow(df) * 100

intv %>%
group_by(incode1) %>%
 summarise(count = n(),
 count_perc = round(n() / nrow(prv)* 100, digits=2) ,
 ALC_perc = round(mean(alc_status) * 100, digits = 2)) %>%
 arrange(desc(count)) %>%
 write.csv("./Feature Engineering/incode1_report.csv")