
######################
############FRAILTY INDEX CALCULATION

setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders")
library(tidyverse)
library(icd)
df <- read.csv("./datasets/New/NACRASCIHI_2_dupRemoved.csv")

tabna <- function(x){table(x, useNA = "ifany")}


ic <- df %>% select(ID, days_to_lefteddate, dx10code1:dx10code10, alc_status)%>%
mutate(IDm = paste(ID, days_to_lefteddate, sep="_"))

codes <- read.csv("./ICD10 codes/frailty_score2.csv")

# codes <- c("F00", "G81","G30", "I69", "R29", "N39", "F05", "W19","S00",
# "R31", "B96", "R41", "R26", "I67", "R56", "R40", "T83", "S06",
# "S42", "E87", "M25", "E86", "R54", "Z50", "F03", "W18", "Z75", "F01",
# "S80", "L03", "H54", "E53", "Z60", "G20", "R55", "S22", "K59", "N17",
# "L89", "Z22", "B95", "L97", "R44", "K26", "I95", "N19", "A41", "Z87", 
# "J96", "X59", "", "", "", "", "", "", "", "",
# "", "", "", "", "", "", "", "", "", "")

# points <- c(7.1, 4.4, 4, 3.7, 3.6, 3.2, 3.2, 3.2, 3.2, 
# 3.0, 2.9, 2.7, 2.6, 2.6, 2.6, 2.5, 2.4, 2.4,
# 2.3,2.3,2.3,2.3, 2.2, 2.1, 2.1, 2.1, 2.0, 2.0,
# 2.0,2.0,1.9,1.9,1.8,1.8,1.8,1.8,1.8,1.8,
# rep(1.7,3), rep(1.6, 6), 1.5,
# rep(1.5, 4), rep(1.4, 6),

# ) 

for (i in 1:nrow(codes)){
	id_ls <- ic %>% filter_at(vars(dx10code1:dx10code10),
	 any_vars(substr(.,1,3) == codes$ICD.Code[i])) %>%
	select(IDm , dx10code1:dx10code10) %>% .$IDm 
	
	var_name <- paste0("frail_",as.character(codes$ICD.Code[i])) #dynamic name in mutate
	
	ic <- ic %>% mutate(!!var_name := ifelse(IDm %in% id_ls, 1, 0))
}


# for (i in 1:nrow(codes)){
	# var_name <- paste0("frail_",as.character(codes$ICD.Code[i])) #dynamic name in mutate

	# ic2 <- ic %>%
	# mutate_at(vars(dx10code1:dx10coSde10), ~substr(.,1,3)) %>%
	# mutate(!!var_name := ifelse(any(vars(dx10code1:dx10code10) %in% codes$ICD.Code[i]), 1, 0))
	
# }
##Occurences are low usually
ic %>% select(frail_F00:frail_R50) %>% colSums / nrow(ic) * 100 %>% round(digits=2)

mat <- as.matrix(ic %>% select(frail_F00:frail_R50))

scores <- mat %*% codes$coef 
####Check Safety
scores[1:5]
ic[1,] %>% select(frail_F00:frail_R50) %>% select_if(.==1)
############
ic$frailty_score <- scores
summary(ic$frailty_score)

ic %>% select(IDm, frailty_score) %>% write.csv("./ICD10 codes/frailty_index.csv", row.names = F)