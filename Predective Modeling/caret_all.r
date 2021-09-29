setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/Models2.5")
library(tidyverse)
library(caret)
library(pROC)
library(PRROC)
library(Metrics)
dfx <- readRDS("dfx_cln_oldR.rds")# no NA in this file
# dfx %>% is.na() %>% colSums() %>% sum == 0
# [1] TRUE

####Specifying top features and also, ED or CiHi
features <- read.csv("mutualInfo_features.csv")

features <- read.csv("mutualInfo_features.csv")
features$X <- levels(features$X)[features$X]
names(features)[1:2] <- c("predictor", "mutInf")
features[grep("^flag", features$predictor), "is_ed"] <- FALSE
features[grep("^patserv", features$predictor), "is_ed"] <- FALSE
features[
which(features$predictor %in% c("acutelos", "scu", "prior_hosp_90d",
"prior_hosp_365d", "readm_90d")), "is_ed"] <- FALSE
features[is.na(features$is_ed), "is_ed"] <- TRUE

# sum(features$is_ed)
# [1] 37
# sum(features$is_ed[1:40])
# [1] 21

sel_feat_ed_small <- features[1:40,] %>% filter(is_ed) %>% .$predictor
sel_feat_ed_big <- features %>% filter(is_ed) %>% .$predictor
sel_feat <- features[1:40,] %>% .$predictor

#dfx <- dfx %>% select(c(match(sel_feat, names(dfx)), ncol(dfx)))

#########Result Functions
###
toClass <- function(prob, threshold) {ifelse(prob > threshold, "Yes", "No" )}


get_cv_results <- function(fpred_main){
### for calculating CV training results from models,
# as training data is balanced, 0.5 threshold was used.
all_result = list()

for (fold in unique(fpred_main$Resample)){
	fpred <- fpred_main %>% filter(Resample == fold)
	tbl <- table(fpred$obs, fpred$pred)
	if (ncol(tbl) == 1) {tbl <- cbind(c(No = 0,Yes = 0), tbl)}
	recall=tbl[2,2]/rowSums(tbl)[2]
	precision=tbl[2,2]/colSums(tbl)[2]
	best <- data.frame(thr = c(0.5))
	best["recall"] <- recall
	best["prec"] <- precision
	best["F1"] <- (2*recall*precision)/(recall+precision)
	best["spec"] <-  tbl[1,1]/ rowSums(tbl)[1]
	best["acc"] <- (tbl[1,1] + tbl[2,2]) / sum(tbl)
	
	print("Now calculation AUC and AUCPR /n")
	fg <- fpred[which(fpred$obs == "Yes"), "Yes"]
	bg <- fpred[which(fpred$obs == "No"), "Yes"]
	#ROC Curve w. AUC
	roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = F)
	pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = F)
	
	result <- list(Model = fold,
	AUC = round(roc$auc, digits= 4),
	PRAUC = round(pr$auc.integral, digits=4),
	Recall = round(best$recall, digits=4),
	Precision = round(best$prec, digits = 4),
	F1 = round(best$F1, digits = 4),
	Spec = round(best$spec, digits= 4),
	Accuracy = round(best$acc, digits = 4),
	Threshold = round(best$thr, digits = 3))
	all_result <- rbind(all_result, result)
}
	return(all_result)
}

get_results <- function(data = test, model, model_name = "no_name", best_thr = TRUE){
	prob=predict(model, data, type='prob') #prob
	prob <- prob$Yes
	if (best_thr){
		m_prob <- mean(prob)
		sd <- sd(prob)
		thr <- seq(m_prob - 2* sd, m_prob + 2*sd, length.out = 200)
		thr_result <- data.frame(thr = thr)
		for (i in 1:length(thr)){
			tbl <- table(data$Class, toClass(prob,thr[i]))
			if (ncol(tbl) == 1) {tbl <- cbind(c(No = 0,Yes = 0), tbl)} #If model predicts all as one class
			recall=tbl[2,2]/rowSums(tbl)[2]
			precision=tbl[2,2]/colSums(tbl)[2]
			thr_result[i, "recall"] <- recall
			thr_result[i, "prec"] <- precision
			thr_result[i, "F1"] <- (2*recall*precision)/(recall+precision)
			thr_result[i, "spec"] <-  tbl[1,1]/ rowSums(tbl)[1]
			thr_result[i, "acc"] <- (tbl[1,1] + tbl[2,2]) / sum(tbl)
		}
		# best <- thr_result %>% arrange(desc(F1)) %>% head(1)
		best <- thr_result[which.max(thr_result$F1),]
		cat("The best selected cut-off point is: ", thr[which.max(thr_result$F1)])
	}
	else
	{
		tbl <- table(data$Class, toClass(prob, 0.5))
		if (ncol(tbl) == 1) {tbl <- cbind(c(No = 0,Yes = 0), tbl)}
		recall=tbl[2,2]/rowSums(tbl)[2]
		precision=tbl[2,2]/colSums(tbl)[2]
		best <- data.frame(thr = c(0.5))
		best["recall"] <- recall
		best["prec"] <- precision
		best["F1"] <- (2*recall*precision)/(recall+precision)
		best["spec"] <-  tbl[1,1]/ rowSums(tbl)[1]
		best["acc"] <- (tbl[1,1] + tbl[2,2]) / sum(tbl)
	}
	print("Now calculation AUC and AUCPR /n")
	fg <- prob[data$Class == "Yes"]
	bg <- prob[data$Class == "No"]
	#ROC Curve w. AUC
	roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = F)
	pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = F)
	all_result <- list(Model = model_name,
	AUC = round(roc$auc, digits= 4),
	PRAUC = round(pr$auc.integral, digits=4),
	Recall = round(best$recall, digits=4),
	Precision = round(best$prec, digits = 4),
	F1 = round(best$F1, digits = 4),
	Spec = round(best$spec, digits= 4),
	Accuracy = round(best$acc, digits = 4),
	Threshold = round(best$thr, digits = 3))
	
	return(all_result)
}
#################################
set.seed(3021)
names(dfx)[ncol(dfx)] <- "Class"
levels(dfx$Class) <- c("No", "Yes")
idx <- createDataPartition(dfx$Class, p = 0.8, list = FALSE)

train <- dfx[idx,]
test <- dfx[-idx,]

formula_smallED <- as.formula(paste0("Class", " ~ ", paste(sel_feat_ed_small, collapse="+")))
formula_bigED <- as.formula(paste0("Class", " ~ ", paste(sel_feat_ed_big, collapse="+")))
formula_EDCihi <- as.formula(paste0("Class", " ~ ", paste(sel_feat, collapse="+")))


############BAGGING IN CARET
# fitControl <- trainControl(method = "cv", number = 5,
# classProbs = TRUE,
# summaryFunction = twoClassSummary,
# sampling = NULL)
# grid<-expand.grid(mtry=c(5,10))
# set.seed(3021)


# t <- Sys.time()
# bagging_imb <- train(formula_smallED,
	# method='treebag',
	# data=train,
	# trControl=fitControl,
	# metric='ROC')
	
# get_results(test, bagging_imb, "Bagging_Imb", FALSE)

# $Model
# [1] "Bagging_Imb"

# $AUC
# [1] 0.552

# $PRAUC
# [1] 0.1741

# $Recall
# [1] 0.3683

# $Precision
# [1] 0.0332

# $F1
# [1] 0.0609

# $Spec
# [1] 0.8829

# $Accuracy
# [1] 0.8773

# $Threshold
# [1] 0.5

# get_results(test, bagging_imb, "Bagging_Imb", TRUE)
# very wierd results! Maybe because of repeats and CV

###########################
# fitControl <- trainControl(
# method = "none", #Only fits one model to train data
# classProbs = TRUE,
# summaryFunction = twoClassSummary,
# sampling = NULL,
# verboseIter = TRUE)
# set.seed(3021)


# t <- Sys.time()
# bagging_imb_EDCihi <- train(formula_EDCihi,
	# method='treebag',
	# data=train,
	# trControl=fitControl,
	# metric='ROC',
	# verbose = T)
	
# print(Sys.time() - t)
# myRes <- get_results(test, bagging_imb_EDCihi, "bagging_imb_EDCihi")

myRes <- rbind(myRes, get_results(test, logit_up, "LR_upSampling"))

# t <- Sys.time()
# bagging_imb_bigED <- train(formula_bigED,
	# method='treebag',
	# data=train,
	# trControl=fitControl,
	# metric='ROC',
	# verbose = T)
	
# print(Sys.time() - t)
# myRes <- rbind(myRes, get_results(test, bagging_imb_bigED, "bagging_imb_bigED"))

#########
####Resampling
# train is the imbalanced train subset of our data set
table(train$Class)
set.seed(3021)
down_train <- downSample(x = train[,-ncol(train)], y = train$Class)
table(down_train$Class)
up_train <- upSample(x = train[,-ncol(train)], y = train$Class)


fitControl <- trainControl(
method = "cv",
number = 5, #5-fold CV
classProbs = TRUE,
savePredictions = TRUE,
summaryFunction = twoClassSummary,
sampling = "down",
verboseIter = TRUE,
returnData = FALSE
)
set.seed(3021)

t <- Sys.time()
bagging_down_EDbig_cv <- train(formula_bigED,
	method='treebag',
	data=down_train,
	trControl=fitControl,
	metric='ROC',
	verbose = T)
	
print(Sys.time() - t)

bagging_down_EDbig_cv$finalModel
# Bagging classification trees with 25 bootstrap replications 

bagging_down_EDbig_cv$pred %>% head
  # pred obs   No  Yes rowIndex parameter Resample
# 1   No  No 0.60 0.40       12      none    Fold1
# 2   No  No 0.76 0.24       13      none    Fold1
# 3   No  No 0.72 0.28       24      none    Fold1
# 4  Yes  No 0.36 0.64       43      none    Fold1
# 5   No  No 0.56 0.44       45      none    Fold1
# 6   No  No 0.68 0.32       51      none    Fold1

bagging_down_EDbig_cv$resample
        # ROC      Sens      Spec Resample
# 1 0.6870468 0.6417933 0.6284195    Fold1
# 2 0.6885814 0.6463906 0.6253277    Fold2
# 3 0.6891561 0.6440350 0.6327128    Fold3
# 4 0.6873964 0.6421597 0.6311550    Fold4
# 5 0.6866558 0.6406915 0.6299772    Fold5




myRes <- rbind(myRes, get_results(test, bagging_down_EDbig_cv, "bagging_down_EDbig_cv"))
myRes <- rbind(myRes, get_results(test, bagging_down_EDbig_cv, "bagging_down_EDbig_cv", best_thr = FALSE))

prob <- predict(bagging_down_EDCIHI_cv, test, type='prob') #prob
prob <- prob$Yes
predClass <- toClass(prob, 0.5)
confusionMatrix(data = predClass, reference = test$Class, positive = "Yes")

# saveRDS(bagging_down_EDCihi_cv, "./caret_models/bag_bigED.rds")


#################################
# t <- Sys.time()
# bagging_down_EDsmall_cv <- train(formula_smallED,
	# method='treebag',
	# data=train,
	# trControl=fitControl,
	# metric='ROC',
	# verbose = T)
	
# print(Sys.time() - t)
# myRes <- rbind(myRes, get_results(test, bagging_down_EDsmall_cv, "bagging_down_EDsmall_cv"))


t <- Sys.time()
bagging_down_EDCIHI_cv <- train(formula_EDCihi,
	method='treebag',
	data=train,
	trControl=fitControl,
	metric='ROC',
	verbose = T)
	
myRes <- rbind(myRes, get_results(test, bagging_down_EDCIHI_cv, "bag_EDCIHI"))
myRes <- rbind(myRes, get_results(test, bagging_down_EDCIHI_cv, "bag_EDCIHI_0.5co", best_thr = FALSE))

print(Sys.time() - t)

###################################
###just CART now######

fitControl <- trainControl(
method = "cv",
number = 5, #5-fold CV
classProbs = TRUE,
savePredictions = TRUE,
summaryFunction = twoClassSummary,
# sampling = "down",
verboseIter = TRUE,
returnData = FALSE)
set.seed(3021)

down_train <- downSample(x = train[,-ncol(train)], y = train$Class)

# samp <- sample(1:nrow(train),nrow(train)/200)
t <- Sys.time()
cart_ED_imbalanced <- train(
	x = train[, match(sel_feat_ed_big, names(dfx))],
	y = train[, "Class"],
	method='rpart',
	tuneLength = 20,
	trControl=fitControl,
	metric='ROC')
	
print(Sys.time() - t)

cart_ED_imbalanced$finalModel
cart_ED_imbalanced$resample

prob <- predict(cart_ED_imbalanced, test, type='prob') #prob
prob <- prob$Yes
predClass <- toClass(prob, 0.5)
confusionMatrix(data = predClass, reference = test$Class, positive = "Yes")

myRes <- rbind(myRes, get_results(test, cart_ED_imbalanced, "cart_ED_imbalanced"))


######upSample CART
up_train <- upSample(x = train[,-ncol(train)], y = train$Class)

samp <- sample(1:nrow(up_train),nrow(up_train)/200)
t <- Sys.time()
cart_ED_upsample <- train(
	x = up_train[, match(sel_feat_ed_big, names(dfx))],
	y = up_train[, "Class"],
	method='rpart2',	
	tuneLength = 20,
	trControl=fitControl,
	metric='ROC')
	
print(Sys.time() - t)

cart_ED_upsample$finalModel
cart_ED_upsample$resample

prob <- predict(cart_ED_upsample, test, type='prob') #prob
prob <- prob$Yes
predClass <- toClass(prob, 0.5)
confusionMatrix(data = predClass, reference = test$Class, positive = "Yes")

myRes <- rbind(myRes, get_results(test, cart_ED_upsample, "cart_ED_upsample"))

# Getting train and test results, max depth of 4 was the final TUNE
fpred <- cart_ED_upsample$pred %>% filter(maxdepth == 4) %>% 
select(obs, pred, Yes, Resample)
cart_ED_tRes <- get_cv_results(fpred)
cart_ED_tRes
rbind(myRes[6,], cart_ED_tRes) %>% write.csv("./caret_models/CART_upSample_ED_TrainTest_RESULTS.csv")


##############
t <- Sys.time()
cart_EDCIHI_upsample <- train(
	x = up_train[, match(sel_feat, names(dfx))],
	y = up_train[, "Class"],
	method='rpart2',	
	tuneLength = 20,
	trControl=fitControl,
	metric='ROC')
	
print(Sys.time() - t)

cart_EDCIHI_upsample$finalModel
cart_EDCIHI_upsample$resample


myRes <- rbind(myRes, get_results(test, cart_EDCIHI_upsample, "cart_EDCIHI_upsample"))
myRes <- rbind(myRes, get_results(test, cart_EDCIHI_upsample, "cart_EDCIHI_upsample", best_thr = FALSE))
# Getting train and test results, max depth of 4 was the final TUNE
fpred <- cart_EDCIHI_upsample$pred %>% filter(maxdepth == 5) %>% 
select(obs, pred, Yes, Resample)
cart_EDCIHI_tRes <- get_cv_results(fpred)
cart_EDCIHI_tRes
rbind(myRes[7:8,], cart_EDCIHI_tRes) %>% write.csv("./caret_models/CART_upSample_EDCIHI_TrainTest_RESULTS.csv")


#here comes the ROC!
prob <- predict(cart_EDCIHI_upsample, test, type='prob') #prob
prob <- prob$Yes

cart_cihi_roc <- data.frame(prob = prob , obs = test$Class)

cart_cihi_roc <- cart_cihi_roc %>% mutate(obs = ifelse(obs == "Yes", 1, 0)) #to match H2o format
cart_cihi_roc %>% saveRDS("./caret_models/cart_cihi_roc.rds")

##########CART models done

fitControl <- trainControl(
method = "cv",
number = 5, #5-fold CV
classProbs = TRUE,
savePredictions = TRUE,
summaryFunction = twoClassSummary,
verboseIter = TRUE,
returnData = FALSE)
set.seed(3021)

##using upsampling on bagging models now
t <- Sys.time()
treebag_ED_upsample <- train(
	x = up_train[, match(sel_feat_ed_big, names(dfx))],
	y = up_train[, "Class"],
	method='treebag',	
	trControl=fitControl,
	metric='ROC')
	
print(Sys.time() - t)

treebag_ED_upsample$finalModel
treebag_ED_upsample$resample

myRes <- rbind(myRes, get_results(test, treebag_ED_upsample, "treebag_ED_upsample"))
#not good results?! AUC 0.66
#bagging_down_EDbig_cv

myRes <- rbind(myRes, get_results(test,  bagging_down_EDbig_cv, " bagging_down_EDbig_cv"))
fpred <- bagging_down_EDbig_cv$pred  %>% select(obs, pred, Yes, Resample)
bag_down_ed_results <- get_cv_results(fpred)
bag_down_ed_results

rbind(myRes[c(1:4,9:10),],bag_down_ed_results)%>% write.csv("./caret_models/BAGGING_dSample_ED_TrainTest_RESULTS.csv")

###Saving prediction for test set for future ROC calculation
nrow(test)
# [1] 274578
prob <- predict(bagging_down_EDbig_cv, test, type='prob') #prob
prob <- prob$Yes

bag_ed_roc <- data.frame(pred = prob , obs = test$Class)

edroc <- edroc %>% mutate(obs = ifelse(obs == "Yes", 1,0))
edroc %>% saveRDS("./caret_models/bag_ed_roc.rds")


##bagging for ed cihi, I think I removed the previous one in the process!

down_train <- downSample(x = train[,-ncol(train)], y = train$Class)
table(down_train$Class)

fitControl <- trainControl(
method = "cv",
number = 5, #5-fold CV
classProbs = TRUE,
savePredictions = TRUE,
summaryFunction = twoClassSummary,
verboseIter = TRUE,
returnData = FALSE)
set.seed(3021)

t <- Sys.time()
bagging_down_EDCIHI_cv <- train(
	x = down_train[, match(sel_feat, names(dfx))],
	y = down_train[, "Class"],
	method='treebag',	
	trControl=fitControl,
	metric='ROC')
print(Sys.time() - t)


myRes <- rbind(myRes, get_results(test,  bagging_down_EDCIHI_cv, " bagging_down_EDCIHI_cv"))
fpred <- bagging_down_EDCIHI_cv$pred  %>% select(obs, pred, Yes, Resample)
bag_down_edcihi_results <- get_cv_results(fpred)
bag_down_edcihi_results %>% write.csv("./caret_models/BAGGING_dSample_EDCIHI_5foldcv.csv")

rbind(myRes[c(1:4,9:10),],bag_down_edcihi_results)%>% write.csv("./caret_models/BAGGING_dSample_EDCIHI_TrainTest_RESULTS.csv")

###Saving prediction for test set for future ROC calculation
nrow(test)
# [1] 274578
prob <- predict(bagging_down_EDCIHI_cv, test, type='prob') #prob
prob <- prob$Yes

bag_cihi_roc <- data.frame(prob = prob , obs = test$Class)

bag_cihi_roc <- bag_cihi_roc %>% mutate(obs = ifelse(obs == "Yes", 1, 0)) #to match H2o format
bag_cihi_roc %>% saveRDS("./caret_models/bag_cihi_roc.rds")


###Saving prediction for test set for future ROC calculation
nrow(test)
# [1] 274578
prob <- predict(bagging_down_EDbig_cv, test, type='prob') #prob
prob <- prob$Yes

bag_ed_roc <- data.frame(pred = prob , obs = test$Class)
bag_ed_roc %>% saveRDS("./caret_models/bag_ed_roc.rds")



###############Testing if caret sub-sampling affects precision recall on imbalanced data
fitControl <- trainControl(
method = "cv",
number = 5, #5-fold CV
classProbs = TRUE,
savePredictions = TRUE,
summaryFunction = twoClassSummary,
sampling = "up",
verboseIter = TRUE,
returnData = FALSE)
set.seed(3021)

t <- Sys.time()
cart_ED_upsample <- train(
	x = train[, match(sel_feat_ed_big, names(dfx))],
	y = train[, "Class"],
	method='rpart2',	
	maxdepth = 4,
	trControl=fitControl,
	metric='ROC')
	
print(Sys.time() - t)

cart_ED_upsample$finalModel
cart_ED_upsample$resample

prob <- predict(cart_ED_upsample, test, type='prob') #prob
prob <- prob$Yes
predClass <- toClass(prob, 0.5)
confusionMatrix(data = predClass, reference = test$Class, positive = "Yes")

myRes <- rbind(myRes, get_results(test, cart_ED_upsample, "cart_ED_upsample"))

# Getting train and test results, max depth of 4 was the final TUNE
fpred <- cart_ED_upsample$pred %>% filter(maxdepth == 14) %>% 
select(obs, pred, Yes, Resample)
cart_ED_tRes <- get_cv_results(fpred)
cart_ED_tRes
rbind(myRes[6,], cart_ED_tRes) %>% write.csv("./caret_models/CART_upSample_ED_TrainTest_RESULTS.csv")

#here comes the ROC!
prob <- predict(cart_ED_upsample, test, type='prob') #prob
prob <- prob$Yes

cart_ed_roc <- data.frame(prob = prob , obs = test$Class)

cart_ed_roc <- cart_ed_roc %>% mutate(obs = ifelse(obs == "Yes", 1, 0)) #to match H2o format
cart_ed_roc %>% saveRDS("./caret_models/cart_ed_roc.rds")
