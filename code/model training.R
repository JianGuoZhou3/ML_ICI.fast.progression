#######21_lab_plus_clin model######
source("./code/clean data.R")
#  add five clinical characteristics and run model  ----
model_name = c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM")
key_clin = c("ECOGGR","BBMI","BONE","LIVER","number_metastasis")

add_clin =  All_Cohort %>% dplyr::select(id,all_of(key_clin))
# 缺失数据填补BBMI 
library(mice)
md.pattern(add_clin)
impdat <- mice(add_clin,m=5,method=c("pmm")) #采用PMM法进行多重填补，m=5
summary(impdat)

impdat$imp$BBMI

add_clin<-complete(impdat)  

Lg_data = lg_data %>% tibble::rownames_to_column("id") %>% merge(add_clin,by="id") %>% 
  tibble::column_to_rownames("id") %>% 
  mutate(ECOGGR=ifelse(ECOGGR==">=1",1,0)) %>% 
  mutate(BONE=ifelse(BONE=="Y",1,0)) %>% 
  mutate(LIVER=ifelse(LIVER=="Y",1,0)) %>% 
  mutate(number_metastasis=as.factor(number_metastasis))
# 亚变量转换
library(caret)
dmy_a <- dummyVars(~., data = Lg_data)
Lg_data <- data.frame(predict(dmy_a, newdata = Lg_data))

train.data = Lg_data[train_id,]
test1.data = Lg_data[test1_id,]
test2.data = Lg_data[test2_id,]

set.seed(123)
#  run model
source("./code/seven.ML.R")

# save model----
save(model.glm, model.lasso, model.svm, model.dt, 
     model.rf, model.xgb, model.gbm,best.iter,
     train.data,test1.data,test2.data,file = "lab_plus_clin.rda")
######21 biomarker model########
source("./code/clean data.R")
# run and save 21 biomarker model
f1 = colnames(lab)[7:27]
model_name = c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM")


All_Cohort$FPFL<-ifelse( All_Cohort$FPFL=="non_FP",0,1)
train.data = lg_data[train_id,] %>% dplyr::select(FPFL,all_of(f1))
test1.data = lg_data[test1_id,] %>% dplyr::select(FPFL,all_of(f1))
test2.data = lg_data[test2_id,] %>% dplyr::select(FPFL,all_of(f1))

set.seed(123)
#  run model
source("./code/seven.ML.R")
save(model.glm, model.lasso, model.svm, model.dt, 
     model.rf, model.xgb, model.gbm,best.iter,train.data,
     test1.data,test2.data,file = "21_lab.rda")

#####impvar.f####
impvar.f = read.csv("Supplementary Table 2 {frequency of relative important variables}.csv")

######Top freq 1########
# models optimization: features selection under the same performance
as.formula(paste('FPFL~',paste(as.character((impvar.f %>% filter(Freq>7/2))$impvar.f),collapse = '+')))
Tf1 = c("ALB","ALT","CRP","LDH","LMR","NEUT","NLR","PLAT","PLR")
train.data = lg_data[train_id,] %>% dplyr::select(FPFL,all_of(Tf1)) 
test1.data = lg_data[test1_id,] %>% dplyr::select(FPFL,all_of(Tf1)) 
test2.data = lg_data[test2_id,] %>% dplyr::select(FPFL,all_of(Tf1)) 

#  run model
set.seed(123)
source("./code/seven.ML.R")

save(model.glm, model.lasso, model.svm, model.dt, model.rf, model.xgb, model.gbm,best.iter,
     train.data,test1.data,test2.data,file = "Top freq 1.rda")

######Top freq 2########
as.formula(paste('FPFL~',paste(as.character((impvar.f %>% filter(Freq>4))$impvar.f),collapse = '+')))
Tf2 = c("ALB","ALT","CRP","LDH","NEUT","PLR")
train.data = lg_data[train_id,] %>% dplyr::select(FPFL,all_of(Tf2)) 
test1.data = lg_data[test1_id,] %>% dplyr::select(FPFL,all_of(Tf2)) 
test2.data = lg_data[test2_id,] %>% dplyr::select(FPFL,all_of(Tf2)) 
#  run model
set.seed(123)
source("./code/seven.ML.R")

save(model.glm, model.lasso, model.svm, model.dt, model.rf, model.xgb, model.gbm,best.iter,
     train.data,test1.data,test2.data,file = "Top freq 2.rda")
######Top freq 3########
as.formula(paste('FPFL~',paste(as.character((impvar.f %>% filter(Freq>5))$impvar.f),collapse = '+')))
Tf3 = c("ALT","CRP","LDH","NEUT")
train.data = lg_data[train_id,] %>% dplyr::select(FPFL,all_of(Tf3)) 
test1.data = lg_data[test1_id,] %>% dplyr::select(FPFL,all_of(Tf3)) 
test2.data = lg_data[test2_id,] %>% dplyr::select(FPFL,all_of(Tf3)) 
#  run model
set.seed(123)
source("./code/seven.ML.R")

save(model.glm, model.lasso, model.svm, model.dt, model.rf, model.xgb, model.gbm,best.iter,
     train.data,test1.data,test2.data,file = "Top freq 3.rda")
