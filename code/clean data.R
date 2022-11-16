# load package----
library(dplyr)
library(flextable)
library(gtsummary)
library(plyr)
library(ggsci)
library(ggpubr)
library(pROC)
library(patchwork)
library(coefplot)
library(glmnet)
library(e1071)
library(rminer)
library(rpart)
library(rpart.plot)
library(randomForest)
library(randomForestExplainer)
library(xgboost)
library(gbm)
library(survminer)
library(survival)

# 数据整备-------
#########R version ######
# R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# Copyright (C) 2021 The R Foundation for Statistical Computing
# Platform: aarch64-apple-darwin20 (64-bit)

All_Cohort = data.table::fread("FP_Cohort_out.csv")
lab = data.table::fread("FP-lab.csv")
library(dplyr)
# 取交集，去除数据不齐的227患者（50+91+18+68）
dv_id = All_Cohort$id %>% intersect(lab$id)
train_id = (All_Cohort %>% filter(cohort_Name=="train"))$id %>% intersect(dv_id) 
test1_id = (All_Cohort %>% filter(cohort_Name=="test1"))$id %>% intersect(dv_id)
test2_id = (All_Cohort %>% filter(cohort_Name=="test2"))$id %>% intersect(dv_id)

Lab_data = lab %>% filter(id %in% dv_id) %>% dplyr::select(id,colnames(lab)[7:27])


All_Cohort$Race<-ifelse(All_Cohort$RACE=="WHITE","WHITE",ifelse(All_Cohort$RACE=="ASIAN","ASIAN","Other"))
All_Cohort$Race = as.factor(All_Cohort$Race)

table(All_Cohort$METSITES)
All_Cohort$number_metastasis<-ifelse(All_Cohort$METSITES=="1","1",ifelse(All_Cohort$METSITES=="2","2","3"))
All_Cohort$number_metastasis = as.factor(All_Cohort$number_metastasis)
All_Cohort$ECOGGR = ifelse(All_Cohort$ECOGGR.x=="0","0",">=1")
All_Cohort$metastasis_3 = ifelse(All_Cohort$number_metastasis<3,"<3",">=3")


f1 = colnames(lab)[7:27]



lg_data = All_Cohort %>% merge(Lab_data,by="id") %>% 
  dplyr::select(FPFL,id,all_of(f1)) %>%
  tibble::column_to_rownames("id") %>%
  mutate(FPFL=ifelse(FPFL=="FP",1,0)) 

# 无穷值的处理-----
min(lg_data$MONO)
min(lg_data$MONO)
max(lg_data$MLR)
max(lg_data$PLR)

table(is.infinite(lg_data$NMR))  

lg_data$MONO=ifelse(lg_data$MONO==0,0.0000001,lg_data$MONO)   # 把MONO==0的用最小值代替，重新计算MLR,

table(round(lg_data$LMR,9)==round(lg_data$LYM/lg_data$MONO,9))
table(round(lg_data$NMR,9)==round(lg_data$NEUT/lg_data$MONO,9))

lg_data$LMR = lg_data$LYM/lg_data$MONO
lg_data$NMR = lg_data$NEUT/lg_data$MONO