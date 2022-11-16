
source("./数据整理.R")
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





library(coefplot)
library(glmnet)
library(e1071)
library(rminer)
library(rpart)
library(rpart.plot)
library(randomForest)
library(randomForestExplainer)

set.seed(123)
{
  {library(coefplot)
    model.glm<-glm(FPFL~.,data = train.data,family =binomial(link = "logit")) #biomial
    factor.glm<-model.glm[["coefficients"]] 
    factor.glm<-data.frame(factor.glm)
    factor.glm$factor<-row.names(factor.glm)
    factor.glm<-data.frame(marker=factor.glm$factor,glm=factor.glm$factor.glm)
    factor.glm$marker<-as.character(factor.glm$marker)
  } # glm
  {
    library(glmnet)
    library(dplyr)
    # Find the best lambda using cross-validation
    x<-data.matrix(train.data[,-1])
    y<-train.data[,1]
    cv.lasso<-cv.glmnet(x,y,alpha =1,family="binomial")
    # Fit the final model on the training data
    model.lasso<-glmnet(x,y,alpha =1,family = "binomial",
                        lambda = cv.lasso$lambda.min)
    mat<-coef(model.lasso)
    factor.lasso<-summary(coef(model.lasso))
    factor.lasso$marker<-rownames(mat)[factor.lasso$i]
    factor.lasso<-factor.lasso[,c(4,3)]
    colnames(factor.lasso)<-c( "marker","lasso" )
    
    
  } # lasso
  {
    
    library(e1071)
    # model.svm<-svm(FPFL ~ .,data.matrix(train.data))
    library(rminer)
    model.svm<-rminer::fit(FPFL ~ .,train.data,model = "svm")
    VariableImportance<-Importance(model.svm,train.data,method = "sensv")
    L=list(runs=1,sen=t(VariableImportance$imp),
           sresponses=VariableImportance$sresponses)
    #mgraph(L,graph = "IMP",leg = names(train.data),col ="gray",Grid = 10)
    factor.svm<-data.frame(marker=names(train.data),svm=t(L[["sen"]]))
    factor.svm$marker<-as.character(factor.svm$marker)
    factor.svm$marker<-ifelse(factor.svm$marker=="FPFL","(Intercept)",factor.svm$marker)
    
    
  } # svm
  {
    library(rpart)
    library(rpart.plot)
    model.dt<- rpart(FPFL ~ .,data=train.data,method = 'class',minsplit=2,minbucket=1,cp=-1)
    factor.dt<-data.frame(model.dt$variable.importance) 
    factor.dt$marker<-rownames(factor.dt)
    factor.dt<-factor.dt[,c(2,1)]
    colnames(factor.dt)<-c("marker","dt")
    
  } # DT
  {
    library(randomForest)
    library(randomForestExplainer)
    model.rf<-model.rf<-randomForest(FPFL ~ .,train.data,ntree=500)
    min_depth_frame<-min_depth_distribution(model.rf)
    mtry<-tuneRF(train.data,train.data[,1],ntreeTry = 500,stepFactor = 1.5,improve = 0.01,trace = TRUE,
                 plot = TRUE) 
    best.m<-mtry[mtry[,2]==min(mtry[,2]),1]
    model.rf<-randomForest(FPFL ~ .,train.data,mtry=best.m,improtance=TRUE,tree=500,localImp = TRUE)
    importance_frame<-measure_importance(model.rf)
    factor.rf<-importance_frame[,c(1,8)] #$p_value
    colnames(factor.rf)<-c("marker","rf")
    factor.rf$marker<-as.character(factor.rf$marker)
    
  } # RF
  {library(xgboost)
    # set.seed(123)
    model.xgb<-xgboost(data = as.matrix(train.data[,-1]),label = train.data[,1],max_depth=2,
                       eta =0.5,nthread=2,nrounds = 5,objective="binary:logistic")
    xgb.importance1<-xgb.importance(model=model.xgb)
    factor.xgb<-xgb.importance1[,c(1,2)]
    colnames(factor.xgb)<-c("marker","xgb")
    
  } # XGBoost
  {
    library(gbm)
    model.gbm <- gbm(FPFL ~ ., data = train.data,
                     distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
                     interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
                     n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
                     verbose = FALSE, n.cores = 1)  
    
    # Check performance using the out-of-bag (OOB) error; the OOB error typically
    # # underestimates the optimal number of iterations
    best.iter <- gbm.perf(model.gbm, method = "OOB")
    # print(best.iter)
    # # Check performance using the 50% heldout test set
    # best.iter <- gbm.perf(model.gbm, method = "test")
    # print(best.iter)
    
    # Check performance using 5-fold cross-validation
    # best.iter <- gbm.perf(model.gbm, method = "cv")
    # factor.gbm<- summary(model.gbm, n.trees = best.iter)  # using estimated best number of trees
    # # Compactly print the first and last trees for curiosity
    # # print(pretty.gbm.tree(gbm1, i.tree = 1))
    # print(pretty.gbm.tree(model.gbm, i.tree = model.gbm$n.trees))
    factor.gbm<-summary.gbm(model.gbm)
    colnames(factor.gbm)<-c("marker","gbm")
    factor.gbm$marker<-as.character(factor.gbm$marker)
    # Predict on the new data using the "best" number of trees; by default,
    # predictions will be on the link scale
    summary(model.gbm)
  } # GBM
}   #  run model

# save model----
# save(model.glm, model.lasso, model.svm, model.dt, model.rf, model.xgb, model.gbm,best.iter,
#      train.data,test1.data,test2.data,file = "lab_plus_clin.rda")


