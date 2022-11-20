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
###run---- 
set.seed(123)
{
  {
    model.glm<-glm(FPFL~.,data = train.data,family =binomial(link = "logit")) #biomial
    factor.glm<-model.glm[["coefficients"]] 
    factor.glm<-data.frame(factor.glm)
    factor.glm$factor<-row.names(factor.glm)
    factor.glm<-data.frame(marker=factor.glm$factor,glm=factor.glm$factor.glm)
    factor.glm$marker<-as.character(factor.glm$marker)
  } # glm
  {
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
    model.svm<-rminer::fit(FPFL ~ .,train.data,model = "svm")
    VariableImportance<-Importance(model.svm,train.data,method = "sensv")
    L=list(runs=1,sen=t(VariableImportance$imp),
           sresponses=VariableImportance$sresponses)
    factor.svm<-data.frame(marker=names(train.data),svm=t(L[["sen"]]))
    factor.svm$marker<-as.character(factor.svm$marker)
    factor.svm$marker<-ifelse(factor.svm$marker=="FPFL","(Intercept)",factor.svm$marker)
  } # svm
  {
    hr_base_model <- rpart(as.factor(FPFL) ~ ., data = train.data, method = "class",  # XC: we need to specify FPFL as factor.  adding as.factor(FPFL). (revise 1).
                           control = rpart.control(cp = 0))
    summary(hr_base_model)
    printcp(hr_base_model)
    cp.frame<- data.frame(printcp(hr_base_model)) 
    cp.frame<- cp.frame[cp.frame$nsplit!=0,]
    min.cp<-cp.frame[cp.frame$xerror %in% min(cp.frame$xerror),]$CP
    model.dt <- prune(hr_base_model, cp = min.cp )
    factor.dt<-data.frame(model.dt$variable.importance) 
    factor.dt$marker<-rownames(factor.dt)
    factor.dt<-factor.dt[,c(2,1)]
    colnames(factor.dt)<-c("marker","dt")
  } # DT
  {
    train.data$FPFL <- as.factor(train.data$FPFL)
    trControl <- trainControl(method = "cv", number = 3, search = "grid")
    tuneGrid  <- expand.grid(.mtry = c(2: (ncol(train.data)-2) ))        
    set.seed(12345)
    rf_default <- train(FPFL~.,
                        data = train.data,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        ntree = 50) # XC: change number of trees in the forest. looks like 300 was too big. (revise 2).
    # Print the results
    print(rf_default)
    best.m <- rf_default$bestTune$mtry
    model.rf <- randomForest(FPFL~., train.data, mtry = best.m, improtance=TRUE, ntree=3, localImp = TRUE) # XC: (revise 4, ntree=3).
    importance_rank<-data.frame(importance(model.rf))
    factor.rf<-importance_rank[order(importance_rank$MeanDecreaseGini,decreasing = T),]
    factor.rf$marker<-rownames(factor.rf)
    factor.rf$rf<-factor.rf$MeanDecreaseGini
    colnames(factor.rf)<-c("marker","rf")
    factor.rf$marker<-as.character(factor.rf$marker)
  } # RF
  {
    # set.seed(123)
    train.data$FPFL<-ifelse(train.data$FPFL==1,1,0)
    model.xgb<-xgboost(data = as.matrix(train.data[,-1]),label = train.data[,1],max_depth=2,
                       eta =0.5,nthread=2,nrounds = 5,objective="binary:logistic")
    xgb.importance1<-xgb.importance(model=model.xgb)
    factor.xgb<-xgb.importance1[,c(1,2)]
    colnames(factor.xgb)<-c("marker","xgb")
  } # XGBoost
  {
    model.gbm <- gbm(FPFL ~ ., data = train.data,
                     distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
                     interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
                     n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
                     verbose = FALSE, n.cores = 1)  
    
    # Check performance using the out-of-bag (OOB) error; the OOB error typically
    best.iter <- gbm.perf(model.gbm, method = "OOB")
    factor.gbm<-summary.gbm(model.gbm)
    colnames(factor.gbm)<-c("marker","gbm")
    factor.gbm$marker<-as.character(factor.gbm$marker)
    summary(model.gbm)
  } # GBM
}   #  run model
