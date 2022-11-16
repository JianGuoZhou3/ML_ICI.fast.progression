
source("./code/clean data.R")
# run and save 21 biomarker model--------

f1 = colnames(lab)[7:27]
model_name = c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM")



train.data = lg_data[train_id,] %>% dplyr::select(FPFL,all_of(f1))
test1.data = lg_data[test1_id,] %>% dplyr::select(FPFL,all_of(f1))
test2.data = lg_data[test2_id,] %>% dplyr::select(FPFL,all_of(f1))

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
   # model.dt<- rpart(FPFL ~ .,data=train.data,method = 'class',minsplit=2,minbucket=1,cp=-1)
    # https://dzone.com/articles/decision-trees-and-pruning-in-r
    # hr_base_model <- rpart(FPFL ~ ., data = train.data, method = "class",xval=5,control = rpart.control(cp = 0))
    hr_base_model <- rpart(FPFL ~ ., data = train.data, method = "class",
                           control = rpart.control(cp = 0))
    summary(hr_base_model)
    #Plot Decision Tree
    #plot(hr_base_model)
    # Examine the complexity plot
    printcp(hr_base_model)
    cp.frame<- data.frame(printcp(hr_base_model)) 
    cp.frame<- cp.frame[cp.frame$nsplit!=0,]
    min.cp<-cp.frame[cp.frame$xerror %in% min(cp.frame$xerror),]$CP
    # plotcp(hr_base_model)
    # ##### Prepruning
    # # Grow a tree with minsplit of 100 and max depth of 8
    # hr_model_preprun <- rpart(left ~ ., data = train, method = "class", 
    #                           control = rpart.control(cp = 0, maxdepth = 8,minsplit = 100))
    # # Compute the accuracy of the pruned tree
    # test$pred <- predict(hr_model_preprun, test, type = "class")
    # accuracy_preprun <- mean(test$pred == test$left)
    # 
    ####Postpruning
    # Prune the hr_base_model based on the optimal cp value
    model.dt <- prune(hr_base_model, cp = min.cp )
    # Compute the accuracy of the pruned tree
    train.data$pred <- predict(model.dt, train.data, type = "class")
    accuracy_postprun <- mean(train.data$pred == train.data$FPFL)
    ###
    factor.dt<-data.frame(model.dt$variable.importance) 
    factor.dt$marker<-rownames(factor.dt)
    factor.dt<-factor.dt[,c(2,1)]
    colnames(factor.dt)<-c("marker","dt")
    
  } # DT
  {
    library(randomForest)
    library(randomForestExplainer)
    library(e1071)
    library(caret)
    
    # XC: a very important step was missed! Otherwise, train.data$FPFL will be treated as continuous variable 
    #     when train the RF.
    train.data$FPFL <- as.factor(train.data$FPFL)
    trControl <- trainControl(method = "cv", number = 3, search = "grid") # number: how many folds for CV. In this e.g., it is 10-fold CV.
    tuneGrid  <- expand.grid(.mtry = c(2: (ncol(train.data)-2) ))          # grid search 'mtry' from 2 to ncol(train.data)-2. 
    set.seed(12345)
    # running time is approx. 2 mins on my computer.
    rf_default <- train(FPFL~.,
                        data = train.data,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        ntree = 300)
    # Print the results
    print(rf_default)
    best.m <- rf_default$bestTune$mtry
    model.rf <- randomForest(FPFL~., train.data, mtry = best.m, improtance=TRUE, ntree=300, localImp = TRUE)
    importance_rank<-data.frame(importance(model.rf))
    factor.rf<-importance_rank[order(importance_rank$MeanDecreaseGini,decreasing = T),]
    factor.rf$marker<-rownames(factor.rf)
    factor.rf$rf<-factor.rf$MeanDecreaseGini
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

# save(model.glm, model.lasso, model.svm, model.dt, model.rf, model.xgb, model.gbm,best.iter,train.data,test1.data,test2.data,file = "21_lab.rda")



