

###########################  Decision Tree:  ###########################

library(rpart)
library(rpart.plot)
# XC: we will not use the following code to construct the DT, because the parameter 'minsplit', 'minbucket', etc.
#     are pre-pruning the tree, see https://dzone.com/articles/decision-trees-and-pruning-in-r.
#     Pre-pruning usually is hard, because we are not sure how complex the tree is. Therefore, post-pruning is preferred. 
#     model.dt<- rpart(FPFL ~ .,data=train.data,method = 'class',minsplit=2,minbucket=1,cp=-1)

#XC:  We use the following code to construct the over-fitted DT by setting 'CP=0' then prune the tree.
model.dt<- rpart(FPFL ~ .,data=train.data,method = 'class',control = rpart.control(cp = 0))
# Examine the complexity plot
printcp(model.dt)
plotcp(model.dt)
model.dt.pruned <- prune(model.dt, cp = 0.02)

#XC: show the accuracy of the pruned tree
test  <- test1.data
train <- train.data
train$pred.preprun <- predict(model.dt,        train, type = "class")
test$pred.preprun  <- predict(model.dt,        test,  type = "class")
test$pred          <- predict(model.dt.pruned, test,  type = "class")
accuracy_base      <- mean(train$pred.preprun == train$FPFL)
accuracy_preprun   <- mean(test$pred.preprun  == test$FPFL)
accuracy_postprun  <- mean(test$pred          == test$FPFL)
data.frame(accuracy_base,accuracy_preprun, accuracy_postprun)
# XC: The accuracy for post-pruned DT is 0.9119718 that looks good.
# accuracy_base  accuracy_preprun  accuracy_postprun
# 0.9283154      0.875             0.9119718


# XC: calculate the importance of the post-pruned DT:
data.frame(model.dt.pruned$variable.importance)
factor.dt<-data.frame(model.dt.pruned$variable.importance) 
factor.dt$marker<-rownames(factor.dt)
factor.dt<-factor.dt[,c(2,1)]
colnames(factor.dt)<-c("marker","dt")
#> factor.dt
#marker         dt
#NEUT   NEUT 13.3290303
#ALB     ALB  5.8222648
#ALT     ALT  4.2099206
#RBC     RBC  2.7958006
#CRP     CRP  2.5891663
#AST     AST  2.4056689
#HGB     HGB  2.2698482
#MONO   MONO  1.9725460
#NLR     NLR  1.9725460
#HCT     HCT  1.5132321
#TSH     TSH  1.2028345
#ALP     ALP  0.9021259
#LMR     LMR  0.9021259
#LDH     LDH  0.7566161
#PLAT   PLAT  0.7172895
#GLUC   GLUC  0.7016534
#NMR     NMR  0.5379671





###########################  Random Forest:  ###########################
# Not like DT, we do not prune the trees in the RF. 
# We will stick to tuning two parameters, namely the 'mtry' and the 'ntree' parameters that 
# have the following affect on our random forest model. There are many other parameters, 
# but these two parameters are perhaps the most likely to have the biggest effect on your final accuracy.

#parameter mtry:  Number of variables randomly sampled as candidates at each split.
#          ntree: Number of trees to grow.


library(randomForest)
library(randomForestExplainer)
library(e1071)
library(caret)

# XC: a very important step was missed! Otherwise, train.data$FPFL will be treated as continuous variable 
#     when train the RF.
train.data$FPFL <- as.factor(train.data$FPFL)

# XC: use grid search and cross-validation approaches to find optimal 'mtry'.  
#     see https://www.guru99.com/r-random-forest-tutorial.html#2

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
model.rf$confusion
# overall accuracy:
sum(diag(model.rf$confusion)) / sum(model.rf$confusion)
#> sum(diag(model.rf$confusion)) / sum(model.rf$confusion)
#  [1] 0.8998963

# check the overall accuracy on test1.data:
test1.data$FPFL <- as.factor(test1.data$FPFL)
confusionMatrix(predict(model.rf, newdata = test1.data), test1.data$FPFL)
# Accuracy : 0.9331          
#  95% CI : (0.9093, 0.9522)
importance_rank<-data.frame(importance(model.rf))
importance_rank<-importance_rank[order(importance_rank$MeanDecreaseGini,decreasing = T),]
importance_frame     <- measure_importance(model.rf)

factor.rf            <- importance_frame[,c(1,8)] #$p_value
colnames(factor.rf)  <- c("marker","rf")
factor.rf$marker     <- as.character(factor.rf$marker)









######################  backup code  ##############################################
# backup code: use tuneRF to find mtry
if(F){
model.rf        <- randomForest(FPFL~ ., train.data, ntree=500, importance= T)
min_depth_frame <- min_depth_distribution(model.rf)

# We can use the tuneRF () function for finding the optimal parameter:
# By default, the random Forest () function uses 500 trees and randomly selected predictors 
# as potential candidates at each split. 
# These parameters can be adjusted by using the tuneRF () function. 

# XC: for the function tuneRF(), the first parameter x should not contain the response variable,
#     which is train.data$FPFL.  I corrected by using "x=train.data[,-1]".
mtry <- tuneRF(x=train.data[,-1], train.data[,1], ntreeTry = 500, stepFactor = 1.05, improve = 1E-3, trace = TRUE, plot = TRUE)
# tuneRF() returns the best optimized value of random variable is 4 corresponding to a OOB error of 9.5%.
# mtry = 4  OOB error = 9.5% 
# Searching left ...
# Searching right ...
best.m <- mtry[mtry[,2]==min(mtry[,2]),1]
}





