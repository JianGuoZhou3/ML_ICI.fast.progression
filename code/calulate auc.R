model_name = c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM")

train.roc.data = data.frame(GLM=(predict(model.glm,train.data,type="response") %>% data.frame())[,1],
                            LASSO= (predict(model.lasso,newx = data.matrix(train.data[,-1])))[,1],
                            SVM=predict(model.svm,data.matrix(train.data[,-1])),
                            DT=as.numeric(predict(model.dt,train.data[,-1],type="class")),
                            RF=predict(model.rf,train.data[,-1]),
                            XGBoost=predict(model.xgb,as.matrix(train.data[,-1])),
                            GBM=predict(model.gbm,train.data[,-1],n.trees = best.iter, type = "link"))


test1.roc.data = data.frame(GLM=(predict(model.glm,test1.data,type="response") %>% data.frame())[,1],
                            LASSO= (predict(model.lasso,newx = data.matrix(test1.data[,-1])))[,1],
                            SVM=predict(model.svm,data.matrix(test1.data[,-1])),
                            DT=as.numeric(predict(model.dt,test1.data[,-1],type="class")),
                            RF=predict(model.rf,test1.data[,-1]),
                            XGBoost=predict(model.xgb,as.matrix(test1.data[,-1])),
                            GBM=predict(model.gbm,test1.data[,-1],n.trees = best.iter, type = "link"))


test2.roc.data = data.frame(GLM=(predict(model.glm,test2.data,type="response") %>% data.frame())[,1],
                            LASSO= (predict(model.lasso,newx = data.matrix(test2.data[,-1])))[,1],
                            SVM=predict(model.svm,data.matrix(test2.data[,-1])),
                            DT=as.numeric(predict(model.dt,test2.data[,-1],type="class")),
                            RF=predict(model.rf,test2.data[,-1]),
                            XGBoost=predict(model.xgb,as.matrix(test2.data[,-1])),
                            GBM=predict(model.gbm,test2.data[,-1],n.trees = best.iter, type = "link"))


train.roc_data = train.data %>% cbind(train.roc.data) %>% dplyr::select(1,all_of(model_name))
test1.roc_data = test1.data %>% cbind(test1.roc.data) %>% dplyr::select(1,all_of(model_name))
test2.roc_data = test2.data %>% cbind(test2.roc.data) %>% dplyr::select(1,all_of(model_name))
train.roc_data$DT<-ifelse(train.roc_data$DT==1,0,1)
test1.roc_data$DT<-ifelse(test1.roc_data$DT==1,0,1)
test2.roc_data$DT<-ifelse(test2.roc_data$DT==1,0,1)
train.roc_data$RF<-ifelse(train.roc_data$RF==1,1,0)
test1.roc_data$RF<-ifelse(test1.roc_data$RF==1,1,0)
test2.roc_data$RF<-ifelse(test2.roc_data$RF==1,1,0)

train_auc = data.frame()
for (i in 1:length(model_name)) {
  print(i)
  roc_temp = roc(train.roc_data$FPFL ~ train.roc_data[,i+1], levels = c(0,1),direction='<')
  train_auc[i,"AUC"] = auc(roc_temp)
}
train_auc[,"Predictor"] = model_name
model_x_train_legend.name = paste(model_name,"AUC",round(as.numeric(train_auc$AUC),4),sep=" ")

test1_auc = data.frame()
for (i in 1:length(model_name)) {
  print(i)
  roc_temp = roc(test1.roc_data$FPFL ~ test1.roc_data[,i+1], levels = c(0,1),direction='<')
  test1_auc[i,"AUC"] = auc(roc_temp)
}
test1_auc[,"Predictor"] = model_name
model_x_test1_legend.name = paste(model_name,"AUC",round(as.numeric(test1_auc$AUC),4),sep=" ")

test2_auc = data.frame()
for (i in 1:length(model_name)) {
  print(i)
  roc_temp = roc(test2.roc_data$FPFL ~ test2.roc_data[,i+1], levels = c(0,1),direction='<')
  test2_auc[i,"AUC"] = auc(roc_temp)
}
test2_auc[,"Predictor"] = model_name
model_x_test2_legend.name = paste(model_name,"AUC",round(as.numeric(test2_auc$AUC),4),sep=" ")
# get auc