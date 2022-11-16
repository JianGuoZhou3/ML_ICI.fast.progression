# auc change with different parameters combinations in train, validation 1 and 2 ------
load("21_lab.rda")
{
  
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
  
  
} # get auc
ori_auc = data.frame(  model = c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM"),
                       train = train_auc$AUC,test1 = test1_auc$AUC,test2 = test2_auc$AUC)

load("Top freq 1.rda")
# load("plus Top freq 1.rda")
{
  
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
  
  
} # get auc
op1_auc = data.frame(  model = c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM"),
                       train = train_auc$AUC,test1 = test1_auc$AUC,test2 = test2_auc$AUC)


load("Top freq 2.rda")
# load("plus Top freq 2.rda")
{
  
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
  
  
} # get auc
op2_auc = data.frame(  model = c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM"),
                       train = train_auc$AUC,test1 = test1_auc$AUC,test2 = test2_auc$AUC)


load("Top freq 3.rda")
# load("plus Top freq 3.rda")
op3_auc = data.frame(  model = c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM"),
                       train = train_auc$AUC,test1 = test1_auc$AUC,test2 = test2_auc$AUC)
{
  
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
  
  
} # get auc


# plot sep------

change_in_tain <- data.frame(combination = rep(c(1, 2, 3, 4), 7),
                             ML_approach = c(model_name, model_name,model_name,model_name),
                             AUC = c(ori_auc[,2],op1_auc[,2],op2_auc[,2],op3_auc[,2])) %>% 
  mutate(ML_approach = forcats::fct_relevel(ML_approach, "GLM", "LASSO", "SVM", "DT", "RF", "XGBoost", "GBM"))

A = ggplot(change_in_tain, aes(x = combination, y = AUC, colour = ML_approach)) +
  geom_point(size = 1) +
  geom_line(aes(group = ML_approach)) +        # 按不同组别画出线条
  scale_color_lancet()+
  scale_y_continuous( limits = c(0.5, 1)) +                           # 修改y轴
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("All", "combination 1", "combination 2", "combination 3")) + # x轴
  theme_bw() + theme( legend.position = "none") + xlab("") + ylab("AUC") + 
  ggtitle("train cohort") 



change_in_test1 <- data.frame(combination = rep(c(1, 2, 3, 4), 7),
                              ML_approach = c(model_name, model_name,model_name,model_name),
                              AUC = c(ori_auc[,3],op1_auc[,3],op2_auc[,3],op3_auc[,3])) %>% 
  mutate(ML_approach = forcats::fct_relevel(ML_approach, "GLM", "LASSO", "SVM", "DT", "RF", "XGBoost", "GBM"))


B = ggplot(change_in_test1, aes(x = combination, y = AUC, colour = ML_approach)) +
  geom_point(size = 1) +
  geom_line(aes(group = ML_approach)) +        # 按不同组别画出线条
  scale_color_lancet()+
  scale_y_continuous( limits = c(0.5, 1)) +                           # 修改y轴
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("All", "combination 1", "combination 2", "combination 3")) + # x轴
  theme_bw() + theme( legend.position = "none") + xlab("") + ylab("AUC") + 
  ggtitle("validation cohort 1") 





change_in_test2 <- data.frame(combination = rep(c(1, 2, 3, 4), 7),
                              ML_approach = c(model_name, model_name,model_name,model_name),
                              AUC = c(ori_auc[,4],op1_auc[,4],op2_auc[,4],op3_auc[,4])) %>% 
  mutate(ML_approach = forcats::fct_relevel(ML_approach, "GLM", "LASSO", "SVM", "DT", "RF", "XGBoost", "GBM"))


C = ggplot(change_in_test2, aes(x = combination, y = AUC, colour = ML_approach)) +
  geom_point(size = 1) +
  geom_line(aes(group = ML_approach)) +        # 按不同组别画出线条
  scale_color_lancet()+
  scale_y_continuous( limits = c(0.5, 1)) +                           # 修改y轴
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("All", "combination 1", "combination 2", "combination 3")) + # x轴
  theme_bw() + theme( legend.position = "right") + xlab("") + ylab("AUC") + 
  ggtitle("validation cohort 2") 



A|B|C + plot_annotation(tag_levels = c('A', '1'))

ggsave("Figure S_auc changes lab only.pdf",width = 15,height = 5)