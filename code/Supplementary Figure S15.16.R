load("Top freq 3.rda")
colnames(All_Cohort) %>% as.data.frame %>% View()

table(All_Cohort$PDL1.1)

PDL1_1_id = (All_Cohort %>% filter(PDL1.1=="1"))$id 

{
  # up 3 roc -----
  
  
  
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
  
  
  train.roc_data = train.data %>% cbind(train.roc.data) %>% dplyr::select(1,all_of(model_name)) %>% tibble::rownames_to_column("id") %>% 
    filter(id %in% PDL1_1_id) %>% tibble::column_to_rownames("id")
  test1.roc_data = test1.data %>% cbind(test1.roc.data) %>% dplyr::select(1,all_of(model_name)) %>% tibble::rownames_to_column("id") %>% 
    filter(id %in% PDL1_1_id) %>% tibble::column_to_rownames("id")
  test2.roc_data = test2.data %>% cbind(test2.roc.data) %>% dplyr::select(1,all_of(model_name)) %>% tibble::rownames_to_column("id") %>% 
    filter(id %in% PDL1_1_id) %>% tibble::column_to_rownames("id")
  
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
  
  roc.list.train <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = train.roc_data)
  roc.list.test1 <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = test1.roc_data)
  roc.list.test2 <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = test2.roc_data)
  
  g.list.train <- pROC::ggroc(roc.list.train, legacy.axes = TRUE,aes=c("linetype", "color")) + 
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    scale_color_lancet() + 
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
    ggtitle("ROC curve in train cohort") +  
    xlab("1-Specificity") + ylab("Sensitivity") + 
    theme(legend.position = "none")+    
    annotate("text", x = 0.6, y = 0.4-0.05*1, label = model_x_train_legend.name[1],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*2, label = model_x_train_legend.name[2],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*3, label = model_x_train_legend.name[3],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*4, label = model_x_train_legend.name[4],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*5, label = model_x_train_legend.name[5],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*6, label = model_x_train_legend.name[6],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*7, label = model_x_train_legend.name[7],hjust = 0)
  
  
  g.list.test1 <- pROC::ggroc(roc.list.test1, legacy.axes = TRUE,aes=c("linetype", "color")) + 
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    scale_color_lancet() + 
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
    ggtitle("ROC curve in validation cohort 1") +  
    xlab("1-Specificity") + ylab("Sensitivity") + 
    theme(legend.position = "none")+    
    annotate("text", x = 0.6, y = 0.4-0.05*1, label = model_x_test1_legend.name[1],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*2, label = model_x_test1_legend.name[2],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*3, label = model_x_test1_legend.name[3],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*4, label = model_x_test1_legend.name[4],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*5, label = model_x_test1_legend.name[5],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*6, label = model_x_test1_legend.name[6],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*7, label = model_x_test1_legend.name[7],hjust = 0)
  
  
  g.list.test2 <- pROC::ggroc(roc.list.test2, legacy.axes = TRUE,aes=c("linetype", "color")) + 
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    scale_color_lancet() + 
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
    ggtitle("ROC curve in validation cohort 2") +  
    xlab("1-Specificity") + ylab("Sensitivity") + 
    theme(legend.position = "right")+    
    annotate("text", x = 0.6, y = 0.4-0.05*1, label = model_x_test2_legend.name[1],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*2, label = model_x_test2_legend.name[2],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*3, label = model_x_test2_legend.name[3],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*4, label = model_x_test2_legend.name[4],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*5, label = model_x_test2_legend.name[5],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*6, label = model_x_test2_legend.name[6],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*7, label = model_x_test2_legend.name[7],hjust = 0) 
  
  up = g.list.train|g.list.test1|g.list.test2
  
  
  up
  
  
  # down auc------
  
  
  Barplot_data = rbind(train_auc,test1_auc,test2_auc)
  Barplot_data$cohort = c(rep("train", 7),rep("test1",7 ),rep("test2", 7))
  Barplot_data$AUC = as.numeric(Barplot_data$AUC)
  
  Barplot_data = Barplot_data %>% 
    mutate(Predictor = forcats::fct_relevel(Predictor, "GLM", "LASSO", "SVM", "DT", "RF", "XGBoost", "GBM"))
  
  down = ggplot(Barplot_data,aes(x = Predictor, y = AUC,
                                 ymin = 0, ymax = 1))+
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    geom_boxplot(aes(color = Predictor))+
    scale_color_lancet()+
    # geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0.75, color="darkgrey", linetype="dashed")+
    ggtitle("AUC value of each models") 
  
  
  
}


up/down + plot_annotation(tag_levels = c('A', '1'))
ggsave("S15 in pdl1 +.pdf",width = 16.2,height = 10)



load("Top freq 3.rda")
PDL1_0_id = (All_Cohort %>% filter(PDL1.1=="0"))$id

{
  # up 3 roc -----
  
  
  
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
  
  
  train.roc_data = train.data %>% cbind(train.roc.data) %>% dplyr::select(1,all_of(model_name)) %>% tibble::rownames_to_column("id") %>% 
    filter(id %in% PDL1_0_id) %>% tibble::column_to_rownames("id")
  test1.roc_data = test1.data %>% cbind(test1.roc.data) %>% dplyr::select(1,all_of(model_name)) %>% tibble::rownames_to_column("id") %>% 
    filter(id %in% PDL1_0_id) %>% tibble::column_to_rownames("id")
  test2.roc_data = test2.data %>% cbind(test2.roc.data) %>% dplyr::select(1,all_of(model_name)) %>% tibble::rownames_to_column("id") %>% 
    filter(id %in% PDL1_0_id) %>% tibble::column_to_rownames("id")
  
  train_auc = data.frame()
  for (i in 1:length(model_name)) {
    print(i)
    roc_temp = roc(train.roc_data$FPFL ~ train.roc_data[,i+1], levels = c(0,1),direction='<')
    train_auc[i,"AUC"] = auc(roc_temp)
  }
  train_auc[,"Predictor"] = model_name
  model_x_train_legend.name = paste(model_name,"AUC",round(as.numeric(train_auc$AUC),4),sep=" ")
  
  
  dim(test1.roc_data)
  # test1_auc = data.frame()
  # for (i in 1:length(model_name)) {
  #   print(i)
  #   roc_temp = roc(test1.roc_data$FPFL ~ test1.roc_data[,i+1], levels = c(0,1),direction='<')
  #   test1_auc[i,"AUC"] = auc(roc_temp)
  # }
  # test1_auc[,"Predictor"] = model_name
  # model_x_test1_legend.name = paste(model_name,"AUC",round(as.numeric(test1_auc$AUC),4),sep=" ")
  
  test2_auc = data.frame()
  for (i in 1:length(model_name)) {
    print(i)
    roc_temp = roc(test2.roc_data$FPFL ~ test2.roc_data[,i+1], levels = c(0,1),direction='<')
    test2_auc[i,"AUC"] = auc(roc_temp)
  }
  test2_auc[,"Predictor"] = model_name
  model_x_test2_legend.name = paste(model_name,"AUC",round(as.numeric(test2_auc$AUC),4),sep=" ")
  
  roc.list.train <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = train.roc_data)
  # roc.list.test1 <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = test1.roc_data)
  roc.list.test2 <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = test2.roc_data)
  
  g.list.train <- pROC::ggroc(roc.list.train, legacy.axes = TRUE,aes=c("linetype", "color")) + 
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    scale_color_lancet() + 
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
    ggtitle("ROC curve in train cohort") +  
    xlab("1-Specificity") + ylab("Sensitivity") + 
    theme(legend.position = "none")+    
    annotate("text", x = 0.6, y = 0.4-0.05*1, label = model_x_train_legend.name[1],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*2, label = model_x_train_legend.name[2],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*3, label = model_x_train_legend.name[3],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*4, label = model_x_train_legend.name[4],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*5, label = model_x_train_legend.name[5],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*6, label = model_x_train_legend.name[6],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*7, label = model_x_train_legend.name[7],hjust = 0)
  
  
  # g.list.test1 <- pROC::ggroc(roc.list.test1, legacy.axes = TRUE,aes=c("linetype", "color")) + 
  #   theme_bw() + # 更换黑白主题，默认为theme_grey() 
  #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
  #   scale_color_lancet() + 
  #   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
  #   ggtitle("ROC curve in validation cohort 1") +  
  #   xlab("1-Specificity") + ylab("Sensitivity") + 
  #   theme(legend.position = "none")+    
  #   annotate("text", x = 0.6, y = 0.4-0.05*1, label = model_x_test1_legend.name[1],hjust = 0)+
  #   annotate("text", x = 0.6, y = 0.4-0.05*2, label = model_x_test1_legend.name[2],hjust = 0)+
  #   annotate("text", x = 0.6, y = 0.4-0.05*3, label = model_x_test1_legend.name[3],hjust = 0)+
  #   annotate("text", x = 0.6, y = 0.4-0.05*4, label = model_x_test1_legend.name[4],hjust = 0)+
  #   annotate("text", x = 0.6, y = 0.4-0.05*5, label = model_x_test1_legend.name[5],hjust = 0)+
  #   annotate("text", x = 0.6, y = 0.4-0.05*6, label = model_x_test1_legend.name[6],hjust = 0)+
  #   annotate("text", x = 0.6, y = 0.4-0.05*7, label = model_x_test1_legend.name[7],hjust = 0)
  # 
  
  g.list.test2 <- pROC::ggroc(roc.list.test2, legacy.axes = TRUE,aes=c("linetype", "color")) + 
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    scale_color_lancet() + 
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
    ggtitle("ROC curve in validation cohort 2") +  
    xlab("1-Specificity") + ylab("Sensitivity") + 
    theme(legend.position = "right")+    
    annotate("text", x = 0.6, y = 0.4-0.05*1, label = model_x_test2_legend.name[1],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*2, label = model_x_test2_legend.name[2],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*3, label = model_x_test2_legend.name[3],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*4, label = model_x_test2_legend.name[4],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*5, label = model_x_test2_legend.name[5],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*6, label = model_x_test2_legend.name[6],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*7, label = model_x_test2_legend.name[7],hjust = 0) 
  
  up = g.list.train|g.list.test2
  
  
  up
  
  
  # down auc------
  
  
  Barplot_data = rbind(train_auc,test1_auc,test2_auc)
  Barplot_data$cohort = c(rep("train", 7),rep("test1",7 ),rep("test2", 7))
  Barplot_data$AUC = as.numeric(Barplot_data$AUC)
  
  Barplot_data = Barplot_data %>% 
    mutate(Predictor = forcats::fct_relevel(Predictor, "GLM", "LASSO", "SVM", "DT", "RF", "XGBoost", "GBM"))
  
  down = ggplot(Barplot_data,aes(x = Predictor, y = AUC,
                                 ymin = 0, ymax = 1))+
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    geom_boxplot(aes(color = Predictor))+
    scale_color_lancet()+
    # geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0.75, color="darkgrey", linetype="dashed")+
    ggtitle("AUC value of each models") 
  
  
  
}


up/down + plot_annotation(tag_levels = c('A', '1'))

ggsave("final in pdl1 -.pdf",width = 16.2,height = 10)