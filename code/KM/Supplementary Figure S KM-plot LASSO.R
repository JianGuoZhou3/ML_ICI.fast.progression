load("Top freq 3.rda")

#A prepare
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
  
  roc.list.train <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = train.roc_data)
  roc.list.test1 <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = test1.roc_data)
  roc.list.test2 <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = test2.roc_data)
  
  g.list.train <- pROC::ggroc(roc.list.train, legacy.axes = TRUE,aes=c("linetype", "color")) + 
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    scale_color_nejm() + 
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
    scale_color_nejm() + 
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
    scale_color_nejm() + 
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
    scale_color_nejm()+
    # geom_jitter(color="black", size=0.4, alpha=0.9) +
    geom_hline(yintercept=0.75, color="darkgrey", linetype="dashed")+
    ggtitle("AUC value of each models") 
  
  
  
  
  
  
  
  # train.roc.sur -----
  train.roc.sur = train.roc_data %>% tibble::rownames_to_column("id")
  
  train.roc.sur = All_Cohort %>% mutate(OS.time = OS/365*12) %>% mutate(PFS.time = PFSINV/365*12) %>% 
    dplyr::select(id,OS.time,OS.CNSR,PFS.time,PFSINV.CNSR) %>% merge(train.roc.sur,by = "id") %>% 
    dplyr::select(FPFL,all_of((model_name)),everything())
  
  # cut off in each models
  train_cut = data.frame()
  for (i in 1:7) {
    print(i)
    roc = roc(train.roc_data$FPFL ~ train.roc_data[,i+1], levels = c(0,1),direction='<')
    train_cut[i,1] = pROC::coords(roc, "best", ret=c("threshold"), transpose = FALSE)
    
    
  }
  rownames(train_cut) = model_name
  
  # train.roc.sur = train.roc.sur %>% mutate(group_GLM = (ifelse(GLM>train_cut[1,],"predict FP","predict non FP")) %>% as.factor() %>% relevel(ref = "predict non FP"))
  # train.roc.sur = train.roc.sur %>% mutate(group_GLM = ifelse(GLM>train_cut[1,],"predict FP","predict non FP"))
  
  
  train.roc.sur = train.roc.sur %>% 
    mutate(group_GLM = ifelse(GLM>train_cut[1,],1,0)) %>% 
    mutate(group_LASSO = ifelse(LASSO>train_cut[2,],1,0)) %>% 
    mutate(group_SVM = ifelse(SVM>train_cut[3,],1,0)) %>% 
    mutate(group_DT = ifelse(DT>train_cut[4,],1,0)) %>% 
    mutate(group_RF = ifelse(RF>train_cut[5,],1,0)) %>% 
    mutate(group_XGBoost = ifelse(XGBoost>train_cut[6,],1,0)) %>% 
    mutate(group_GBM = ifelse(GBM>train_cut[7,],1,0))
  
  table(train.roc.sur$FPFL)
  table(train.roc.sur$group_GLM)
  
  
  # test1.roc.sur -----
  test1.roc.sur = test1.roc_data %>% tibble::rownames_to_column("id")
  
  test1.roc.sur = All_Cohort %>% mutate(OS.time = OS/365*12) %>% mutate(PFS.time = PFSINV/365*12) %>% 
    dplyr::select(id,OS.time,OS.CNSR,PFS.time,PFSINV.CNSR) %>% merge(test1.roc.sur,by = "id") %>% 
    dplyr::select(FPFL,all_of((model_name)),everything())
  
  # cut off in each models
  test1_cut = data.frame()
  for (i in 1:7) {
    print(i)
    roc = roc(test1.roc_data$FPFL ~ test1.roc_data[,i+1], levels = c(0,1),direction='<')
    test1_cut[i,1] = pROC::coords(roc, "best", ret=c("threshold"), transpose = FALSE)
    
    
  }
  rownames(test1_cut) = model_name
  
  
  test1.roc.sur = test1.roc.sur %>% 
    mutate(group_GLM = ifelse(GLM>test1_cut[1,],1,0)) %>% 
    mutate(group_LASSO = ifelse(LASSO>test1_cut[2,],1,0)) %>% 
    mutate(group_SVM = ifelse(SVM>test1_cut[3,],1,0)) %>% 
    mutate(group_DT = ifelse(DT>test1_cut[4,],1,0)) %>% 
    mutate(group_RF = ifelse(RF>test1_cut[5,],1,0)) %>% 
    mutate(group_XGBoost = ifelse(XGBoost>test1_cut[6,],1,0)) %>% 
    mutate(group_GBM = ifelse(GBM>test1_cut[7,],1,0))
  
  table(test1.roc.sur$FPFL)
  table(test1.roc.sur$group_GLM)
  
  # test2.roc.sur -----
  
  test2.roc.sur = test2.roc_data %>% tibble::rownames_to_column("id")
  
  test2.roc.sur = All_Cohort %>% mutate(OS.time = OS/365*12) %>% mutate(PFS.time = PFSINV/365*12) %>% 
    dplyr::select(id,OS.time,OS.CNSR,PFS.time,PFSINV.CNSR) %>% merge(test2.roc.sur,by = "id") %>% 
    dplyr::select(FPFL,all_of((model_name)),everything())
  
  # cut off in each models
  test2_cut = data.frame()
  for (i in 1:7) {
    print(i)
    roc = roc(test2.roc_data$FPFL ~ test2.roc_data[,i+1], levels = c(0,1),direction='<')
    test2_cut[i,1] = pROC::coords(roc, "best", ret=c("threshold"), transpose = FALSE)
  }
  rownames(test2_cut) = model_name
  
  test2.roc.sur = test2.roc.sur %>% 
    mutate(group_GLM = ifelse(GLM>test2_cut[1,],1,0)) %>% 
    mutate(group_LASSO = ifelse(LASSO>test2_cut[2,],1,0)) %>% 
    mutate(group_SVM = ifelse(SVM>test2_cut[3,],1,0)) %>% 
    mutate(group_DT = ifelse(DT>test2_cut[4,],1,0)) %>% 
    mutate(group_RF = ifelse(RF>test2_cut[5,],1,0)) %>% 
    mutate(group_XGBoost = ifelse(XGBoost>test2_cut[6,],1,0)) %>% 
    mutate(group_GBM = ifelse(GBM>test2_cut[7,],1,0))
  
  table(test2.roc.sur$FPFL)
  table(test2.roc.sur$group_GLM)
  
  
  # all cohort test by opt model------
  load("Top freq 3.rda")
  all.data = rbind(train.data,test1.data,test2.data)
  all.roc.data = data.frame(GLM=(predict(model.glm,all.data,type="response") %>% data.frame())[,1],
                            LASSO= (predict(model.lasso,newx = data.matrix(all.data[,-1])))[,1],
                            SVM=predict(model.svm,data.matrix(all.data[,-1])),
                            DT=as.numeric(predict(model.dt,all.data[,-1],type="class")),
                            RF=predict(model.rf,all.data[,-1]),
                            XGBoost=predict(model.xgb,as.matrix(all.data[,-1])),
                            GBM=predict(model.gbm,all.data[,-1],n.trees = best.iter, type = "link"))
  
  
  all.roc_data = all.data %>% cbind(all.roc.data) %>% dplyr::select(1,all_of(model_name))
  
  all_auc = data.frame()
  for (i in 1:length(model_name)) {
    print(i)
    roc_temp = roc(all.roc_data$FPFL ~ all.roc_data[,i+1], levels = c(0,1),direction='<')
    all_auc[i,"AUC"] = auc(roc_temp)
  }
  all_auc[,"Predictor"] = model_name
  model_x_all_legend.name = paste(model_name,"AUC",round(as.numeric(all_auc$AUC),4),sep=" ")
  
  roc.list.all <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = all.roc_data)
  
  
  
  g.list.all <- pROC::ggroc(roc.list.all, legacy.axes = TRUE,aes=c("linetype", "color")) + 
    theme_bw() + # 更换黑白主题，默认为theme_grey() 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
    scale_color_nejm() + 
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
    ggtitle("ROC curve in all cohort") +  
    xlab("1-Specificity") + ylab("Sensitivity") + 
    theme(legend.position = "none")+    
    annotate("text", x = 0.6, y = 0.4-0.05*1, label = model_x_all_legend.name[1],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*2, label = model_x_all_legend.name[2],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*3, label = model_x_all_legend.name[3],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*4, label = model_x_all_legend.name[4],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*5, label = model_x_all_legend.name[5],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*6, label = model_x_all_legend.name[6],hjust = 0)+
    annotate("text", x = 0.6, y = 0.4-0.05*7, label = model_x_all_legend.name[7],hjust = 0)
  # g.list.all
  ggsave("Figure S_model_opt in all cohort.pdf",width = 5,height = 5)
  
  # all cohort predicted FP and non-FP KM plot -----
  all.roc.sur = all.roc_data %>% tibble::rownames_to_column("id")
  
  all.roc.sur = All_Cohort %>% mutate(OS.time = OS/365*12) %>% mutate(PFS.time = PFSINV/365*12) %>% 
    dplyr::select(id,OS.time,OS.CNSR,PFS.time,PFSINV.CNSR) %>% merge(all.roc.sur,by = "id") %>% 
    dplyr::select(FPFL,all_of((model_name)),everything())
  
  # cut off in each models
  all_cut = data.frame()
  for (i in 1:7) {
    print(i)
    roc = roc(all.roc_data$FPFL ~ all.roc_data[,i+1], levels = c(0,1),direction='<')
    all_cut[i,1] = pROC::coords(roc, "best", ret=c("threshold"), transpose = FALSE)
    
    
  }
  rownames(all_cut) = model_name
  
  # all.roc.sur = all.roc.sur %>% mutate(group_GLM = (ifelse(GLM>all_cut[1,],"predict FP","predict non FP")) %>% as.factor() %>% relevel(ref = "predict non FP"))
  # all.roc.sur = all.roc.sur %>% mutate(group_GLM = ifelse(GLM>all_cut[1,],"predict FP","predict non FP"))
  
  
  all.roc.sur = all.roc.sur %>% 
    mutate(group_GLM = ifelse(GLM>all_cut[1,],1,0)) %>% 
    mutate(group_LASSO = ifelse(LASSO>all_cut[2,],1,0)) %>% 
    mutate(group_SVM = ifelse(SVM>all_cut[3,],1,0)) %>% 
    mutate(group_DT = ifelse(DT>all_cut[4,],1,0)) %>% 
    mutate(group_RF = ifelse(RF>all_cut[5,],1,0)) %>% 
    mutate(group_XGBoost = ifelse(XGBoost>all_cut[6,],1,0)) %>% 
    mutate(group_GBM = ifelse(GBM>all_cut[7,],1,0))
  
  table(all.roc.sur$FPFL)
  table(all.roc.sur$group_GLM)
  
  all.data$FPFL %>% table()
  
  
}  


#B in LASSO
{
  # train OS-----
  
  # LASSO
  Figure_3a = survfit(Surv(OS.time, OS.CNSR) ~ group_LASSO, data = train.roc.sur) %>% 
    ggsurvplot(train.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Overall survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "nejm")
  res_cox = coxph(Surv(OS.time, OS.CNSR) ~ group_LASSO, data = train.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3a$plot = Figure_3a$plot+ggplot2::annotate("text",x = 17, y = 0.75,label = annotext) + 
    ggplot2::annotate("text",x = 17, y = 0.65,label = P)
  
  Figure_3a$table <- Figure_3a$table
  # Figure_3a
  
  # test1 OS-----
  
  
  # LASSO
  Figure_3c = survfit(Surv(OS.time, OS.CNSR) ~ group_LASSO, data = test1.roc.sur) %>% 
    ggsurvplot(test1.roc.sur, 
               break.time.by = 3,
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Overall survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "nejm")
  
  res_cox = coxph(Surv(OS.time, OS.CNSR) ~ group_LASSO, data = test1.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3c$plot = Figure_3c$plot+ggplot2::annotate("text",x = 12, y = 0.7,label = annotext)+ggplot2::annotate("text",x = 12, y = 0.6,label = P)
  Figure_3c$table <- Figure_3c$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3c
  # test2 OS-----
  
  # LASSO
  Figure_3e = survfit(Surv(OS.time, OS.CNSR) ~ group_LASSO, data = test2.roc.sur) %>% 
    ggsurvplot(test2.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Overall survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "nejm")
  
  res_cox = coxph(Surv(OS.time, OS.CNSR) ~ group_LASSO, data = test2.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3e$plot = Figure_3e$plot+ggplot2::annotate("text",x = 15, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 17, y = 0.65,label = P)
  Figure_3e$table <- Figure_3e$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3e
  
  # train PFS-----
  
  # LASSO
  Figure_3b = survfit(Surv(PFS.time, PFSINV.CNSR) ~ group_LASSO, data = train.roc.sur) %>% 
    ggsurvplot(train.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Progression-Free survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "nejm")
  
  res_cox = coxph(Surv(PFS.time, PFSINV.CNSR) ~ group_LASSO, data = train.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3b$plot = Figure_3b$plot+ggplot2::annotate("text",x = 18, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 18, y = 0.65,label = P)
  Figure_3b$table <- Figure_3b$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3b
  
  # test1 PFS-----
  
  
  # LASSO
  Figure_3d = survfit(Surv(PFS.time, PFSINV.CNSR) ~ group_LASSO, data = test1.roc.sur) %>% 
    ggsurvplot(test1.roc.sur, 
               break.time.by = 3,
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Progression-Free survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "nejm")
  
  res_cox = coxph(Surv(PFS.time, PFSINV.CNSR) ~ group_LASSO, data = test1.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  Figure_3d$plot = Figure_3d$plot+ggplot2::annotate("text",x = 12, y = 0.7,label = annotext)+ggplot2::annotate("text",x = 12, y = 0.6,label = P)
  Figure_3d$table <- Figure_3d$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3d
  
  # test2 PFS-----
  
  # LASSO
  Figure_3f = survfit(Surv(PFS.time, PFSINV.CNSR) ~ group_LASSO, data = test2.roc.sur) %>% 
    ggsurvplot(test2.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Progression-Free survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "nejm")
  
  res_cox = coxph(Surv(PFS.time, PFSINV.CNSR) ~ group_LASSO, data = test2.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3f$plot = Figure_3f$plot + ggplot2::annotate("text",x = 16, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 16, y = 0.65,label = P)
  Figure_3f$table <- Figure_3f$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3f
  
  
  
  
  # Figure_3g-----
  Figure_3g = survfit(Surv(OS.time, OS.CNSR) ~ group_LASSO, data = all.roc.sur) %>% 
    ggsurvplot(test2.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Overall survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "nejm")
  
  res_cox = coxph(Surv(OS.time, OS.CNSR) ~ group_LASSO, data = all.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3g$plot = Figure_3g$plot+ggplot2::annotate("text",x = 15, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 17, y = 0.65,label = P)
  Figure_3g$table <- Figure_3g$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3g
  
  # Figure_3h-----
  Figure_3h = survfit(Surv(PFS.time, PFSINV.CNSR) ~ group_LASSO, data = all.roc.sur) %>% 
    ggsurvplot(test2.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Progression-Free survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "nejm")
  
  res_cox = coxph(Surv(PFS.time, PFSINV.CNSR) ~ group_LASSO, data = all.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3h$plot = Figure_3h$plot+ggplot2::annotate("text",x = 15, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 17, y = 0.65,label = P)
  Figure_3h$table <- Figure_3h$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3h
  
  
  
  
  
}

# C patch -----
{
  surv2patch <- function(p) (p$plot / p$table) + plot_layout(heights = c(4,1))  # 调节plot与table的比例
  p1 = (surv2patch(Figure_3a)|surv2patch(Figure_3c)|surv2patch(Figure_3e)|surv2patch(Figure_3g)) 
  p2 = (surv2patch(Figure_3b)|surv2patch(Figure_3d)|surv2patch(Figure_3f)|surv2patch(Figure_3h)) 
  p1/p2 + plot_annotation(tag_levels = 'A')
  
  ggsave("Figure_S_KM_LASSO.pdf",width = 25.45,height = 12.5)
} #4*2