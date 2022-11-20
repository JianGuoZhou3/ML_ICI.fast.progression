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
  mutate(group_DT = DT) %>% 
  mutate(group_RF = RF) %>% 
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
  mutate(group_DT = DT) %>% 
  mutate(group_RF = RF) %>% 
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
  mutate(group_DT = DT) %>% 
  mutate(group_RF = RF) %>% 
  mutate(group_XGBoost = ifelse(XGBoost>test2_cut[6,],1,0)) %>% 
  mutate(group_GBM = ifelse(GBM>test2_cut[7,],1,0))

table(test2.roc.sur$FPFL)
table(test2.roc.sur$group_GLM)


# all cohort test by opt model------
all.data = rbind(train.data,test1.data,test2.data)
all.roc.data = data.frame(GLM=(predict(model.glm,all.data,type="response") %>% data.frame())[,1],
                          LASSO= (predict(model.lasso,newx = data.matrix(all.data[,-1])))[,1],
                          SVM=predict(model.svm,data.matrix(all.data[,-1])),
                          DT=as.numeric(predict(model.dt,all.data[,-1],type="class")),
                          RF=predict(model.rf,all.data[,-1]),
                          XGBoost=predict(model.xgb,as.matrix(all.data[,-1])),
                          GBM=predict(model.gbm,all.data[,-1],n.trees = best.iter, type = "link"))


all.roc_data = all.data %>% cbind(all.roc.data) %>% dplyr::select(1,all_of(model_name))
all.roc_data$DT<-ifelse(all.roc_data$DT==2,1,0)
all.roc_data$RF<-ifelse(all.roc_data$RF==1,1,0)
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
  scale_color_lancet() + 
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
# ggsave("Figure S_model_opt in all cohort.pdf",width = 5,height = 5)

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
  mutate(group_DT = DT) %>% 
  mutate(group_RF = RF) %>% 
  mutate(group_XGBoost = ifelse(XGBoost>all_cut[6,],1,0)) %>% 
  mutate(group_GBM = ifelse(GBM>all_cut[7,],1,0))

table(all.roc.sur$FPFL)
table(all.roc.sur$group_GLM)
all.data$FPFL %>% table()

