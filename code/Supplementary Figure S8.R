

load("Top freq 3.rda")  # 4 markers panel： otpimal model

combined.data = rbind(train.data,test1.data,test2.data)


combined.roc.data = data.frame(GLM=(predict(model.glm,combined.data,type="response") %>% data.frame())[,1],
                            LASSO= (predict(model.lasso,newx = data.matrix(combined.data[,-1])))[,1],
                            SVM=predict(model.svm,data.matrix(combined.data[,-1])),
                            DT=as.numeric(predict(model.dt,combined.data[,-1],type="class")),
                            RF=predict(model.rf,combined.data[,-1]),
                            XGBoost=predict(model.xgb,as.matrix(combined.data[,-1])),
                            GBM=predict(model.gbm,combined.data[,-1],n.trees = best.iter, type = "link"))


combined.roc_data = combined.data %>% cbind(combined.roc.data) %>% dplyr::select(1,all_of(model_name))

combined_auc = data.frame()
for (i in 1:length(model_name)) {
  print(i)
  roc_temp = roc(combined.roc_data$FPFL ~ combined.roc_data[,i+1], levels = c(0,1),direction='<')
  combined_auc[i,"AUC"] = auc(roc_temp)
}
combined_auc[,"Predictor"] = model_name
model_x_combined_legend.name = paste(model_name,"AUC",round(as.numeric(combined_auc$AUC),4),sep=" ")


roc.list.combined <- pROC::roc(FPFL ~ GLM + LASSO + SVM + DT + RF + XGBoost + GBM, levels = c(0,1),direction='<',data = combined.roc_data)


g.list.combined <- pROC::ggroc(roc.list.combined, legacy.axes = TRUE,aes=c("linetype", "color")) + 
  theme_bw() + # 更换黑白主题，默认为theme_grey() 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
  scale_color_lancet() + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
  ggtitle("ROC curve in combined cohort") +  
  xlab("1-Specificity") + ylab("Sensitivity") + 
  theme(legend.position = "none")+    
  annotate("text", x = 0.6, y = 0.4-0.05*1, label = model_x_combined_legend.name[1],hjust = 0)+
  annotate("text", x = 0.6, y = 0.4-0.05*2, label = model_x_combined_legend.name[2],hjust = 0)+
  annotate("text", x = 0.6, y = 0.4-0.05*3, label = model_x_combined_legend.name[3],hjust = 0)+
  annotate("text", x = 0.6, y = 0.4-0.05*4, label = model_x_combined_legend.name[4],hjust = 0)+
  annotate("text", x = 0.6, y = 0.4-0.05*5, label = model_x_combined_legend.name[5],hjust = 0)+
  annotate("text", x = 0.6, y = 0.4-0.05*6, label = model_x_combined_legend.name[6],hjust = 0)+
  annotate("text", x = 0.6, y = 0.4-0.05*7, label = model_x_combined_legend.name[7],hjust = 0)


g.list.combined
