# train OS-----
  km.plot<- function(model_name){
    surv.os = as.formula(paste('Surv(OS.time, OS.CNSR) ~',model_name))
    surv.pfs = as.formula(paste('Surv(PFS.time, PFSINV.CNSR) ~',model_name))
    # train OS-----
    Figure_3a = surv_fit(surv.os,data = train.roc.sur) %>% 
      ggsurvplot(train.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Overall survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "lancet")
  res_cox = coxph(surv.os, data = train.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3a$plot = Figure_3a$plot+ggplot2::annotate("text",x = 17, y = 0.75,label = annotext) + 
    ggplot2::annotate("text",x = 17, y = 0.65,label = P)
  
  Figure_3a$table <- Figure_3a$table
  # Figure_3a
  
  # test1 OS-----
  # MLmethod
  Figure_3c = surv_fit(surv.os,data = test1.roc.sur) %>% 
    ggsurvplot(test1.roc.sur, 
               break.time.by = 3,
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Overall survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "lancet")
  
  res_cox = coxph(surv.os, data = test1.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3c$plot = Figure_3c$plot+ggplot2::annotate("text",x = 12, y = 0.7,label = annotext)+ggplot2::annotate("text",x = 12, y = 0.6,label = P)
  Figure_3c$table <- Figure_3c$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3c
  # test2 OS-----
  
  # MLmethod
  Figure_3e = surv_fit(surv.os, data = test2.roc.sur) %>% 
    ggsurvplot(test2.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Overall survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "lancet")
  
  res_cox = coxph(surv.os, data = test2.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3e$plot = Figure_3e$plot+ggplot2::annotate("text",x = 15, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 17, y = 0.65,label = P)
  Figure_3e$table <- Figure_3e$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3e
  
  # train PFS-----
  
  # MLmethod
  Figure_3b = surv_fit(surv.pfs, data = train.roc.sur) %>% 
    ggsurvplot(train.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Progression-Free survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "lancet")
  
  res_cox = coxph(surv.pfs, data = train.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3b$plot = Figure_3b$plot+ggplot2::annotate("text",x = 18, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 18, y = 0.65,label = P)
  Figure_3b$table <- Figure_3b$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3b
  
  # test1 PFS-----
  
  
  # MLmethod
  Figure_3d = surv_fit(surv.pfs, data = test1.roc.sur) %>% 
    ggsurvplot(test1.roc.sur, 
               break.time.by = 3,
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Progression-Free survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "lancet")
  
  res_cox = coxph(surv.pfs, data = test1.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  Figure_3d$plot = Figure_3d$plot+ggplot2::annotate("text",x = 12, y = 0.7,label = annotext)+ggplot2::annotate("text",x = 12, y = 0.6,label = P)
  Figure_3d$table <- Figure_3d$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3d
  
  # test2 PFS-----
  
  # MLmethod
  Figure_3f = surv_fit(surv.pfs, data = test2.roc.sur) %>% 
    ggsurvplot(test2.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Progression-Free survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "lancet")
  
  res_cox = coxph(surv.pfs, data = test2.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3f$plot = Figure_3f$plot + ggplot2::annotate("text",x = 16, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 16, y = 0.65,label = P)
  Figure_3f$table <- Figure_3f$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3f
  
  # Figure_3g-----
  Figure_3g = surv_fit(surv.os, data = all.roc.sur) %>% 
    ggsurvplot(test2.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Overall survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "lancet")
  
  res_cox = coxph(surv.os, data = all.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3g$plot = Figure_3g$plot+ggplot2::annotate("text",x = 15, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 17, y = 0.65,label = P)
  Figure_3g$table <- Figure_3g$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3g
  
  # Figure_3h-----
  Figure_3h = surv_fit(surv.pfs, data = all.roc.sur) %>% 
    ggsurvplot(test2.roc.sur, 
               legend.title = "", legend = c(0.8,0.99), legend.labs = c("predicted non-FP", "predicted FP"),# 图例标题/位置/标签
               ylab = "Progression-Free survival (%)", xlab = "Time (Months)",title ="",surv.scale="percent", 
               surv.median.line = "hv", surv.plot.height = 0.6,
               risk.table.title = "No. at risk",risk.table = T, risk.table.fontsize = 3, risk.table.height = 0.3, 
               palette = "lancet")
  
  res_cox = coxph(surv.pfs, data = all.roc.sur)
  HR = paste("HR",round(summary(res_cox)$conf.int[1],2))
  CI = paste("(","95%CI:",round(summary(res_cox)$conf.int[3],2),"-",round(summary(res_cox)$conf.int[4],2),")",sep = "")
  P = paste("P",ifelse(round(summary(res_cox)$coef[5],4)==0,"< 0.0001",round(summary(res_cox)$coef[5],4)))
  annotext = paste(HR,CI)
  
  Figure_3h$plot = Figure_3h$plot+ggplot2::annotate("text",x = 15, y = 0.75,label = annotext)+ggplot2::annotate("text",x = 17, y = 0.65,label = P)
  Figure_3h$table <- Figure_3h$table + theme(plot.title = element_text(size = 10))+labs(x='Time (Months)')  #  同时也去掉了risk table上的strata  
  # Figure_3h
  surv2patch <- function(p) (p$plot / p$table) + plot_layout(heights = c(4,1))  # 调节plot与table的比例
  p1 = (surv2patch(Figure_3a)|surv2patch(Figure_3c)|surv2patch(Figure_3e)|surv2patch(Figure_3g)) 
  p2 = (surv2patch(Figure_3b)|surv2patch(Figure_3d)|surv2patch(Figure_3f)|surv2patch(Figure_3h)) 
  p1/p2 + plot_annotation(tag_levels = 'A')
  fig.km = paste('./Figure/Figure_S_KM_',model_name,".pdf")
  ggsave(fig.km,width = 25.45,height = 12.5)
  }
  