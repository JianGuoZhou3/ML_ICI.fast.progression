# Figure S 相对重要性----

load("21_lab.rda")  # source("./21_lab model.R")
library(vip)
p_glm = vip(model.glm) + ggtitle("GLM")
p_lasso = vip(model.lasso) + ggtitle("LASSO")

{
  set.seed(123)  # for reproducibility
  p_svm =  vip(model.svm,
               method = "permute", 
               target = "FPFL", 
               metric = "rsquared",
               pred_wrapper = kernlab::predict, train = train.data) + ggtitle("SVM")
} # svm

p_dt = vip(model.dt) + ggtitle("DT")
p_rf = vip(model.rf) + ggtitle("RF")
p_xgb = vip(model.xgb) + ggtitle("XGBoost")
p_gbm = vip(model.gbm) + ggtitle("GBM")

p_glm$data
p_glm$data %>% filter(Sign =="POS")

p_lasso$data
p_lasso$data %>% filter(Sign =="POS")


impvar.f = c(p_glm$data$Variable,
             # (p_glm$data %>% filter(Sign =="POS"))$Variable,
             (p_lasso$data %>% filter(Sign =="POS"))$Variable,
             p_svm$data$Variable,
             p_dt$data$Variable,
             p_rf$data$Variable,
             p_xgb$data$Variable,
             p_gbm$data$Variable)


impvar.f = table(impvar.f) %>% as.data.frame()

write.csv(impvar.f,file = "Supplementary Table 2 {frequency of relative important variables}.csv")  # 

as.formula(paste('FPFL~',
                 paste(as.character((impvar.f %>% filter(Freq>7/2))$impvar.f),collapse = '+')))
# ALB + ALT + CRP + LDH + LMR + NEUT + PLR

library(patchwork)
impplot = p_glm+p_lasso+p_svm+p_dt+p_rf+p_xgb+p_gbm + plot_annotation(tag_levels = c('A', '1')) 
impplot
ggsave("Supplementary Figure S4 relitive importacnce plot.pdf",width = 16.2,height = 10)




