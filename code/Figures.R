#####21 bio marker models######
load("21_lab.rda")   # 21 markers panel
# Calculate AUC
source("./code/calulate auc.R")
source("./code/plot.auc.R")
# save -----
up/down + plot_annotation(tag_levels = c('A', '1'))
ggsave("./Figure/21 markers model.pdf",width = 16.2,height = 10)
View(compare_means(AUC ~ Predictor, data = Barplot_data))
Barplot_data_21<-Barplot_data
Barplot_data_21$group = "21_marker"
#####21 bio marker plus clin models######
load("lab_plus_clin.rda")   # 21 markers panel
# Calculate AUC
source("./code/calulate auc.R")
source("./code/plot.auc.R")
# save -----
up/down + plot_annotation(tag_levels = c('A', '1'))
ggsave("./Figure/lab_plus_clin model.pdf",width = 16.2,height = 10)
View(compare_means(AUC ~ Predictor, data = Barplot_data))
Barplot_data_lab_plus_clin<-Barplot_data
Barplot_data_lab_plus_clin$group = "lab_plus_clin"
######9 markers model#######
load("./Top freq 1.rda")  # 9 markers panel
# Calculate AUC
source("./code/calulate auc.R")
source("./code/plot.auc.R")
# save -----
up/down + plot_annotation(tag_levels = c('A', '1'))
ggsave("./Figure/9 markers model.pdf",width = 16.2,height = 10)
Barplot_data_9 = Barplot_data
Barplot_data_9$group = "9_marker"
######6 markers model#######
load("./Top freq 2.rda")  # 6 markers panel
# Calculate AUC
source("./code/calulate auc.R")
source("./code/plot.auc.R")
# save -----
up/down + plot_annotation(tag_levels = c('A', '1'))
ggsave("./Figure/4 markers model.pdf",width = 16.2,height = 10)
Barplot_data_6 = Barplot_data
Barplot_data_6$group = "6_marker"
######4 markers model#######
load("./Top freq 3.rda")  # 4 markers panel： optimal model
# Calculate AUC
source("./code/calulate auc.R")
source("./code/plot.auc.R")
# save -----
up/down + plot_annotation(tag_levels = c('A', '1'))
ggsave("./Figure/4 markers model.pdf",width = 16.2,height = 10)
Barplot_data_4 = Barplot_data
Barplot_data_4$group = "4_marker"
# View(compare_means(AUC ~ Predictor, data = Barplot_data_4)) 
##### compare auc 4 vs 21#####
my_data = rbind(Barplot_data_4,Barplot_data_21)
colnames(my_data)
res.aov2 <- aov(AUC ~ Predictor + group , data = my_data)
summary(res.aov2)
ggplot(my_data,aes(x = Predictor, y = AUC,
                   ymin = 0, ymax = 1))+
  theme_bw() + # 更换黑白主题，默认为theme_grey() 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # 隐藏主、次网格线  
  geom_boxplot(aes(color = group))+
  scale_color_lancet()+
  # geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_hline(yintercept=0.75, color="darkgrey", linetype="dashed")+
  ggtitle("AUC value of each models") + stat_compare_means(method = "anova")
ggsave(filename="./Figure/Figure S3.Comparing the AUC values of markers models.pdf",width = 16.2,height = 10)

##########km plot for 4 marker panel#######
load("Top freq 3.rda")
# Calculate AUC
source("./code/calulate auc.R")
source("./code/plot.auc.R")
#km
source("./code/km.plot.R")
models<-c("group_GLM","group_LASSO","group_SVM","group_DT","group_RF","group_XGBoost","group_GBM")
lapply(models, km.plot)
