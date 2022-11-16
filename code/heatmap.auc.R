library(ComplexHeatmap)
library(circlize)
library(tidyverse)
library(ggplot2)
# install.packages("officer")
# install.packages("rvg")
# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("flextable")
# install.packages("xtable")
# install.packages("rgl")
# install.packages("stargazer")
# install.packages("tikzDevice")
# install.packages("xml2")
# install.packages("broom")
# install.packages("devtools")
# library(devtools)
# install.packages("export")
# devtools::install_github("tomwenseleers/export")
# https://github.com/tomwenseleers/export
library(export)

Top = HeatmapAnnotation(Subtype=anno_block(gp = gpar(fill = c('#fe6d73','#17c3b2',"#1242E4")), 
                                           labels = c('training dataset',
                                                      'validation cohort 1',"validation cohort 2"), 
                                           height = unit(6,'mm'),
                                           labels_gp = gpar(cex = 0.7, col = "white",fontface='bold')),
                        annotation_legend_param=list(labels_gp = gpar(fontsize = 10),border = T,
                                                     title_gp = gpar(fontsize = 10,fontface = "bold"),
                                                     ncol=1),
                        border = T,show_annotation_name = F)

# hh <- matrix(abs(rnorm(60,0.8,0.4)),5,12)%>%as.data.frame()
# hh[hh>1] <- 0.91

library(readxl)
hh <- read_excel("hh.21.panel.xlsx")
hh <- read_excel("hh.9.panel.xlsx")
hh <- read_excel("hh.6.panel.xlsx")
hh <- read_excel("hh.4.panel.xlsx")
hh <- data.frame(hh)
rownames(hh)<-hh$ML.method
hh<-hh[,-1]
colnames(hh)<-c(rep(c("GLM","LASSO","SVM","DT","RF","XGBoost","GBM"),3))
cell_fun = function(j, i, x, y, width, height, fill) {
  grid.text(round(hh[i, j],digits = 2), x, y, gp = gpar(fontsize = 9))}

# Step 1: Call the pdf command to start the plot
pdf(file = "21 model auc all.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 5.5) # The height of the plot in inches
pdf(file = "9 model auc all.pdf", width = 8, height = 5.5) 
pdf(file = "6 model auc all.pdf", width = 8, height = 5.5) 
pdf(file = "4 model auc all.pdf", width = 8, height = 5.5) 

# Step 2: Create the plot with R code

Heatmap(hh,name = 'Sig',
        top_annotation = Top,
        column_split = c(rep(1,7),rep(2,7),rep(3,7)),
        column_title = NULL,
        col = colorRamp2(c(0.3,0.8,1),c('#caf0f8','#90e0ef','#00b4d8')),
        cell_fun = cell_fun,
        show_row_names = T,show_column_names = T,
        row_names_side = 'left',
        column_names_rot = 60,
        column_names_gp = gpar(fontsize = 10),
        row_names_gp = gpar(fontsize = 10),
        cluster_rows = F,border = T,
        cluster_columns = F,show_heatmap_legend = F,
        gap = unit(1.2,'mm'),
        column_gap = unit(1.5,'mm'))

#graph2pdf(file='Performance-rf.pdf',height=2.8,width=5.5) #export

# Step 3: Run dev.off() to create the file!
dev.off()

