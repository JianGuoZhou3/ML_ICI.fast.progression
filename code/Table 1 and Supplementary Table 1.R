# Table 1 Baseline blood test between FP and non-FP patients.----

# 次处应加上Vivli 平台上整理出该结果的代码

source("./code/clean data.R")




# 计算Table 1----
table_1_var = c("FPFL","cohort_Name",
                "RBC","HGB", "PLAT","NEUT","LYM","MONO",
                "HCT","NLR", "PLR","NMR", "LMR",
                "CRP", "ALB", "BILI", "ALT", "AST","LDH","ALP", "CREAT" ,"GLUC" ,"TSH")
library(gtsummary)
library(flextable)

All_Cohort %>% merge(Lab_data,by="id") %>% 
  dplyr::select(all_of(table_1_var)) %>%
  tbl_summary(by = FPFL) %>% 
  add_overall() %>% add_p(simutate.p.value=T) %>% 
  as_flex_table() %>% save_as_docx(path="Table 1.docx")

# add_p(
#   x,
#   test = NULL,
#   pvalue_fun = NULL,
#   group = NULL,
#   include = everything(),
#   test.args = NULL,
#   exclude = NULL,
#   ...
# )


# x	
# Object with class tbl_summary from the tbl_summary function
#
# test	
# List of formulas specifying statistical tests to perform for each variable, e.g. list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test"). Common tests include "t.test", "aov", "wilcox.test", "kruskal.test", "chisq.test", "fisher.test", and "lme4" (for clustered data). See tests for details, more tests, and instruction for implementing a custom test.
# 
# Tests default to "kruskal.test" for continuous variables ("wilcox.test" when "by" variable has two levels), "chisq.test.no.correct" for categorical variables with all expected cell counts >=5, and "fisher.test" for categorical variables with any expected cell count <5.
# 计算s Table 1----
S_table_1_var = c("FPFL","cohort_Name",
                "SEX.y","AGE.y","ECOGGR",
                "BBMI","BWT","BHT","BTEMP",
                "Race",
                "HIST","NEVER","PDL1.1",
                "STATUS","LUNG","BRAIN","BONE","LIVER",
                "number_metastasis","metastasis_3")


All_Cohort %>% merge(Lab_data,by="id") %>% 
  dplyr::select(all_of(S_table_1_var)) %>%
  tbl_summary(by = FPFL) %>% 
  add_overall() %>% add_p(simutate.p.value=T) %>% 
  as_flex_table() %>% save_as_docx(path="S Table 1.docx")

