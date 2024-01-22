rm(list = ls())
setwd("D:/R/Breast Cancer Bone Metastases/1")

# read data
data <- read.csv('imputation_20_zhongliu.csv')

##1.基线比较，训练集(肿瘤医院)与验证集(云大昆华)比较
#install.packages("table1")
library(table1) 
data_T <- read.csv('data_table1.csv')
data_T <- read.csv('缺失值插补前后比较.csv')

#分类变量  
data_T$T_stage <- factor(data_T$T_stage)
data_T$N_stage <- factor(data_T$N_stage)
data_T$Laterality <- factor(data_T$Laterality)
data_T$ER <- factor(data_T$ER)
data_T$PR <- factor(data_T$PR)
data_T$HER2 <- factor(data_T$HER2)
data_T$Ki67 <- factor(data_T$Ki67)
data_T$Molecular_typing <- factor(data_T$Molecular_typing)
data_T$Surgery_of_primary_site <- factor(data_T$Surgery_of_primary_site)
data_T$Chemotherapy_before_bone_metastasis <- factor(data_T$Chemotherapy_before_bone_metastasis)
data_T$Chemotherapy_after_bone_metastasis <- factor(data_T$Chemotherapy_after_bone_metastasis)
data_T$Endocrine_therapy_before_bone_metastasis <- factor(data_T$Endocrine_therapy_before_bone_metastasis)
data_T$Endocrine_therapy_after_bone_metastasis <- factor(data_T$Endocrine_therapy_after_bone_metastasis)
data_T$Targeted_therapy <- factor(data_T$Targeted_therapy)
data_T$Surgery_of_the_bone_metastasis <- factor(data_T$Surgery_of_the_bone_metastasis)
data_T$Anti_bone_metastasis_tumor_therapy <- factor(data_T$Anti_bone_metastasis_tumor_therapy)
data_T$Radiotherapy_of_primary_site <- factor(data_T$Radiotherapy_of_primary_site)
data_T$Radiotherapy_of_bone_metastasis <- factor(data_T$Radiotherapy_of_bone_metastasis)
data_T$SREs <- factor(data_T$SREs)
data_T$Metastasis_to_other_sites <- factor(data_T$Metastasis_to_other_sites)
data_T$Liver_metastasis <- factor(data_T$Liver_metastasis)
data_T$Lung_metastasis <- factor(data_T$Lung_metastasis)
data_T$Brain_metastasis <- factor(data_T$Brain_metastasis)
data_T$Survival_status <- factor(data_T$Survival_status)
#data_T$Survival_status <- as.numeric(data_T$Survival_status)

#数值变量转换
data_T$Survival_time <- as.numeric(data_T$Survival_time)
data_T$Age <- as.numeric(data_T$Age)
data_T$Number_of_chemotherapy_cycles_before_bone_metastasis <- as.numeric(data_T$Number_of_chemotherapy_cycles_before_bone_metastasis)
data_T$Number_of_chemotherapy_cycles_after_bone_metastasis <- as.numeric(data_T$Number_of_chemotherapy_cycles_after_bone_metastasis)
data_T$ALP <- as.numeric(data_T$ALP)
data_T$GGT <- as.numeric(data_T$GGT)
data_T$ALT <- as.numeric(data_T$ALT)
data_T$AST <- as.numeric(data_T$AST)
data_T$Albumin <- as.numeric(data_T$Albumin)
data_T$Total_protein <- as.numeric(data_T$Total_protein)
data_T$Platelet <- as.numeric(data_T$Platelet)
data_T$Hemoglobin <- as.numeric(data_T$Hemoglobin)


#训练集与验证集基线资料
#install.packages("scitb")
#install.packages("stringi")
#install.packages("nortest")
library(scitb)
data_T$Group<-as.factor(data_T$Group)
str(data_T)
allVars<-c("Age","BMI","T_stage","N_stage","Laterality",
           "ER.","ER","PR.","PR","HER2",
           "Ki67.","Ki67","Molecular_typing","Tumor_size","Surgery_of_primary_site",
           "Chemotherapy_before_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis","Chemotherapy_after_bone_metastasis","Endocrine_therapy_before_bone_metastasis",
           "Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Surgery_of_the_bone_metastasis","Anti_bone_metastasis_tumor_therapy","Radiotherapy_of_primary_site",
           "Radiotherapy_of_bone_metastasis","SREs","Metastasis_to_other_sites","Liver_metastasis","Lung_metastasis",
           "Brain_metastasis","ALP","GGT","ALT","AST",
           "Albumin","Total_protein","Platelet","Hemoglobin","RBC",
           "Neutrophil","Lymphocytes","PLR","NLR","SII",
           "PNI","WBC","CA724","CA199","CA153",
           "CA125","AFP","CEA","Serum_calcium","LDL",
           "HDL","Total_cholesterol","Triglyceride","Survival_time","Survival_status")#要纳入的变量
fvars<-c("T_stage","N_stage","Laterality","ER","PR","HER2","Ki67","Molecular_typing","Surgery_of_primary_site","Chemotherapy_before_bone_metastasis",
         "Chemotherapy_after_bone_metastasis","Endocrine_therapy_before_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy",
         "Surgery_of_the_bone_metastasis","Anti_bone_metastasis_tumor_therapy","Radiotherapy_of_primary_site","Radiotherapy_of_bone_metastasis","SREs",
         "Metastasis_to_other_sites","Liver_metastasis","Lung_metastasis",
         "Brain_metastasis","Survival_status")#分类变量
strata<-"Group"#分组变量

#一键生成统计结果
out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=data_T)

write.csv(out,file = 'Table1.csv')
write.csv(out,file = '插补前后比较.csv')

## 基线比较第二种，总的+训练集+验证集比较
#install.packages("gtsummary")
library(gtsummary)
#简单统计描述
#可以使用tbl_summary()汇总信息。
#直接将数据集名称放在函数中即可输出整个数据集的统计描述结果。
data_T1 <- data_T[,-2]
data_T1 %>% tbl_summary(by = Group,
                       digits = list(all_continuous() ~ 2)) %>% 
  add_p() %>% 
  add_overall() %>% 
  add_stat_label()

#色卡
mycol<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
         "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
         "#E41A1C","#377EB8","#4DAF4A","#FF7F00","#FFFF33",
         "#C51B7D","#DE77AE","#F7F7F7","#E6F5D0","#B8E186",
         "#7FBC41","#4D9221","#00A600FF","#3EBB00FF","#8BD000FF",
         "#E6E600FF","#E8C32EFF","#EBB25EFF","#EDB48EFF","#F0C9C0FF")

###
###.独立危险因素
###
# read data
dataI <- read.csv('data_table1.csv')

#分类变量  
dataI$T_stage <- factor(dataI$T_stage)
dataI$N_stage <- factor(dataI$N_stage)
dataI$Laterality <- factor(dataI$Laterality)
dataI$ER <- factor(dataI$ER)
dataI$PR <- factor(dataI$PR)
dataI$HER2 <- factor(dataI$HER2)
dataI$Ki67 <- factor(dataI$Ki67)
dataI$Molecular_typing <- factor(dataI$Molecular_typing)
dataI$Surgery_of_primary_site <- factor(dataI$Surgery_of_primary_site)
dataI$Chemotherapy_before_bone_metastasis <- factor(dataI$Chemotherapy_before_bone_metastasis)
dataI$Chemotherapy_after_bone_metastasis <- factor(dataI$Chemotherapy_after_bone_metastasis)
dataI$Endocrine_therapy_before_bone_metastasis <- factor(dataI$Endocrine_therapy_before_bone_metastasis)
dataI$Endocrine_therapy_after_bone_metastasis <- factor(dataI$Endocrine_therapy_after_bone_metastasis)
dataI$Targeted_therapy <- factor(dataI$Targeted_therapy)
dataI$Surgery_of_the_bone_metastasis <- factor(dataI$Surgery_of_the_bone_metastasis)
dataI$Anti_bone_metastasis_tumor_therapy <- factor(dataI$Anti_bone_metastasis_tumor_therapy)
dataI$Radiotherapy_of_primary_site <- factor(dataI$Radiotherapy_of_primary_site)
dataI$Radiotherapy_of_bone_metastasis <- factor(dataI$Radiotherapy_of_bone_metastasis)
dataI$SREs <- factor(dataI$SREs)
dataI$Metastasis_to_other_sites <- factor(dataI$Metastasis_to_other_sites)
dataI$Liver_metastasis <- factor(dataI$Liver_metastasis)
dataI$Lung_metastasis <- factor(dataI$Lung_metastasis)
dataI$Brain_metastasis <- factor(dataI$Brain_metastasis)
dataI$Survival_status <- factor(dataI$Survival_status)
dataI$Survival_status <- as.numeric(dataI$Survival_status)

#数值变量转换
dataI$Survival_time <- as.numeric(dataI$Survival_time)
dataI$Age <- as.numeric(dataI$Age)
dataI$Number_of_chemotherapy_cycles_before_bone_metastasis <- as.numeric(dataI$Number_of_chemotherapy_cycles_before_bone_metastasis)
dataI$Number_of_chemotherapy_cycles_after_bone_metastasis <- as.numeric(dataI$Number_of_chemotherapy_cycles_after_bone_metastasis)
dataI$ALP <- as.numeric(dataI$ALP)
dataI$GGT <- as.numeric(dataI$GGT)
dataI$ALT <- as.numeric(dataI$ALT)
dataI$AST <- as.numeric(dataI$AST)
dataI$Albumin <- as.numeric(dataI$Albumin)
dataI$Total_protein <- as.numeric(dataI$Total_protein)
dataI$Platelet <- as.numeric(dataI$Platelet)
dataI$Hemoglobin <- as.numeric(dataI$Hemoglobin)

str(dataI)
#install.packages("ezcox")
library(ezcox)
results <- ezcox(dataI, time = "Survival_time",status = "Survival_status",
                 covariates = c("Age","BMI","T_stage","N_stage","Laterality",
                                "ER.","ER","PR.","PR","HER2",
                                "Ki67.","Ki67","Molecular_typing","Tumor_size","Surgery_of_primary_site",
                                "Chemotherapy_before_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis","Chemotherapy_after_bone_metastasis","Endocrine_therapy_before_bone_metastasis",
                                "Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Surgery_of_the_bone_metastasis","Anti_bone_metastasis_tumor_therapy","Radiotherapy_of_primary_site",
                                "Radiotherapy_of_bone_metastasis","SREs","Metastasis_to_other_sites","Liver_metastasis","Lung_metastasis",
                                "Brain_metastasis","ALP","GGT","ALT","AST",
                                "Albumin","Total_protein","Platelet","Hemoglobin","RBC",
                                "Neutrophil","Lymphocytes","PLR","NLR","SII",
                                "PNI","WBC","CA724","CA199","CA153",
                                "CA125","AFP","CEA","Serum_calcium","LDL",
                                "HDL","Total_cholesterol","Triglyceride"))
write.csv(results,"Univariate Cox results(肿瘤+昆华云大).csv")
#单因素分析结果
#Age
#N_stage
#PR.
#Ki67.
#Ki67
#Chemotherapy_before_bone_metastasis
#Number_of_chemotherapy_cycles_before_bone_metastasis
#Number_of_chemotherapy_cycles_after_bone_metastasis
#Chemotherapy_after_bone_metastasis
#Endocrine_therapy_after_bone_metastasis
#Targeted_therapy
#Anti_bone_metastasis_tumor_therapy
#Radiotherapy_of_bone_metastasis
#SREs
#Metastasis_to_other_sites
#Liver_metastasis
#Brain_metastasis
#ALP
#GGT
#AST
#Albumin
#Hemoglobin
#RBC
#PNI
#CA153
#CA125
#AFP
#CEA #28个


#单因素COX删选出的因素，28个
#多因素
library(survival)
#MCOX<-coxph(Surv(Survival_time,Survival_status== 2) ~ Age+N_stage+PR.+Ki67.+Ki67+
                 #Chemotherapy_before_bone_metastasis+Number_of_chemotherapy_cycles_before_bone_metastasis+Number_of_chemotherapy_cycles_after_bone_metastasis+Chemotherapy_after_bone_metastasis+Endocrine_therapy_after_bone_metastasis+
                 #Targeted_therapy+Anti_bone_metastasis_tumor_therapy+Radiotherapy_of_bone_metastasis+SREs+Metastasis_to_other_sites+
                 #Liver_metastasis+Brain_metastasis+ALP+GGT+AST+
                 #Albumin+Hemoglobin+RBC+PNI+CA153+
                 #CA125+AFP+CEA, data =dataI)#28个

#加载逐步回归包
library(MASS)
stepAIC(MCOX,direction="backward")#剔除了8个变量
MCOX <-coxph(formula = Surv(Survival_time, Survival_status == 2) ~ Age + N_stage + PR. + Ki67. + Ki67 + 
               Chemotherapy_before_bone_metastasis + Number_of_chemotherapy_cycles_before_bone_metastasis + Number_of_chemotherapy_cycles_after_bone_metastasis + Endocrine_therapy_after_bone_metastasis + Targeted_therapy + 
               Radiotherapy_of_bone_metastasis + Liver_metastasis + Brain_metastasis + ALP + GGT + 
               AST + Hemoglobin + RBC + CA153 + CA125, data = dataI)#20个变量

#计算分类及连续变量的GVIF（这个方法更简单）
#  加载所需的库
library(car)
# 使用'car'包中的vif()函数计算VIF
vif_values <- vif(MCOX)
# 打印VIF值
print(vif_values)
VIFCOX <- as.data.frame(vif_values)
#保存为逗号分割文本
write.csv(VIFCOX,file = 'VIFCOX.csv')
VIFCOX <- read.csv('VIFCOX.csv')
# 画柱形图
#色卡
mycol<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
         "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
         "#E41A1C","#377EB8","#4DAF4A","#FF7F00","#FFFF33",
         "#C51B7D","#DE77AE","#F7F7F7","#E6F5D0","#B8E186",
         "#7FBC41","#4D9221","#00A600FF","#3EBB00FF","#8BD000FF",
         "#E6E600FF","#E8C32EFF","#EBB25EFF","#EDB48EFF","#F0C9C0FF")
ggplot(aes(x=reorder(Variable,GVIF),y=GVIF,fill=Variable),data=VIFCOX)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Generalised Variance Inflation Factor(GVIF) of Univariate Cox")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 

##计算向后逐步回归法筛选完的变量的VIF值，＜10说明不存在多重共线性
#15个变量构建新数据框，用来计算VIF
data.MCOX = dataI[,c("Survival_time","Survival_status",
                     "Age","N_stage","PR.","Ki67.","Ki67", 
                     "Chemotherapy_before_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy",
                     "Radiotherapy_of_bone_metastasis","Liver_metastasis","Brain_metastasis","ALP","GGT", 
                     "AST","Hemoglobin","RBC","CA153","CA125")]#20个
# 创建了一个小函数计算每个预测变量的方差膨胀因子VIF(检测变量的多重共线性)(其实从这里开始，和2.1部分一个道理)
vif_values.MCOX <- sapply(colnames(data.MCOX)[-c(1:2)], function(var) {
  cor_var <- summary(lm(as.formula(paste(var, "~ .")), data = data.MCOX))$r.squared
  1 / (1 - cor_var)
})
# 创建包含VIF值的数据框
vif_data.MCOX <- data.frame(Variable = colnames(data.MCOX)[-c(1:2)], VIF = vif_values.MCOX)
# 打印VIF值
print(vif_data.MCOX)
vif_data.MCOX1 <-na.omit(vif_data.MCOX)
# 画柱形图
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=vif_data.MCOX1)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Variance Inflation Factor(VIF) of Univariate Cox")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 

#多变量COX
library(autoReg)
table_MCOX<-autoReg(MCOX,uni=TRUE,threshold=0.05)
library(rrtable)
table2docx(table_MCOX)
library(survminer)
ggforest(MCOX,noDigits=4)
#16个独立危险因素
#Age
#N_stage
#PR.
#Ki67.
#Chemotherapy_before_bone_metastasis
#Number_of_chemotherapy_cycles_before_bone_metastasis
#Number_of_chemotherapy_cycles_after_bone_metastasis
#Endocrine_therapy_after_bone_metastasis
#Targeted_therapy
#Liver_metastasis
#Brain_metastasis
#ALP
#AST
#Hemoglobin
#CA153
#CA125
#16个


##RCS采用第二种
#install.packages("rcssci")
#结局变量转换，结局变量必须是1,0
data3 <- dataI
data3$Survival_status <-replace(data3$Survival_status,data3$Survival_status==1,0)
data3$Survival_status <-replace(data3$Survival_status,data3$Survival_status==2,1)
#install.packages("rcssci_cox")
library(rcssci)
#Age
Age <- rcssci_cox(data=data3, y = "Survival_status",x = "Age",
                  covs=c("N_stage","PR.","Ki67.","Chemotherapy_before_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis",
                         "Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                         "ALP","AST","Hemoglobin","CA153","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#PR.
PR. <- rcssci_cox(data=data3, y = "Survival_status",x = "PR.",
                  covs=c("N_stage","Age","Ki67.","Chemotherapy_before_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis",
                         "Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                         "ALP","AST","Hemoglobin","CA153","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#Ki67.
Ki67. <- rcssci_cox(data=data3, y = "Survival_status",x = "Ki67.",
                  covs=c("N_stage","Age","PR.","Chemotherapy_before_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis",
                         "Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                         "ALP","AST","Hemoglobin","CA153","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#Number_of_chemotherapy_cycles_before_bone_metastasis
Number_of_chemotherapy_cycles_before_bone_metastasis <- rcssci_cox(data=data3, y = "Survival_status",x = "Number_of_chemotherapy_cycles_before_bone_metastasis",
                    covs=c("N_stage","Age","PR.","Chemotherapy_before_bone_metastasis","Ki67.",
                           "Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                           "ALP","AST","Hemoglobin","CA153","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#Number_of_chemotherapy_cycles_after_bone_metastasis
Number_of_chemotherapy_cycles_after_bone_metastasis <- rcssci_cox(data=data3, y = "Survival_status",x = "Number_of_chemotherapy_cycles_after_bone_metastasis",
                   covs=c("N_stage","Age","PR.","Chemotherapy_before_bone_metastasis","Ki67.",
                          "Number_of_chemotherapy_cycles_before_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                          "ALP","AST","Hemoglobin","CA153","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#ALP
ALP <- rcssci_cox(data=data3, y = "Survival_status",x = "ALP",
                   covs=c("N_stage","Age","PR.","Chemotherapy_before_bone_metastasis","Ki67.",
                          "Number_of_chemotherapy_cycles_before_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                          "Number_of_chemotherapy_cycles_after_bone_metastasis","AST","Hemoglobin","CA153","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#AST
AST <- rcssci_cox(data=data3, y = "Survival_status",x = "AST",
                  covs=c("N_stage","Age","PR.","Chemotherapy_before_bone_metastasis","Ki67.",
                         "Number_of_chemotherapy_cycles_before_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                         "Number_of_chemotherapy_cycles_after_bone_metastasis","ALP","Hemoglobin","CA153","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#Hemoglobin
Hemoglobin <- rcssci_cox(data=data3, y = "Survival_status",x = "Hemoglobin",
                  covs=c("N_stage","Age","PR.","Chemotherapy_before_bone_metastasis","Ki67.",
                         "Number_of_chemotherapy_cycles_before_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                         "Number_of_chemotherapy_cycles_after_bone_metastasis","ALP","AST","CA153","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#CA153
CA153 <- rcssci_cox(data=data3, y = "Survival_status",x = "CA153",
                         covs=c("N_stage","Age","PR.","Chemotherapy_before_bone_metastasis","Ki67.",
                                "Number_of_chemotherapy_cycles_before_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                                "Number_of_chemotherapy_cycles_after_bone_metastasis","ALP","AST","Hemoglobin","CA125"),time = "Survival_time", prob=0.1,filepath = "./RCS")
#CA125
CA125 <- rcssci_cox(data=data3, y = "Survival_status",x = "CA125",
                    covs=c("N_stage","Age","PR.","Chemotherapy_before_bone_metastasis","Ki67.",
                           "Number_of_chemotherapy_cycles_before_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                           "Number_of_chemotherapy_cycles_after_bone_metastasis","ALP","AST","Hemoglobin","CA153"),time = "Survival_time", prob=0.1,filepath = "./RCS")


###
###1.0##LASSO回归生存资料特征筛选
###
library(tidyverse)
library(glmnet)
library(survival)

data1<-data

#分类变量  
data1$T_stage <- factor(data1$T_stage)
data1$N_stage <- factor(data1$N_stage)
data1$Laterality <- factor(data1$Laterality)
data1$ER2 <- factor(data1$ER2)
data1$PR2 <- factor(data1$PR2)
data1$HER2 <- factor(data1$HER2)
data1$Ki67 <- factor(data1$Ki67)
data1$Molecular_typing <- factor(data1$Molecular_typing)
data1$Surgery_of_primary_site <- factor(data1$Surgery_of_primary_site)
data1$Chemotherapy_before_bone_metastasis <- factor(data1$Chemotherapy_before_bone_metastasis)
data1$Chemotherapy_after_bone_metastasis <- factor(data1$Chemotherapy_after_bone_metastasis)
data1$Endocrine_therapy_before_bone_metastasis <- factor(data1$Endocrine_therapy_before_bone_metastasis)
data1$Endocrine_therapy_after_bone_metastasis <- factor(data1$Endocrine_therapy_after_bone_metastasis)
data1$Targeted_therapy <- factor(data1$Targeted_therapy)
data1$Surgery_of_the_bone_metastasis <- factor(data1$Surgery_of_the_bone_metastasis)
data1$Anti_bone_metastasis_tumor_therapy <- factor(data1$Anti_bone_metastasis_tumor_therapy)
data1$Radiotherapy_of_primary_site <- factor(data1$Radiotherapy_of_primary_site)
data1$Radiotherapy_of_bone_metastasis <- factor(data1$Radiotherapy_of_bone_metastasis)
data1$SREs <- factor(data1$SREs)
data1$Metastasis_to_other_sites <- factor(data1$Metastasis_to_other_sites)
data1$Liver_metastasis <- factor(data1$Liver_metastasis)
data1$Lung_metastasis <- factor(data1$Lung_metastasis)
data1$Brain_metastasis <- factor(data1$Brain_metastasis)
data1$Survival_status <- factor(data1$Survival_status)
data1$Survival_status <- as.numeric(data1$Survival_status)

#数值变量转换
data1$Survival_time <- as.numeric(data1$Survival_time)
data1$Age <- as.numeric(data1$Age)
data1$Number_of_chemotherapy_cycles_before_bone_metastasis <- as.numeric(data1$Number_of_chemotherapy_cycles_before_bone_metastasis)
data1$Number_of_chemotherapy_cycles_after_bone_metastasis <- as.numeric(data1$Number_of_chemotherapy_cycles_after_bone_metastasis)
data1$ALP <- as.numeric(data1$ALP)
data1$GGT <- as.numeric(data1$GGT)
data1$ALT <- as.numeric(data1$ALT)
data1$AST <- as.numeric(data1$AST)
data1$Albumin <- as.numeric(data1$Albumin)
data1$Total_protein <- as.numeric(data1$Total_protein)
data1$Platelet <- as.numeric(data1$Platelet)
data1$Hemoglobin <- as.numeric(data1$Hemoglobin)

CandidateVariables <- c("Age","BMI","T_stage","N_stage","Laterality",
                        "ER1","ER2","PR1","PR2","HER2",
                        "Ki67_1","Ki67","Molecular_typing","Tumor_size","Surgery_of_primary_site",
                        "Chemotherapy_before_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis","Chemotherapy_after_bone_metastasis","Endocrine_therapy_before_bone_metastasis",
                        "Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Surgery_of_the_bone_metastasis","Anti_bone_metastasis_tumor_therapy","Radiotherapy_of_primary_site",
                        "Radiotherapy_of_bone_metastasis","SREs","Metastasis_to_other_sites","Liver_metastasis","Lung_metastasis",
                        "Brain_metastasis","ALP","GGT","ALT","AST",
                        "Albumin","Total_protein","Platelet","Hemoglobin","RBC",
                        "Neutrophil","Lymphocytes","PLR","NLR","SII",
                        "PNI","WBC","CA724","CA199","CA153",
                        "CA125","AFP","CEA","Serum_calcium","LDL",
                        "HDL","Total_cholesterol","Triglyceride")#  个分类变量，  个变量
# Make data frame into matrix
tmp.y <- Surv(data1$Survival_time,data1$Survival_status)
tmp.x <- model.matrix(~.,data1[CandidateVariables])

lasso<- glmnet(tmp.x, tmp.y, 
               family = 'cox', 
               nlambda=1000, 
               alpha=1)
set.seed(123)
lassoCV <- cv.glmnet(tmp.x, tmp.y, family = "cox",
                     type.measure = "deviance",
                     nfolds =10)

#下面我们绘制LASSO回归的变量惩罚过程及10折CV，获取最佳lambda值。
#绘制CV曲线图，选择最佳lambda值
par(mfrow=c(1,2))
plot(lasso, xvar="lambda")
plot(lassoCV)

#接着我们查看交叉验证的结果，可以看到lambda.min对应的非零特征为14个
lassoCV

#我们进一步提取这14个特征
se_lambda<-lassoCV$lambda.min #求出最小值一个标准误的λ值
se_coef<-coef(lassoCV, s = "lambda.min")##λ=最小值一个标准误时各变量的系数值
se_coef
#(Intercept)       18个变量                     .           
#Age                                                   0.0053468895
#N_stageN2                                             0.0090476207
#PR1                                                  -0.2005471767
#Ki67_1                                                0.4297675888
#Surgery_of_primary_siteYes                           -0.0497323844
#Number_of_chemotherapy_cycles_before_bone_metastasis  0.0267208021
#Number_of_chemotherapy_cycles_after_bone_metastasis  -0.0448176330
#Endocrine_therapy_after_bone_metastasisYes           -0.2385868842
#Targeted_therapyYes                                  -0.1766971042
#Anti_bone_metastasis_tumor_therapyYes                -0.0048050894
#Metastasis_to_other_sitesYes                          0.1569774285
#Liver_metastasisYes                                   0.3287202779
#Brain_metastasisYes                                   0.0843262384
#ALP                                                   0.0019302481
#GGT                                                   0.0003540165
#Hemoglobin                                           -0.0012852240
#CA153                                                 0.0009739845
#CA125                                                 0.0044738531
           


index<-which(se_coef!=0)#非零系数
coef<-se_coef[index]#对应回归系数
diffvariables=row.names(se_coef)[index]#非零变量
lasso.result.se<-cbind(diffvariables,coef)#输出结果
lasso.result.se<-as.data.frame(lasso.result.se)
lasso.result.se$coef<-as.numeric(lasso.result.se$coef)

#下面我们把非零回归系数和其对应的回归系数可视化
library(colorspace)
# 选取某一颜色画板中的4种颜色
mycol1 <- qualitative_hcl(27, palette = "Dark 3")
ggplot(aes(x=reorder(diffvariables,coef),y=coef,fill=diffvariables),data=lasso.result.se)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("LASSO identified variables")+
  scale_fill_manual(values = mycol1)+
  theme(legend.position = "")

#2.0 XGBoost特征选择

#对类别变量进行独热编码
library(caret)

# 定义独热编码函数
dummy <- dummyVars(" ~ .", data=data)

# 在数据框df上执行独热编码函数
final_data <- data.frame(predict(dummy, newdata=data))

# 查看结果
final_data

#删除第一列
final_data<-final_data[-1]#删除Patient_ID
final_data<-final_data[-1]#删除Survival_statusDead


library(xgboost)
set.seed(123)
label <- ifelse(final_data$Survival_statusDead, final_data$Survival_time, -final_data$Survival_time)
x_data <- as.matrix(final_data[,c(3:ncol(final_data))])#把data视为一个矩阵（可以把它看成是一个表格，表格有行和列），方括号里面的逗号前面表示的是行，逗号前啥都没有，表示的是所有行都选中，逗号后面的括号里面的ncol表示的是列数，ncol包着data表示选中data这个表格中的所有列数，3：表示从第三列开始。
x_label <- label
x_val <- xgb.DMatrix(x_data,
                     label =x_label)

## 训练超参数
# 设置拟合器
fitControl = trainControl(method = "cv", #cv耗时短，也可选择boot，但耗时长
                          number = 10,  #5折交叉验证
                          search = "grid" # 也可设定为random
                         )
#设置为数据框才能运行，上面的矩阵无法运行
final_data1 <- final_data
final_data1$Survival_time <- ifelse(final_data1$Survival_statusDead, final_data1$Survival_time, -final_data1$Survival_time)
final_data2 <- final_data1[,c(2:ncol(final_data1))]
caret_xgb = train(Survival_time~.,
                  data = final_data2,
                  method="xgbTree", #指定算法
                  trControl=fitControl,
                  nthread = 8,  #开8个线程
                  set.seed(123))
caret_xgb$bestTune  #查看最优参数
#nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#7      50         1 0.3     0              0.6                1         1

# data surv_xgboost
xgb_param<-list("objective"="survival:cox",
                "eval_metric" = "cox-nloglik")

xgb.model<-xgb.train(params = xgb_param,
                     data =x_val,
                     nrounds = 50,max_depth = 1,gamma=0,colsample_bytree=0.6,min_child_weight=1, subsample=1,
                     eta=0.3,
                     watchlist = list(val2 = x_val),set.seed(123))

#接着看一下xgboost模型中cox 比例风险回归的负偏对数似然随着迭代次数的变化。

#data error plot
e<-data.frame(xgb.model$evaluation_log)
ggplot(aes(x=iter,y=val2_cox_nloglik),data=e)+
  geom_point(color="pink",size=3)+
  geom_line()+
  theme_bw()

#那xgboost模型中哪些变量对于结局事件的发生比较重要呢？我们都知道xgboost是一个典型的黑箱模型，
#即失去了模型的可解释性。在我们之前的推文中我们介绍了使用SHAP值对xgboost模型做进一步解释。
#当然，今天我们的重点并不是这个。下面我们将做xgboost模型中的特征重要性评价。
imp<-xgb.importance(feature_names = colnames(x_data),model = xgb.model)
print(imp)

#以上是所有特征的Gain、Cover等值，这里我们按照第一个VIP指标排序，选择前15个重要的额变量并进行可视化。
important.variable<-imp[,1:2]
#                                                Feature        Gain      Cover Frequency
#1:  Number_of_chemotherapy_cycles_after_bone_metastasis 0.168923563 0.12003979      0.12
#2:                                                CA153 0.087830106 0.08001425      0.08
#3:                                                CA125 0.087442396 0.04000867      0.04
#4:                                                  AST 0.072487586 0.04003304      0.04
#5:                                                  PR1 0.060121639 0.07999423      0.08
#6:                                                  ALP 0.059316920 0.06001288      0.06
#7: Number_of_chemotherapy_cycles_before_bone_metastasis 0.057068062 0.04000472      0.04
#8:                                                  BMI 0.051645824 0.07997713      0.08
#9:                                               Ki67_1 0.046212835 0.05999434      0.06
#10:                                                  RBC 0.037418855 0.03999138      0.04
#11:                                                  Age 0.034328351 0.04000360      0.04
#12:                                   Liver_metastasisNo 0.033595110 0.02002266      0.02
#13:                                                  SII 0.030126364 0.02001751      0.02
#14:           Endocrine_therapy_after_bone_metastasisYes 0.028872006 0.02001902      0.02
#15:                                                  WBC 0.021374404 0.03997707      0.04
#16:                                                  GGT 0.017914920 0.02000612      0.02
#17:                                           Hemoglobin 0.015137777 0.02000393      0.02
#18:                                                  AFP 0.014219043 0.02000389      0.02
#19:                                              Albumin 0.013090947 0.02000199      0.02
#20:                                   Targeted_therapyNo 0.010493654 0.01999096      0.02
#21:            Endocrine_therapy_after_bone_metastasisNo 0.009833135 0.01998731      0.02
#22:                                   Brain_metastasisNo 0.009421012 0.01998557      0.02
#23:                                                CA724 0.009353309 0.01998386      0.02
#24:                                                  PLR 0.008371479 0.01997664      0.02
#25:                                  Liver_metastasisYes 0.007770672 0.01997555      0.02
#26:                                        Serum_calcium 0.007630031 0.01997390      0.02


library(colorspace)
# 选取某一颜色画板中的4种颜色
mycol1 <- qualitative_hcl(27, palette = "Dark 3")

ggplot(aes(x=reorder(Feature,Gain),y=Gain,fill=Feature),data=important.variable)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("important variable in XGBoost")+
  scale_fill_manual(values = mycol1)+
  theme(legend.position = "")


# 计算Shap值
library(shapviz)
library(ggplot2)
library(xgboost)

shp <- shapviz(xgb.model, X_pred = x_data, 
               X = final_data2)
# 蜂窝图，本质是散点图
sv_importance(shp, kind = "beeswarm")
# 条形图 and 散点图
sv_importance(shp, kind = "both", show_numbers = T, bee_width = 0.2)


####
####3.0 #随机森林生存资料特征筛选
#install.packages("randomForestSRC")
library(randomForestSRC)
set.seed(123)
rf.model<- rfsrc(Surv(Survival_time,Survival_statusDead) ~ ., data =final_data,#final_data进行了独热编码
                 ntree = 1000, 
                 splitrule="logrank",
                 importance = TRUE,
                 nodesize = 15)

#首先看下模型的信息
rf.model
#(OOB) Requested performance error: 0.27532139

#超参数调优（条不出最优参数）
#对于随机生存森林来说，是通过OOB error来判断的。randomForestSRC为我们提供了tune.rfsrc函数，可以同时调整mtry和nodesize两个超参数：
set.seed(123)
o <- tune.rfsrc(Surv(Survival_time,Survival_statusDead)~., data = final_data,
                ntreeTry = 1000
                )
o$optimal
## nodesize     mtry 
##      35        93

set.seed(123)
rf.model1 <- rfsrc(Surv(Survival_time,Survival_statusDead)~., data = final_data,
                   ntree = 1000,
                   mtry = 93,
                   nodesize = 35,
                   importance = T
                   )
rf.model1
#(OOB) Requested performance error: 0.30000944  


#可视化第3棵树
# 开启pdf设备
#pdf("plot.pdf")
plot(get.tree(rf.model,3))
# 关闭pdf设备
#dev.off()

#可视化树和OOB之间的关系
plot(rf.model)

#计算变量重要性的可信区间，通过多次重抽样的方法获取。
rfs.smp <- subsample(rf.model, B=1000, #重抽样次数
                     subratio = 0.2 #重抽样比例
                    )
plot.subsample(rfs.smp)


# 注意这个是 1-C，和第15行的结果是一个值
get.cindex(rf.model$yvar[,1], rf.model$yvar[,2], rf.model$predicted.oob)
## [1] 0.2753214

# 所以C-index需要自己计算
c_index <- 1 - 0.2753214
c_index
## [1] 0.7246786
#查看# IB and OOB mortality，随机生存森林给出的风险分数risk score。这个风险分数有一个专门的名字，叫mortality，通过以下方式获取：
head(rf.model$predicted)# 这个就是预测值，当概率用
# 把risk score添加到原数据集中
#data1$riskScore <- rf.model1$predicted
#dim(data1)#看有几行几列
## [1] 383   62

#不同于randomforest包，生存资料的随机森林包里面plot函数会同时给出OOB和VIP的结果。只不过不重要，我们手动提取VIP的结果。
#首先我们使用var.select函数提取VIP结果，然后我们整理成数据框方便作图。
rf.top<-var.select(rf.model)
rf.top
#$topvars
#[1] "Number_of_chemotherapy_cycles_before_bone_metastasis" "CA125"                                               
#[3] "Number_of_chemotherapy_cycles_after_bone_metastasis"  "AST"                                                 
#[5] "CA153"                                                "Hemoglobin"                                          
#[7] "RBC"                                                  "Albumin"                                             
#[9] "ALP"                                                  "GGT"                                                 
#[11] "Age"                                                  "BMI"                                                 
#[13] "Ki67_1"                                               "Platelet"                                            
#[15] "Serum_calcium"                                        "ALT"                                                 
#[17] "PLR"                                                  "HDL"                                                 
#[19] "Liver_metastasisNo"                                   "Neutrophil"                                          
#[21] "Lymphocytes"                                          "PNI"                                                 
#[23] "AFP"                                                  "NLR"                                                 
#[25] "Total_cholesterol"                                    "Liver_metastasisYes"                                 
#[27] "CA199"                                                "Total_protein"                                       
#[29] "Endocrine_therapy_after_bone_metastasisYes"           "CEA"                                                 
#[31] "WBC"                                                  "SII"                                                 
#[33] "Triglyceride"                                         "LDL"                                                 
#[35] "Metastasis_to_other_sitesYes"                         "Tumor_size"                                          
#[37] "PR1"                                                  "Metastasis_to_other_sitesNo"                         
#[39] "Endocrine_therapy_after_bone_metastasisNo"                                                                 

rf.top2<-data.frame(Feature=rf.top$topvars,vimp=rf.top$varselect[rf.top$topvars,2])

#绘图
library(ggplot2)
# 安装并加载colorspace包
#install.packages("colorspace")
library(colorspace)
# 选取某一颜色画板中的4种颜色
mycol1 <- qualitative_hcl(40, palette = "Dark 3")
mycol1
## [1] "#E16A86" "#909800" "#00AD9A" "#9183E6"


ggplot(aes(x=reorder(Feature,vimp),y=vimp,fill=Feature),data=rf.top2)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("VIP variables in Randomforest")+
  scale_fill_manual(values =mycol1)+
  theme(legend.position = "")

# 筛选重要性大于0.01的变量（15个）
rf.model$importance
res.val <- rf.model$importance[rf.model$importance > 0.009]#15个变量
plot.df <- data.frame(val = names(res.val),
                      importance = res.val)
head(plot.df)

#针对这些变量画图：
ggplot(aes(x=reorder(val,importance),y=importance,fill=val),data=plot.df)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle(" variables(importance > 0.01) in Randomforest")+
  scale_fill_manual(values =mycol)+
  theme(legend.position = "")

rf.importance<-c("Number_of_chemotherapy_cycles_after_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis","CA125","AST","CA153",                                                    
                 "RBC","ALP","Hemoglobin","Albumin","Age",                                                        
                 "Platelet","GGT","BMI","Ki67_1","Serum_calcium")                                              


#好了，我们通过三个ML算法分别筛选出了他们里面比较重要的特征，当然，
#你也可以继续选择其他算法。下一步我们取三个算法中重要基因的交集，用于后续模型建立。

#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("yanlinlin82/ggvenn")
#install.packages("ggvenn")
library(ggvenn)
#COX.Feature<-c("Age","N_stage","PR1","Ki67_1","Ki67","Chemotherapy_before_bone_metastasis",
               #"Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis",
               #"Chemotherapy_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy",
               #"Anti_bone_metastasis_tumor_therapy","Radiotherapy_of_bone_metastasis","Metastasis_to_other_sites",
               #"Liver_metastasis","Lung_metastasis","Brain_metastasis","ALP","GGT","ALT","AST","Albumin","Hemoglobin",
               #"RBC","PNI","CA199","CA153","CA125","CEA")

Topgene<- list(
  LASSO =lasso.result.se$diffvariables, 
  XGBoost=important.variable$Feature, 
  RF=rf.importance
  #RF= rf.top2$Feature#,
  #COX= COX.Feature
  )

ggvenn(
  Topgene, 
  fill_color = c("#0073C2FF", "#EFC000FF", "#CD534CFF"#,"#4DAF4A"
                 ),
  stroke_size = 0.5, set_name_size = 4
)

#基于交集基因建模，首先我们提取交集基因并重新建立数据集用于后续分析。


#hubvariables<-intersect(intersect(intersect(lasso.result.se$diffvariables,important.variable$Feature),rf.top2$Feature),COX.Feature)
#只是机器学习的，COX的删除了
hubvariables<-intersect(intersect(lasso.result.se$diffvariables,important.variable$Feature),#rf.top2$Feature
                        rf.importance)
hubvariables
#11个交集变量(39个随机森林) [1] "Age"                                                  "PR1"                                                  "Ki67_1"                                              
             #[4] "Number_of_chemotherapy_cycles_before_bone_metastasis" "Number_of_chemotherapy_cycles_after_bone_metastasis"  "Endocrine_therapy_after_bone_metastasisYes"          
             #[7] "Liver_metastasisYes"                                  "ALP"                                                  "GGT"                                                 
             #[10] "Hemoglobin"                                          "CA153"                                                "CA125"

#9个交集变量(15个随机森林) [1] "Age"                                                  "Ki67_1"                                              
            #[3] "Number_of_chemotherapy_cycles_before_bone_metastasis" "Number_of_chemotherapy_cycles_after_bone_metastasis" 
            #[5] "ALP"                                                  "GGT"                                                 
            #[7] "Hemoglobin"                                           "CA153"                                               
            #[9] "CA125"


#建模数据
#有时候用不了了，有时候可以用
modeldata<-final_data%>%
  select(Survival_statusDead, Survival_time, hubvariables)

#modeldata = final_data[,c("Survival_time","Survival_statusDead",
                          #"Age","PR1","Ki67_1","Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis",
                          #"ALP","GGT","CA153","CA125","Hemoglobin",
                          #"Endocrine_therapy_after_bone_metastasisYes")]

#下面我们直接使用这个变量进行多因素Cox回归分析，筛选有预后意义的特征，并进行模型建立。
#install.packages("devtools")
#devtools::install_github("cardiomoon/autoReg")

library(autoReg)
library(officer)
library(rrtable)
library(survival)
library(survminer)
library(regplot)
library(rms)
#不做多因素了，直接建模
#fit<-coxph(Surv(Survival_time,Survival_statusDead) ~ ., data =modeldata)
#table<-autoReg(fit,uni=TRUE,threshold=0.05)
#table2docx(table)

#多因素的结果还行，就是这个表格不太好看，我们进一步绘制森林图。
#ggforest(model=fit,noDigits=6)
#多因素有意义的变量9个：Age, PR1, Ki67_1, Number_of_chemotherapy_cycles_before_bone_metastasis, 
                     #Number_of_chemotherapy_cycles_after_bone_metastasis,CA153,CA125，ALP，Endocrine_therapy_after_bone_metastasisYes


  #11个机器学习变量直接建模型，不进行上面的多变量COX
#模型可视化及评价
#最后我们进行模型的可视化可评价。
#首先绘制列线图
#modeldata$Survival_time=modeldata$Survival_time
#res.cox=coxph(Surv(Survival_time,Survival_statusDead) ~Age+PR1+Ki67_1+Number_of_chemotherapy_cycles_before_bone_metastasis+Number_of_chemotherapy_cycles_after_bone_metastasis+
                #Endocrine_therapy_after_bone_metastasisYes+Liver_metastasisYes+ALP+GGT+Hemoglobin+
                #CA153+CA125, data =modeldata,x=TRUE)#12个变量
res.cox=coxph(Surv(Survival_time,Survival_status==2) ~Age+PR1+Ki67_1+Number_of_chemotherapy_cycles_before_bone_metastasis+ 
              Number_of_chemotherapy_cycles_after_bone_metastasis+CA153+CA125+ALP+Endocrine_therapy_after_bone_metastasis, data =data1,model = TRUE,x=TRUE)#9个变量，逐步向后回归没有删除变量


#加载逐步回归包
library(MASS)
stepAIC(res.cox,direction="backward")#1个被剔除
#res.cox <- coxph(formula = Surv(Survival_time, Survival_statusDead) ~ Age + 
                   #PR1 + Ki67_1 + Number_of_chemotherapy_cycles_before_bone_metastasis + 
                   #Number_of_chemotherapy_cycles_after_bone_metastasis + Endocrine_therapy_after_bone_metastasisYes + 
                   #Liver_metastasisYes + ALP + Hemoglobin + CA153 + CA125, data = modeldata, 
                   #x = TRUE)#11个变量，删除了GGT

#计算分类及连续变量的GVIF（这个方法更简单）
#  加载所需的库
library(car)
# 使用'car'包中的vif()函数计算VIF
vif_values <- vif(res.cox)
# 打印VIF值
print(vif_values)
VIFMML <- as.data.frame(vif_values)
#保存为逗号分割文本
write.csv(VIFMML,file = 'VIFMML.csv')
VIFMML <- read.csv('VIFMML.csv')
# 画柱形图
#色卡
mycol<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
         "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
         "#E41A1C","#377EB8","#4DAF4A","#FF7F00","#FFFF33",
         "#C51B7D","#DE77AE","#F7F7F7","#E6F5D0","#B8E186",
         "#7FBC41","#4D9221","#00A600FF","#3EBB00FF","#8BD000FF",
         "#E6E600FF","#E8C32EFF","#EBB25EFF","#EDB48EFF","#F0C9C0FF")
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=VIFMML)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Variance Inflation Factor(VIF) of MML")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 


##计算向后逐步回归法筛选完的变量的VIF值，＜10说明不存在多重共线性
#15个变量构建新数据框，用来计算VIF
data.MML = data1[,c("Survival_time","Survival_status",
                    "Age","PR1","Ki67_1","Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis",
                    "Endocrine_therapy_after_bone_metastasis","ALP","CA153","CA125")]
# 创建了一个小函数计算每个预测变量的方差膨胀因子VIF(检测变量的多重共线性)(其实从这里开始，和2.1部分一个道理)
vif_values.MML <- sapply(colnames(data.MML)[-c(1, 2)], function(var) {
  cor_var <- summary(lm(as.formula(paste(var, "~ .")), data = data.MML))$r.squared
  1 / (1 - cor_var)
})
# 创建包含VIF值的数据框
vif_data.MML <- data.frame(Variable = colnames(data.MML)[-c(1, 2)], VIF = vif_values.MML)
# 打印VIF值
print(vif_data.MML)
vif_data.MML1 <-na.omit(vif_data.MML)
# 画柱形图
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=vif_data.MML1)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Variance Inflation Factor(VIF) of MML")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "")




###
#单模型比较
#1.lasso
#取消多因素分析？直接纳入COX建模？
#fit_lasso<-coxph(Surv(Survival_time,Survival_status== 2) ~ Age+N_stage+PR1+Ki67_1+Surgery_of_primary_site+
                   #Number_of_chemotherapy_cycles_before_bone_metastasis+Number_of_chemotherapy_cycles_after_bone_metastasis+
                   #Endocrine_therapy_after_bone_metastasis+Targeted_therapy+Anti_bone_metastasis_tumor_therapy+
                   #Metastasis_to_other_sites+Liver_metastasis+Brain_metastasis+ALP+GGT+Hemoglobin+CA153+CA125, data =data1)
#table_lasso<-autoReg(fit_lasso,uni=TRUE,threshold=0.05)
#table2docx(table_lasso)
#ggforest(model=fit_lasso,noDigits=4)

#！采纳！绘制时间依赖ROC曲线评价模型的区分度
dev.off()
library(riskRegression)
Outcome1 <- "Surv(Survival_time, Survival_status== 2)"
#FinalVariables1 <- c("Age","N_stage","PR1","Ki67_1","Surgery_of_primary_site",
                     #"Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis",
                     #"Hemoglobin","CA153","CA125")#13个
FinalVariables1 <- c("Age","N_stage","PR1","Ki67_1","Surgery_of_primary_site",
                     "Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Anti_bone_metastasis_tumor_therapy",
                     "Metastasis_to_other_sites","Liver_metastasis","Brain_metastasis","ALP","GGT",                                                   
                     "Hemoglobin","CA153","CA125")#18个全部纳入
Formula1 <- formula(paste(paste(Outcome1,"~", collapse=" "), 
                         paste(FinalVariables1, collapse=" + ")))
# fit a model with all selected varaibles
#model.step1 <- coxph(Formula1, data=data1,x=TRUE)
#加载逐步回归包
library(MASS)
stepAIC(model.step1,direction="backward")#剔除了3个变量：GGT,Anti_bone_metastasis_tumor_therapy,Metastasis_to_other_sites
model.step1 <- coxph(formula = Surv(Survival_time, Survival_status == 2) ~ Age + 
        N_stage + PR1 + Ki67_1 + Surgery_of_primary_site + Number_of_chemotherapy_cycles_before_bone_metastasis + 
        Number_of_chemotherapy_cycles_after_bone_metastasis + Endocrine_therapy_after_bone_metastasis + 
        Targeted_therapy + Liver_metastasis + Brain_metastasis + 
        ALP + Hemoglobin + CA153 + CA125, data = data1, model = TRUE,x = TRUE)#15个变量,剔除了3个变量

#计算分类及连续变量的GVIF（这个方法更简单）
#  加载所需的库
library(car)
# 使用'car'包中的vif()函数计算VIF
vif_values <- vif(model.step1)
# 打印VIF值
print(vif_values)
VIFLasso <- as.data.frame(vif_values)
#保存为逗号分割文本
write.csv(VIFLasso,file = 'VIFLasso.csv')
VIFLasso <- read.csv('VIFLasso.csv')
# 画柱形图
#色卡
mycol<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
         "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
         "#E41A1C","#377EB8","#4DAF4A","#FF7F00","#FFFF33",
         "#C51B7D","#DE77AE","#F7F7F7","#E6F5D0","#B8E186",
         "#7FBC41","#4D9221","#00A600FF","#3EBB00FF","#8BD000FF",
         "#E6E600FF","#E8C32EFF","#EBB25EFF","#EDB48EFF","#F0C9C0FF")
ggplot(aes(x=reorder(Variable,GVIF),y=GVIF,fill=Variable),data=VIFLasso)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Generalised Variance Inflation Factor(GVIF) of Lasso")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 

##计算向后逐步回归法筛选完的变量的VIF值，＜10说明不存在多重共线性
#15个变量构建新数据框，用来计算VIF
data.lasso = data1[,c("Survival_time","Survival_status",
                      "Age","N_stage","PR1","Ki67_1","Surgery_of_primary_site","Number_of_chemotherapy_cycles_before_bone_metastasis", 
                      "Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis", 
                      "Targeted_therapy","Liver_metastasis","Brain_metastasis", 
                      "ALP","Hemoglobin","CA153","CA125")]
# 创建了一个小函数计算每个预测变量的方差膨胀因子VIF(检测变量的多重共线性)(其实从这里开始，和2.1部分一个道理)
vif_values.lasso <- sapply(colnames(data.lasso)[-c(1, 2)], function(var) {
  cor_var <- summary(lm(as.formula(paste(var, "~ .")), data = data.lasso))$r.squared
  1 / (1 - cor_var)
})
# 创建包含VIF值的数据框
vif_data.lasso <- data.frame(Variable = colnames(data.lasso)[-c(1, 2)], VIF = vif_values.lasso)
# 打印VIF值
print(vif_data.lasso)
vif_data.lasso1 <-na.omit(vif_data.lasso)
# 画柱形图
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=vif_data.lasso1)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Variance Inflation Factor(VIF) of lasso")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "")

#
fit_lasso1 <- Score(list("lasso"=model.step1),Surv(time = Survival_time, Survival_status== 2)~1,
             data=data1,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
###提取AUC
d <- as.data.frame(fit_lasso1$AUC$score)
a <- d[d$times %in% c(1*365,3*365,5*365),]
a$AUC_com <- paste(round(a$AUC,3),'(',round(a$lower,3),',',round(a$upper,3),')',sep='')

col = c("darkcyan","tomato","purple")  ##制作一个颜色变量
plotROC(fit_lasso1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        legend="",
        cex=1,
        auc.in.legend = T,  ##控制是否在图上显示图例
        times = 1*365)  ##绘制某个时间点         

##一个模型的多个时间点ROC,绘制多个时间点的ROC曲线，给大家介绍2种展示方式，一个是常见的ROC曲线，另外一个是ROC面积随时间变化的折线图。
##time=12
plotROC(fit_lasso1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        cex=0.5,
        legend="",
        auc.in.legend = F, ##不显示默认图例，在后面编辑新图例
        times = 1*365)
##time=36
plotROC(fit_lasso1,col=col[2],legend = '', cex=0.5,times = 3*365,auc.in.legend = F,add=T)
##time=60
plotROC(fit_lasso1,col=col[3], times = 5*365,  add=T, cex=0.5, legend = '',auc.in.legend = F)
###加上图例
leg <- paste(c("1-year AUC:","3-year AUC:","5-year AUC:"),a$AUC_com)
legend(0.6,0.2,legend=leg,cex = 0.8,bty='n',title='our model',
       col=col,lwd=3)
# 无剔除              剔除变量逐步向后COX
#1年 AUC0.828      0.828
#3年 AUC0.845      0.842 
#5年 AUC0.783      0.778

#2.XGBoost单模型
library(survival)
#fit_XGBoost<-coxph(Surv(Survival_time,Survival_status== 2) ~ Number_of_chemotherapy_cycles_after_bone_metastasis+
                   #CA125+CA153+AST+ALP+PR1+Age+BMI+Ki67_1+WBC+Triglyceride+Platelet+Number_of_chemotherapy_cycles_before_bone_metastasis+
                   #GGT+ALT+RBC+Metastasis_to_other_sites+NLR+CA724+Hemoglobin+AFP+CA199+CEA+PLR+Albumin+
                  #Neutrophil+SII+Serum_calcium+HDL+Lymphocytes, data =data1)#top30

#library(autoReg)
#table_XGBoost<-autoReg(fit_XGBoost,uni=TRUE,threshold=0.05)
#library(rrtable)
#table2docx(table_XGBoost)
#library(survminer)
#ggforest(model=fit_XGBoost,noDigits=4)

#！采纳！绘制时间依赖ROC曲线评价模型的区分度
dev.off()
library(riskRegression)
Outcome2 <- "Surv(Survival_time, Survival_status== 2)"
#FinalVariables2 <- c("Number_of_chemotherapy_cycles_after_bone_metastasis","CA125","CA153","PR1","Age","Ki67_1",
                     #"Number_of_chemotherapy_cycles_before_bone_metastasis","WBC","Metastasis_to_other_sites","Serum_calcium")
FinalVariables2 <- c("Number_of_chemotherapy_cycles_after_bone_metastasis","CA125","AST","CA153","ALP",
                     "Number_of_chemotherapy_cycles_before_bone_metastasis","SII","PR1","RBC","Liver_metastasis",
                     "BMI","GGT","Ki67_1","Age","Endocrine_therapy_after_bone_metastasis",
                     "Hemoglobin","WBC","Albumin","AFP","Serum_calcium",
                     "Targeted_therapy","CA724","Brain_metastasis","PLR","Platelet",
                     "NLR")#26个变量
Formula2 <- formula(paste(paste(Outcome2,"~", collapse=" "), 
                          paste(FinalVariables2, collapse=" + ")))
# fit a model with all selected varaibles
#model.step2 <- coxph(Formula2, data=data1,x=TRUE)
#加载逐步回归包
library(MASS)
stepAIC(model.step2,direction="backward")#剔除了8个变量
model.step2 <-coxph(formula = Surv(Survival_time, Survival_status == 2) ~ Number_of_chemotherapy_cycles_after_bone_metastasis + 
                      CA125 + CA153 + ALP + Number_of_chemotherapy_cycles_before_bone_metastasis + 
                      SII + PR1 + Liver_metastasis + Ki67_1 + Age + Endocrine_therapy_after_bone_metastasis + 
                      Hemoglobin + WBC + Albumin + Serum_calcium + Targeted_therapy + 
                      Brain_metastasis + PLR, data = data1, model = TRUE,x = TRUE)#18个变量

#计算分类及连续变量的GVIF（这个方法更简单）
#  加载所需的库
library(car)
# 使用'car'包中的vif()函数计算VIF
vif_values <- vif(model.step2)
# 打印VIF值
print(vif_values)
VIFXGBoost <- as.data.frame(vif_values)
#保存为逗号分割文本
write.csv(VIFXGBoost,file = 'VIFXGBoost.csv')
VIFXGBoost <- read.csv('VIFXGBoost.csv')
# 画柱形图
#色卡
mycol<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
         "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
         "#E41A1C","#377EB8","#4DAF4A","#FF7F00","#FFFF33",
         "#C51B7D","#DE77AE","#F7F7F7","#E6F5D0","#B8E186",
         "#7FBC41","#4D9221","#00A600FF","#3EBB00FF","#8BD000FF",
         "#E6E600FF","#E8C32EFF","#EBB25EFF","#EDB48EFF","#F0C9C0FF")
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=VIFXGBoost)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Generalised Variance Inflation Factor(GVIF) of XGBoost")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 

##计算向后逐步回归法筛选完的变量的VIF值，＜10说明不存在多重共线性
#15个变量构建新数据框，用来计算VIF
data.XGBoost = data1[,c("Survival_time","Survival_status",
                        "Number_of_chemotherapy_cycles_after_bone_metastasis","CA125","CA153","ALP",
                        "Number_of_chemotherapy_cycles_before_bone_metastasis","SII","PR1","Liver_metastasis","Ki67_1","Age",
                        "Endocrine_therapy_after_bone_metastasis","Hemoglobin","WBC","Albumin","Serum_calcium","Targeted_therapy",
                        "Brain_metastasis","PLR")]#18个
# 创建了一个小函数计算每个预测变量的方差膨胀因子VIF(检测变量的多重共线性)(其实从这里开始，和2.1部分一个道理)
vif_values.XGBoost <- sapply(colnames(data.XGBoost)[-c(1, 2)], function(var) {
  cor_var <- summary(lm(as.formula(paste(var, "~ .")), data = data.XGBoost))$r.squared
  1 / (1 - cor_var)
})
# 创建包含VIF值的数据框
vif_data.XGBoost <- data.frame(Variable = colnames(data.XGBoost)[-c(1, 2)], VIF = vif_values.XGBoost)
# 打印VIF值
print(vif_data.XGBoost)
vif_data.XGBoost1 <-na.omit(vif_data.XGBoost)
# 画柱形图
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=vif_data.XGBoost1)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Variance Inflation Factor(VIF) of XGBoost")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 

fit_XGBoost1 <- Score(list("XGBoost"=model.step2),Surv(time = Survival_time, Survival_status== 2)~1,
                    data=data1,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
###提取AUC
d <- as.data.frame(fit_XGBoost1$AUC$score)
a <- d[d$times %in% c(1*365,3*365,5*365),]
a$AUC_com <- paste(round(a$AUC,3),'(',round(a$lower,3),',',round(a$upper,3),')',sep='')

col = c("darkcyan","tomato","purple")  ##制作一个颜色变量
plotROC(fit_XGBoost1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        legend="",
        cex=1,
        auc.in.legend = T,  ##控制是否在图上显示图例
        times = 1*365)  ##绘制某个时间点         

##一个模型的多个时间点ROC,绘制多个时间点的ROC曲线，给大家介绍2种展示方式，一个是常见的ROC曲线，另外一个是ROC面积随时间变化的折线图。
##time=12
plotROC(fit_XGBoost1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        cex=0.5,
        legend="",
        auc.in.legend = F, ##不显示默认图例，在后面编辑新图例
        times = 1*365)
##time=36
plotROC(fit_XGBoost1,col=col[2],legend = '', cex=0.5,times = 3*365,auc.in.legend = F,add=T)
##time=60
plotROC(fit_XGBoost1,col=col[3], times = 5*365,  add=T, cex=0.5, legend = '',auc.in.legend = F)
###加上图例
leg <- paste(c("1-year AUC:","3-year AUC:","5-year AUC:"),a$AUC_com)
legend(0.6,0.2,legend=leg,cex = 0.8,bty='n',title='our model',
       col=col,lwd=3)
#无剔除       逐步向后剔除
#1年 AUC0.812   0.821
#3年 AUC0.848   0.845
#3年 AUC0.802   0.782



#3.randomForestSRC单模型
library(survival)

dev.off()
library(riskRegression)
Outcome3 <- "Surv(Survival_time, Survival_status== 2)"

FinalVariables3 <- c("Number_of_chemotherapy_cycles_after_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis","CA125","AST","CA153",                                                    
                     "RBC","ALP","Hemoglobin","Albumin","Age",                                                        
                     "Platelet","GGT","BMI","Ki67_1","Serum_calcium")#15个变量

Formula3 <- formula(paste(paste(Outcome3,"~", collapse=" "), 
                          paste(FinalVariables3, collapse=" + ")))
# fit a model with all selected varaibles
#model.step3 <- coxph(Formula3, data=data1,x=TRUE)
#加载逐步回归包
library(MASS)
stepAIC(model.step3,direction="backward")

model.step3 <-coxph(formula = Surv(Survival_time, Survival_status == 2) ~ Number_of_chemotherapy_cycles_after_bone_metastasis + 
                      Number_of_chemotherapy_cycles_before_bone_metastasis + CA125 + 
                      CA153 + ALP + Hemoglobin + Albumin + Age + Ki67_1 + Serum_calcium, 
                    data = data1, x = TRUE)#10个变量，剔除了5个变量

#计算分类及连续变量的GVIF（这个方法更简单）
#  加载所需的库
library(car)
# 使用'car'包中的vif()函数计算VIF
vif_values <- vif(model.step3)
# 打印VIF值
print(vif_values)
VIFrandomForestSRC <- as.data.frame(vif_values)
#保存为逗号分割文本
write.csv(VIFrandomForestSRC,file = 'VIFrandomForestSRC.csv')
VIFrandomForestSRC <- read.csv('VIFrandomForestSRC.csv')
# 画柱形图
#色卡
mycol<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
         "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
         "#E41A1C","#377EB8","#4DAF4A","#FF7F00","#FFFF33",
         "#C51B7D","#DE77AE","#F7F7F7","#E6F5D0","#B8E186",
         "#7FBC41","#4D9221","#00A600FF","#3EBB00FF","#8BD000FF",
         "#E6E600FF","#E8C32EFF","#EBB25EFF","#EDB48EFF","#F0C9C0FF")
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=VIFrandomForestSRC)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Generalised Variance Inflation Factor(GVIF) of XGBoost")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 

##计算向后逐步回归法筛选完的变量的VIF值，＜10说明不存在多重共线性
#17个变量构建新数据框，用来计算VIF
data.RandomForestSRC = data1[,c("Survival_time","Survival_status",
                             "Number_of_chemotherapy_cycles_before_bone_metastasis", 
                             "CA125","Number_of_chemotherapy_cycles_after_bone_metastasis", 
                             "CA153","Hemoglobin","Albumin","ALP","Age","Ki67_1","Serum_calcium", 
                             "Liver_metastasis","Neutrophil","Lymphocytes","Endocrine_therapy_after_bone_metastasis", 
                             #"WBC", #与中性粒细胞形成共线性，WBC的VIF最大
                             "LDL","PR1")]

data.RandomForestSRC = data1[,c("Survival_time","Survival_status",
                                "Number_of_chemotherapy_cycles_before_bone_metastasis", 
                                "CA125","Number_of_chemotherapy_cycles_after_bone_metastasis", 
                                "CA153","Hemoglobin","Albumin","ALP","Age","Ki67_1","Serum_calcium")]
# 创建了一个小函数计算每个预测变量的方差膨胀因子VIF(检测变量的多重共线性)(其实从这里开始，和2.1部分一个道理)
vif_values.RandomForestSRC <- sapply(colnames(data.RandomForestSRC)[-c(1, 2)], function(var) {
  cor_var <- summary(lm(as.formula(paste(var, "~ .")), data = data.RandomForestSRC))$r.squared
  1 / (1 - cor_var)
})
# 创建包含VIF值的数据框
vif_data.RandomForestSRC <- data.frame(Variable = colnames(data.RandomForestSRC)[-c(1, 2)], VIF = vif_values.RandomForestSRC)
# 打印VIF值
print(vif_data.RandomForestSRC)
vif_data.RandomForestSRC1 <-na.omit(vif_data.RandomForestSRC)
# 画柱形图
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=vif_data.RandomForestSRC1)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Variance Inflation Factor(VIF) of RandomForestSRC")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 


fit_randomForestSRC1 <- Score(list("randomForestSRC"=model.step3),Surv(time = Survival_time, Survival_status== 2)~1,
                      data=data1,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
###提取AUC
d <- as.data.frame(fit_randomForestSRC1$AUC$score)
a <- d[d$times %in% c(1*365,3*365,5*365),]
a$AUC_com <- paste(round(a$AUC,3),'(',round(a$lower,3),',',round(a$upper,3),')',sep='')

col = c("darkcyan","tomato","purple")  ##制作一个颜色变量
plotROC(fit_randomForestSRC1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        legend="",
        cex=1,
        auc.in.legend = T,  ##控制是否在图上显示图例
        times = 1*365)  ##绘制某个时间点         

##一个模型的多个时间点ROC,绘制多个时间点的ROC曲线，给大家介绍2种展示方式，一个是常见的ROC曲线，另外一个是ROC面积随时间变化的折线图。
##time=12
plotROC(fit_randomForestSRC1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        cex=0.5,
        legend="",
        auc.in.legend = F, ##不显示默认图例，在后面编辑新图例
        times = 1*365)
##time=36
plotROC(fit_randomForestSRC1,col=col[2],legend = '', cex=0.5,times = 3*365,auc.in.legend = F,add=T)
##time=60
plotROC(fit_randomForestSRC1,col=col[3], times = 5*365,  add=T, cex=0.5, legend = '',auc.in.legend = F)
###加上图例
leg <- paste(c("2-year AUC:","3-year AUC:","5-year AUC:"),a$AUC_com)
legend(0.6,0.2,legend=leg,cex = 0.8,bty='n',title='our model',
       col=col,lwd=3)
#无剔除        逐步向后剔除   删除了WBC(与中性粒形成共线性)
#1年 AUC0.815    0.823              0.803
#3年 AUC0.846    0.835              0.803 
#5年 AUC0.79     0.775              0.744


#4.单因素+多因素
#install.packages("ezcox")
library(ezcox)
results <- ezcox(data1, time = "Survival_time",status = "Survival_status",
                 covariates = c("Age","BMI","T_stage","N_stage","Laterality",
                                "ER1","ER2","PR1","PR2","HER2",
                                "Ki67_1","Ki67","Molecular_typing","Tumor_size","Surgery_of_primary_site",
                                "Chemotherapy_before_bone_metastasis","Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis","Chemotherapy_after_bone_metastasis","Endocrine_therapy_before_bone_metastasis",
                                "Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Surgery_of_the_bone_metastasis","Anti_bone_metastasis_tumor_therapy","Radiotherapy_of_primary_site",
                                "Radiotherapy_of_bone_metastasis","SREs","Metastasis_to_other_sites","Liver_metastasis","Lung_metastasis",
                                "Brain_metastasis","ALP","GGT","ALT","AST",
                                "Albumin","Total_protein","Platelet","Hemoglobin","RBC",
                                "Neutrophil","Lymphocytes","PLR","NLR","SII",
                                "PNI","WBC","CA724","CA199","CA153",
                                "CA125","AFP","CEA","Serum_calcium","LDL",
                                "HDL","Total_cholesterol","Triglyceride"))
write.csv(results,"Univariate Cox results.csv")
#单因素分析结果
#Age
#N_stage
#PR1
#Ki67_1
#Ki67
#Chemotherapy_before_bone_metastasis
#Number_of_chemotherapy_cycles_before_bone_metastasis
#Number_of_chemotherapy_cycles_after_bone_metastasis
#Chemotherapy_after_bone_metastasis
#Endocrine_therapy_after_bone_metastasis
#Targeted_therapy
#Anti_bone_metastasis_tumor_therapy
#Radiotherapy_of_bone_metastasis
#Metastasis_to_other_sites
#Liver_metastasis
#Lung_metastasis
#Brain_metastasis
#ALP
#GGT
#ALT
#AST
#Albumin
#Hemoglobin
#RBC
#PNI
#CA199
#CA153
#CA125
#CEA #29个


#单因素COX删选出的因素，29个
#多因素
library(survival)
#fit_COX<-coxph(Surv(Survival_time,Survival_status== 2) ~ Age+N_stage+PR1+Ki67_1+Ki67+
                 #Chemotherapy_before_bone_metastasis+Number_of_chemotherapy_cycles_before_bone_metastasis+Number_of_chemotherapy_cycles_after_bone_metastasis+Chemotherapy_after_bone_metastasis+Endocrine_therapy_after_bone_metastasis+
                 #Targeted_therapy+Anti_bone_metastasis_tumor_therapy+Radiotherapy_of_bone_metastasis+Metastasis_to_other_sites+Liver_metastasis+
                 #Lung_metastasis+Brain_metastasis+ALP+GGT+ALT+
                 #AST+Albumin+Hemoglobin+RBC+PNI+
                 #CA199+CA153+CA125+CEA, data =data1)#29个

#加载逐步回归包
library(MASS)
stepAIC(fit_COX,direction="backward")#剔除了14个变量
fit_COX <-coxph(formula = Surv(Survival_time, Survival_status == 2) ~ Age + N_stage + PR1 + Ki67_1 + Ki67 + 
                  Number_of_chemotherapy_cycles_before_bone_metastasis + Number_of_chemotherapy_cycles_after_bone_metastasis + Endocrine_therapy_after_bone_metastasis + Targeted_therapy + Liver_metastasis + 
                  Brain_metastasis + ALP + Hemoglobin + CA153 + CA125, data = data1)#15个变量


##计算向后逐步回归法筛选完的变量的VIF值，＜10说明不存在多重共线性
#15个变量构建新数据框，用来计算VIF
data.COX1 = data1[,c("Survival_time","Survival_status",
                        "Age","N_stage","PR1","Ki67_1","Ki67", 
                        "Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis",
                        "Brain_metastasis","ALP","Hemoglobin","CA153","CA125")]#15个
# 创建了一个小函数计算每个预测变量的方差膨胀因子VIF(检测变量的多重共线性)(其实从这里开始，和2.1部分一个道理)
vif_values.COX1 <- sapply(colnames(data.COX1)[-c(1, 2)], function(var) {
  cor_var <- summary(lm(as.formula(paste(var, "~ .")), data = data.COX1))$r.squared
  1 / (1 - cor_var)
})
# 创建包含VIF值的数据框
vif_data.COX1 <- data.frame(Variable = colnames(data.COX1)[-c(1, 2)], VIF = vif_values.COX1)
# 打印VIF值
print(vif_data.COX1)
vif_data.COX2 <-na.omit(vif_data.COX1)
# 画柱形图
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=vif_data.COX2)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Variance Inflation Factor(VIF) of Univariate Cox")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 

#多变量COX
library(autoReg)
table_COX<-autoReg(fit_COX,uni=TRUE,threshold=0.05)
library(rrtable)
table2docx(table_COX)
library(survminer)
ggforest(fit_COX,noDigits=4)
#13个独立危险因素


dat <- read.csv("COX筛选出的13个变量可视化.csv")
#下面我们把非零回归系数和其对应的回归系数可视化
diffvariables <- c("Ki67#",
                   "Liver metastasisYes",
                   "N stageN2",
                   "N stageN1",
                   "Brain metastasisYes",
                   "N stageN3",
                   "Number of chemotherapy cycles before bone metastasis",
                   "Age",
                   "CA125",
                   "CA153",
                   "Hemoglobin",
                   "Number of chemotherapy cycles after bone metastasis",
                   "Endocrine therapy after bone metastasisYes",
                   "Targeted therapyYes", 
                   "PR#")
ggplot(aes(x=reorder(diffvariables,coef),y=coef,fill=diffvariables),data=dat)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("COX identified variables")+
  scale_fill_manual(values = mycol1)+
  theme(legend.position = "")




##RCS第一种
#加载所需要的包
#library(ggplot2)
#install.packages('rms')
#library(rms)   

#data2 <- data1
#data2$Survival_status <-replace(data2$Survival_status,data2$Survival_status==1,0)
#data2$Survival_status <-replace(data2$Survival_status,data2$Survival_status==2,1)

# 对数据进行打包，整理
#dd <- datadist(data2) #为后续程序设定数据环境
#options(datadist='dd') #为后续程序设定数据环境

# 拟合模型ESR
#fit<- cph(Surv(Survival_time,Survival_status) ~ rcs(Number_of_chemotherapy_cycles_after_bone_metastasis,4) + CA125+CA153+Hemoglobin+Ki67_1+
            #Number_of_chemotherapy_cycles_before_bone_metastasis+Age+PR1
          #,data=data2)  # 节点数设为4
# 非线性检验
# P=0.0431,<0.05为存在非线性关系
#anova(fit)#P=0.0541,线性关系

# 查看HR预测表
# 看一下预测的HR所对因的age
#HR<-Predict(fit, Number_of_chemotherapy_cycles_after_bone_metastasis,fun=exp,ref.zero = TRUE)
#head(HR)

# 绘图
#ggplot()+
  #geom_line(data=HR, aes(Number_of_chemotherapy_cycles_after_bone_metastasis,yhat),
            #linetype="solid",size=1,alpha = 0.7,colour="#0070b9")+
  #geom_ribbon(data=HR, 
              #aes(Number_of_chemotherapy_cycles_after_bone_metastasis,ymin = lower, ymax = upper),
              #alpha = 0.1,fill="#0070b9")+
  #theme_classic()+
  #geom_hline(yintercept=1, linetype=2,size=1)+
  #geom_vline(xintercept=4.0953068,size=1,color = '#d40e8c')+ #查表HR=1对应的age
  #labs(title = "Survival Risk", x="CAR", y="HR (95%CI)") 
#可以看到，CRP＞9.4371388，死亡风险随CRP变化不是很明显（减慢）；CRP＜9.4371388前，死亡风险随CRP的增加而快速增加。



#！采纳！绘制COX时间依赖ROC曲线评价模型的区分度
dev.off()
library(riskRegression)
Outcome4 <- "Surv(Survival_time, Survival_status== 2)"
FinalVariables4 <- c("Age","N_stage","PR1","Ki67_1","Number_of_chemotherapy_cycles_before_bone_metastasis",
                     "Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis","Targeted_therapy","Liver_metastasis","Brain_metastasis",
                     "Hemoglobin","CA153","CA125")#13个
Formula4 <- formula(paste(paste(Outcome4,"~", collapse=" "), 
                          paste(FinalVariables4, collapse=" + ")))
# fit a model with all selected varaibles
model.step4 <- coxph(Formula4, data=data1,x=TRUE)
#加载逐步回归包
library(MASS)
stepAIC(model.step4,direction="backward")#再次验证，无变量剔除

#计算分类及连续变量的GVIF（这个方法更简单）
#  加载所需的库
library(car)
# 使用'car'包中的vif()函数计算VIF
vif_values <- vif(model.step4)
# 打印VIF值
print(vif_values)
VIFCox1 <- as.data.frame(vif_values)
#保存为逗号分割文本
write.csv(VIFCox1,file = 'VIFCox1.csv')
VIFCox1 <- read.csv('VIFCox1.csv')
# 画柱形图
#色卡
mycol<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3",
         "#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
         "#E41A1C","#377EB8","#4DAF4A","#FF7F00","#FFFF33",
         "#C51B7D","#DE77AE","#F7F7F7","#E6F5D0","#B8E186",
         "#7FBC41","#4D9221","#00A600FF","#3EBB00FF","#8BD000FF",
         "#E6E600FF","#E8C32EFF","#EBB25EFF","#EDB48EFF","#F0C9C0FF")
ggplot(aes(x=reorder(Variable,GVIF),y=GVIF,fill=Variable),data=VIFCox1)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Generalised Variance Inflation Factor(GVIF) of Univariate + Multivariate COX")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "") 



#计算score
fit_COX1 <- Score(list("COX"=model.step4),Surv(time = Survival_time, Survival_status== 2)~1,
                              data=data1,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
###提取AUC
d <- as.data.frame(fit_COX1$AUC$score)
a <- d[d$times %in% c(1*365,3*365,5*365),]
a$AUC_com <- paste(round(a$AUC,3),'(',round(a$lower,3),',',round(a$upper,3),')',sep='')

col = c("darkcyan","tomato","purple")  ##制作一个颜色变量
plotROC(fit_COX1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        legend="",
        cex=1,
        auc.in.legend = T,  ##控制是否在图上显示图例
        times = 1*365)  ##绘制某个时间点         

##一个模型的多个时间点ROC,绘制多个时间点的ROC曲线，给大家介绍2种展示方式，一个是常见的ROC曲线，另外一个是ROC面积随时间变化的折线图。
##time=12
plotROC(fit_COX1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        cex=0.5,
        legend="",
        auc.in.legend = F, ##不显示默认图例，在后面编辑新图例
        times = 1*365)
##time=36
plotROC(fit_COX1,col=col[2],legend = '', cex=0.5,times = 3*365,auc.in.legend = F,add=T)
##time=60
plotROC(fit_COX1,col=col[3], times = 5*365,  add=T, cex=0.5, legend = '',auc.in.legend = F)
###加上图例
leg <- paste(c("1-year AUC:","3-year AUC:","5-year AUC:"),a$AUC_com)
legend(0.6,0.2,legend=leg,cex = 0.8,bty='n',title='our model',
       col=col,lwd=3)
#1年 AUC0.774
#3年 AUC0.819
#5年 AUC0.789


########################################
####多个模型比较，挑选做列线图的模型####
########################################

#1.0 模型AIC比较；AIC,越小越好
AIC(res.cox,model.step1,model.step2,model.step3,model.step4)
#df      AIC
#res.cox      9 2718.156
#model.step1 17 2684.829
#model.step2 18 2685.336
#model.step3 10 2717.951
#model.step4 15 2689.230

###比较多个模型的区分度C-Index
#pec包外部验证
library(pec)

#！连续型数据要以数值型呈现
data1$Age <- as.numeric(data1$Age)
data1$Number_of_chemotherapy_cycles_before_bone_metastasis <- as.numeric(data1$Number_of_chemotherapy_cycles_before_bone_metastasis)
data1$Number_of_chemotherapy_cycles_after_bone_metastasis <- as.numeric(data1$Number_of_chemotherapy_cycles_after_bone_metastasis)
data1$ALP <- as.numeric(data1$ALP)
data1$GGT <- as.numeric(data1$GGT)
data1$ALT <- as.numeric(data1$ALT)
data1$AST <- as.numeric(data1$AST)
data1$Albumin <- as.numeric(data1$Albumin)
data1$Total_protein <- as.numeric(data1$Total_protein)
data1$Platelet <- as.numeric(data1$Platelet)
data1$Hemoglobin <- as.numeric(data1$Hemoglobin)


data1$Survival_time <- as.numeric(data1$Survival_time)
data1$Survival_status <- as.numeric(data1$Survival_status)
data1$Survival_time=data1$Survival_time

#res.cox1<-coxph(Surv(Survival_time,Survival_status== 2) ~Age+PR1+Ki67_1+Number_of_chemotherapy_cycles_before_bone_metastasis+Number_of_chemotherapy_cycles_after_bone_metastasis+
                  #CA153+CA125+Endocrine_therapy_after_bone_metastasis+ALP+GGT+
                  #Hemoglobin, data =data1,x=TRUE)#11个变量
#加载逐步回归包
library(MASS)
stepAIC(res.cox1,direction="backward")#剔除1个变量：GGT
res.cox1<-coxph(formula = Surv(Survival_time, Survival_status == 2) ~ Age + 
        PR1 + Ki67_1 + Number_of_chemotherapy_cycles_before_bone_metastasis + 
        Number_of_chemotherapy_cycles_after_bone_metastasis + CA153 + 
        CA125 + Endocrine_therapy_after_bone_metastasis + ALP + Hemoglobin, 
      data = data1, x = TRUE)#10个变量

##计算向后逐步回归法筛选完的变量的VIF值，＜10说明不存在多重共线性
#13个变量构建新数据框，用来计算VIF
data.MUTIPLE_ML = data1[,c("Survival_time","Survival_status",
                           "Age","PR1","Ki67_1","Number_of_chemotherapy_cycles_before_bone_metastasis","Number_of_chemotherapy_cycles_after_bone_metastasis",
                           "ALP","CA153","CA125","Hemoglobin","Endocrine_therapy_after_bone_metastasis")]
# 创建了一个小函数计算每个预测变量的方差膨胀因子VIF(检测变量的多重共线性)(其实从这里开始，和2.1部分一个道理)
vif_values.MUTIPLE_ML <- sapply(colnames(data.MUTIPLE_ML)[-c(1, 2)], function(var) {
  cor_var <- summary(lm(as.formula(paste(var, "~ .")), data = data.MUTIPLE_ML))$r.squared
  1 / (1 - cor_var)
})
# 创建包含VIF值的数据框
vif_data.MUTIPLE_ML <- data.frame(Variable = colnames(data.MUTIPLE_ML)[-c(1, 2)], VIF = vif_values.MUTIPLE_ML)
# 打印VIF值
print(vif_data.MUTIPLE_ML)
vif_data.MUTIPLE_ML1 <-na.omit(vif_data.MUTIPLE_ML)
# 画柱形图
ggplot(aes(x=reorder(Variable,VIF),y=VIF,fill=Variable),data=vif_data.MUTIPLE_ML1)+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x="")+
  ggtitle("Variance Inflation Factor(VIF) of MUTIPLE_ML")+
  scale_fill_manual(values = mycol)+
  theme(legend.position = "")

fit_Multiple_ML_methods1 <- Score(list("Multiple ML methods"=res.cox1),Surv(time = Survival_time, Survival_status== 2)~1,
             data=data1,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))

##
#时间依赖性c_index
library(pec)
c_index  <- cindex(list("Multiple ML methods"=res.cox,"lasso"=model.step1, 
                        "XGBoost"=model.step2,
                        "randomForestSRC"=model.step3
                        ,"COX"=model.step4),
                   formula=Surv(Survival_time,Survival_status== 2)~.,
                   data=data1,
                   eval.times=seq(0*365,5.7*365,36.5),
                   splitMethod="bootcv",
                   B=1000
                   )
plot(c_index,col=c("#FDB462","#377EB8","#4DAF4A","black","#C51B7D"),xlim = c(0*365,5.7*365))
#最好的是lasso

#第二种方法
#画（1,3,5年）相关性c-index
#计算C指数
library(pec)
A1<-cindex(list("Multiple ML methods"=res.cox,
                "lasso"=model.step1, 
                "XGBoost"=model.step2,
                "randomForestSRC"=model.step3,
                "COX"=model.step4),
           formula=Surv(Survival_time,Survival_status== 2)~.,
           data=data1,
           eval.times=c(1*365,2*365,3*365),
           splitMethod="bootcv",
           B=1000)
#提取C指数并进一步处理
Cindex<-as.data.frame(A1$AppCindex)
library(ggsci)
library(tidyverse)
Cindex%>%
  pivot_longer(cols = 1:5,
               names_to = "Model",
               values_to ="Value")%>%
  arrange(Model)%>%
  mutate(Time=rep(c("1-year","2-year","3-year"),5))->Cindex

Cindex$Model<-factor(Cindex$Model,levels =unique(Cindex$Model))
Cindex$Time<-factor(Cindex$Time,levels =unique(Cindex$Time))
#可视化之极坐标图
p2<-
  ggplot(data = Cindex,aes(x=Time,y=Value,fill=Model))+
  geom_bar(stat = "identity",position = "dodge",alpha=0.8,width = 0.5,color="black")+
  scale_fill_aaas()+
  labs(x="",y="C-index")+
  scale_y_continuous(limits = c(0,0.85),breaks = seq(0,0.85,0.1))+
  geom_hline(yintercept =c(0.7,0.8),linetype="dashed",linewidth=0.5)+
  theme_bw()+
  theme(panel.grid.major = element_line(linewidth = 1,color = "white"),
        axis.text = element_text(color = "black"),
        panel.border = element_rect(linewidth = 1.5),
        panel.background = element_rect(fill = "lightgrey"),
        legend.key.size = unit(0.4,'cm'),
        legend.background = element_rect(color = "black"))+
  coord_polar()
p2
#可视化之柱状图
p1<-
  ggplot(data = Cindex,aes(x=Time,y=Value,fill=Model))+
  geom_bar(stat = "identity",position = "dodge",alpha=0.8,width = 0.5,color="black")+
  scale_fill_aaas()+
  labs(x="",y="C-index")+
  scale_y_continuous(limits = c(0,0.85),breaks = seq(0,0.85,0.1),expand = c(0,0))+
  geom_hline(yintercept =c(0.7,0.8),linetype="dashed",linewidth=0.8)+
  theme_bw()+
  theme(panel.grid.major = element_line(linewidth = 1),
        axis.text = element_text(color = "black"),
        panel.border = element_rect(linewidth = 1.5),
        legend.position = "top",
        legend.key.size = unit(0.4,'cm'),
        legend.background = element_rect(color = "black",fill = "lightgrey"))
p1


###校准曲线
#第1种（采用），能显示auc及brier,不设置时间，不然AUC与前面不一致
xs <- Score(list("Multiple ML methods"=res.cox,
                 "lasso"=model.step1, 
                 "XGBoost"=model.step2,
                 "randomForestSRC"=model.step3,
                 "COX"=model.step4), 
            Surv(Survival_time,Survival_status== 2) ~ 1, 
            data = data1,
            plots="cal", 
            metrics=c("AUC","Brier"),
            split.method="bootcv",B=1000,#,
            times=2*365
            )
plotCalibration(xs, brier.in.legend=TRUE,auc.in.legend=FALSE,method = "quantile")
#第2种，不能显示Brier得分及AUC
calPlot(list("Multiple ML methods"=res.cox,
             "lasso"=model.step1, 
             "XGBoost"=model.step2,
             "randomForestSRC"=model.step3
             ,"COX"=model.step4
             ),
        time=5*365,#设置想要观察的时间点
        data=data1,#,
        splitMethod = "BootCv",
        B=1000,method = "quantile"
        )
#final model与XGBoost几乎一模一样

library(riskRegression)

###计算时间AUC
pk3<- Score(list("Multiple ML methods"=res.cox,"lasso"=model.step1, 
                 "XGBoost"=model.step2,
                 "randomForestSRC"=model.step3
                 ,"COX"=model.step4),
            formula=Surv(Survival_time,Survival_status== 2)~1,
            data=data1,
            metrics="auc", 
            null.model=F, 
            times=seq(0*365,5*365,36.5),B=1000)
auc<-plotAUC(pk3,col=c("#FDB462","#377EB8","#4DAF4A","black","#C51B7D"))
#这个图没有C-index好看

###5个模型在某个时间ROC曲线比较
f <- Score(list("Multiple ML methods"=res.cox,"lasso"=model.step1, 
                "XGBoost"=model.step2,
                "randomForestSRC"=model.step3
                ,"COX"=model.step4),Surv(Survival_time,Survival_status== 2)~1,
                data=data1,se.fit=1L,times=c(1*365,2*365,3*365), plots="roc",metrics=c("AUC"))
#绘制5个模型的在1年的ROC曲线
mycol1<-c("#FDB462","#377EB8","#4DAF4A","black","#C51B7D")
  #c("#FDB462","#BEBADA","#3EBB00FF","#80B1D3","#E41A1C")
plotROC(f, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=mycol1,#col[1:5],
        legend=c('Multiple ML methods','lasso','XGBoost','randomForestSRC','COX'), 
        times = 1*365)
#绘制5个模型的在2年的ROC曲线
plotROC(f, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=mycol1,#col[1:5],
        legend=c('Multiple ML methods','lasso','XGBoost','randomForestSRC','COX'), 
        times = 2*365)
#绘制5个模型的在3年的ROC曲线
plotROC(f, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=mycol1,#col[1:5],
        legend=c('Multiple ML methods','lasso','XGBoost','randomForestSRC','COX'), 
        times = 3*365)
#绘制5个模型的在5年的ROC曲线
plotROC(f, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=mycol1,#col[1:5],
        legend=c('Multiple ML methods','lasso','XGBoost','randomForestSRC','COX'), 
        times = 5*365)

#DCA曲线
#结局变量转换，结局变量必须是1,0
#data2 <- data1
#data2$Survival_status <-replace(data2$Survival_status,data2$Survival_status==1,0)
#data2$Survival_status <-replace(data2$Survival_status,data2$Survival_status==2,1)
library(rmda)
set.seed(123)
Multiple_ML_methods <- decision_curve(Survival_status ~ Age+PR1+Ki67_1+Number_of_chemotherapy_cycles_before_bone_metastasis+ 
                                        Number_of_chemotherapy_cycles_after_bone_metastasis+CA153+CA125+ALP+
                                        Endocrine_therapy_after_bone_metastasis, data = data2,
                                        thresholds = seq(0, 1, by = 0.05), bootstraps = 1000)#9个变量
lasso <- decision_curve(Survival_status ~ Age + N_stage + PR1 + Ki67_1 + Surgery_of_primary_site + Number_of_chemotherapy_cycles_before_bone_metastasis + 
                          Number_of_chemotherapy_cycles_after_bone_metastasis + Endocrine_therapy_after_bone_metastasis + Targeted_therapy + 
                          Liver_metastasis + Brain_metastasis + ALP + Hemoglobin + CA153 + CA125, data = data2,
                          thresholds = seq(0, 1, by = 0.05), bootstraps = 1000)#15个变量
XGBoost <- decision_curve(Survival_status ~ Number_of_chemotherapy_cycles_after_bone_metastasis + 
                            CA125 + CA153 + ALP + Number_of_chemotherapy_cycles_before_bone_metastasis + 
                            SII + PR1 + Liver_metastasis + Ki67_1 + Age + Endocrine_therapy_after_bone_metastasis + 
                            Hemoglobin + WBC + Albumin + Serum_calcium + Targeted_therapy + 
                            Brain_metastasis + PLR, data = data2,thresholds = seq(0, 1, by = 0.05), bootstraps = 1000)#18个变量
randomForestSRC <- decision_curve(Survival_status ~ Number_of_chemotherapy_cycles_after_bone_metastasis + Number_of_chemotherapy_cycles_before_bone_metastasis + 
                                    CA125 + CA153 + ALP + Hemoglobin + Albumin + Age + Ki67_1 + 
                                    Serum_calcium, data = data2,thresholds = seq(0, 1, by = 0.05), bootstraps = 1000)#10个变量
COX <- decision_curve(Survival_status ~ Age+N_stage+PR1+Ki67_1+Number_of_chemotherapy_cycles_before_bone_metastasis+Number_of_chemotherapy_cycles_after_bone_metastasis+
                      Endocrine_therapy_after_bone_metastasis+Targeted_therapy+Liver_metastasis+Brain_metastasis+
                      Hemoglobin+CA153+CA125, 
                      data = data2,thresholds = seq(0, 1, by = 0.05), bootstraps = 1000)#13个变量

list<-list(Multiple_ML_methods,lasso,XGBoost,randomForestSRC,COX)
plot_decision_curve(list, curve.names = c("Multiple ML methods","lasso","XGBoost","randomForestSRC","COX","Baseline Model"),
                    #curve.names = "Baseline Model",
                    cost.benefit.axis =F,
                    col= mycol1,
                    confidence.intervals=F,
                    standardize = T)
###计算连续性NRI    #model.step4   #res.cox1 无统计学意义，相比较于COX
#放弃
set.seed(123)
#install.packages("nricens")
library(nricens)
nricens(mdl.std=model.step4,
        mdl.new=model.step2,
        t0=365*5,
        cut=0,updown="diff",#diff表示计算连续性NRI
        niter=1000#bootstrap=1000
        )


##3.一个/多个模型的多个时间点AUC,这个图是把每个时间点的AUC求出来，然后绘制成的折线图。有可信区间显示，但是叠加在一起不好看
fit1 <- Score(list("final model"=res.cox1,"lasso"=model.step1, 
                   "XGBoost"=model.step2,
                   "randomForestSRC"=model.step3
                   ,"COX"=model.step4),Surv(Survival_time,Survival_status== 2)~1,data=data1,se.fit=1L,times=seq(0*365,5*365,36.5), plots="roc",metrics=c("AUC"))
auc<-plotAUC(fit1,conf.int = 1L,col=mycol1);auc
data1=as.matrix(data1)
setDT(as.data.frame.matrix(data1), keep.rownames = "Pr")[]
data.table(unclass(data1), keep.rownames = "Pr",skip_absent=TRUE)
#会报错,运行之后会影响前面的代码运行
A3<-riskRegression::Score(list("final model"=res.cox1,"lasso"=model.step1, 
                               "XGBoost"=model.step2,
                               "randomForestSRC"=model.step3
                               ,"COX"=model.step4),
                          formula=Surv(Survival_time,Survival_status== 2)~1,data=data1,
                          metrics="auc",
                          null.model=F,
                          times=seq(0*365,5*365,36.5),
                          splitMethod="bootcv",
                          B=1000)
plotAUC(A3)
#会报错

##5个模型的生存曲线
#重新构建数据
data_survival1 <- data1
#整合lp值
data_survival1$lp_Multiple_ML_methods <- predict(res.cox, newdata=data_survival1, type="risk")   # linear predictor, type="lp"or"risk",lp与risk（有的是负数，有的大于1）所做的分组是一样的
data_survival1$lp_lasso <- predict(model.step1, newdata=data_survival1, type="risk")             # survival 是[0,1]
data_survival1$lp_XGBoost <- predict(model.step2, newdata=data_survival1, type="risk") 
data_survival1$lp_randomForestSRC <- predict(model.step3, newdata=data_survival1, type="risk") 
data_survival1$lp_COX <- predict(model.step4, newdata=data_survival1, type="risk")
#构建lp数据
data_survival <-data_survival1[,c(1:3,62:66)]
#保存为逗号分割文本
write.csv(data_survival,file = 'data_zhongliu_survival1.csv')
#生存分析
#
data_survival <- read.csv('data_zhongliu_survival1.csv')
#计算lp四分位数
quantile(data_survival$lp_Multiple_ML_methods)
#0%        25%        50%        75%       100% 
#-2.0879417 -0.6637914 -0.1652164  0.3903014  2.5520230           #1/3=1.5466549
#               -0.5412868    1.0053681
quantile(data_survival$lp_lasso)
#0%         25%         50%         75%        100% 
#0.1066832  0.4734862  0.9089739  1.9200784 12.9845564      #1/3=4.36374653   #1/2=6.4389366
#               4.47042973    8.834176263
#                       6.5456198
quantile(data_survival$lp_XGBoost)
#0%         25%         50%         75%        100% 
#-2.20204940 -0.71339004 -0.01213004  0.79603921  2.74638969 
quantile(data_survival$lp_randomForestSRC)
#0%        25%        50%        75%       100% 
#-1.9802938 -0.5845000  0.0697135  0.8280520  2.5938302 
quantile(data_survival$lp_COX)
#0%        25%        50%        75%       100% 
#-1.5906150 -0.2609816  0.2936303  1.0761519  3.0063649 

#时间改为月
data_survival$Survival_time=data_survival$Survival_time/30
#Multiple_ML_methods
fit_Multiple_ML_methods <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_Multiple_ML_methods,  # 创建生存对象 
               data = data_survival) # 数据集来源
fit_Multiple_ML_methods # 查看拟合曲线信息
summary(fit_Multiple_ML_methods)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_Multiple_ML_methods, # 创建的拟合对象
           data = data_survival,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "Multiple ML methods", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"),
             #c("high risk group","low risk group","medium risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_survival$Group_Multiple_ML_methods <- factor(data_survival$Group_Multiple_ML_methods)
is.factor(data_survival$Group_Multiple_ML_methods)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_Multiple_ML_methods,
                             data = data_survival)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#lasso
fit_lasso <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_lasso1,  # 创建生存对象 
                                   data = data_survival) # 数据集来源
fit_lasso # 查看拟合曲线信息
summary(fit_lasso)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_lasso, # 创建的拟合对象
           data = data_survival,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "lasso", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"),
             #c("high risk group","low risk group","medium risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_survival$Group_lasso <- factor(data_survival$Group_lasso)
is.factor(data_survival$Group_lasso)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_lasso,
                             data = data_survival)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#XGBoost
fit_XGBoost <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_XGBoost,  # 创建生存对象 
                     data = data_survival) # 数据集来源
fit_XGBoost # 查看拟合曲线信息
summary(fit_XGBoost)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_XGBoost, # 创建的拟合对象
           data = data_survival,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "XGBoost", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"),
             #c("high risk group","low risk group","medium risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_survival$Group_XGBoost <- factor(data_survival$Group_XGBoost)
is.factor(data_survival$Group_XGBoost)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_XGBoost,
                             data = data_survival)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#randomForestSRC
fit_randomForestSRC <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_randomForestSRC,  # 创建生存对象 
                       data = data_survival) # 数据集来源
fit_randomForestSRC # 查看拟合曲线信息
summary(fit_randomForestSRC)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_randomForestSRC, # 创建的拟合对象
           data = data_survival,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "randomForestSRC", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"),
                        #c("high risk group","low risk group","medium risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_survival$Group_randomForestSRC <- factor(data_survival$Group_randomForestSRC1)
is.factor(data_survival$Group_randomForestSRC1)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_randomForestSRC1,
                             data = data_survival)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#COX
fit_COX <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_COX,  # 创建生存对象 
                               data = data_survival) # 数据集来源
fit_COX # 查看拟合曲线信息
summary(fit_COX)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_COX, # 创建的拟合对象
           data = data_survival,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "COX", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"),
                        #c("high risk group","low risk group","medium risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_survival$Group_COX <- factor(data_survival$Group_COX1)
is.factor(data_survival$Group_COX1)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_COX1,
                             data = data_survival)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")
######5个模型低中高全部都有意义#######

###IDI-NRI(暂时不用，不适合)
# 计算
library(PredictABEL) #安装包
data4 <- read.csv('data_zhongliu_survival IDI-NRI.csv')
data4$Survival_status <-replace(data4$Survival_status,data4$Survival_status==1,0)
data4$Survival_status <-replace(data4$Survival_status,data4$Survival_status==2,1)
# cOutcome指定因变量所在的列序号；predrisk1与predrisk2是预测值；
# cutoff是截断点，可以自行调整，这里我用的是官方给的实例
IDI.NRI1<-reclassification(data=data4,cOutcome = 2,predrisk1 = data4$lp_Multiple_ML_methods,predrisk2 = data4$lp_lasso,
                           cutoff = c(0,0.5,1))#cutoff值设定预测风险值0.5为类别划分标准时

IDI.NRI2<-reclassification(data=data4,cOutcome = 2,predrisk1 = data1$prob.all.B,predrisk2 = data1$prob.protein.B,
                           cutoff = c(0,0.5,1))
IDI.NRI3<-reclassification(data=data1,cOutcome = 2,predrisk2 = data1$prob.clinical.B,predrisk1 = data1$prob.protein.B,
                           cutoff = c(0,0.1,0.3,1))

write.csv(data1,file = 'data1.csv')

#模型解释
#install.packages("survex")
library(survex)
#cox_exp <- explain(res.cox)
#lasso_exp <- explain(model.step1)
XGB_exp <- explain(model.step2)

#测量性能
#mp_cox <- model_performance(cox_exp)
#mp_lasso <- model_performance(lasso_exp)
mp_XGB <- model_performance(XGB_exp)

#plot(mp_cox)
#plot(mp_lasso)
plot(mp_XGB)

#plot(mp_cox, metrics_type="scalar")
#plot(mp_lasso, metrics_type="scalar")
plot(mp_XGB, metrics_type="scalar")



##模型解释（以XGBoost为例）
#全局解释
#接下来，我们检查每个变量如何在全局层面影响模型的预测。为此，我们使用 model_parts() 函数。它计算排列变量重要性，不同之处在于损失函数是时间相关的（默认为 loss_brier_score()），因此每个变量的影响在每个考虑的时间点都可能不同。

model_parts_XGB <- model_parts(XGB_exp)
plot(model_parts_XGB)#对于lasso模型，骨转移前后化疗及肝转移 变量的排列都会导致损失函数的最大增加。这3个变量对模型进行预测最为重要。

#我们可以使用另一个损失函数来确保这一观察是一致的。让我们使用 loss_one_minus_cd_auc()，但也改变绘图类型，以显示给定变量排列后的损失函数与所有变量的完整模型损失之间的差异。这意味着 y 轴上的值仅表示每个变量排列后损失函数的变化。
model_parts_XGB_auc <- model_parts(XGB_exp, loss_function=loss_one_minus_cd_auc, type="difference")

# NOTE: this may take a long time, so a progress bar is available. To enable it, use:
# progressr::with_progress(model_parts(rsf_exp, loss_function=loss_one_minus_cd_auc, type="difference"))
plot(model_parts_XGB_auc)

#偏依赖 (出不来图)
#此包提供的下一种全局解释类型是偏依赖图。这是使用 model_profile() 函数计算的。这些图显示将一个变量设置为不同值平均会如何影响模型的预测。同样，这是偏依赖的扩展，已知于回归和分类任务，通过扩展以考虑时间维度应用于生存模型。
#注意，我们需要设置 categorical_variables 参数以避免非常规值，例如治疗值为 0.5。所有因素都被自动检测为类别，但如果您想将数值变量视为分类变量，则需要在此处设置。
model_profile_XGB <- model_profile(XGB_exp,categorical_variables=c("Endocrine_therapy_after_bone_metastasis",
                                                             "Targeted_therapy","Liver_metastasis","Brain_metastasis"))
plot(model_profile_XGB, facet_ncol = 1)

#局部解释
#局部变量归因(不采用，全部值都是增加生存的)
#此包提供的另一种功能是局部解释。predict_parts() 函数可用于评估变量在为选定观察进行预测时的重要性。这可以通过两种方法完成，SurvSHAP(t) 和 SurvLIME。
#SurvSHAP(t)
#SurvSHAP(t) SurvSHAP(t)对生存模型的扩展。它们将预测分解为各个变量。

predict_parts_XGB_33 <- predict_parts(XGB_exp, data1[33,])
plot(predict_parts_XGB_33)

#SurvLIME
#归因变量重要性的另一种方法是 SurvLIME 方法。它通过找到一个替代比例风险模型来近似感兴趣观察点周围局部区域的生存模型。然后使用找到的模型的系数归因变量重要性。

predict_parts_XGB_5_lime <- predict_parts(XGB_exp, data1[5,], type="survlime")
plot(predict_parts_XGB_5_lime, type="local_importance")





#####################
#######外部验证######
#####################
#云大昆华
## calculate linear predictor in zhongliu
data_yundakunhua <- read.csv('imputation_20_yundakunhua.csv')
data_yundakunhua$lp_Multiple_ML_methods <- predict(res.cox, newdata=data_yundakunhua, type="risk")   # linear predictor
data_yundakunhua$lp_lasso <- predict(model.step1, newdata=data_yundakunhua, type="risk") 
data_yundakunhua$lp_XGBoost <- predict(model.step2, newdata=data_yundakunhua, type="risk") 
data_yundakunhua$lp_randomForestSRC <- predict(model.step3, newdata=data_yundakunhua, type="risk") 
data_yundakunhua$lp_COX <- predict(model.step4, newdata=data_yundakunhua, type="risk") 

data_yk <-data_yundakunhua[,c(1:3,62:66)]
#保存为逗号分割文本
write.csv(data_yk,file = 'data_yk_survival1.csv')
#生存分析
#
#data_yk1 <- read.csv('data_yk_survival1.csv')
data_yundakunhua <- read.csv('data_yk_survival1.csv')
#建立新模型
#分类变量转化
data_yundakunhua$T_stage <- factor(data_yundakunhua$T_stage)
data_yundakunhua$N_stage <- factor(data_yundakunhua$N_stage)
data_yundakunhua$Bilateral <- factor(data_yundakunhua$Bilateral)
data_yundakunhua$ER2 <- factor(data_yundakunhua$ER2)
data_yundakunhua$PR2 <- factor(data_yundakunhua$PR2)
data_yundakunhua$HER2 <- factor(data_yundakunhua$HER2)
data_yundakunhua$Ki67 <- factor(data_yundakunhua$Ki67)
data_yundakunhua$Molecular_typing <- factor(data_yundakunhua$Molecular_typing)
data_yundakunhua$Surgery_of_primary_site <- factor(data_yundakunhua$Surgery_of_primary_site)
data_yundakunhua$Chemotherapy_before_bone_metastasis <- factor(data_yundakunhua$Chemotherapy_before_bone_metastasis)
data_yundakunhua$Chemotherapy_after_bone_metastasis <- factor(data_yundakunhua$Chemotherapy_after_bone_metastasis)
data_yundakunhua$Endocrine_therapy_before_bone_metastasis <- factor(data_yundakunhua$Endocrine_therapy_before_bone_metastasis)
data_yundakunhua$Endocrine_therapy_after_bone_metastasis <- factor(data_yundakunhua$Endocrine_therapy_after_bone_metastasis)
data_yundakunhua$Targeted_therapy <- factor(data_yundakunhua$Targeted_therapy)
data_yundakunhua$Surgery_of_the_bone_metastasis <- factor(data_yundakunhua$Surgery_of_the_bone_metastasis)
data_yundakunhua$Anti_bone_metastasis_tumor_therapy <- factor(data_yundakunhua$Anti_bone_metastasis_tumor_therapy)
data_yundakunhua$Radiotherapy_of_primary_site <- factor(data_yundakunhua$Radiotherapy_of_primary_site)
data_yundakunhua$Radiotherapy_of_bone_metastasis <- factor(data_yundakunhua$Radiotherapy_of_bone_metastasis)
data_yundakunhua$SREs <- factor(data_yundakunhua$SREs)
data_yundakunhua$Metastasis_to_other_sites <- factor(data_yundakunhua$Metastasis_to_other_sites)
data_yundakunhua$Liver_metastasis <- factor(data_yundakunhua$Liver_metastasis)
data_yundakunhua$Lung_metastasis <- factor(data_yundakunhua$Lung_metastasis)
data_yundakunhua$Brain_metastasis <- factor(data_yundakunhua$Brain_metastasis)
data_yundakunhua$Survival_status <- factor(data_yundakunhua$Survival_status)

data_yundakunhua$Survival_status <- as.numeric(data_yundakunhua$Survival_status)

#！连续型数据要以数值型呈现
data_yundakunhua$Survival_time <- as.numeric(data_yundakunhua$Survival_time)
data_yundakunhua$Age <- as.numeric(data_yundakunhua$Age)
data_yundakunhua$BMI <- as.numeric(data_yundakunhua$BMI)
data_yundakunhua$ER1 <- as.numeric(data_yundakunhua$ER1)
data_yundakunhua$PR1 <- as.numeric(data_yundakunhua$PR1)
data_yundakunhua$Ki67_1 <- as.numeric(data_yundakunhua$Ki67_1)
data_yundakunhua$Tumor_size <- as.numeric(data_yundakunhua$Tumor_size)
data_yundakunhua$Number_of_chemotherapy_cycles_before_bone_metastasis <- as.numeric(data_yundakunhua$Number_of_chemotherapy_cycles_before_bone_metastasis)
data_yundakunhua$Number_of_chemotherapy_cycles_after_bone_metastasis <- as.numeric(data_yundakunhua$Number_of_chemotherapy_cycles_after_bone_metastasis)
data_yundakunhua$Age <- as.numeric(data_yundakunhua$Age)

data_yundakunhua$Platelet <- as.numeric(data_yundakunhua$Platelet)
data_yundakunhua$Hemoglobin <- as.numeric(data_yundakunhua$Hemoglobin)

#library(rms)
#f_final_model <- cph(Surv(time = Survival_time, Survival_status== 2) ~ lp_final_model,
              #x=T, y=T, surv=T,
              #data=data_yundakunhua)
#f_lasso <- cph(Surv(time = Survival_time, Survival_status== 2) ~ lp_lasso,
                     #x=T, y=T, surv=T,
                     #data=data_yundakunhua)
#f_XGBoost <- cph(Surv(time = Survival_time, Survival_status== 2) ~ lp_XGBoost,
                     #x=T, y=T, surv=T,
                     #data=data_yundakunhua)
#f_randomForestSRC <- cph(Surv(time = Survival_time, Survival_status== 2) ~ lp_randomForestSRC,
                     #x=T, y=T, surv=T,
                     #data=data_yundakunhua)
#f_COX <- cph(Surv(time = Survival_time, Survival_status== 2) ~ lp_COX,
                     #x=T, y=T, surv=T,
                     #data=data_yundakunhua)

#绘制calibration
library(riskRegression)
#data_yunda$Survival_time=data_yunda$Survival_time
#1年
#dev.off()
plotCalibration(Score(list("2-year"=f_COX),Surv(time = Survival_time, Survival_status== 2)~1,
                      data=data_yundakunhua,times=1*365, plots="cal",metrics=c("AUC","Brier"),split.method="bootcv",B=100,N=nrow(data_yunda)),
                method="quantile",cens.method="local",xlim = c(0, 1.0),
                ylim = c(0, 1.0),xlab = "nomogram predicted event probability",ylab = "Obeserved event frequencies",col = "blue")
#3年
plotCalibration(Score(list("3-year"=f_COX),Surv(Survival_time, Survival_status== 2)~1,
                      data=data_yundakunhua,times=3*365, plots="cal",metrics=c("AUC","Brier"),split.method="bootcv",B=100,N=nrow(data_yunda)),
                method="quantile",cens.method="local",xlim = c(0, 1.0),
                ylim = c(0, 1.0),xlab = "nomogram predicted event probability",ylab = "Obeserved event frequencies",col = "green")
#5年
plotCalibration(Score(list("10-year"=f_COX),Surv(Survival_time, Survival_status== 2)~1,
                      data=data_yundakunhua,times=5*365, plots="cal",metrics=c("AUC","Brier"),split.method="bootcv",B=100,N=nrow(data_yunda)),
                method="quantile",cens.method="local",xlim = c(0, 1.0),
                ylim = c(0, 1.0),xlab = "nomogram predicted event probability",ylab = "Obeserved event frequencies",col = "purple")
##AUC=57、58、59


#绘制多个模型在在某个时间点的AUC
data_yundakunhua <- read.csv('data_yk_survival1.csv')
data_yundakunhua$Survival_status <- as.factor(data_yundakunhua$Survival_status)
data_yundakunhua$Survival_status <- as.numeric(data_yundakunhua$Survival_status)
data_yundakunhua$Survival_time <-as.numeric(data_yundakunhua$Survival_time)

f_Multiple_ML_methods <- coxph(Surv(time = Survival_time, Survival_status== 2) ~ lp_Multiple_ML_methods,x=TRUE,
                     data=data_yundakunhua)
fit1 <- Score(list("Multiple ML methods"=f_Multiple_ML_methods),Surv(Survival_time,Survival_status== 2)~1,data=data_yundakunhua,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
#auc <- as.data.frame(fit1$AUC$score)

f_lasso <- coxph(Surv(time = Survival_time, Survival_status== 2) ~ lp_lasso,x=TRUE,
               data=data_yundakunhua)
fit2 <- Score(list("lasso"=f_lasso),Surv(Survival_time,Survival_status== 2)~1,data=data_yundakunhua,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
#auc <- as.data.frame(fit2$AUC$score)

f_XGBoost <- coxph(Surv(time = Survival_time, Survival_status== 2) ~ lp_XGBoost,x=TRUE,
                 data=data_yundakunhua)
fit3 <- Score(list("XGBoost"=f_XGBoost),Surv(Survival_time,Survival_status== 2)~1,data=data_yundakunhua,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
#auc <- as.data.frame(fit3$AUC$score)

f_randomForestSRC <- coxph(Surv(time = Survival_time, Survival_status== 2) ~ lp_randomForestSRC,x=TRUE,
                         data=data_yundakunhua)
fit4 <- Score(list("randomForestSRC"=f_randomForestSRC),Surv(Survival_time,Survival_status== 2)~1,data=data_yundakunhua,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
#auc <- as.data.frame(fit4$AUC$score)

f_COX <- coxph(Surv(time = Survival_time, Survival_status== 2) ~ lp_COX,x=TRUE,
             data=data_yundakunhua)
fit5 <- Score(list("COX"=f_COX),Surv(Survival_time,Survival_status== 2)~1,data=data_yundakunhua,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
#auc <- as.data.frame(fit5$AUC$score)

fit6 <- Score(list("Multiple ML methods"=f_Multiple_ML_methods,"lasso"=f_lasso, 
                   "XGBoost"=f_XGBoost,
                   "randomForestSRC"=f_randomForestSRC,
                   "COX"=f_COX)
              ,Surv(Survival_time,Survival_status== 2)~1,data=data_yundakunhua,se.fit=1L,times=c(1*365,2*365,3*365), plots="roc",metrics=c("AUC"))
#绘制5个模型的在1个年的ROC曲线
mycol1<-c("#FDB462","#377EB8","#4DAF4A","black","#C51B7D")
plotROC(fit6, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=mycol1,#col[1:5],
        legend=c('Multiple ML methods','lasso','XGBoost','randomForestSRC','COX'), 
        times = 1*365)
plotROC(fit6, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=mycol1,#col[1:5],
        legend=c('Multiple ML methods','lasso','XGBoost','randomForestSRC','COX'), 
        times = 2*365)
plotROC(fit6, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=mycol1,#col[1:5],
        legend=c('Multiple ML methods','lasso','XGBoost','randomForestSRC','COX'), 
        times = 3*365)
plotROC(fit6, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=mycol1,#col[1:5],
        legend=c('Multiple ML methods','lasso','XGBoost','randomForestSRC','COX'), 
        times = 5*365)

#
##绘制时间依赖性C指数,第一种
dev.off()
library(pec)
c_index  <- cindex(list("Multiple ML methods"=f_Multiple_ML_methods,"lasso"=f_lasso, 
                        "XGBoost"=f_XGBoost,
                        "randomForestSRC"=f_randomForestSRC,
                        "COX"=f_COX),
                   formula=Surv(Survival_time,Survival_status== 2)~.,
                   data=data_yundakunhua,
                   eval.times=seq(0*365,5.7*365,36.5),
                   splitMethod="bootcv",
                   B=1000
)
plot(c_index,xlim = c(0*365,5.6*365),col=c("#FDB462","#377EB8","#4DAF4A","black","#C51B7D"))#XGBoost最好，其次lasso

#时间依赖性C-index第二种方法
#画（1,3,5年）相关性c-index
#计算C指数
library(pec)
A2<-cindex(list("Multiple ML methods"=f_Multiple_ML_methods,
                "lasso"=f_lasso, 
                "XGBoost"=f_XGBoost,
                "randomForestSRC"=f_randomForestSRC,
                "COX"=f_COX),
           formula=Surv(Survival_time,Survival_status== 2)~.,
           data=data_yundakunhua,
           eval.times=c(1*365,2*365,3*365),
           splitMethod="bootcv",
           B=1000)
#提取C指数并进一步处理
Cindex<-as.data.frame(A2$AppCindex)
library(ggsci)
library(tidyverse)
Cindex%>%
  pivot_longer(cols = 1:5,
               names_to = "Model",
               values_to ="Value")%>%
  arrange(Model)%>%
  mutate(Time=rep(c("1-year","2-year","3-year"),5))->Cindex

Cindex$Model<-factor(Cindex$Model,levels =unique(Cindex$Model))
Cindex$Time<-factor(Cindex$Time,levels =unique(Cindex$Time))
#可视化之极坐标图
p2<-
  ggplot(data = Cindex,aes(x=Time,y=Value,fill=Model))+
  geom_bar(stat = "identity",position = "dodge",alpha=0.8,width = 0.5,color="black")+
  scale_fill_aaas()+
  labs(x="",y="C-index")+
  scale_y_continuous(limits = c(0,0.85),breaks = seq(0,0.85,0.1))+
  geom_hline(yintercept =c(0.7,0.8),linetype="dashed",linewidth=0.5)+
  theme_bw()+
  theme(panel.grid.major = element_line(linewidth = 1,color = "white"),
        axis.text = element_text(color = "black"),
        panel.border = element_rect(linewidth = 1.5),
        panel.background = element_rect(fill = "lightgrey"),
        legend.key.size = unit(0.4,'cm'),
        legend.background = element_rect(color = "black"))+
  coord_polar()
p2
#可视化之柱状图
p1<-
  ggplot(data = Cindex,aes(x=Time,y=Value,fill=Model))+
  geom_bar(stat = "identity",position = "dodge",alpha=0.8,width = 0.5,color="black")+
  scale_fill_aaas()+
  labs(x="",y="C-index")+
  scale_y_continuous(limits = c(0,0.85),breaks = seq(0,0.85,0.1),expand = c(0,0))+
  geom_hline(yintercept =c(0.7,0.8),linetype="dashed",linewidth=0.8)+
  theme_bw()+
  theme(panel.grid.major = element_line(linewidth = 1),
        axis.text = element_text(color = "black"),
        panel.border = element_rect(linewidth = 1.5),
        legend.position = "top",
        legend.key.size = unit(0.4,'cm'),
        legend.background = element_rect(color = "black",fill = "lightgrey"))
p1

###计算时间AUC
pk4<- Score(list("Multiple ML methods"=f_Multiple_ML_methods,"lasso"=f_lasso, 
                 "XGBoost"=f_XGBoost,
                 "randomForestSRC"=f_randomForestSRC,
                 "COX"=f_COX),
            formula=Surv(Survival_time,Survival_status== 2)~1,
            data=data_yundakunhua,
            metrics="auc", 
            null.model=F, 
            times=seq(0*365,4.5*365,36.5))
auc<-plotAUC(pk4,col=c("#FDB462","#377EB8","#4DAF4A","black","#C51B7D"))#XGBoost最好，其次lasso

###校准曲线
#第1种，能显示auc及brier,不设置时间，不然AUC与前面不一致
xs1 <- Score(list("Multiple ML methods"=f_Multiple_ML_methods,"lasso"=f_lasso, 
                 "XGBoost"=f_XGBoost,
                 "randomForestSRC"=f_randomForestSRC,
                 "COX"=f_COX), 
            Surv(Survival_time,Survival_status== 2) ~ 1, 
            data = data_yundakunhua,
            plots="cal", 
            metrics=c("AUC","Brier"),
            split.method="bootcv",B=1000,
            times=2*365)#1年的Brier值最低
plotCalibration(xs1, brier.in.legend=TRUE,auc.in.legend=FALSE,method = "quantile")

#DCA曲线
#结局变量转换，结局变量必须是1,0
data3 <- data_yundakunhua
data3$Survival_status <-replace(data3$Survival_status,data3$Survival_status==1,0)
data3$Survival_status <-replace(data3$Survival_status,data3$Survival_status==2,1)
library(rmda)
set.seed(123)
Multiple_ML_methodst <- decision_curve(Survival_status ~ lp_Multiple_ML_methods, data = data3,
                                      thresholds = seq(0, 1, by = 0.05), bootstraps = 100)#10个变量
lassot <- decision_curve(Survival_status ~ lp_lasso, data = data3,
                        thresholds = seq(0, 1, by = 0.05), bootstraps = 100)
XGBoostt <- decision_curve(Survival_status ~ lp_XGBoost, 
                          data = data3,thresholds = seq(0, 1, by = 0.05), bootstraps = 100)
randomForestSRCt <- decision_curve(Survival_status ~ lp_randomForestSRC, data = data3,thresholds = seq(0, 1, by = 0.05), bootstraps = 100)
COXt <- decision_curve(Survival_status ~ lp_COX, 
                      data = data3,thresholds = seq(0, 1, by = 0.05), bootstraps = 100)

list<-list(Multiple_ML_methodst,lassot,XGBoostt,randomForestSRCt,COXt)
plot_decision_curve(list, curve.names = c("Multiple ML methods","lasso","XGBoost","randomForestSRC","COX","Baseline Model"),
                    #curve.names = "Baseline Model",
                    cost.benefit.axis =F,
                    col= mycol1,
                    confidence.intervals=F,
                    standardize = T,B=1000)#COX与lasso的最好，COX第一

###生存曲线
#
rm(list = ls())
data_yk1 <- read.csv('data_yk1.csv')
str(data_yk1)
data_yk1$Survival_status <- as.factor(data_yk1$Survival_status)
data_yk1$Survival_status <- as.numeric(data_yk1$Survival_status)
data_yk1$Survival_time <-as.numeric(data_yk1$Survival_time)

data_yk1$Group_lasso <- factor(data_yk1$Group_lasso)


#时间改为月
data_yk1$Survival_time=data_yk1$Survival_time/30
#Multiple_ML_methods
library(survival)
fit_Multiple_ML_methods <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_Multiple_ML_methods,  # 创建生存对象 
                                   data = data_yk1) # 数据集来源
fit_Multiple_ML_methods # 查看拟合曲线信息
summary(fit_Multiple_ML_methods)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_Multiple_ML_methods, # 创建的拟合对象
           data = data_yk1,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "Multiple ML methods", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_yk1$Group_Multiple_ML_methods <- factor(data_yk1$Group_Multiple_ML_methods)
is.factor(data_yk1$Group_Multiple_ML_methods)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_Multiple_ML_methods,
                             data = data_yk1)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#lasso
fit_lasso <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_lasso,  # 创建生存对象 
                     data = data_yk1) # 数据集来源
fit_lasso # 查看拟合曲线信息
summary(fit_lasso)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_lasso, # 创建的拟合对象
           data = data_yk1,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "lasso", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_yk1$Group_lasso <- factor(data_yk1$Group_lasso)
is.factor(data_yk1$Group_lasso)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_lasso,
                             data = data_yk1)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#XGBoost
fit_XGBoost <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_XGBoost,  # 创建生存对象 
                       data = data_yk1) # 数据集来源
fit_XGBoost # 查看拟合曲线信息
summary(fit_XGBoost)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_XGBoost, # 创建的拟合对象
           data = data_yk1,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "XGBoost", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_yk1$Group_XGBoost <- factor(data_yk1$Group_XGBoost)
is.factor(data_yk1$Group_XGBoost)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_XGBoost,
                             data = data_yk1)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#randomForestSRC
fit_randomForestSRC <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_randomForestSRC,  # 创建生存对象 
                               data = data_yk1) # 数据集来源
fit_randomForestSRC # 查看拟合曲线信息
summary(fit_randomForestSRC)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_randomForestSRC, # 创建的拟合对象
           data = data_yk1,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "randomForestSRC", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_yk1$Group_randomForestSRC <- factor(data_yk1$Group_randomForestSRC)
is.factor(data_yk1$Group_randomForestSRC)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_randomForestSRC,
                             data = data_yk1)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#COX
fit_COX <- survfit(Surv(Survival_time,Survival_status== 2) ~ Group_COX,  # 创建生存对象 
                   data = data_yk1) # 数据集来源
fit_COX # 查看拟合曲线信息
summary(fit_COX)  #使用summary()函数输出更多详细信息。
library(survminer)
ggsurvplot(fit_COX, # 创建的拟合对象
           data = data_yk1,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv",  # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(month)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "COX", # 设置图例标题，这里设置不显示标题，用空格替代
           legend.labs = c("high risk group","low risk group"), # 指定图例分组标签
           break.x.by = 20, # 设置x轴刻度间距
           fontsize=5,
           title="low risk group vs high risk group P<0.001")  

#改为因子
data_yk1$Group_COX <- factor(data_yk1$Group_COX)
is.factor(data_yk1$Group_COX)
#[1] TRUE
library(survminer) # 加载包
restest <- pairwise_survdiff(Surv(Survival_time,Survival_status== 2) ~ Group_COX,
                             data = data_yk1)
restest

symnum(restest$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")
######5个模型低中高全部都有意义#######


#################
##经过训练集与验证集的评价，XGB模型各方面表现良好，以XGB来建立列线图####
#################


nomo=regplot(model.step2,
             plots = c("bean", "boxes"),
             clickable=F,
             points = TRUE,
             failtime =c(1*365,2*365,3*365),
             title = "XGBoost")

#瀑布图
# read data
data_h <- read.csv('heatmapall.csv')
str(data_h)
data_h$Survival_status <- factor(data_h$Survival_status)
data_h$Patient_ID <- as.numeric(data_h$Patient_ID)
#个体预测风险得分的可视化
waterdata<-data_h[,c(3,22)]
waterdata$id<-rownames(waterdata)
#waterdata$Survival_status<-factor(waterdata$Survival_status,levels = c(0,1),labels = c("Alive","Death"))
waterdata$z_score<-mosaic::zscore(waterdata$lp_XGBoost)
#install.packages("mosaic")
#install.packages("ggridges")

#样本不多的情况下
waterdata2<-waterdata[sample(seq_along(waterdata$id), size = 1 * nrow(waterdata), replace = TRUE),]
library(ggpubr)
comp<-compare_means(z_score~Survival_status,data = waterdata2,method ="t.test",var.equal=TRUE)
#install.packages("ggsci")
#library(ggsci)
ggplot(aes(x=reorder(id,z_score),y=z_score,fill=Survival_status),data=waterdata2)+
  geom_bar(stat = "identity",position = position_dodge(),alpha=0.8)+
  scale_fill_aaas()+
  labs(x="",y="Predicted probability (z-score)")+
  ggtitle("z-score transformed predicted probability of each patient")+
  theme_bw()+
  theme(legend.position = c(0.16,0.9),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.key.size = unit(0.4,'cm'),
        legend.background = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 1),
        panel.background = element_rect(fill = "lightgrey"),
        axis.title = element_text(face = "bold.italic"),
        plot.title = element_text(face = "bold.italic",color = "black",size = 13,hjust =0.5))+
  geom_hline(yintercept = 0,linewidth=1)+
  annotate(geom = "text",label=paste("P value of t-test:",comp$p.format),x=20,y=2)

#样本多的情况下
data_h$Survival_status <- factor(data_h$Survival_status)
data_h$Group <- factor(data_h$Group)
#data_h$Age <- factor(data_h$Age)
#data_h$N_stage <- factor(data_h$N_stage)
#data_h$PR <- factor(data_h$PR)
#data_h$Ki67 <- factor(data_h$Ki67)
#data_h$Surgery_of_primary_site <- factor(data_h$Surgery_of_primary_site)
#data_h$Number_of_chemotherapy_cycles_before_bone_metastasis <- factor(data_h$Number_of_chemotherapy_cycles_before_bone_metastasis)
#data_h$Number_of_chemotherapy_cycles_after_bone_metastasis <- factor(data_h$Number_of_chemotherapy_cycles_after_bone_metastasis)
data_h$Endocrine_therapy_after_bone_metastasis <- factor(data_h$Endocrine_therapy_after_bone_metastasis)
data_h$Targeted_therapy <- factor(data_h$Targeted_therapy)
data_h$Liver_metastasis <- factor(data_h$Liver_metastasis)
data_h$Brain_metastasis <- factor(data_h$Brain_metastasis)
#data_h$ALP <- factor(data_h$ALP)
#data_h$Hemoglobin <- factor(data_h$Hemoglobin)
#data_h$CA153 <- factor(data_h$CA153)
#data_h$CA125 <- factor(data_h$CA125)

data_h$Survival_time <- as.numeric(data_h$Survival_time)
data_h$Age <- as.numeric(data_h$Age)
data_h$Number_of_chemotherapy_cycles_before_bone_metastasis <- as.numeric(data_h$Number_of_chemotherapy_cycles_before_bone_metastasis)
data_h$Number_of_chemotherapy_cycles_after_bone_metastasis <- as.numeric(data_h$Number_of_chemotherapy_cycles_after_bone_metastasis)
data_h$ALP <- as.numeric(data_h$ALP)
data_h$Hemoglobin <- as.numeric(data_h$Hemoglobin)
data_h$Albumin <- as.numeric(data_h$Albumin)

#devtools::install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)
heatmapdata<-data_h
#heatmapdata<-waterdata1[,c(2,3:19)]
heatmapdata$z_score<-mosaic::zscore(heatmapdata$lp_XGBoost)
#heatmapdata<-heatmapdata[,-c(1,4)]
heatmapdata$Survival_time<-heatmapdata$Survival_time/365

heatmapdata%>%
  arrange(Survival_status)->heatmapdata
survdata<-data.frame(row.names = rownames(heatmapdata),OS=heatmapdata[,2])#产生点注释生存数据
heatmapdata<-heatmapdata[,-c(1:3)]#其余注释数据
str(heatmapdata)
ha=HeatmapAnnotation(survival_time = anno_points(survdata,size =unit(0.1,'cm'),#点注释
                                                 gp=gpar(col="#cc513c")),df=heatmapdata,#点注释属性及其他临床特征注释
                     col = list(#Age=c("＜50"="#eeebda","≥50"="#fac13a"),#设置部分临床特征注释颜色
                                #N_stage=c("N0"="#eeebda","N1"="#fac13a","N2"="#cc513c","N3"="#8E2323"),
                                #PR=c("≤0.01"="#eeebda","≥0.01 and ≤0.5"="#fac13a","＞0.5"="#cc513c"),
                                #Ki67=c("≤0.1"="#eeebda","＞0.1 and ≤0.3"="#fac13a","＞0.3"="#cc513c"),
                                #Surgery_of_primary_site=c("No"="#eeebda","Yes"="#fac13a"#,"Unknown"="#cc513c"
                                                          #),
                                #Number_of_chemotherapy_cycles_before_bone_metastasis=c("≤5"="#eeebda","＞5"="#fac13a"),
                                #Number_of_chemotherapy_cycles_after_bone_metastasis=c("≤3"="#eeebda","＞3"="#fac13a"),
                                Endocrine_therapy_after_bone_metastasis=c("No"="#eeebda","Yes"="#fac13a"),
                                Targeted_therapy=c("No"="#eeebda","Yes"="#fac13a"),
                                Liver_metastasis=c("No"="#eeebda","Yes"="#fac13a"),
                                Brain_metastasis=c("No"="#eeebda","Yes"="#fac13a"),
                                #ALP=c("＜100"="#eeebda","≥100"="#fac13a"),
                                #Hemoglobin=c("＜130"="#eeebda","≥130"="#fac13a"),
                                #CA153=c("＜45"="#eeebda","≥45"="#fac13a"),
                                #CA125=c("＜25"="#eeebda","≥25"="#fac13a"),
                                Survival_status=c("Alive"="#A6D854","Dead"="#E78AC3"),
                                Group=c("Training set"="#C51B7D","Validation set"="#FF7F00")))
heat<-Heatmap(matrix(nrow=0, ncol=nrow(heatmapdata)),top_annotation = ha)
draw(heat,annotation_legend_side="bottom")

#外部验证的
data_t <- read.csv('heatmapt.csv')
str(data_t)
#样本多的情况下
data_t$Survival_status <- factor(data_t$Survival_status)
#data_t$Age <- factor(data_t$Age)
#data_t$N_stage <- factor(data_t$N_stage)
#data_t$PR <- factor(data_t$PR)
#data_t$Ki67 <- factor(data_t$Ki67)
#data_t$Surgery_of_primary_site <- factor(data_t$Surgery_of_primary_site)
#data_t$Number_of_chemotherapy_cycles_before_bone_metastasis <- factor(data_t$Number_of_chemotherapy_cycles_before_bone_metastasis)
#data_t$Number_of_chemotherapy_cycles_after_bone_metastasis <- factor(data_t$Number_of_chemotherapy_cycles_after_bone_metastasis)
data_t$Endocrine_therapy_after_bone_metastasis <- factor(data_t$Endocrine_therapy_after_bone_metastasis)
data_t$Targeted_therapy <- factor(data_t$Targeted_therapy)
data_t$Liver_metastasis <- factor(data_t$Liver_metastasis)
data_t$Brain_metastasis <- factor(data_t$Brain_metastasis)
#data_t$ALP <- factor(data_t$ALP)
#data_t$Hemoglobin <- factor(data_t$Hemoglobin)
#data_t$CA153 <- factor(data_t$CA153)
#data_t$CA125 <- factor(data_t$CA125)

data_t$Survival_time <- as.numeric(data_t$Survival_time)
data_t$Age <- as.numeric(data_t$Age)
data_t$Number_of_chemotherapy_cycles_before_bone_metastasis <- as.numeric(data_t$Number_of_chemotherapy_cycles_before_bone_metastasis)
data_t$Number_of_chemotherapy_cycles_after_bone_metastasis <- as.numeric(data_t$Number_of_chemotherapy_cycles_after_bone_metastasis)
data_t$ALP <- as.numeric(data_t$ALP)
data_t$Hemoglobin <- as.numeric(data_t$Hemoglobin)
data_h$Albumin <- as.numeric(data_h$Albumin)

library(ComplexHeatmap)
heatmapdata<-data_t
#heatmapdata<-waterdata1[,c(2,3:19)]
heatmapdata$z_score<-mosaic::zscore(heatmapdata$lp_XGBoost)
#heatmapdata<-heatmapdata[,-c(1,4)]
heatmapdata$Survival_time<-heatmapdata$Survival_time/365

heatmapdata%>%
  arrange(Survival_status)->heatmapdata
survdata<-data.frame(row.names = rownames(heatmapdata),OS=heatmapdata[,2])#产生点注释生存数据
heatmapdata<-heatmapdata[,-c(1:3)]#其余注释数据

ha=HeatmapAnnotation(survival_time = anno_points(survdata,size =unit(0.1,'cm'),#点注释
                                                 gp=gpar(col="#cc513c")),df=heatmapdata,#点注释属性及其他临床特征注释
                     col = list(#Age=c("＜50"="#eeebda","≥50"="#fac13a"),#设置部分临床特征注释颜色
                       #N_stage=c("N0"="#eeebda","N1"="#fac13a","N2"="#cc513c","N3"="#8E2323"),
                       #PR=c("≤0.01"="#eeebda","≥0.01 and ≤0.5"="#fac13a","＞0.5"="#cc513c"),
                       #Ki67=c("≤0.1"="#eeebda","＞0.1 and ≤0.3"="#fac13a","＞0.3"="#cc513c"),
                       #Surgery_of_primary_site=c("No"="#eeebda","Yes"="#fac13a"#,"Unknown"="#cc513c"
                       #),
                       #Number_of_chemotherapy_cycles_before_bone_metastasis=c("≤5"="#eeebda","＞5"="#fac13a"),
                       #Number_of_chemotherapy_cycles_after_bone_metastasis=c("≤3"="#eeebda","＞3"="#fac13a"),
                       Endocrine_therapy_after_bone_metastasis=c("No"="#eeebda","Yes"="#fac13a"),
                       Targeted_therapy=c("No"="#eeebda","Yes"="#fac13a"),
                       Liver_metastasis=c("No"="#eeebda","Yes"="#fac13a"),
                       Brain_metastasis=c("No"="#eeebda","Yes"="#fac13a"),
                       #ALP=c("＜100"="#eeebda","≥100"="#fac13a"),
                       #Hemoglobin=c("＜130"="#eeebda","≥130"="#fac13a"),
                       #CA153=c("＜45"="#eeebda","≥45"="#fac13a"),
                       #CA125=c("＜25"="#eeebda","≥25"="#fac13a"),
                       Survival_status=c("Alive"="#A6D854","Dead"="#E78AC3")))
heat<-Heatmap(matrix(nrow=0, ncol=nrow(heatmapdata)),top_annotation = ha)
draw(heat,annotation_legend_side="bottom")










#接下来做模型评价，首先我们计算列线图预测的风险

nomoRisk=predict(model.step1, data=modeldata, type="risk")
data_lasso1 <- data1
modeldata=cbind(data_lasso1,nomoRisk)

#保存为逗号分割文本
write.csv(modeldata,file = 'modeldata.csv')
#绘制时间依赖ROC曲线评价模型的区分度
library(timeROC)
#install.packages("survivalROC")
library(survivalROC)
time_roc <- timeROC(
  T =modeldata$Survival_time,
  delta =modeldata$Survival_status==2,
  marker =modeldata$nomoRisk, #方向相反加个-
  cause = 1,
  weighting="marginal", #uses the Kaplan-Meier
  times = c(1*365,3*365,5*365),
  ROC = TRUE,
  iid = TRUE)
#查看置信区间
time_roc
#Time-dependent-Roc curve estimated using IPCW  (n=383, without competing risks). 
#Cases Survivors Censored AUC (%)   se
#t=365    101       280        2   82.63 2.20
#t=1095   231       118       34   84.19 2.12
#t=1825   263        22       98   77.83 5.42



#！采纳！绘制时间依赖ROC曲线评价模型的区分度
library(riskRegression)
Outcome <- "Surv(Survival_time, Survival_status==2)"
FinalVariables <- c("Age","N_stage","PR1","Ki67_1","Surgery_of_primary_site","Number_of_chemotherapy_cycles_before_bone_metastasis", 
                    "Number_of_chemotherapy_cycles_after_bone_metastasis","Endocrine_therapy_after_bone_metastasis", 
                    "Targeted_therapy","Liver_metastasis","Brain_metastasis", 
                    "ALP","Hemoglobin","CA153","CA125")#15个变量
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(FinalVariables, collapse=" + ")))
# fit a model with all selected varaibles
model.step <- coxph(Formula, data=modeldata,x=TRUE)
fit <- Score(list("model.step"=model.step),Surv(time = Survival_time, Survival_status==2)~1,
             data=modeldata,se.fit=1L,times=c(1*365,3*365,5*365), plots="roc",metrics=c("AUC"))
###提取AUC
d <- as.data.frame(fit$AUC$score)
a <- d[d$times %in% c(1*365,3*365,5*365),]
a$AUC_com <- paste(round(a$AUC,3),'(',round(a$lower,3),',',round(a$upper,3),')',sep='')

col = c("darkcyan","tomato","purple")  ##制作一个颜色变量
plotROC(fit, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        legend="",
        cex=1,
        auc.in.legend = T,  ##控制是否在图上显示图例
        times = 1*365)  ##绘制某个时间点         

##一个模型的多个时间点ROC,绘制多个时间点的ROC曲线，给大家介绍2种展示方式，一个是常见的ROC曲线，另外一个是ROC面积随时间变化的折线图。
##time=1年
dev.off()
plotROC(fit, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        cex=0.5,
        legend="",
        auc.in.legend = F, ##不显示默认图例，在后面编辑新图例
        times = 1*365)
##time=3
plotROC(fit,col=col[2],legend = '', cex=0.5,times = 3*365,auc.in.legend = F,add=T)
##time=6
plotROC(fit,col=col[3], times = 5*365,  add=T, cex=0.5, legend = '',auc.in.legend = F)
###加上图例
leg <- paste(c("1-year AUC:","3-year AUC:","5-year AUC:"),a$AUC_com)
legend(0.6,0.2,legend=leg,cex = 0.8,bty='n',title='our model',
       col=col,lwd=3)
#1年 AUC 0.828
#3年 AUC 0.842
#5年 AUC 0.778

#这里我们不再示例如何计算C指数，直接进行校正曲线的绘制。
#校正曲线绘制
#pdf(file="calibration.pdf", width=10, height=10)
# 1年OS校正曲线
#f <- cph(Surv(Survival_time,Survival_statusDead) ~ nomoRisk, x=T, y=T, surv=T, data=modeldata, time.inc=1*365)
#cal <- calibrate(f, cmethod="KM", method="boot", u=1*365, m=round(nrow(modeldata)/3), B=1000)
#plot(cal, xlim=c(0,1), ylim=c(0,1),
#xlab="Nomogram-predicted OS (%)", ylab="Observed OS (%)", lwd=1.5, col=mycol[11], sub=F)
#3年OS校正曲线
#f <-cph(Surv(Survival_time,Survival_statusDead) ~ nomoRisk, x=T, y=T, surv=T, data=modeldata, time.inc=3*365)
#cal <- calibrate(f, cmethod="KM", method="boot", u=3*365, m=round(nrow(data)/3), B=1000)
#plot(cal, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", lwd=1.5, col=mycol[12], sub=F, add=T)
#5年OS校正曲线
#f <-cph(Surv(Survival_time,Survival_statusDead) ~ nomoRisk, x=T, y=T, surv=T, data=modeldata, time.inc=5*365)
#cal <- calibrate(f, cmethod="KM", method="boot", u=5*365, m=round(nrow(data)/3), B=1000)
#plot(cal, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="",  lwd=1.5, col=mycol[13], sub=F, add=T)
#legend('bottomright', c('1-year', '3-year', '5-year'),
#col=mycol[11:13], lwd=1.5, bty = 'n')
#dev.off()

pdf(file="calibration.pdf", width=10, height=10)
# 1年OS校正曲线
f <- cph(Surv(Survival_time,Survival_status==2) ~ Age + 
           N_stage + PR1 + Ki67_1 + Surgery_of_primary_site + Number_of_chemotherapy_cycles_before_bone_metastasis + 
           Number_of_chemotherapy_cycles_after_bone_metastasis + Endocrine_therapy_after_bone_metastasis + 
           Targeted_therapy + Liver_metastasis + Brain_metastasis + 
           ALP + Hemoglobin + CA153 + CA125, x=T, y=T, surv=T, data=data1, time.inc=1*365)
cal <- calibrate(f, cmethod="KM", method="boot", u=1*365, m=round(nrow(data1)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1),
     xlab="Nomogram-predicted OS (%)", ylab="Observed OS (%)", lwd=1.5, col=mycol[11], sub=F)
#3年OS校正曲线
f <- cph(Surv(Survival_time,Survival_status==2) ~ Age + 
           N_stage + PR1 + Ki67_1 + Surgery_of_primary_site + Number_of_chemotherapy_cycles_before_bone_metastasis + 
           Number_of_chemotherapy_cycles_after_bone_metastasis + Endocrine_therapy_after_bone_metastasis + 
           Targeted_therapy + Liver_metastasis + Brain_metastasis + 
           ALP + Hemoglobin + CA153 + CA125, x=T, y=T, surv=T, data=data1, time.inc=3*365)
cal <- calibrate(f, cmethod="KM", method="boot", u=3*365, m=round(nrow(data1)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", lwd=1.5, col=mycol[12], sub=F, add=T)
#5年OS校正曲线
f <-cph(Surv(Survival_time,Survival_status==2) ~ Age + 
          N_stage + PR1 + Ki67_1 + Surgery_of_primary_site + Number_of_chemotherapy_cycles_before_bone_metastasis + 
          Number_of_chemotherapy_cycles_after_bone_metastasis + Endocrine_therapy_after_bone_metastasis + 
          Targeted_therapy + Liver_metastasis + Brain_metastasis + 
          ALP + Hemoglobin + CA153 + CA125, x=T, y=T, surv=T, data=data1, time.inc=5*365)
cal <- calibrate(f, cmethod="KM", method="boot", u=5*365, m=round(nrow(data1)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="",  lwd=1.5, col=mycol[13], sub=F, add=T)
legend('bottomright', c('1-year', '3-year', '5-year'),
       col=mycol[11:13], lwd=1.5, bty = 'n')
dev.off()

#绘制calibration
library(rms)
f_cph <- cph(Surv(time = Survival_time, Survival_status==2) ~ Age + N_stage + PR1 + Ki67_1 + Surgery_of_primary_site + 
               Number_of_chemotherapy_cycles_before_bone_metastasis + Number_of_chemotherapy_cycles_after_bone_metastasis + 
               Endocrine_therapy_after_bone_metastasis + Targeted_therapy + Liver_metastasis + Brain_metastasis + 
               ALP + Hemoglobin + CA153 + CA125,#15个变量
             x=T, y=T, surv=T,
             data=data1)

library(riskRegression)
#1年
plotCalibration(Score(list("1-year"=f_cph),Surv(time = Survival_time, Survival_status==2)~ 1,
                      data=data1,times=1*365, plots="cal",metrics=c("AUC","Brier"),split.method="bootcv",B=100,N=nrow(data1)),
                method="quantile",cens.method="local",xlim = c(0, 1.0),
                ylim = c(0, 1.0),xlab = "nomogram predicted event probability",ylab = "Obeserved event frequencies",col = "blue")
#3年
plotCalibration(Score(list("3-year"=f_cph),Surv(Survival_time, Survival_status==2)~1,
                      data=data1,times=3*365, plots="cal",metrics=c("AUC","Brier"),split.method="bootcv",B=100,N=nrow(data1)),
                method="quantile",cens.method="local",xlim = c(0, 1.0),
                ylim = c(0, 1.0),xlab = "nomogram predicted event probability",ylab = "Obeserved event frequencies",col = "green")
#6年
plotCalibration(Score(list("5-year"=f_cph),Surv(Survival_time, Survival_status==2)~1,
                      data=data1,times=5*365, plots="cal",metrics=c("AUC","Brier"),split.method="bootcv",B=100,N=nrow(data1)),
                method="quantile",cens.method="local",xlim = c(0, 1.0),
                ylim = c(0, 1.0),xlab = "nomogram predicted event probability",ylab = "Obeserved event frequencies",col = "purple")



