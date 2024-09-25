rm(list = ls())

library(tidyverse)
library(dplyr)
library(gridExtra)
library(car)
library(vegan)

setwd("/work/larylab/Mao_Ding/CSS")

#########################################
### Create a list of rds files for B6_AJ
########################################

folder_path = "/work/larylab/Mao_Ding/CSS/results"

file_list = list.files(folder_path, pattern = "^B6_AJ.*\\.rds$", full.names = TRUE)

B6_AJ_list = list()

for(file in file_list) {
  data = readRDS(file)
  B6_AJ_list[[file]] = data
}

###################################
### Plot the boxplot for the two groups
###################################

par(mfrow = c(3,5))

for(i in 1:length(B6_AJ_list)) {
  file = B6_AJ_list[[i]]
  boxplot(file[,2] ~ file[,1], data = file, main = colnames(file)[2], xlab = "Strain", ylab = colnames(file)[2])
}


################################
###### Mean Effect #######
################################

### ANOVA ###
#### Initiate summary df for each ANOVA model ####
summary_aov_df = data.frame(
                         Variable = character(),
                         Df = integer(),
                         Sum_sq = numeric(),
                         Mean_sq = numeric(),
                         F_value = numeric(),
                         P_value = numeric())

for(i in 1:length(B6_AJ_list)) {
  file = B6_AJ_list[[i]]
  print(colnames(file)[2])

  summary_aov_current = summary(aov(file[,2] ~ file[,1], data = file))
  
  summary_aov_file = data.frame(
                           #Variable = rownames(summary_aov_current[[1]]),
                           Variable = c(colnames(file)[2], "Residuals"),
                           Df = summary_aov_current[[1]]$Df,
                           Sum_sq = summary_aov_current[[1]]$`Sum Sq`,
                           Mean_sq = summary_aov_current[[1]]$`Mean Sq`,
                           F_value = summary_aov_current[[1]]$`F value`,
                           P_value = summary_aov_current[[1]]$`Pr(>F)`)
  
  summary_aov_df = bind_rows(summary_aov_df, summary_aov_file)
  
}

saveRDS(summary_aov_df, "results/summary_aov_df.rds")

####################
### PERMANOVA ###
#####################

lv_wt = readRDS("/work/larylab/Mao_Ding/CSS/results/B6_AJ_Liv_wt.rds")

lv_wt_permaov = vegan::adonis2(lv_wt$Liv_wt ~ lv_wt$Strain, data = lv_wt, method = "euc")

print(lv_wt_permaov)


summary_permaov_df = data.frame(
  Variables = character(),
  Df = integer(),
  Sum_of_Sq = numeric(),
  R_Sq = numeric(),
  F_Score = numeric(),
  P_vale = numeric()
)

for(i in 1:length(B6_AJ_list)) {
  file = B6_AJ_list[[i]]
  print(colnames(file)[2])
  
  traits = file[2]
  strains = file[1]
  
  permaov_current = vegan::adonis2(traits ~ strains, data = file, method = "euc")
  
  rownames(result_pmaov)[1] = colnames(file)[2]
  
  summary_permaov_file = data.frame(
      Variables = rownames(result_pmaov),
      Df = result_pmaov$Df,
      Sum_of_Sq = result_pmaov$SumOfSqs,
      R_Sq = result_pmaov$R2,
      F_Score = result_pmaov$F,
      P_vale = result_pmaov$`Pr(>F)`                      
  )
  
  summary_permaov_df = bind_rows(summary_permaov_df, summary_permaov_file)
}


BMI = file[,2]
strains = file[,1]

result_pmaov = vegan::adonis2(BMI ~ strains, data = file, method = "euc")

print(result_pmaov)

result_pmaov$Df
result_pmaov$SumOfSqs
result_pmaov$R2
rownames(result_pmaov)[1] = colnames(file)[2]

summary_permaov_file = data.frame(
                        Variables = rownames(result_pmaov),
                        Df = result_pmaov$Df,
                        Sum_of_Sqs = result_pmaov$SumOfSqs,
                        R_Sqr = result_pmaov$R2,
                        F_Score = result_pmaov$F,
                        P_vale = result_pmaov$`Pr(>F)`

)


################################
###### Variance Effect #######
################################

### Levene ###
#### Initiate summary df for each ANOVA model ####
summary_levene_df = data.frame(
    Variable = character(),
    Df = integer(),
    F_value = numeric(),
    P_value = numeric())


for(i in 1:length(B6_AJ_list)) {
  file = B6_AJ_list[[i]]
  print(colnames(file)[2])
  
  levene_current = car::leveneTest(file[[2]] ~ file[[1]], data = file)
  
  summary_levene_file = data.frame(
    Variable = c(colnames(file)[2], NA),
    Df = levene_current$Df,
    F_value = levene_current$`F value`,
    P_value = levene_current$`Pr(>F)`)
  
  summary_levene_df = bind_rows(summary_levene_df, summary_levene_file)
  
}

saveRDS(summary_levene_df, "results/summary_levene_df.rds")
