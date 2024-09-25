rm(list = ls())
library(openxlsx)
library(tidyverse)
library(dplyr)

library(MASS)

setwd("/work/larylab/Mao_Ding/CSS/")

liv_wt_b6_css10_css17 = readRDS("results/liv_wt_b6_css10_css17.rds")

table(liv_wt_b6_css10_css17$Strain)

########################
### B6 vs CSS_10 ###
########################
mean_diff_b6_css10 = abs(mean(liv_wt_b6$Norm_Liv_Weight) - mean(liv_wt_css10$Norm_Liv_Weight))

sd_ratio_inf_b6_css10 = sd(liv_wt_b6$Norm_Liv_Weight)/sd(liv_wt_css10$Norm_Liv_Weight)

sample_ratio_b6_css10 = nrow(liv_wt_b6)/nrow(liv_wt_css10)

mean_diff_b6_css10 = abs(mean(liv_wt_b6$Norm_Liv_Weight) - mean(liv_wt_css10$Norm_Liv_Weight))

sd_ratio_inf_b6_css10 = sd(liv_wt_b6$Norm_Liv_Weight)/sd(liv_wt_css10$Norm_Liv_Weight)

sample_ratio_b6_css10 = nrow(liv_wt_b6)/nrow(liv_wt_css10)

########################
### B6 vs CSS_17 ###
########################
mean_diff_b6_css17 = abs(mean(liv_wt_b6$Norm_Liv_Weight) - mean(liv_wt_css17$Norm_Liv_Weight))

sd_ratio_inf_b6_css17 = sd(liv_wt_b6$Norm_Liv_Weight)/sd(liv_wt_css17$Norm_Liv_Weight)

sample_ratio_b6_css17 = nrow(liv_wt_b6)/nrow(liv_wt_css17)

mean_diff_b6_css17 = abs(mean(liv_wt_b6$Norm_Liv_Weight) - mean(liv_wt_css17$Norm_Liv_Weight))

sd_ratio_inf_b6_css17 = sd(liv_wt_b6$Norm_Liv_Weight)/sd(liv_wt_css17$Norm_Liv_Weight)

sample_ratio_b6_css17 = nrow(liv_wt_b6)/nrow(liv_wt_css17)


############################################
### Variance_Effect: Levene's Test
############################################

b6_css10_n_liv_wt_levene = car::leveneTest(Norm_Liv_Weight ~ Strain, data = liv_wt_b6_css10)
print(b6_css10_n_liv_wt_levene)

b6_css17_n_liv_wt_levene = car::leveneTest(Norm_Liv_Weight ~ Strain, data = liv_wt_b6_css17)
print(b6_css17_n_liv_wt_levene)

############################################
### Mean_Effect: Ranked-ANOVA Test
############################################
b6_css10_n_liv_wt_rk_anova = anova(lm(rank(Norm_Liv_Weight) ~ Strain, data = liv_wt_b6_css10))
print(b6_css10_n_liv_wt_rk_anova)

b6_css17_n_liv_wt_rk_anova = anova(lm(rank(Norm_Liv_Weight) ~ Strain, data = liv_wt_b6_css17))
print(b6_css17_n_liv_wt_rk_anova)

############################################
### Mean_Effect: Permutation(Raw) Test
############################################
meanDiff <- function(Value, Strain) {
  abs(mean(Value[Strain == 1]) - mean(Value[Strain == 0]))
}

permutation_tests = function(temp_df,nperm,fxn,alpha) {
  
  # Get actual absolute mean difference.
  actual_diff <- do.call(fxn, list(temp_df$Value, temp_df$Strain))
  # Find shuffled mean differences.
  shuffled_diff <- sapply(1:nperm, function(p){
    shuffled_idx <- sample(1:nrow(temp_df))
    shuffled_X <- temp_df$Strain[shuffled_idx]
    return(do.call(fxn,list(temp_df$Value, shuffled_X)))
  })
  
  percentile <- ecdf(shuffled_diff)
  p = 1 - percentile(actual_diff)
  return(list(p = p,diff = actual_diff, crit = quantile(shuffled_diff,p=c(alpha/2,(1-alpha/2)))))
}

############################
## B6 vs CSS10
############################
liv_wt_b6_css10_temp <- liv_wt_b6_css10[,c(1,3)]
colnames(liv_wt_b6_css10_temp) = c("Strain","Value")

liv_wt_b6_css10_temp$Strain <- ifelse(liv_wt_b6_css10_temp$Strain == "B6", "0", "1")

b6_css10_n_liv_wt_perm_raw = permutation_tests(temp_df = liv_wt_b6_css10_temp, nperm = 1000, fxn = meanDiff, alpha = 0.5)$p
print(b6_css10_n_liv_wt_perm_raw)

#############################
## B6 vs CSS17 ##
#############################
liv_wt_b6_css17_temp <- liv_wt_b6_css17[,c(1,3)]
colnames(liv_wt_b6_css17_temp) = c("Strain","Value")

liv_wt_b6_css17_temp$Strain <- ifelse(liv_wt_b6_css17_temp$Strain == "B6", "0", "1")

b6_css17_n_liv_wt_perm_raw = permutation_tests(temp_df = liv_wt_b6_css17_temp, nperm = 1000, fxn = meanDiff, alpha = 0.5)$p
print(b6_css17_n_liv_wt_perm_raw)


