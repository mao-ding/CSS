rm(list = ls())

library(openxlsx)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(MASS)

setwd("/work/larylab/Mao_Ding/CSS/")


B6_weight = read.xlsx("/work/larylab/Mao_Ding/CSS/data/B6.xlsx", sheet = 1)
B6_NMR = read.xlsx("/work/larylab/Mao_Ding/CSS/data/B6.xlsx", sheet = 2)

CSS10_weight = read.xlsx("/work/larylab/Mao_Ding/CSS/data/CSS10.xlsx", sheet = 1)
CSS10_NMR = read.xlsx("/work/larylab/Mao_Ding/CSS/data/CSS10.xlsx", sheet = 2)

CSS17_weight = read.xlsx("/work/larylab/Mao_Ding/CSS/data/CSS17.xlsx", sheet = 1)
CSS17_NMR = read.xlsx("/work/larylab/Mao_Ding/CSS/data/CSS17.xlsx", sheet = 2)

###############################################################################

liv_wt_b6_css10_css17 = na.omit(data.frame(Strain = c(B6_weight$Strain, CSS10_weight$Strain, CSS17_weight$Strain),
                                               Liv_Weight = c(B6_weight$Liver_Weight, CSS10_weight$Liver_Weight, CSS17_weight$Liver_Weight),
                                               Norm_Liv_Weight = c(B6_weight$Normalized_Liver_Weight, CSS10_weight$Normalized_Liver_Weight, CSS17_weight$Normalized_Liver_Weight)))

liv_wt_b6_css10_css17$Strain = factor(liv_wt_b6_css10_css17$Strain, levels = c("B6","CSS10","CSS17"))
saveRDS(liv_wt_b6_css10_css17, "results/liv_wt_b6_css10_css17.rds")

##########################################
liv_wt_b6 = liv_wt_b6_css10_css17 %>%
  filter(Strain == "B6")
saveRDS(liv_wt_b6, "results/liv_wt_b6.rds")

liv_wt_css10 = liv_wt_b6_css10_css17 %>%
  filter(Strain == "CSS10")
saveRDS(liv_wt_css10, "results/liv_wt_css10.rds")

liv_wt_css17 = liv_wt_b6_css10_css17 %>%
  filter(Strain == "CSS17")
saveRDS(liv_wt_css17, "results/liv_wt_css17.rds")

liv_wt_b6_css10 = liv_wt_b6_css10_css17 %>%
  filter(Strain %in% c("B6","CSS10"))
saveRDS(liv_wt_b6_css10, "results/liv_wt_b6_css10.rds")

liv_wt_b6_css17 = liv_wt_b6_css10_css17 %>%
  filter(Strain %in% c("B6","CSS17"))
saveRDS(liv_wt_b6_css17, "results/liv_wt_b6_css17.rds")
############################################











### Normalized_Liver_Weight ###
ggplotly(ggplot(liv_weight_b6_css10_css17, aes(x = Norm_Liv_Weight, fill = Strain, color = Strain)) +
           geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.2, bins = 30) +
           geom_density(alpha = 0.2) +
           scale_fill_manual(values = c(2,3,4)) +
           scale_color_manual(values = c(2,3,4)) +
           scale_x_continuous(limits = c(0,0.08), breaks = seq(0, 0.08, by = 0.02), labels = seq(0, 0.08, by = 0.02)) +
           theme_minimal() +
           ggtitle("Density Plot by Strain") +
           ylab("Density") +
           xlab("Normalized_Liver_Weight"))


############################
### Gaussian Fit ###
############################
liv_weight_filtered = liv_weight_b6_css10_css17 %>%
  filter(Strain == "CSS17")

fit = fitdistr(liv_weight_filtered$Norm_Liv_Weight, "normal")
print(fit)

x = seq(from = 0.02, to = max(liv_weight_filtered$Norm_Liv_Weight), length.out = length(liv_weight_filtered$Norm_Liv_Weight))
y = dnorm(x, mean = fit$estimate['mean'], sd = fit$estimate['sd'])

hist(liv_weight_filtered$Norm_Liv_Weight, probability = TRUE, breaks = 20, main = "CSS17 Normalized Liver Weight with Fitted Normal Distribution",
     xlab = "Normalized Liver Weight", col = "lightgray", border = "white", xlim = c(0.02, max(liv_weight_filtered$Norm_Liv_Weight)))

lines(x, y, col = "blue", lwd = 2)

legend("topright", legend = c("Fitted Normal Distribution"), col = c("blue"), lwd = 2)


