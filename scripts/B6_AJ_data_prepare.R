rm(list = ls())

library(tidyverse)
library(dplyr)
library(gridExtra)
library(car)

setwd("/work/larylab/Mao_Ding/CSS")


css = na.omit(read.csv("data/CSS_raw_data.csv", header = T))
sapply(css, class)

css_traits = css[3:ncol(css)]

#### Generate a histogram of each of the 13 traits ####
par(mfrow = c(3,5))

for(col_name in names(css_traits)) {
  dens = density(css_traits[[col_name]])
  hist(css_traits[[col_name]], prob = TRUE, main = paste("Histogram of", col_name), xlab = col_name, ylim = c(0, max(dens$y)))
  lines(dens, col = "red", lwd = 1)
}

par(mfrow = c(1,1))



########################################################
#### Extract each trait within "C57BL/6J" and "A/J" ####
########################################################
create_traits_df <- function(df, col_name, strains) {
  combined_df <- df %>%
    select(Strain, all_of(col_name)) %>%
    filter(Strain %in% strains)
  return(combined_df)
}

traits = colnames(css[3:ncol(css)])

traits_dfs = list()

for (trait in traits) {
  traits_df = create_traits_df(css, trait, c("C57BL/6J", "A/J"))
  
  traits_df$Strain = factor(traits_df$Strain, levels = c("C57BL/6J", "A/J"))
  
  rds_file = file.path("results", paste0("B6_AJ_", trait, ".rds"))
  csv_file = file.path("results", paste0("B6_AJ_", trait, ".csv"))
  
  saveRDS(traits_df, file = rds_file)
  
  write.csv(traits_df, file = csv_file, row.names = FALSE)
  
  traits_dfs[[trait]] = traits_df
}


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
