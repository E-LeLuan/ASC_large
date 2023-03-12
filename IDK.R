library(tidyverse)
library(rstatix)
library(ggpubr)

#Multivariate mixed model
myplot <- ggboxplot(
  eliminated, x = "condition_number", y = "RT4ms",
  fill = "condition_number", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~Group_Status)
myplot

myplot2 <- ggboxplot(
  eliminated, x = "Group_Status", y = "RT4ms",
  fill = "Group_Status", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~condition_number)
myplot2

compare_means(RT4ms ~ Group_Status, data = all_data_join, 
              group.by = "condition_number")
