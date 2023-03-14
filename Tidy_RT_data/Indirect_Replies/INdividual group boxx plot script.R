#Indirect Replies Final Analysis
library(readr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggdist)
library(ggthemes)
library(ggstatsplot)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)

alldata_IR_RT <- read_csv("Tidy_RT_data/Indirect_Replies/alldata_IR_RT.csv", 
                          col_types = cols(RT1 = col_number(), 
                                           RT2 = col_number(), RT3 = col_number(), 
                                           RT4 = col_number(), RT5 = col_number(), 
                                           RT6 = col_number(), RT1ms = col_number(), 
                                           RT2ms = col_number(), RT3ms = col_number(), 
                                           RT4ms = col_number(), RT5ms = col_number(), 
                                           RT6ms = col_number(), TT = col_number()))
#View(alldata_IR_RT)

#Add Individual Difference Measures
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Reduced_IDs_IR.csv", 
                           col_types = cols(SRS_total_score_raw = col_number(), 
                                            SRS_total_score_t = col_number(), 
                                            EQ = col_number(), Total_RAN = col_number(), 
                                            Total_reading_cluster = col_number()))
#Combined ID's with Reaction Time Data
all_data_join <- inner_join(alldata_IR_RT, Reduced_IDs_IR, by = "participant")
#View(all_data_join)
# Scale the ID measures...
all_data_join$SRS_total_score_raw <- scale(all_data_join$SRS_total_score_raw)
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)

library(ggpubr)
library(ggstatsplot)
ggbetweenstats(all_data_join, condition_number, RT4ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$RT4ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT4ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT4ms > (Q[1] - 2.0*iqr) & all_data_join$RT4ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT4ms, outlier.tagging = TRUE) 
#Model including group status interaction and IDs with ouliers removed as crazy fixations of 29,460 ms
model_int4 <- lmer(RT4ms ~ condition_number*Group_Status + Total_reading_cluster + SRS_total_score_t + EQ + Total_RAN + (1 | participant) +  (1 | item_number) , data = eliminated, REML = TRUE)
summary(model_int4)

# Seperate analysis based on group
# Create subset data lists
ASC_Group <- filter(eliminated, Group_Status == "ASC")
TD_Group <- filter(eliminated, Group_Status == "TD")

#ASC ONLY BOXPLOT
model_ASC <- lmer(RT4ms ~ condition_number + (1 | participant) +  (1 | item_number) , data = ASC_Group, REML = TRUE)
summary(model_ASC)


# Boxplot just Reading information
myplot3 <- ggboxplot(
  ASC_Group, x = "condition_number", y = "RT4ms",
  fill = "condition_number", palette = "jco", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Critical Reply Region", y = "Reading time in Milliseconds", x = "Indirect Reply Sentiment")

# Box plot ASC
p <- ggboxplot(ASC_Group, x = "condition_number", y = "RT4ms",
               color = "Group_Status", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = condition_number))
p + stat_compare_means(aes(group = condition_number), label = "p.signif")

compare_means(RT4ms ~ condition_number, data = ASC_Group)

#Difference between conditions by group
compare_means(RT4ms ~ condition_number, data = eliminated)
# Box plot facetted by "Group_Status"
p <- ggboxplot(all_data_join, x = "condition_number", y = "RT4ms",
               color = "condition_number", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = condition_number))
p + stat_compare_means(aes(group = condition_number), label = "p.signif")


compare_means(RT4ms ~ condition_nuber, data = eliminated)
p <- ggpaired(all_data_join, x = "Group_Status", y = "RT4ms",
              color = "condition_number", palette = "jco",
              line.color = "gray", line.size = 0.4,
              facet.by = "condition_number", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif")
p + labs(x = "Group Status")
p + labs(y = "Reading Time in ms.")myplot3

#Raincloud plot
ASC_Group %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Critical Reply Region") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in milliseconds", x = "Indirect Reply Sentiment") + 
  coord_flip()

#NON-AUTISTIC ONLY BOXPLOT
model_TD <- lmer(RT4ms ~ condition_number + (1 | participant) +  (1 | item_number) , data = TD_Group, REML = TRUE)
summary(model_TD)


# Boxplot just Reading information
myplot3 <- ggboxplot(
  TD_Group, x = "condition_number", y = "RT4ms",
  fill = "condition_number", palette = "jco", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Critical Reply Region", y = "Reading time in Milliseconds", x = "Indirect Reply Sentiment")
myplot3
#Raincloud plot
TD_Group %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Critical Reply Region") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in milliseconds", x = "Indirect Reply Sentiment") + 
  coord_flip()

