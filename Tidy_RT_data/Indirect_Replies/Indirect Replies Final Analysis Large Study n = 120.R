#Indirect Replies Final Analysis Large Study n = 120
library(readr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggpubr)
library(ggstatsplot)

alldata_IR_RT <- read_csv("Tidy_RT_data/Indirect_Replies/alldata_IR_RT.csv", 
                          col_types = cols(RT1 = col_number(), 
                                           RT2 = col_number(), RT3 = col_number(), 
                                           RT4 = col_number(), RT5 = col_number(), 
                                           RT6 = col_number(), RT1ms = col_number(), 
                                           RT2ms = col_number(), RT3ms = col_number(), 
                                           RT4ms = col_number(), RT5ms = col_number(), 
                                           RT6ms = col_number()))

#View(alldata_IR_RT)

#Add Individual Difference Measures
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Indirect_Replies/Reduced_IDs_IR.csv", 
                           col_types = cols(total_RAW_score = col_number(), 
                                            total_t_score = col_number(), EQ_score = col_number()))
#View(Reduced_IDs_IR)

#Combined ID's with Reaction Time Data
all_data_join <- inner_join(alldata_IR_RT, Reduced_IDs_IR, by = "participant")
View(all_data_join)
# Scale the ID measures...
all_data_join$total_t_score <- scale(all_data_join$total_t_score)
all_data_join$EQ_score <- scale(all_data_join$EQ_score)

#Lets have a look at the models we have reported (note: Tidy_IR_sript.R in Indirect_Replies folder has more exploration of the data)

#Region 2-> Sentiment Manipulation ROI
# Model including covariates + GS and condition interaction
model_int2 <- lmer(RT2ms ~ condition_number*Group_Status + total_t_score + EQ_score + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_int2)
# Summary Stats using emmeans package
library(emmeans)
SER21 = emmeans(model_int2, specs = 'condition_number')
summary(SER21)
SER22 = emmeans(model_int2, specs = 'condition_number', 'Group_Status')
summary(SER22)
#visual Aid
all_data_join %>% 
  ggplot(aes(x = condition_number, y = RT2ms, colour = Group_Status)) + ggtitle("Reaction Time Region 2") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)


#Region 4-> Critical Reply ROI
#Remove outliers
ggbetweenstats(all_data_join, condition_number, RT4ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$RT4ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT4ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT4ms > (Q[1] - 2.0*iqr) & all_data_join$RT4ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT4ms, outlier.tagging = TRUE) 
#Model including group status interaction and IDs with ouliers removed as crazy fixations of 29,460 ms
model_int4 <- lmer(RT4ms ~ condition_number*Group_Status + total_t_score + EQ_score + (1 | participant) +  (1 | item_number) , data = eliminated, REML = TRUE)
summary(model_int4)
## Summary Stats using emmeans package
SER41 = emmeans(model_int4, specs = 'condition_number')
summary(SER41)
SER42 = emmeans(model_int4, specs = 'condition_number', 'Group_Status')
summary(SER42)
#Violin plots by group_status
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = FALSE)


#Region 5-> Post-Critical Wrap-Up ROI
#Remove outliers
ggbetweenstats(all_data_join, condition_number, RT5ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$RT5ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT5ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT5ms > (Q[1] - 2.0*iqr) & all_data_join$RT5ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT5ms, outlier.tagging = TRUE) 
# Model including group status interaction and IDs with ouliers removed as crazy fixations 181,155
model_int5 <- lmer(RT5ms ~  condition_number * Group_Status + total_t_score + EQ_score + (1 | participant) +  (1 | item_number) , data = eliminated, REML = TRUE)
summary(model_int5)
#Summary Stats using emmeans
SE5 = emmeans(model_int5, specs = 'condition_number')
summary(SE5)
SE5 = emmeans(model_int5, specs = 'condition_number', 'Group_Status')
summary(SE5)
#Violin plots by group_status
eliminated %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reaction Time Region 5") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Region TT-> Total Time accross all regions
all_data_join <- all_data_join %>% group_by(participant) %>%
  mutate(TT = (RT1ms + RT2ms + RT3ms + RT4ms + RT5ms + RT6ms))
#Remove outliers
#ggbetweenstats(all_data_join, condition_number, TT, outlier.tagging = TRUE)
#Q <- quantile(all_data_join$TT, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
#iqr <- IQR(all_data_join$TT)
#up <-  Q[2]+2.0*iqr # Upper Range  
#low<- Q[1]-2.0*iqr # Lo
#eliminated<- subset(all_data_join, all_data_join$TT > (Q[1] - 2.0*iqr) & all_data_join$TT < (Q[2]+2.0*iqr))
#ggbetweenstats(eliminated, condition_number, TT, outlier.tagging = TRUE) 
# Model including group status interaction and IDs with ouliers removed as crazy fixations 1,299,978
model_intTT <- lmer(TT ~ condition_number * Group_Status + total_t_score + EQ_score + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_intTT)
#Summary Stats using emmeans
SETT = emmeans(model_intTT, specs = 'condition_number')
summary(SETT)
SETT = emmeans(model_intTT, specs = 'condition_number', 'Group_Status')
summary(SETT)
#Violin plots by group_status
all_data_join %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Indirect_Replies") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)
