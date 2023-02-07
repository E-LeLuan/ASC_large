# Prediction tidy script
library(Matrix)
library(lme4)
library(lmerTest)
library(emmeans)
library(stats)
library(fitdistrplus)
library(tidyverse)
library(buildmer)
library(performance)
library(see)
library(sjPlot)
library(ggrepel)
library(ggstatsplot)

set.seed(42)

# Importing the data into R.

library(readr)
P1_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P1_ASC_LARGE.csv")
P2_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P2_ASC_LARGE.csv")
P3_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P3_ASC_LARGE.csv")
P4_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P4_ASC_LARGE.csv")
P5_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P5_ASC_LARGE.csv")
P6_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P6_ASC_LARGE.csv")
P7_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P7_ASC_LARGE.csv")
P8_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P8_ASC_LARGE.csv")
P9_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P9_ASC_LARGE.csv")
P10_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P10_ASC_LARGE.csv")
P11_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P11_ASC_LARGE.csv")
P12_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P12_ASC_LARGE.csv")
P13_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P13_ASC_LARGE.csv")
P14_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P14_ASC_LARGE.csv")
P15_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P15_ASC_LARGE.csv")
P16_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P16_ASC_LARGE.csv")
P17_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P17_ASC_LARGE.csv")
P18_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P18_ASC_LARGE.csv")
P19_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P19_ASC_LARGE.csv")
P20_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P20_ASC_LARGE.csv")
P21_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P21_ASC_LARGE.csv")
P22_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P22_ASC_LARGE.csv")
P23_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P23_ASC_LARGE.csv")
P24_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P24_ASC_LARGE.csv")
P25_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P25_ASC_LARGE.csv")
P26_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P26_ASC_LARGE.csv")
P27_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P27_ASC_LARGE.csv")
P28_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P28_ASC_LARGE.csv")
P29_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P29_ASC_LARGE.csv")
P30_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P30_ASC_LARGE.csv")
P31_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P31_ASC_LARGE.csv")
P32_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P32_ASC_LARGE.csv")
P33_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P33_ASC_LARGE.csv")
P34_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P34_ASC_LARGE.csv")
P35_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P35_ASC_LARGE.csv")
P36_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P36_ASC_LARGE.csv")
P37_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P37_ASC_LARGE.csv")
P38_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P38_ASC_LARGE.csv")
P39_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P39_ASC_LARGE.csv")
P40_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P40_ASC_LARGE.csv")
P41_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P41_ASC_LARGE.csv")
P42_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P42_ASC_LARGE.csv")
P43_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P43_ASC_LARGE.csv")
P44_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P44_ASC_LARGE.csv")
P45_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P45_ASC_LARGE.csv")
P46_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P46_ASC_LARGE.csv")
P47_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P47_ASC_LARGE.csv")
P48_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P48_ASC_LARGE.csv")
P49_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P49_ASC_LARGE.csv")
P50_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P50_ASC_LARGE.csv")
P51_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P51_ASC_LARGE.csv")
P52_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P52_ASC_LARGE.csv")
P53_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P53_ASC_LARGE.csv")
P54_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P54_ASC_LARGE.csv")
P55_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P55_ASC_LARGE.csv")
P56_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P56_ASC_LARGE.csv")
P57_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P57_ASC_LARGE.csv")
P58_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P58_ASC_LARGE.csv")
P59_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P59_ASC_LARGE.csv")
P60_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P60_ASC_LARGE.csv")
P61_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P61_ASC_LARGE.csv")
P62_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P62_ASC_LARGE.csv")
P63_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P63_ASC_LARGE.csv")
P64_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P64_ASC_LARGE.csv")
P65_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P65_ASC_LARGE.csv")
P66_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P66_ASC_LARGE.csv")
P67_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P67_ASC_LARGE.csv")
P68_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P68_ASC_LARGE.csv")
P69_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P69_ASC_LARGE.csv")
P70_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P70_ASC_LARGE.csv")
P71_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P71_ASC_LARGE.csv")
P72_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P72_ASC_LARGE.csv")
P73_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P73_ASC_LARGE.csv")
P74_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P74_ASC_LARGE.csv")
P75_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P75_ASC_LARGE.csv")
P76_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P76_ASC_LARGE.csv")
P77_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P77_ASC_LARGE.csv")
P78_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P78_ASC_LARGE.csv")
P79_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P79_ASC_LARGE.csv")
P80_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P80_ASC_LARGE.csv")
P81_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P81_ASC_LARGE.csv")
P82_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P82_ASC_LARGE.csv")
P83_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P83_ASC_LARGE.csv")
P84_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P84_ASC_LARGE.csv")
P85_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P85_ASC_LARGE.csv")
P86_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P86_ASC_LARGE.csv")
P87_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P87_ASC_LARGE.csv")
P88_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P88_ASC_LARGE.csv")
P89_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P89_ASC_LARGE.csv")
P90_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P90_ASC_LARGE.csv")
P91_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P91_ASC_LARGE.csv")
P92_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P92_ASC_LARGE.csv")
P93_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P93_ASC_LARGE.csv")
P94_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P94_ASC_LARGE.csv")
P95_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P95_ASC_LARGE.csv")
P96_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P96_ASC_LARGE.csv")
P97_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P97_ASC_LARGE.csv")
P98_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P98_ASC_LARGE.csv")
P99_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P99_ASC_LARGE.csv")
P100_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P100_ASC_LARGE.csv")
P101_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P101_ASC_LARGE.csv")
P102_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P102_ASC_LARGE.csv")
P103_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P103_ASC_LARGE.csv")
P104_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P104_ASC_LARGE.csv")
P105_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P105_ASC_LARGE.csv")
P106_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P106_ASC_LARGE.csv")
P107_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P107_ASC_LARGE.csv")
P108_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P108_ASC_LARGE.csv")
P109_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P109_ASC_LARGE.csv")
P110_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P110_ASC_LARGE.csv")
P111_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P111_ASC_LARGE.csv")
P112_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P112_ASC_LARGE.csv")
P113_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P113_ASC_LARGE.csv")
P114_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P114_ASC_LARGE.csv")
P115_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P115_ASC_LARGE.csv")
P116_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P116_ASC_LARGE.csv")
P117_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P117_ASC_LARGE.csv")
P118_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P118_ASC_LARGE.csv")
P119_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P119_ASC_LARGE.csv")
P120_ASC_LARGE <- read_csv("Tidy_RT_data/Prediction/P120_ASC_LARGE.csv")
# Combining the individual data spreadsheets into one data frame.

alldata_Pred_RT <- rbind (P1_ASC_LARGE, P2_ASC_LARGE, P3_ASC_LARGE,P4_ASC_LARGE, P5_ASC_LARGE,
                  P6_ASC_LARGE, P7_ASC_LARGE, P8_ASC_LARGE, P9_ASC_LARGE, P10_ASC_LARGE,
                  P11_ASC_LARGE, P12_ASC_LARGE, P13_ASC_LARGE, P14_ASC_LARGE, P15_ASC_LARGE,
                  P16_ASC_LARGE,P17_ASC_LARGE, P18_ASC_LARGE, P19_ASC_LARGE,P20_ASC_LARGE,
                  P21_ASC_LARGE, P22_ASC_LARGE, P23_ASC_LARGE,P24_ASC_LARGE, P25_ASC_LARGE, P26_ASC_LARGE,
                  P27_ASC_LARGE, P28_ASC_LARGE, P29_ASC_LARGE, P30_ASC_LARGE, P31_ASC_LARGE,P32_ASC_LARGE,
                  P33_ASC_LARGE, P34_ASC_LARGE, P35_ASC_LARGE, P36_ASC_LARGE, P37_ASC_LARGE, P38_ASC_LARGE,
                  P39_ASC_LARGE,P40_ASC_LARGE, P41_ASC_LARGE, P42_ASC_LARGE, P43_ASC_LARGE, P44_ASC_LARGE,
                  P45_ASC_LARGE, P46_ASC_LARGE, P47_ASC_LARGE,P48_ASC_LARGE, P49_ASC_LARGE, P50_ASC_LARGE,
                  P51_ASC_LARGE, P52_ASC_LARGE, P53_ASC_LARGE, P54_ASC_LARGE, P55_ASC_LARGE,P56_ASC_LARGE,
                  P57_ASC_LARGE, P58_ASC_LARGE,P59_ASC_LARGE, P60_ASC_LARGE, P61_ASC_LARGE, P62_ASC_LARGE, 
                  P63_ASC_LARGE, P64_ASC_LARGE, P65_ASC_LARGE, P66_ASC_LARGE, P67_ASC_LARGE, P68_ASC_LARGE,
                  P69_ASC_LARGE,P70_ASC_LARGE, P71_ASC_LARGE, P72_ASC_LARGE, P73_ASC_LARGE, P74_ASC_LARGE,
                  P75_ASC_LARGE, P76_ASC_LARGE, P77_ASC_LARGE,P78_ASC_LARGE, P79_ASC_LARGE, P80_ASC_LARGE, 
                  P81_ASC_LARGE, P82_ASC_LARGE, P83_ASC_LARGE, P84_ASC_LARGE, P85_ASC_LARGE, P86_ASC_LARGE, 
                  P87_ASC_LARGE, P88_ASC_LARGE, P89_ASC_LARGE, P90_ASC_LARGE, P91_ASC_LARGE, P92_ASC_LARGE, 
                  P93_ASC_LARGE, P94_ASC_LARGE, P95_ASC_LARGE, P96_ASC_LARGE, P97_ASC_LARGE, P98_ASC_LARGE, 
                  P99_ASC_LARGE, P100_ASC_LARGE, P101_ASC_LARGE, P102_ASC_LARGE, P103_ASC_LARGE, P104_ASC_LARGE, 
                  P105_ASC_LARGE, P106_ASC_LARGE, P107_ASC_LARGE, P108_ASC_LARGE, P109_ASC_LARGE, P110_ASC_LARGE, 
                  P111_ASC_LARGE, P112_ASC_LARGE, P113_ASC_LARGE, P114_ASC_LARGE, P115_ASC_LARGE, P116_ASC_LARGE,
                  P117_ASC_LARGE, P118_ASC_LARGE, P119_ASC_LARGE, P120_ASC_LARGE)
#view(alldata_Pred_RT)

alldata_Pred_RT <- alldata_Pred_RT%>%
  mutate(Group_Status = participant <= 60)

# Rename TRUE FALSE to more meaningful labels.
alldata_Pred_RT$Group_Status[alldata_Pred_RT$Group_Status == 'TRUE'] <- "ASC"
alldata_Pred_RT$Group_Status[alldata_Pred_RT$Group_Status == 'FALSE'] <- "TD"
#view(alldata_Pred_RT)

#Rename condition_number to more meaningful numbers
alldata_Pred_RT$condition_number <- recode(alldata_Pred_RT$condition_number, "1" = "facilitated", "2" = "unfacilitated")
#view(alldata_Pred_RT)

alldata_Pred_RT <- alldata_Pred_RT%>%
  mutate(RT1ms = RT1*1000)
alldata_Pred_RT <- alldata_Pred_RT%>%
  mutate(RT2ms = RT2*1000)
alldata_Pred_RT <- alldata_Pred_RT%>%
  mutate(RT3ms = RT3*1000)
alldata_Pred_RT <- alldata_Pred_RT%>%
  mutate(RT4ms = RT4*1000)
alldata_Pred_RT <- alldata_Pred_RT%>%
  mutate(RT5ms = RT5*1000)
alldata_Pred_RT <- alldata_Pred_RT%>%
  mutate(RT6ms = RT6*1000)
################Lognormal analysis as Weibull is closest to lognormal and gamma#############################
#view(alldata_Pred_RT)
# Let's have a look at region 3 Which is our Prediction region

#view(alldata_Pred_RT)

alldata_Pred_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

#Violin plots
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
#alldata_Pred_RT %>% 
#  ggplot(aes(x = condition_number, y = RT3ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
#  labs(y = "Reading time in seconds", x = "Prediction") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = none)

#Violin plots by group_status
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
#alldata_Pred_RT %>% 
#  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
#  labs(y = "Reading time in seconds", x = "Prediction") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = none)


# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops participant and item random effects
modelRT3ms <- lmer(RT3ms ~ condition_number + (1 | participant) + (1 | item_number), data = alldata_Pred_RT,
                   REML = TRUE) 
summary(modelRT3ms)

model.nullRT3ms <- lmer(RT3ms ~ (1 | participant) + (1 | item_number), alldata_Pred_RT) 

anova(modelRT3ms,model.nullRT3ms)

#add in group_stATUS and shows TD driving the effect
#Maximal model with no singularity of fit error drops participant and item random effects
modelRT3msGS <- lmer(RT3ms ~ condition_number + Group_Status + (1 | participant) + (1 | item_number), data = alldata_Pred_RT,
                     REML = TRUE) 
summary(modelRT3msGS)

#All the data for this model looks pretty normal.
check_model(modelRT3ms)
qqnorm(residuals(modelRT3ms))
qqline(residuals(modelRT3ms))
descdist(alldata_Pred_RT$RT3ms)


#Now Let's add in individual differences
#Import Individual difference measures
Reduced_IDs_Pred <- read_csv("Tidy_RT_data/Prediction/Reduced_IDs_Pred.csv")
#View(Reduced_IDs_Pred)

all_data_join <- inner_join(alldata_Pred_RT, Reduced_IDs_Pred, by = "participant")
#View(all_data_join)

# Scale the ID measures...
all_data_join$total_RAW_score <- scale(all_data_join$total_RAW_score)
all_data_join$total_t_score <- scale(all_data_join$total_t_score)
all_data_join$EQ_score <- scale(all_data_join$EQ_score)

# Model including covariates
model_alldatacov_RT3ms <- lmer(RT3ms ~ condition_number + total_t_score + EQ_score + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_RT3ms)
#NO significant difference in reading the predictive information, this is good as it suggests their are no length, frequency,
# or other effects of our manipulation.

# Seperate analysis based on group
# Create subset data lists
ASC_Group <- filter(all_data_join, Group_Status == "ASC")
TD_Group <- filter(all_data_join, Group_Status == "TD")

# Seperate analysis based on group
#Significant
modelTT_TD <- lmer(RT3ms ~ condition_number + total_t_score + EQ_score + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelTT_TD)                   

#Not significant suggesrs TD driving the effect on TT
modelTT_ASC <- lmer(RT3ms ~ condition_number + total_t_score + EQ_score + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelTT_ASC)



# Let's have a look at region 4 Which is our critical/ Question region

#Violin plots
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

#Boxplt
#alldata_Pred_RT %>% 
#  ggplot(aes(x = condition_number, y = RT4ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
#  labs(y = "Reading time in seconds", x = "Prediction") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = none)

#Violin plots by group_status
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
#alldata_Pred_RT %>% 
#  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
#  labs(y = "Reading time in seconds", x = "Prediction") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = none)

#Descriptives
alldata_Pred_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))



# Model assuming normality of residuals maximal structure
#model.nullR4 <- lmer(RT4ms ~ (1 + condition_number | participant) + (1 + condition_number | item_number), alldata_Pred_RT) 
#Maximal model with no singularity of fit error drops item random effects
modelRT4ms <- lmer(RT4ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), data = alldata_Pred_RT,
                   REML = TRUE) 
summary(modelRT4ms)
model.nullRT4ms <- lmer(RT4ms ~ (1 + condition_number | participant) + (1 | item_number), alldata_Pred_RT) 
anova(modelRT4ms,model.nullRT4ms)

#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing.
#Maximal model with no singularity of fit error drops item random effects
modelRT4msGS <- lmer(RT4ms ~ condition_number + Group_Status + (1 + condition_number | participant) + (1 | item_number), data = alldata_Pred_RT,
                     REML = TRUE) 
summary(modelRT4msGS)

# THIS IS ALL SIGNIFICANT THERE IS A DIFFERENCE WITH FACILITATED CONDITIONS BEING READ SIGNIFICANTLY FASTER THAN UNFACILITATED!!! Whoop Whoop

# It Worked!!!!!

#anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelRT4ms)
qqnorm(residuals(modelRT4ms))
qqline(residuals(modelRT4ms))
descdist(alldata_Pred_RT$RT4ms)

# Model including covariates
model_alldatacov_RT4ms <- lmer(RT4ms ~ condition_number + total_t_score + EQ_score + (1 + condition_number | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_RT4ms)

#ANOVA
#two.way <- aov(RT4ms ~ condition_number * Group_Status + total_t_score + EQ_score, data = all_data_join)

#summary(two.way)

# Seperate analysis based on group
#Significant
modelTT_TD <- lmer(RT4ms ~ condition_number + total_t_score + EQ_score + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelTT_TD)                   

#Significant
modelTT_ASC <- lmer(RT4ms ~ condition_number + total_t_score + EQ_score + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelTT_ASC)


# Let's have a look at region 5 Which is our post-critical/ REply region

#Violin plots
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
#alldata_Pred_RT %>% 
#  ggplot(aes(x = condition_number, y = RT5ms, colour = condition_number)) + ggtitle("Reaction Time Region 4") +
#  labs(y = "Reading time in seconds", x = "Prediction") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = none)

#Violin plots by group_status
alldata_Pred_RT %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
#alldata_Pred_RT %>% 
#  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reaction Time Region 4") +
#  labs(y = "Reading time in seconds", x = "Prediction") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = none)

#Descriptives
alldata_Pred_RT %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))



# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops item random effects
modelRT5ms <- lmer(RT5ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), data = alldata_Pred_RT,
                   REML = TRUE) 
summary(modelRT5ms)
model.nullRT5ms <- lmer(RT5ms ~ (1 + condition_number | participant) + (1 | item_number), alldata_Pred_RT) 
anova(modelRT5ms,model.nullRT5ms)

#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing mechanisms for ASC and TD.
#Maximal model with no singularity of fit error drops item random effects
modelRT5msGS <- lmer(RT5ms ~ condition_number + Group_Status + (1 + condition_number | participant) + (1 | item_number), data = alldata_Pred_RT,
                     REML = TRUE) 
summary(modelRT5msGS)

# THIS IS ALL SIGNIFICANT THERE IS A DIFFERENCE WITH FACILITATED CONDITIONS BEING READ SIGNIFICANTLY FASTER THAN UNFACILITATED!!! Whoop Whoop

# It Worked!!!!!


# Model including covariates
model_alldatacov_RT5ms <- lmer(RT5ms ~ condition_number + total_t_score + EQ_score + 
                                 (1 + condition_number | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_RT5ms)


#All the data for this model looks pretty normal.
check_model(modelRT5ms)
qqnorm(residuals(modelRT5ms))
qqline(residuals(modelRT5ms))
descdist(alldata_Pred_RT$RT5ms)

# Seperate analysis based on group
#Significant
modelTT_TD <- lmer(RT5ms ~ condition_number + total_t_score + EQ_score + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelTT_TD)                   

#Significant
modelTT_ASC <- lmer(RT5ms ~ condition_number + total_t_score + EQ_score + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelTT_ASC)

## Let's have a look at total reading time across all regions

all_data_join <- all_data_join %>% group_by(participant) %>%
  mutate(TT = (RT1ms + RT2ms + RT3ms + RT4ms + RT5ms + RT6ms))

#view(alldata_Pred_RT)
#IT WORKED!!!!!
#Violin plots
all_data_join %>% 
  ggplot(aes(x = condition_number, y = TT, colour = condition_number)) + ggtitle("Reaction Time TT") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
#alldata_Pred_RT %>% 
#  ggplot(aes(x = condition_number, y = TT, colour = condition_number)) + ggtitle("Reaction Time TT") +
#  labs(y = "Reading time in seconds", x = "Prediction") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = none)

#Violin plots by group_status
all_data_join %>% 
  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Reaction Time TT") +
  labs(y = "Reading time in seconds", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = none)

#Boxplt
#alldata_Pred_RT %>% 
#  ggplot(aes(x = condition_number, y = TT, colour = Group_Status)) + ggtitle("Reaction Time TT") +
#  labs(y = "Reading time in seconds", x = "Prediction") +
#  geom_boxplot()+  
#  geom_jitter(alpha = .2, width = .1) +
#  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
#  guides(scale = none)

#Descriptives
all_data_join %>% 
  group_by(condition_number) %>%
  summarise(mean(TT), sd(TT))


# Model assuming normality of residuals maximal structure
#Maximal model with no singularity of fit error drops item random effects
# SIngular fit error no matter what, seperate analysis by participants and by items the items model has singular fit and participant model fits
modelTT1 <- lmer(TT ~ condition_number + (1 | participant) + (1 | item_number), data = all_data_join,
                REML = TRUE) 
summary(modelTT1)

#Have a lookat outliers as that standard deviation is crazy out!
#ggbetweenstats(alldata_Pred_RT, condition_number, TT, outlier.tagging = TRUE)
Q <- quantile(all_data_join$TT, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$TT)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$TT > (Q[1] - 2.0*iqr) & all_data_join$TT < (Q[2]+2.0*iqr))
#ggbetweenstats(eliminated, condition_number, TT, outlier.tagging = TRUE) 

eliminated %>% 
  group_by(condition_number) %>%
  summarise(mean(TT), sd(TT))

modelTT1 <- lmer(TT ~ condition_number + (1 | participant) + (1 + condition_number | item_number), data = eliminated,
                 REML = TRUE) 
summary(modelTT1)


#add in group_stATUS and shows neither group is responsible for the effect suggesting similar processing mechanisms for ASC and TD.
#Maximal model with no singularity of fit error drops item random effects
modelTTGS <- lmer(TT ~ condition_number + Group_Status + (1 | participant) + (1 + condition_number | item_number), data = eliminated,
                  REML = TRUE) 
summary(modelTTGS)

# Model including covariates
model_alldatacov_TT <- lmer(TT ~ condition_number + total_t_score + EQ_score + (1 | participant) +  (1 + condition_number | item_number) , data = eliminated, REML = TRUE)
summary(model_alldatacov_TT)

#All the data for this model looks pretty normal.
check_model(modelTT1)
qqnorm(residuals(modelTT1))
qqline(residuals(modelTT1))
descdist(alldata_Pred_RT$TT)

# Seperate analysis based on group
# Create subset data lists
ASC_Group <- filter(eliminated, Group_Status == "ASC")
TD_Group <- filter(eliminated, Group_Status == "TD")

#Significant
modelTT_TD <- lmer(TT ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelTT_TD)                   

#Not significant suggesrs TD driving the effect on TT
modelTT_ASC <- lmer(TT ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelTT_ASC) 

# Removed outliers it works now!!!!!

# count the ASC TD indices
count1 <- all_data_join %>% group_by(Group_Status,overall_clinical_range) %>% 
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame()
view(count1)



################Lognormal analysis as Weibull is closest to lognormal and gamma#############################
#With Gamma we can include more random effects including maximal structure with random slopes for particiapnt and item 
#check for 0's because gamma doesnt like 0s
#sum(is.na(alldata_Pred_RT$RT1ms))
#sum(is.na(alldata_Pred_RT$RT2ms))
#sum(is.na(alldata_Pred_RT$RT3ms))
#sum(is.na(alldata_Pred_RT$RT4ms))
#sum(is.na(alldata_Pred_RT$RT5ms))
#sum(is.na(alldata_Pred_RT$RT6ms))

#GAMMA supports a more complex structure including random effects but removes any significant effects
#Error in (function (fr, X, reTrms, family, nAGQ = 1L, verbose = 0L, maxit = 100L,  : 
#                      PIRLS loop resulted in NaN value
#Keep getting error why???? Works if i change link from inverse to log

#not significant
GammaRT3ms <- glmer(RT3ms ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                    family = Gamma (link = "log"), data = alldata_Pred_RT)
summary(GammaRT3ms)

#not significant with maximal model but is significant with the maximal model for lognormal (removing by item random slopes) 
GammaRT4ms <- glmer(RT4ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), 
                    family = Gamma (link = "log"), data = alldata_Pred_RT)
summary(GammaRT4ms)

#Region 5 significant unfacilitated read slower than facilitated (Same result either way)
GammaRT5ms <- glmer(RT5ms ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                    family = Gamma (link = "log"), data = alldata_Pred_RT)
summary(GammaRT5ms)

#Region 5 with equivelant maximal model for lognormal (Same result either way)
GammaRT5ms <- glmer(RT5ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), 
                    family = Gamma (link = "log"), data = alldata_Pred_RT)
summary(GammaRT5ms)

#not significant
GammaRTT <- glmer(TT ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
                  family = Gamma (link = "log"), data = all_data_join)
summary(GammaRTT)

# Using same parameters as lognormal model as below is not workable as model fails to converge under gamma without the most maximal effects structure
#Have a lookat outliers as that standard deviation is crazy out!
#ggbetweenstats(alldata_Pred_RT, condition_number, TT, outlier.tagging = TRUE)
#Q <- quantile(all_data_join$TT, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
#iqr <- IQR(all_data_join$TT)
#up <-  Q[2]+2.0*iqr # Upper Range  
#low<- Q[1]-2.0*iqr # Lo
#eliminated<- subset(all_data_join, all_data_join$TT > (Q[1] - 2.0*iqr) & all_data_join$TT < (Q[2]+2.0*iqr))
#ggbetweenstats(eliminated, condition_number, TT, outlier.tagging = TRUE) 

#GammaRTT <- glmer(TT ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), 
#                  family = Gamma (link = "log"), data = all_data_join)
#summary(GammaRTT)

#Export a CSV of the new data set...
#write.csv(alldata_Pred_RT,"//nask.man.ac.uk/home$/Desktop/ASC_large/Tidy_RT_data/Prediction\\alldata_Pred_RT.csv", row.names = TRUE)

#write.csv(all_data_join,"//nask.man.ac.uk/home$/Desktop/ASC_large/Tidy_RT_data/Prediction\\all_data_join.csv", row.names = TRUE)


