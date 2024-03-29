# Indirect_Replies tidy script
# Indirect_Replies tidy script
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
P1_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P1_ASC_LARGE.csv")
P2_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P2_ASC_LARGE.csv")
P3_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P3_ASC_LARGE.csv")
P4_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P4_ASC_LARGE.csv")
P5_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P5_ASC_LARGE.csv")
P6_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P6_ASC_LARGE.csv")
P7_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P7_ASC_LARGE.csv")
P8_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P8_ASC_LARGE.csv")
P9_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P9_ASC_LARGE.csv")
P10_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P10_ASC_LARGE.csv")
P11_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P11_ASC_LARGE.csv")
P12_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P12_ASC_LARGE.csv")
P13_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P13_ASC_LARGE.csv")
P14_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P14_ASC_LARGE.csv")
P15_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P15_ASC_LARGE.csv")
P16_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P16_ASC_LARGE.csv")
P17_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P17_ASC_LARGE.csv")
P18_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P18_ASC_LARGE.csv")
P19_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P19_ASC_LARGE.csv")
P20_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P20_ASC_LARGE.csv")
P21_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P21_ASC_LARGE.csv")
P22_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P22_ASC_LARGE.csv")
P23_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P23_ASC_LARGE.csv")
P24_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P24_ASC_LARGE.csv")
P25_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P25_ASC_LARGE.csv")
P26_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P26_ASC_LARGE.csv")
P27_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P27_ASC_LARGE.csv")
P28_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P28_ASC_LARGE.csv")
P29_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P29_ASC_LARGE.csv")
P30_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P30_ASC_LARGE.csv")
P31_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P31_ASC_LARGE.csv")
P32_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P32_ASC_LARGE.csv")
P33_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P33_ASC_LARGE.csv")
P34_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P34_ASC_LARGE.csv")
P35_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P35_ASC_LARGE.csv")
P36_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P36_ASC_LARGE.csv")
P37_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P37_ASC_LARGE.csv")
P38_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P38_ASC_LARGE.csv")
P39_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P39_ASC_LARGE.csv")
P40_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P40_ASC_LARGE.csv")
P41_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P41_ASC_LARGE.csv")
P42_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P42_ASC_LARGE.csv")
P43_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P43_ASC_LARGE.csv")
P44_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P44_ASC_LARGE.csv")
P45_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P45_ASC_LARGE.csv")
P46_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P46_ASC_LARGE.csv")
P47_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P47_ASC_LARGE.csv")
P48_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P48_ASC_LARGE.csv")
P49_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P49_ASC_LARGE.csv")
P50_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P50_ASC_LARGE.csv")
P51_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P51_ASC_LARGE.csv")
P52_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P52_ASC_LARGE.csv")
P53_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P53_ASC_LARGE.csv")
P54_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P54_ASC_LARGE.csv")
P55_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P55_ASC_LARGE.csv")
P56_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P56_ASC_LARGE.csv")
P57_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P57_ASC_LARGE.csv")
P58_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P58_ASC_LARGE.csv")
P59_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P59_ASC_LARGE.csv")
P60_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P60_ASC_LARGE.csv")
P61_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P61_ASC_LARGE.csv")
P62_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P62_ASC_LARGE.csv")
P63_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P63_ASC_LARGE.csv")
P64_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P64_ASC_LARGE.csv")
P65_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P65_ASC_LARGE.csv")
P66_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P66_ASC_LARGE.csv")
P67_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P67_ASC_LARGE.csv")
P68_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P68_ASC_LARGE.csv")
P69_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P69_ASC_LARGE.csv")
P70_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P70_ASC_LARGE.csv")
P71_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P71_ASC_LARGE.csv")
P72_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P72_ASC_LARGE.csv")
P73_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P73_ASC_LARGE.csv")
P74_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P74_ASC_LARGE.csv")
P75_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P75_ASC_LARGE.csv")
P76_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P76_ASC_LARGE.csv")
P77_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P77_ASC_LARGE.csv")
P78_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P78_ASC_LARGE.csv")
P79_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P79_ASC_LARGE.csv")
P80_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P80_ASC_LARGE.csv")
P81_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P81_ASC_LARGE.csv")
P82_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P82_ASC_LARGE.csv")
P83_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P83_ASC_LARGE.csv")
P84_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P84_ASC_LARGE.csv")
P85_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P85_ASC_LARGE.csv")
P86_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P86_ASC_LARGE.csv")
P87_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P87_ASC_LARGE.csv")
P88_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P88_ASC_LARGE.csv")
P89_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P89_ASC_LARGE.csv")
P90_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P90_ASC_LARGE.csv")
P91_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P91_ASC_LARGE.csv")
P92_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P92_ASC_LARGE.csv")
P93_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P93_ASC_LARGE.csv")
P94_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P94_ASC_LARGE.csv")
P95_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P95_ASC_LARGE.csv")
P96_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P96_ASC_LARGE.csv")
P97_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P97_ASC_LARGE.csv")
P98_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P98_ASC_LARGE.csv")
P99_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P99_ASC_LARGE.csv")
P100_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P100_ASC_LARGE.csv")
P101_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P101_ASC_LARGE.csv")
P102_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P102_ASC_LARGE.csv")
P103_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P103_ASC_LARGE.csv")
P104_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P104_ASC_LARGE.csv")
P105_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P105_ASC_LARGE.csv")
P106_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P106_ASC_LARGE.csv")
P107_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P107_ASC_LARGE.csv")
P108_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P108_ASC_LARGE.csv")
P109_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P109_ASC_LARGE.csv")
P110_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P110_ASC_LARGE.csv")
P111_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P111_ASC_LARGE.csv")
P112_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P112_ASC_LARGE.csv")
P113_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P113_ASC_LARGE.csv")
P114_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P114_ASC_LARGE.csv")
P115_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P115_ASC_LARGE.csv")
P116_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P116_ASC_LARGE.csv")
P117_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P117_ASC_LARGE.csv")
P118_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P118_ASC_LARGE.csv")
P119_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P119_ASC_LARGE.csv")
P120_ASC_LARGE <- read_csv("Tidy_RT_data/Indirect_Replies/P120_ASC_LARGE.csv")
# Combining the individual data spreadsheets into one data frame.

alldata_IR_RT <- rbind (P1_ASC_LARGE, P2_ASC_LARGE, P3_ASC_LARGE,P4_ASC_LARGE, P5_ASC_LARGE,
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
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(Group_Status = participant <= 60)

# Rename TRUE FALSE to more meaningful labels.
alldata_IR_RT$Group_Status[alldata_IR_RT$Group_Status == 'TRUE'] <- "ASC"
alldata_IR_RT$Group_Status[alldata_IR_RT$Group_Status == 'FALSE'] <- "TD"
#view(alldata_IR_RT)

#Rename condition_number to more meaningful numbers
alldata_IR_RT$condition_number <- recode(alldata_IR_RT$condition_number, "1" = "Negative", "2" = "Positive", "3" = "Neutral")
#view(alldata_IR_RT)

#NO NA's so we don't need to throw away any 0 values like we do in other scripts when there has been a problem
#with data collection using the eyetracker.
#sum(is.na(alldata_IR_RT$RT1))
#sum(is.na(alldata_IR_RT$RT2))
#sum(is.na(alldata_IR_RT$RT3))
#sum(is.na(alldata_IR_RT$RT4))
#sum(is.na(alldata_IR_RT$RT5))
#sum(is.na(alldata_IR_RT$RT6))

#Create ms. over second so as to be comparable with previous studies
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT1ms = RT1*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT2ms = RT2*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT3ms = RT3*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT4ms = RT4*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT5ms = RT5*1000)
alldata_IR_RT <- alldata_IR_RT%>%
  mutate(RT6ms = RT6*1000)
#Now Let's add in individual differences
#Import Individual difference measures
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Indirect_Replies/Reduced_IDs_IR.csv")
#View(Reduced_IDs_IR)

all_data_join <- inner_join(alldata_IR_RT, Reduced_IDs_IR, by = "participant")


#View(all_data_join)

# Scale the ID measures...
all_data_join$total_t_score <- scale(all_data_join$total_t_score)
all_data_join$total_RAW_score <- scale(all_data_join$total_RAW_score)
all_data_join$EQ_score <- scale(all_data_join$EQ_score)


# Model including covariates 
#mlm1 <- lm(cbind(condition_number, Group_Status) ~ RT2ms + total_t_score + EQ_score, data = all_data_join)
#summary(mlm1)

#Model R2 including covariates and GS
model_alldata_RT2msGS <- lmer(RT2ms ~  condition_number * Group_Status + total_t_score + EQ_score + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_alldata_RT2msGS)

SER2 = emmeans(model_alldata_RT2msGS, specs = 'condition_number', 'Group_Status')
summary(SER2)
SER2b = emmeans(model_alldata_RT2msGS, specs = 'condition_number')
summary(SER2b)

library(emmeans)
emmeans(model_alldata_RT2msGS , pairwise ~ condition_number * Group_Status)


library(ggpubr)
compare_means(RT2ms ~ condition_number, data = all_data_join, 
              group.by = "Group_Status", method = "t.test")

# Box plot facetted by "Group_Status"
p <- ggboxplot(all_data_join, x = "condition_number", y = "RT2ms",
               color = "Group_Status", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = Group_Status))
p + stat_compare_means(aes(group = Group_Status), label = "p.signif")

#THIS IS CONFUSING 
#Difference between conditions by group
compare_means(RT2ms ~ Group_Status, data = all_data_join, 
              group.by = "condition_number")
# Box plot facetted by "Group_Status"
p <- ggboxplot(all_data_join, x = "Group_Status", y = "RT2ms",
               color = "condition_number", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = condition_number))
p + stat_compare_means(aes(group = condition_number), label = "p.signif")

compare_means(RT2ms ~ Group_Status, data = all_data_join, 
              group.by = "condition_number", paired = TRUE)
p <- ggpaired(all_data_join, x = "Group_Status", y = "RT2ms",
              color = "Group_Status", palette = "jco",
              line.color = "gray", line.size = 0.4,
              facet.by = "condition_number", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.signif")
#p + labs(x = "Group Status")
#p + labs(y = "Reading Time in ms.")
all_data_join %>%
  group_by(condition_number, Group_Status) %>%
  get_summary_stats(RT2ms, type = "mean_sd")



#Region 3
#Model including covariates and group status 
Q <- quantile(all_data_join$RT3ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT3ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT3ms > (Q[1] - 2.0*iqr) & all_data_join$RT3ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT3ms, outlier.tagging = TRUE) 

#By Group
eliminated %>% 
  group_by(condition_number, Group_Status) %>%
  summarise(mean(RT3ms), sd(RT3ms))

modelRT3msGS <- lmer(RT3ms ~ condition_number + Group_Status + (1 | participant) + (1 | item_number), data = eliminated,REML = TRUE) 
summary(modelRT3msGS)


SER3 = emmeans(model_alldatacov_RT3msGS, specs = 'condition_number', 'Group_Status')
summary(SER3)

#Region 4

#R4
#Model including covariates and Group Status
#Singularity of fit error lets try with removed outliers
#ggbetweenstats(all_data_join, condition_number, RT2ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$RT4ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT4ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT4ms > (Q[1] - 2.0*iqr) & all_data_join$RT4ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT4ms, outlier.tagging = TRUE)

model_alldatacov_RT4ms <- lmer(RT4ms ~ condition_number*Group_Status + total_t_score + EQ_score + (1 | participant) +  (1 | item_number) , data = eliminated, REML = TRUE)
summary(model_alldatacov_RT4ms)

SERR4 = emmeans(model_alldatacov_RT4ms, specs = 'condition_number', 'Group_Status')
summary(SERR4)

emmeans(model_alldatacov_RT4ms , pairwise ~ condition_number * Group_Status)
library(ggpubr)
# Box plot facetted by "Group_Status"
p <- ggboxplot(eliminated, x = "Group_Status", y = "RT4ms",
               color = "condition_number", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = condition_number))
p + stat_compare_means(aes(group = condition_number), label = "p.signif")

eliminated %>%
  group_by(condition_number, Group_Status) %>%
  get_summary_stats(RT4ms, type = "mean_sd")

#Region 5
#Have a lookat outliers as that standard deviation is crazy out!
ggbetweenstats(all_data_join, condition_number, RT5ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$RT5ms, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$RT5ms)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$RT5ms > (Q[1] - 2.0*iqr) & all_data_join$RT5ms < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, condition_number, RT5ms, outlier.tagging = TRUE) 
#
eliminated %>% 
  group_by(condition_number, Group_Status) %>%
  summarise(mean(RT5ms), sd(RT5ms))
# Model including covariates & Group_Status
model_alldatacov_RT5ms <- lmer(RT5ms ~ condition_number + total_t_score + EQ_score + Group_Status + (1 | participant) +  (1 | item_number) , data = eliminated, REML = TRUE)
summary(model_alldatacov_RT5ms)


## Let's have a look at total reading time across all regions

all_data_join <- all_data_join %>% group_by(participant) %>%
  mutate(TT = (RT1ms + RT2ms + RT3ms + RT4ms + RT5ms + RT6ms))
#Descriptives
all_data_join %>% 
  group_by(condition_number, Group_Status) %>%
  summarise(mean(TT), sd(TT))

# Model including covariates & Group Status
model_alldatacov_TTGS <- lmer(TT ~ condition_number + total_t_score + EQ_score + Group_Status + (1 | participant) +  (1 | item_number) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_TTGS)

SERTT = emmeans(model_alldatacov_TTGS, specs = 'condition_number', 'Group_Status')
summary(SERTT)
