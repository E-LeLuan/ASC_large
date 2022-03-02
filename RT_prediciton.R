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

alldata <- rbind (P1_ASC_LARGE, P2_ASC_LARGE, P3_ASC_LARGE,P4_ASC_LARGE, P5_ASC_LARGE,
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

# Make sure we've combined are data correctly. 
view(alldata)  

# alldata <- alldata %>% 
#  add_column(Group_status = NA)

# Create a new column to specify group status ASC vs TD.
alldata <- alldata%>%
  mutate(Group_Status = participant <= 60)

# Rename TRUE FALSE to more meaningful labels.
alldata$Group_Status[alldata$Group_Status == 'TRUE'] <- "ASC"
alldata$Group_Status[alldata$Group_Status == 'FALSE'] <- "TD"


# Assign condition labels, 1 = facilitated, 2 = unfacilitated
# alldata$condition_number <- recode(alldata$condition_number, "1" = "Facilitated", "2" = "unfacilitated")

view(alldata)

#Let's have a look at region 4

# Turn seconds into milliseconds to be comparable with previous studies
alldata <- alldata%>%
  mutate(RT3ms = RT3*1000)
alldata <- alldata%>%
  mutate(RT4ms = RT4*1000)
alldata <- alldata%>%
  mutate(RT5ms = RT5*1000)

# Double check it has combined and relabeled correctly.
view(alldata)

# RT3 & RT4ms descriptive
alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

#Plots
alldata %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Prediciton") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

alldata %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Question") +
  labs(y = "Reading time in ms.", x = "Question") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

alldata %>% 
  ggplot(aes(x = condition_number, y = RT5ms, colour = Group_Status)) + ggtitle("Reply") +
  labs(y = "Reading time in ms.", x = "Reply") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

# Model assuming normality of residuals maximal structure


# Region 3- Prediction
# With interaction
model.nullR3 <- lmer(RT3ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR3 <- lmer(RT3ms ~ condition_number*Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR3)

anova(model.nullR3, modelR3)

#remove group status
model.nullR3b <- lmer(RT3ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR3b <- lmer(RT3ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR3b)

anova(model.nullR3b, modelR3b)

# No singualr fit
model.nullR3c <- lmer(RT3ms ~ (1 | participant) + (1 | item_number), alldata) 
modelR3c <- lmer(RT3ms ~ condition_number*Group_Status + (1 | participant) + (1 | item_number), alldata) 

summary(modelR3c)

anova(model.nullR3c, modelR3c)

check_model(modelR3)
qqnorm(residuals(modelR3))
qqline(residuals(modelR3))
descdist(alldata$RT3ms)


# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")

#Subset descriptives
ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

# Separate models for groups

#ASC
model.nullR3ASC <- lmer(RT4ms ~ (1 + condition_number| participant) + (1 | item_number), ASC_Group) 
modelR3ASC <- lmer(RT4ms ~ condition_number + (1 + condition_number| participant) + (1 | item_number), ASC_Group) 

summary(modelR3ASC)

anova(model.nullR3ASC, modelR3ASC)

#TD

model.nullR3TD <- lmer(RT3ms ~ (1 + condition_number | participant) + (1 | item_number), TD_Group) 
modelR3TD <- lmer(RT3ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), TD_Group) 

summary(modelR4TD)

anova(model.nullR3TD, modelR3TD)




# Region 4- Question
# With interaction
model.nullR4 <- lmer(RT4ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR4 <- lmer(RT4ms ~ condition_number*Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR4)

anova(model.nullR4, modelR4)

#remove group status
model.nullR4b <- lmer(RT4ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR4b <- lmer(RT4ms ~ condition_number + Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR4b)

anova(model.nullR4b, modelR42)

check_model(modelR4)
qqnorm(residuals(modelR4))
qqline(residuals(modelR4))
descdist(alldata$RT4ms)

# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")

#Subset descriptives
ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

# Separate models for groups

#ASC
model.nullR4ASC <- lmer(RT4ms ~ (1 + condition_number| participant) + (1 | item_number), ASC_Group) 
modelR4ASC <- lmer(RT4ms ~ condition_number + (1 + condition_number| participant) + (1 | item_number), ASC_Group) 

summary(modelR4ASC)

anova(model.nullR4ASC, modelR4ASC)

#TD

model.nullR4TD <- lmer(RT4ms ~ (1 | participant) + (1 | item_number), TD_Group) 
modelR4TD <- lmer(RT4ms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 

summary(modelR4TD)

anova(model.nullR4TD, modelR4TD)


# Region 5- Reply
# With interaction ignore singular fit
model.nullR5 <- lmer(RT5ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR5 <- lmer(RT3ms ~ condition_number*Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR5)

anova(model.nullR5, modelR5)

#remove group status and singular fit
model.nullR5b <- lmer(RT5ms ~ (1 + condition_number| participant) + (1 | item_number), alldata) 
modelR5b <- lmer(RT5ms ~ condition_number + Group_Status + (1 + condition_number | participant) + (1 | item_number), alldata) 

summary(modelR5b)

anova(model.nullR5b, modelR5b)

check_model(modelR5)
qqnorm(residuals(modelR5))
qqline(residuals(modelR5))
descdist(alldata$RT5ms)


# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")

#Subset descriptives
ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

ASC_Group %>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

TD_Group%>% 
  group_by(condition_number) %>%
  summarise(mean(RT5ms), sd(RT5ms))

# Separate models for groups

#ASC
model.nullR5ASC <- lmer(RT5ms ~ (1 + condition_number| participant) + (1 | item_number), ASC_Group) 
modelR5ASC <- lmer(RT5ms ~ condition_number + (1 + condition_number| participant) + (1 | item_number), ASC_Group) 

summary(modelR5ASC)

anova(model.nullR5ASC, modelR5ASC)

#TD

model.nullR5TD <- lmer(RT5ms ~ (1 + condition_number | participant) + (1 | item_number), TD_Group) 
modelR5TD <- lmer(RT5ms ~ condition_number + (1 + condition_number | participant) + (1 | item_number), TD_Group) 

summary(modelR5TD)

anova(model.nullR5TD, modelR5TD)

# Create Total time variable 

#Total time
# Create Total time variable 
alldata <- alldata%>%
  mutate(TT = RT1+RT2+RT3+RT4+RT5+RT6) %>% 
  mutate(TTms = TT*1000)

view(alldata)

#model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelTT <- lmer(TTms ~ condition_number * Group_Status + (1 | participant) + (1 + condition_number | item_number), alldata) 
summary(modelTT)
check_model(modelTT)
qqnorm(residuals(modelTT))
qqline(residuals(modelTT))
descdist(alldata$TT)
#ranef(modelR4)

# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")

# Seperate analysis based on group
modelTT_TD <- lmer(TTms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelTT_TD)                   

modelTT_ASC <- lmer(TTms ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelTT_ASC) 
