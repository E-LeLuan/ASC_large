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
library(optimx)
library(dfoptim)
library(parallel)
library(minqa)

set.seed(1234)

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
  #recode(Group_Status, TRUE = "ASC", FALSE = "TD", default = NA)}


# Assign condition labels, 1 = positive, 2 = negative, 3 = neutral
alldata$condition_number <- recode(alldata$condition_number, "1" = "positive", "2" = "negative", "3" = "neutral")

# Double check it has combined and relabeled correctly.
view(alldata)

#Let's have a look at region 4

# Turn seconds into milliseconds to be comparable with previous studies
alldata <- alldata%>%
  mutate(RT2ms = RT2*1000)
alldata <- alldata%>%
  mutate(RT3ms = RT3*1000)
alldata <- alldata%>%
  mutate(RT4ms = RT4*1000)

# Double check it has combined and relabeled correctly.
view(alldata)


# Descriptives
alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT2ms), sd(RT2ms))

alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT3ms), sd(RT3ms))

alldata %>% 
  group_by(condition_number) %>%
  summarise(mean(RT4ms), sd(RT4ms))

view(alldata)


#Visualization

alldata %>% 
  ggplot(aes(x = condition_number, y = RT2ms, colour = Group_Status)) + ggtitle("Manipulation") +
  labs(y = "Reading time in ms.", x = "Manipulation") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

alldata %>% 
  ggplot(aes(x = condition_number, y = RT3ms, colour = Group_Status)) + ggtitle("Question") +
  labs(y = "Reading time in ms.", x = "Question") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')

alldata %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Indirect Reply") +
  labs(y = "Reading time in ms.", x = "Indirect Reply") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(scale = 'none')


# Some serious outliers may need removing at this point --> need to figure out how to do this!

alldata %>% 
  ggplot(aes(x = condition_number, y = RT4ms, colour = Group_Status)) + ggtitle("Prediciton") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_boxplot()

# Create subset data lists
ASC_Group <- filter(alldata, Group_Status == "ASC")
TD_Group <- filter(alldata, Group_Status == "TD")


# Struggling to make the code work from here on.
# Model the data

#alldata$participant <- as.factor(alldata$participant)
#alldata$condition_number <- as.factor(alldata$condition_number)
#alldata$Group_Status <- as.factor(alldata$Group_Status)
#alldata$RT4ms <- as.factor(alldata$RT4ms)
#alldata$item_number <- as.factor(alldata$item_number)

# Model assuming normality of residuals maximal structure


# Region 2- Manipulation
# Using allFit() with (g)lmer by Josh Nugent to optimize the model automatically https://joshua-nugent.github.io/allFit/ 
#model.nullR3 <- lmer(RT3ms ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR2 <- glmer(RT2ms ~ condition_number * Group_Status + (1 + condition_number | participant) + 
                   (1 + condition_number | item_number), alldata, family = Gamma) 
diff_optims <- allFit(modelR2, maxfun = 1e5, parallel = 'multicore', ncpus = detectCores())
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)

convergence_results <- lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
working_indices <- sapply(convergence_results, is.null)

if(sum(working_indices)==0){
  print("No algorithms from allFit converged.")
  print("You may still be able to use the results, but proceed with extreme caution.")
  first_fit <- NULL
} else {
  first_fit <- diff_optims[working_indices][[1]]
}
first_fit

modelR2 <- glmer(RT2ms ~ condition_number * Group_Status + (1 | participant) + 
                   (1 | item_number), alldata, family = Gamma)

summary(modelR2)
check_model(modelR2)
qqnorm(residuals(modelR2))
qqline(residuals(modelR2))
descdist(alldata$RT2ms)
#ranef(modelR4)

# Seperate analysis based on group
#modelR3_TD <- glmer(RT3ms ~ condition_number + (1 + condition_number | participant) + (1 + condition_number | item_number), TD_Group) 
#summary(modelR3_TD)                   

#modelR3_ASC <- lmer(RT3ms ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
#summary(modelR3_ASC)                   

# Region 3- Question

#model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR3 <- glmer(RT3ms ~ condition_number * Group_Status + (1 + condition_number | participant) + (1 + condition_number | item_number), alldata, family = Gamma) 
summary(modelR3)
diff_optims <- allFit(modelR3, maxfun = 1e5, parallel = 'multicore', ncpus = detectCores())
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)

convergence_results <- lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
working_indices <- sapply(convergence_results, is.null)

if(sum(working_indices)==0){
  print("No algorithms from allFit converged.")
  print("You may still be able to use the results, but proceed with extreme caution.")
  first_fit <- NULL
} else {
  first_fit <- diff_optims[working_indices][[1]]
}
first_fit

check_model(modelR3)
qqnorm(residuals(modelR3))
qqline(residuals(modelR3))
descdist(alldata$RT3ms)

modelR3 <- glmer(RT3ms ~ condition_number * Group_Status + (1 | participant) + (1 | item_number), alldata, family = Gamma) 
summary(modelR3)


#ranef(modelR4)

# No singular fit 
modelR3 <- lmer(RT3ms ~ condition_number * Group_Status + (1 | participant) + (1 | item_number), alldata) 
summary(modelR3)
check_model(modelR3)
qqnorm(residuals(modelR3))
qqline(residuals(modelR3))
descdist(alldata$RT3ms)



# Seperate analysis based on group
modelR3_TD <- lmer(RT3ms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelR3_TD)                   

modelR4_ASC <- lmer(RT4ms ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelR4_ASC)      


#Region 4 Indirect Reply

#model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR4 <- lmer(RT4ms ~ condition_number * Group_Status + (1 | participant) + (1 | item_number), alldata) 
summary(modelR4)
check_model(modelR4)
qqnorm(residuals(modelR4))
qqline(residuals(modelR4))
descdist(alldata$RT4ms)
#ranef(modelR4)


# Seperate analysis based on group
modelR4_TD <- lmer(RT4ms ~ condition_number + (1 | participant) + (1 | item_number), TD_Group) 
summary(modelR4_TD)                   

modelR4_ASC <- lmer(RT4ms ~ condition_number + (1 | participant) + (1 | item_number), ASC_Group) 
summary(modelR4_ASC)  


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
