#EQ auto tidy script (attempt)


library(tidyverse)


set.seed(1234)

# Importing the data into R.

library(readr)
P1_ASC_LARGE_EQ <- read_csv("EQ_data/P1_ASC_LARGE_EQ.csv")
P2_ASC_LARGE_EQ <- read_csv("EQ_data/P2_ASC_LARGE_EQ.csv")
P3_ASC_LARGE_EQ <- read_csv("EQ_data/P3_ASC_LARGE_EQ.csv")
P4_ASC_LARGE_EQ <- read_csv("EQ_data/P4_ASC_LARGE_EQ.csv")
P5_ASC_LARGE_EQ <- read_csv("EQ_data/P5_ASC_LARGE_EQ.csv")
P6_ASC_LARGE_EQ <- read_csv("EQ_data/P6_ASC_LARGE_EQ.csv")
P7_ASC_LARGE_EQ <- read_csv("EQ_data/P7_ASC_LARGE_EQ.csv")
P8_ASC_LARGE_EQ <- read_csv("EQ_data/P8_ASC_LARGE_EQ.csv")
P9_ASC_LARGE_EQ <- read_csv("EQ_data/P9_ASC_LARGE_EQ.csv")
P10_ASC_LARGE_EQ <- read_csv("EQ_data/P10_ASC_LARGE_EQ.csv")
P11_ASC_LARGE_EQ <- read_csv("EQ_data/P11_ASC_LARGE_EQ.csv")
P12_ASC_LARGE_EQ <- read_csv("EQ_data/P12_ASC_LARGE_EQ.csv")
P13_ASC_LARGE_EQ <- read_csv("EQ_data/P13_ASC_LARGE_EQ.csv")
P14_ASC_LARGE_EQ <- read_csv("EQ_data/P14_ASC_LARGE_EQ.csv")
P15_ASC_LARGE_EQ <- read_csv("EQ_data/P15_ASC_LARGE_EQ.csv")
P16_ASC_LARGE_EQ <- read_csv("EQ_data/P16_ASC_LARGE_EQ.csv")
P17_ASC_LARGE_EQ <- read_csv("EQ_data/P17_ASC_LARGE_EQ.csv")
P18_ASC_LARGE_EQ <- read_csv("EQ_data/P18_ASC_LARGE_EQ.csv")
P19_ASC_LARGE_EQ <- read_csv("EQ_data/P19_ASC_LARGE_EQ.csv")
P20_ASC_LARGE_EQ <- read_csv("EQ_data/P20_ASC_LARGE_EQ.csv")
P21_ASC_LARGE_EQ <- read_csv("EQ_data/P21_ASC_LARGE_EQ.csv")
P22_ASC_LARGE_EQ <- read_csv("EQ_data/P22_ASC_LARGE_EQ.csv")
P23_ASC_LARGE_EQ <- read_csv("EQ_data/P23_ASC_LARGE_EQ.csv")
P24_ASC_LARGE_EQ <- read_csv("EQ_data/P24_ASC_LARGE_EQ.csv")
P25_ASC_LARGE_EQ <- read_csv("EQ_data/P25_ASC_LARGE_EQ.csv")
P26_ASC_LARGE_EQ <- read_csv("EQ_data/P26_ASC_LARGE_EQ.csv")
P27_ASC_LARGE_EQ <- read_csv("EQ_data/P27_ASC_LARGE_EQ.csv")
P28_ASC_LARGE_EQ <- read_csv("EQ_data/P28_ASC_LARGE_EQ.csv")
P29_ASC_LARGE_EQ <- read_csv("EQ_data/P29_ASC_LARGE_EQ.csv")
P30_ASC_LARGE_EQ <- read_csv("EQ_data/P30_ASC_LARGE_EQ.csv")
P31_ASC_LARGE_EQ <- read_csv("EQ_data/P31_ASC_LARGE_EQ.csv")
P32_ASC_LARGE_EQ <- read_csv("EQ_data/P32_ASC_LARGE_EQ.csv")
P33_ASC_LARGE_EQ <- read_csv("EQ_data/P33_ASC_LARGE_EQ.csv")
P34_ASC_LARGE_EQ <- read_csv("EQ_data/P34_ASC_LARGE_EQ.csv")
P35_ASC_LARGE_EQ <- read_csv("EQ_data/P35_ASC_LARGE_EQ.csv")
P36_ASC_LARGE_EQ <- read_csv("EQ_data/P36_ASC_LARGE_EQ.csv")
P37_ASC_LARGE_EQ <- read_csv("EQ_data/P37_ASC_LARGE_EQ.csv")
P38_ASC_LARGE_EQ <- read_csv("EQ_data/P38_ASC_LARGE_EQ.csv")
P39_ASC_LARGE_EQ <- read_csv("EQ_data/P39_ASC_LARGE_EQ.csv")
P40_ASC_LARGE_EQ <- read_csv("EQ_data/P40_ASC_LARGE_EQ.csv")
P41_ASC_LARGE_EQ <- read_csv("EQ_data/P41_ASC_LARGE_EQ.csv")
P42_ASC_LARGE_EQ <- read_csv("EQ_data/P42_ASC_LARGE_EQ.csv")
P43_ASC_LARGE_EQ <- read_csv("EQ_data/P43_ASC_LARGE_EQ.csv")
P44_ASC_LARGE_EQ <- read_csv("EQ_data/P44_ASC_LARGE_EQ.csv")
P45_ASC_LARGE_EQ <- read_csv("EQ_data/P45_ASC_LARGE_EQ.csv")
P46_ASC_LARGE_EQ <- read_csv("EQ_data/P46_ASC_LARGE_EQ.csv")
P47_ASC_LARGE_EQ <- read_csv("EQ_data/P47_ASC_LARGE_EQ.csv")
P48_ASC_LARGE_EQ <- read_csv("EQ_data/P48_ASC_LARGE_EQ.csv")
P49_ASC_LARGE_EQ <- read_csv("EQ_data/P49_ASC_LARGE_EQ.csv")
P50_ASC_LARGE_EQ <- read_csv("EQ_data/P50_ASC_LARGE_EQ.csv")
P51_ASC_LARGE_EQ <- read_csv("EQ_data/P51_ASC_LARGE_EQ.csv")
P52_ASC_LARGE_EQ <- read_csv("EQ_data/P52_ASC_LARGE_EQ.csv")
P53_ASC_LARGE_EQ <- read_csv("EQ_data/P53_ASC_LARGE_EQ.csv")
P54_ASC_LARGE_EQ <- read_csv("EQ_data/P54_ASC_LARGE_EQ.csv")
P55_ASC_LARGE_EQ <- read_csv("EQ_data/P55_ASC_LARGE_EQ.csv")
P56_ASC_LARGE_EQ <- read_csv("EQ_data/P56_ASC_LARGE_EQ.csv")
P57_ASC_LARGE_EQ <- read_csv("EQ_data/P57_ASC_LARGE_EQ.csv")
P58_ASC_LARGE_EQ <- read_csv("EQ_data/P58_ASC_LARGE_EQ.csv")
P59_ASC_LARGE_EQ <- read_csv("EQ_data/P59_ASC_LARGE_EQ.csv")
P60_ASC_LARGE_EQ <- read_csv("EQ_data/P60_ASC_LARGE_EQ.csv")
P61_ASC_LARGE_EQ <- read_csv("EQ_data/P61_ASC_LARGE_EQ.csv")
P62_ASC_LARGE_EQ <- read_csv("EQ_data/P62_ASC_LARGE_EQ.csv")
P63_ASC_LARGE_EQ <- read_csv("EQ_data/P63_ASC_LARGE_EQ.csv")
P64_ASC_LARGE_EQ <- read_csv("EQ_data/P64_ASC_LARGE_EQ.csv")
P65_ASC_LARGE_EQ <- read_csv("EQ_data/P65_ASC_LARGE_EQ.csv")
P66_ASC_LARGE_EQ <- read_csv("EQ_data/P66_ASC_LARGE_EQ.csv")
P67_ASC_LARGE_EQ <- read_csv("EQ_data/P67_ASC_LARGE_EQ.csv")
P68_ASC_LARGE_EQ <- read_csv("EQ_data/P68_ASC_LARGE_EQ.csv")
P69_ASC_LARGE_EQ <- read_csv("EQ_data/P69_ASC_LARGE_EQ.csv")
P70_ASC_LARGE_EQ <- read_csv("EQ_data/P70_ASC_LARGE_EQ.csv")
P71_ASC_LARGE_EQ <- read_csv("EQ_data/P71_ASC_LARGE_EQ.csv")
P72_ASC_LARGE_EQ <- read_csv("EQ_data/P72_ASC_LARGE_EQ.csv")
P73_ASC_LARGE_EQ <- read_csv("EQ_data/P73_ASC_LARGE_EQ.csv")
P74_ASC_LARGE_EQ <- read_csv("EQ_data/P74_ASC_LARGE_EQ.csv")
P75_ASC_LARGE_EQ <- read_csv("EQ_data/P75_ASC_LARGE_EQ.csv")
P76_ASC_LARGE_EQ <- read_csv("EQ_data/P76_ASC_LARGE_EQ.csv")
P77_ASC_LARGE_EQ <- read_csv("EQ_data/P77_ASC_LARGE_EQ.csv")
P78_ASC_LARGE_EQ <- read_csv("EQ_data/P78_ASC_LARGE_EQ.csv")
P79_ASC_LARGE_EQ <- read_csv("EQ_data/P79_ASC_LARGE_EQ.csv")
P80_ASC_LARGE_EQ <- read_csv("EQ_data/P80_ASC_LARGE_EQ.csv")
P81_ASC_LARGE_EQ <- read_csv("EQ_data/P81_ASC_LARGE_EQ.csv")
P82_ASC_LARGE_EQ <- read_csv("EQ_data/P82_ASC_LARGE_EQ.csv")
P83_ASC_LARGE_EQ <- read_csv("EQ_data/P83_ASC_LARGE_EQ.csv")
P84_ASC_LARGE_EQ <- read_csv("EQ_data/P84_ASC_LARGE_EQ.csv")
P85_ASC_LARGE_EQ <- read_csv("EQ_data/P85_ASC_LARGE_EQ.csv")
P86_ASC_LARGE_EQ <- read_csv("EQ_data/P86_ASC_LARGE_EQ.csv")
P87_ASC_LARGE_EQ <- read_csv("EQ_data/P87_ASC_LARGE_EQ.csv")
P88_ASC_LARGE_EQ <- read_csv("EQ_data/P88_ASC_LARGE_EQ.csv")
P89_ASC_LARGE_EQ <- read_csv("EQ_data/P89_ASC_LARGE_EQ.csv")
P90_ASC_LARGE_EQ <- read_csv("EQ_data/P90_ASC_LARGE_EQ.csv")
P91_ASC_LARGE_EQ <- read_csv("EQ_data/P91_ASC_LARGE_EQ.csv")
P92_ASC_LARGE_EQ <- read_csv("EQ_data/P92_ASC_LARGE_EQ.csv")
P93_ASC_LARGE_EQ <- read_csv("EQ_data/P93_ASC_LARGE_EQ.csv")
P94_ASC_LARGE_EQ <- read_csv("EQ_data/P94_ASC_LARGE_EQ.csv")
P95_ASC_LARGE_EQ <- read_csv("EQ_data/P95_ASC_LARGE_EQ.csv")
P96_ASC_LARGE_EQ <- read_csv("EQ_data/P96_ASC_LARGE_EQ.csv")
P97_ASC_LARGE_EQ <- read_csv("EQ_data/P97_ASC_LARGE_EQ.csv")
P98_ASC_LARGE_EQ <- read_csv("EQ_data/P98_ASC_LARGE_EQ.csv")
P99_ASC_LARGE_EQ <- read_csv("EQ_data/P99_ASC_LARGE_EQ.csv")
P100_ASC_LARGE_EQ <- read_csv("EQ_data/P100_ASC_LARGE_EQ.csv")
P101_ASC_LARGE_EQ <- read_csv("EQ_data/P101_ASC_LARGE_EQ.csv")
P102_ASC_LARGE_EQ <- read_csv("EQ_data/P102_ASC_LARGE_EQ.csv")
P103_ASC_LARGE_EQ <- read_csv("EQ_data/P103_ASC_LARGE_EQ.csv")
P104_ASC_LARGE_EQ <- read_csv("EQ_data/P104_ASC_LARGE_EQ.csv")
P105_ASC_LARGE_EQ <- read_csv("EQ_data/P105_ASC_LARGE_EQ.csv")
P106_ASC_LARGE_EQ <- read_csv("EQ_data/P106_ASC_LARGE_EQ.csv")
P107_ASC_LARGE_EQ <- read_csv("EQ_data/P107_ASC_LARGE_EQ.csv")
P108_ASC_LARGE_EQ <- read_csv("EQ_data/P108_ASC_LARGE_EQ.csv")
P109_ASC_LARGE_EQ <- read_csv("EQ_data/P109_ASC_LARGE_EQ.csv")
P110_ASC_LARGE_EQ <- read_csv("EQ_data/P110_ASC_LARGE_EQ.csv")
P111_ASC_LARGE_EQ <- read_csv("EQ_data/P111_ASC_LARGE_EQ.csv")
P112_ASC_LARGE_EQ <- read_csv("EQ_data/P112_ASC_LARGE_EQ.csv")
P113_ASC_LARGE_EQ <- read_csv("EQ_data/P113_ASC_LARGE_EQ.csv")
P114_ASC_LARGE_EQ <- read_csv("EQ_data/P114_ASC_LARGE_EQ.csv")
P115_ASC_LARGE_EQ <- read_csv("EQ_data/P115_ASC_LARGE_EQ.csv")
P116_ASC_LARGE_EQ <- read_csv("EQ_data/P116_ASC_LARGE_EQ.csv")
P117_ASC_LARGE_EQ <- read_csv("EQ_data/P117_ASC_LARGE_EQ.csv")
P118_ASC_LARGE_EQ <- read_csv("EQ_data/P118_ASC_LARGE_EQ.csv")
P119_ASC_LARGE_EQ <- read_csv("EQ_data/P119_ASC_LARGE_EQ.csv")
P120_ASC_LARGE_EQ <- read_csv("EQ_data/P120_ASC_LARGE_EQ.csv")

# Combining the individual data spreadsheets into one data frame.

alldata_EQ <- rbind (P1_ASC_LARGE_EQ, P2_ASC_LARGE_EQ, P3_ASC_LARGE_EQ,P4_ASC_LARGE_EQ, P5_ASC_LARGE_EQ,
                  P6_ASC_LARGE_EQ, P7_ASC_LARGE_EQ, P8_ASC_LARGE_EQ, P9_ASC_LARGE_EQ, P10_ASC_LARGE_EQ,
                  P11_ASC_LARGE_EQ, P12_ASC_LARGE_EQ, P13_ASC_LARGE_EQ, P14_ASC_LARGE_EQ, P15_ASC_LARGE_EQ,
                  P16_ASC_LARGE_EQ,P17_ASC_LARGE_EQ, P18_ASC_LARGE_EQ, P19_ASC_LARGE_EQ,P20_ASC_LARGE_EQ,
                  P21_ASC_LARGE_EQ, P22_ASC_LARGE_EQ, P23_ASC_LARGE_EQ,P24_ASC_LARGE_EQ, P25_ASC_LARGE_EQ, P26_ASC_LARGE_EQ,
                  P27_ASC_LARGE_EQ, P28_ASC_LARGE_EQ, P29_ASC_LARGE_EQ, P30_ASC_LARGE_EQ, P31_ASC_LARGE_EQ,P32_ASC_LARGE_EQ,
                  P33_ASC_LARGE_EQ, P34_ASC_LARGE_EQ, P35_ASC_LARGE_EQ, P36_ASC_LARGE_EQ, P37_ASC_LARGE_EQ, P38_ASC_LARGE_EQ,
                  P39_ASC_LARGE_EQ,P40_ASC_LARGE_EQ, P41_ASC_LARGE_EQ, P42_ASC_LARGE_EQ, P43_ASC_LARGE_EQ, P44_ASC_LARGE_EQ,
                  P45_ASC_LARGE_EQ, P46_ASC_LARGE_EQ, P47_ASC_LARGE_EQ,P48_ASC_LARGE_EQ, P49_ASC_LARGE_EQ, P50_ASC_LARGE_EQ,
                  P51_ASC_LARGE_EQ, P52_ASC_LARGE_EQ, P53_ASC_LARGE_EQ, P54_ASC_LARGE_EQ, P55_ASC_LARGE_EQ,P56_ASC_LARGE_EQ,
                  P57_ASC_LARGE_EQ, P58_ASC_LARGE_EQ,P59_ASC_LARGE_EQ, P60_ASC_LARGE_EQ, P61_ASC_LARGE_EQ, P62_ASC_LARGE_EQ, 
                  P63_ASC_LARGE_EQ, P64_ASC_LARGE_EQ, P65_ASC_LARGE_EQ, P66_ASC_LARGE_EQ, P67_ASC_LARGE_EQ, P68_ASC_LARGE_EQ,
                  P69_ASC_LARGE_EQ,P70_ASC_LARGE_EQ, P71_ASC_LARGE_EQ, P72_ASC_LARGE_EQ, P73_ASC_LARGE_EQ, P74_ASC_LARGE_EQ,
                  P75_ASC_LARGE_EQ, P76_ASC_LARGE_EQ, P77_ASC_LARGE_EQ,P78_ASC_LARGE_EQ, P79_ASC_LARGE_EQ, P80_ASC_LARGE_EQ, 
                  P81_ASC_LARGE_EQ, P82_ASC_LARGE_EQ, P83_ASC_LARGE_EQ, P84_ASC_LARGE_EQ, P85_ASC_LARGE_EQ, P86_ASC_LARGE_EQ, 
                  P87_ASC_LARGE_EQ, P88_ASC_LARGE_EQ, P89_ASC_LARGE_EQ, P90_ASC_LARGE_EQ, P91_ASC_LARGE_EQ, P92_ASC_LARGE_EQ, 
                  P93_ASC_LARGE_EQ, P94_ASC_LARGE_EQ, P95_ASC_LARGE_EQ, P96_ASC_LARGE_EQ, P97_ASC_LARGE_EQ, P98_ASC_LARGE_EQ, 
                  P99_ASC_LARGE_EQ, P100_ASC_LARGE_EQ, P101_ASC_LARGE_EQ, P102_ASC_LARGE_EQ, P103_ASC_LARGE_EQ, P104_ASC_LARGE_EQ, 
                  P105_ASC_LARGE_EQ, P106_ASC_LARGE_EQ, P107_ASC_LARGE_EQ, P108_ASC_LARGE_EQ, P109_ASC_LARGE_EQ, P110_ASC_LARGE_EQ, 
                  P111_ASC_LARGE_EQ, P112_ASC_LARGE_EQ, P113_ASC_LARGE_EQ, P114_ASC_LARGE_EQ, P115_ASC_LARGE_EQ, P116_ASC_LARGE_EQ,
                  P117_ASC_LARGE_EQ, P118_ASC_LARGE_EQ, P119_ASC_LARGE_EQ, P120_ASC_LARGE_EQ)

#view(alldata_EQ)

alldata_EQ <- alldata_EQ%>%
  mutate(Group_Status = participant <= 60)

# Rename TRUE FALSE to more meaningful labels.
alldata_EQ$Group_Status[alldata_EQ$Group_Status == 'TRUE'] <- "ASC"
alldata_EQ$Group_Status[alldata_EQ$Group_Status == 'FALSE'] <- "TD"
view(alldata_EQ)

# EQ scoring ignore none as these are filler items we are not interested in. 

#We want to add a column called EQ_tidy that converts the values to the appropriate score. 
# What the code should do is take items that are positively scored and turn a 1 into a 2, 
# turn a 2 into a 1, and turn 3 and 4 into a 0. 
# What the code should do is take items that are negatively scored and turn a 4 into a 2, 
# turn a 3 into a 1, and turn 1 and 2 into a 0. 

alldata_EQ <- alldata_EQ %>% mutate(EQ_tidy = case_when 
                              (EQ_scoring_type == 'positive' &  EQ_resp == 1 ~ 2, 
                                EQ_scoring_type == 'positive' &  EQ_resp == 2 ~ 1,
                                EQ_scoring_type == 'positive' &  EQ_resp == 3 ~ 0,
                                EQ_scoring_type == 'positive' &  EQ_resp == 4 ~ 0,
                                EQ_scoring_type == 'none' ~ 0,
                                EQ_scoring_type == 'negative' &  EQ_resp == 1 ~ 0,
                                EQ_scoring_type == 'negative' &  EQ_resp == 2 ~ 0,
                                EQ_scoring_type == 'negative' &  EQ_resp == 3 ~ 1,
                                EQ_scoring_type == 'negative' &  EQ_resp == 4 ~ 2,))
view(alldata_EQ)

# It worked!!!!!

# Then we can add this column for each participant to get their individual EQ score
EQ_score <- alldata_EQ %>%
  group_by(participant) %>%
  summarise_at(vars(EQ_tidy), list(EQ_score = sum))

alldata_EQ <- inner_join(alldata_EQ, EQ_score, by = "participant")

view(alldata_EQ)

#Let's look at the difference between the two groups
alldata_EQ %>% 
  group_by(Group_Status) %>%
  summarise(mean(EQ_score), sd(EQ_score))

#Group_Status `mean(EQ_score)` `sd(EQ_score)`
#<chr>                   <dbl>          <dbl>
#1 ASC                      23.7           10.4
#2 TD                       46.2           13.4
#Add in group status
Reduced_IDs_IR <- alldata_EQ%>%
  mutate(Group_Status = participant <= 30)

# Rename TRUE FALSE to more meaningful labels.
Reduced_IDs_IR$Group_Status[Reduced_IDs_IR$Group_Status == 'TRUE'] <- "ASC"
Reduced_IDs_IR$Group_Status[Reduced_IDs_IR$Group_Status == 'FALSE'] <- "TD"
view(Reduced_IDs_IR)


Reduced_IDs_IR <- Reduced_IDs_IR %>%
  distinct(participant, .keep_all = TRUE)

view(Reduced_IDs_IR)



library(ggpubr)

#EQ
compare_means(EQ_score ~ Group_Status, data = Reduced_IDs_IR, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_IDs_IR, x = "Group_Status", y = "EQ_score",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
# Change method
p + stat_compare_means(method = "t.test")

t.test(EQ_score ~ Group_Status, data = Reduced_IDs_IR)
Reduced_IDs_IR %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(EQ_score),
            sdASC = sd(EQ_score))

#Export a CSV of the new data set...
write.csv(Reduced_IDs_IR,"//Desktop/ASC_large/EQ_data\\alldataEQ.csv", row.names = TRUE)
