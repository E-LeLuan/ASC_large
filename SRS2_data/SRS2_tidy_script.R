#SRS2 auto tidy script


library(tidyverse)

set.seed(1234)

# Importing the data into R.

library(readr)
P1_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P1_ASC_LARGE_SRS2.csv")
P2_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P2_ASC_LARGE_SRS2.csv")
P3_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P3_ASC_LARGE_SRS2.csv")
P4_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P4_ASC_LARGE_SRS2.csv")
P5_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P5_ASC_LARGE_SRS2.csv")
P6_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P6_ASC_LARGE_SRS2.csv")
P7_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P7_ASC_LARGE_SRS2.csv")
P8_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P8_ASC_LARGE_SRS2.csv")
P9_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P9_ASC_LARGE_SRS2.csv")
P10_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P10_ASC_LARGE_SRS2.csv")
P11_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P11_ASC_LARGE_SRS2.csv")
P12_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P12_ASC_LARGE_SRS2.csv")
P13_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P13_ASC_LARGE_SRS2.csv")
P14_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P14_ASC_LARGE_SRS2.csv")
P15_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P15_ASC_LARGE_SRS2.csv")
P16_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P16_ASC_LARGE_SRS2.csv")
P17_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P17_ASC_LARGE_SRS2.csv")
P18_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P18_ASC_LARGE_SRS2.csv")
P19_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P19_ASC_LARGE_SRS2.csv")
P20_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P20_ASC_LARGE_SRS2.csv")
P21_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P21_ASC_LARGE_SRS2.csv")
P22_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P22_ASC_LARGE_SRS2.csv")
P23_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P23_ASC_LARGE_SRS2.csv")
P24_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P24_ASC_LARGE_SRS2.csv")
P25_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P25_ASC_LARGE_SRS2.csv")
P26_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P26_ASC_LARGE_SRS2.csv")
P27_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P27_ASC_LARGE_SRS2.csv")
P28_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P28_ASC_LARGE_SRS2.csv")
P29_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P29_ASC_LARGE_SRS2.csv")
P30_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P30_ASC_LARGE_SRS2.csv")
P31_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P31_ASC_LARGE_SRS2.csv")
P32_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P32_ASC_LARGE_SRS2.csv")
P33_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P33_ASC_LARGE_SRS2.csv")
P34_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P34_ASC_LARGE_SRS2.csv")
P35_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P35_ASC_LARGE_SRS2.csv")
P36_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P36_ASC_LARGE_SRS2.csv")
P37_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P37_ASC_LARGE_SRS2.csv")
P38_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P38_ASC_LARGE_SRS2.csv")
P39_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P39_ASC_LARGE_SRS2.csv")
P40_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P40_ASC_LARGE_SRS2.csv")
P41_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P41_ASC_LARGE_SRS2.csv")
P42_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P42_ASC_LARGE_SRS2.csv")
P43_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P43_ASC_LARGE_SRS2.csv")
P44_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P44_ASC_LARGE_SRS2.csv")
P45_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P45_ASC_LARGE_SRS2.csv")
P46_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P46_ASC_LARGE_SRS2.csv")
P47_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P47_ASC_LARGE_SRS2.csv")
P48_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P48_ASC_LARGE_SRS2.csv")
P49_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P49_ASC_LARGE_SRS2.csv")
P50_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P50_ASC_LARGE_SRS2.csv")
P51_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P51_ASC_LARGE_SRS2.csv")
P52_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P52_ASC_LARGE_SRS2.csv")
P53_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P53_ASC_LARGE_SRS2.csv")
P54_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P54_ASC_LARGE_SRS2.csv")
P55_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P55_ASC_LARGE_SRS2.csv")
P56_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P56_ASC_LARGE_SRS2.csv")
P57_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P57_ASC_LARGE_SRS2.csv")
P58_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P58_ASC_LARGE_SRS2.csv")
P59_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P59_ASC_LARGE_SRS2.csv")
P60_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P60_ASC_LARGE_SRS2.csv")
P61_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P1_ASC_LARGE_SRS2.csv")
P62_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P2_ASC_LARGE_SRS2.csv")
P63_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P3_ASC_LARGE_SRS2.csv")
P64_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P4_ASC_LARGE_SRS2.csv")
P65_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P5_ASC_LARGE_SRS2.csv")
P66_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P6_ASC_LARGE_SRS2.csv")
P67_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P7_ASC_LARGE_SRS2.csv")
P68_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P8_ASC_LARGE_SRS2.csv")
P69_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P9_ASC_LARGE_SRS2.csv")
P70_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P10_ASC_LARGE_SRS2.csv")
P71_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P11_ASC_LARGE_SRS2.csv")
P72_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P12_ASC_LARGE_SRS2.csv")
P73_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P13_ASC_LARGE_SRS2.csv")
P74_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P14_ASC_LARGE_SRS2.csv")
P75_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P15_ASC_LARGE_SRS2.csv")
P76_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P16_ASC_LARGE_SRS2.csv")
P77_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P17_ASC_LARGE_SRS2.csv")
P78_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P18_ASC_LARGE_SRS2.csv")
P79_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P19_ASC_LARGE_SRS2.csv")
P80_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P20_ASC_LARGE_SRS2.csv")
P81_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P21_ASC_LARGE_SRS2.csv")
P82_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P22_ASC_LARGE_SRS2.csv")
P83_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P23_ASC_LARGE_SRS2.csv")
P84_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P24_ASC_LARGE_SRS2.csv")
P85_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P25_ASC_LARGE_SRS2.csv")
P86_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P26_ASC_LARGE_SRS2.csv")
P87_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P27_ASC_LARGE_SRS2.csv")
P88_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P28_ASC_LARGE_SRS2.csv")
P89_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P29_ASC_LARGE_SRS2.csv")
P90_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P30_ASC_LARGE_SRS2.csv")
P91_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P31_ASC_LARGE_SRS2.csv")
P92_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P32_ASC_LARGE_SRS2.csv")
P93_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P33_ASC_LARGE_SRS2.csv")
P94_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P34_ASC_LARGE_SRS2.csv")
P95_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P35_ASC_LARGE_SRS2.csv")
P96_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P36_ASC_LARGE_SRS2.csv")
P97_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P37_ASC_LARGE_SRS2.csv")
P98_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P38_ASC_LARGE_SRS2.csv")
P99_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P39_ASC_LARGE_SRS2.csv")
P100_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P40_ASC_LARGE_SRS2.csv")
P101_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P41_ASC_LARGE_SRS2.csv")
P102_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P42_ASC_LARGE_SRS2.csv")
P103_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P43_ASC_LARGE_SRS2.csv")
P104_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P44_ASC_LARGE_SRS2.csv")
P105_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P45_ASC_LARGE_SRS2.csv")
P106_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P46_ASC_LARGE_SRS2.csv")
P107_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P47_ASC_LARGE_SRS2.csv")
P108_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P48_ASC_LARGE_SRS2.csv")
P109_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P49_ASC_LARGE_SRS2.csv")
P110_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P50_ASC_LARGE_SRS2.csv")
P111_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P51_ASC_LARGE_SRS2.csv")
P112_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P52_ASC_LARGE_SRS2.csv")
P113_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P53_ASC_LARGE_SRS2.csv")
P114_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P54_ASC_LARGE_SRS2.csv")
P115_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P55_ASC_LARGE_SRS2.csv")
P116_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P56_ASC_LARGE_SRS2.csv")
P117_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P57_ASC_LARGE_SRS2.csv")
P118_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P58_ASC_LARGE_SRS2.csv")
P119_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P59_ASC_LARGE_SRS2.csv")
P120_ASC_LARGE_SRS2 <- read_csv("SRS2_data/P60_ASC_LARGE_SRS2.csv")


# Combining the individual data spreadsheets into one data frame.

alldata <- rbind (P1_ASC_LARGE_SRS2, P2_ASC_LARGE_SRS2, P3_ASC_LARGE_SRS2,P4_ASC_LARGE_SRS2, P5_ASC_LARGE_SRS2,
                  P6_ASC_LARGE_SRS2, P7_ASC_LARGE_SRS2, P8_ASC_LARGE_SRS2, P9_ASC_LARGE_SRS2, P10_ASC_LARGE_SRS2,
                  P11_ASC_LARGE_SRS2, P12_ASC_LARGE_SRS2, P13_ASC_LARGE_SRS2, P14_ASC_LARGE_SRS2, P15_ASC_LARGE_SRS2,
                  P16_ASC_LARGE_SRS2,P17_ASC_LARGE_SRS2, P18_ASC_LARGE_SRS2, P19_ASC_LARGE_SRS2,P20_ASC_LARGE_SRS2,
                  P21_ASC_LARGE_SRS2, P22_ASC_LARGE_SRS2, P23_ASC_LARGE_SRS2,P24_ASC_LARGE_SRS2, P25_ASC_LARGE_SRS2, P26_ASC_LARGE_SRS2,
                  P27_ASC_LARGE_SRS2, P28_ASC_LARGE_SRS2, P29_ASC_LARGE_SRS2, P30_ASC_LARGE_SRS2, P31_ASC_LARGE_SRS2,P32_ASC_LARGE_SRS2,
                  P33_ASC_LARGE_SRS2, P34_ASC_LARGE_SRS2, P35_ASC_LARGE_SRS2, P36_ASC_LARGE_SRS2, P37_ASC_LARGE_SRS2, P38_ASC_LARGE_SRS2,
                  P39_ASC_LARGE_SRS2,P40_ASC_LARGE_SRS2,P41_ASC_LARGE_SRS2, P42_ASC_LARGE_SRS2, P43_ASC_LARGE_SRS2, P44_ASC_LARGE_SRS2,
                  P45_ASC_LARGE_SRS2, P46_ASC_LARGE_SRS2, P47_ASC_LARGE_SRS2,P48_ASC_LARGE_SRS2, P49_ASC_LARGE_SRS2, P50_ASC_LARGE_SRS2,
                  P51_ASC_LARGE_SRS2, P52_ASC_LARGE_SRS2, P53_ASC_LARGE_SRS2, P54_ASC_LARGE_SRS2, P55_ASC_LARGE_SRS2,P56_ASC_LARGE_SRS2,
                  P57_ASC_LARGE_SRS2, P58_ASC_LARGE_SRS2,P59_ASC_LARGE_SRS2, P60_ASC_LARGE_SRS2, P61_ASC_LARGE_SRS2,
                  P62_ASC_LARGE_SRS2, P63_ASC_LARGE_SRS2, P64_ASC_LARGE_SRS2, P65_ASC_LARGE_SRS2,P66_ASC_LARGE_SRS2,
                  P67_ASC_LARGE_SRS2, P68_ASC_LARGE_SRS2, P69_ASC_LARGE_SRS2, P70_ASC_LARGE_SRS2, P71_ASC_LARGE_SRS2,
                  P72_ASC_LARGE_SRS2, P73_ASC_LARGE_SRS2, P74_ASC_LARGE_SRS2, P75_ASC_LARGE_SRS2, P76_ASC_LARGE_SRS2,
                  P77_ASC_LARGE_SRS2, P78_ASC_LARGE_SRS2, P79_ASC_LARGE_SRS2, P80_ASC_LARGE_SRS2, P81_ASC_LARGE_SRS2,
                  P82_ASC_LARGE_SRS2, P83_ASC_LARGE_SRS2, P84_ASC_LARGE_SRS2, P85_ASC_LARGE_SRS2, P86_ASC_LARGE_SRS2,
                  P87_ASC_LARGE_SRS2, P88_ASC_LARGE_SRS2, P89_ASC_LARGE_SRS2, P90_ASC_LARGE_SRS2, P91_ASC_LARGE_SRS2,
                  P92_ASC_LARGE_SRS2, P93_ASC_LARGE_SRS2, P94_ASC_LARGE_SRS2, P95_ASC_LARGE_SRS2, P96_ASC_LARGE_SRS2,
                  P97_ASC_LARGE_SRS2, P98_ASC_LARGE_SRS2, P99_ASC_LARGE_SRS2, P100_ASC_LARGE_SRS2, P101_ASC_LARGE_SRS2,
                  P102_ASC_LARGE_SRS2, P103_ASC_LARGE_SRS2, P104_ASC_LARGE_SRS2, P105_ASC_LARGE_SRS2, P106_ASC_LARGE_SRS2,
                  P107_ASC_LARGE_SRS2, P108_ASC_LARGE_SRS2, P109_ASC_LARGE_SRS2, P110_ASC_LARGE_SRS2, P111_ASC_LARGE_SRS2,
                  P112_ASC_LARGE_SRS2, P113_ASC_LARGE_SRS2, P114_ASC_LARGE_SRS2, P115_ASC_LARGE_SRS2, P116_ASC_LARGE_SRS2,
                  P117_ASC_LARGE_SRS2, P118_ASC_LARGE_SRS2, P119_ASC_LARGE_SRS2, P120_ASC_LARGE_SRS2)


# check it yup all good
#view(alldata)

alldata <- alldata%>%
  mutate(Group_Status = participant <= 60)

# Rename TRUE FALSE to more meaningful labels.
alldata$Group_Status[alldata$Group_Status == 'TRUE'] <- "ASC"
alldata$Group_Status[alldata$Group_Status == 'FALSE'] <- "TD"

# check it yup all good
#view(alldata)

# SRS2 scoring ignore none as these are filler items we are not interested in. 

#We want to add a column called SRS2_tidy that converts the values to the appropriate score. 
# What the code should do is take items that are positively scored and turn a 1 into a 2, 
# turn a 2 into a 1, and turn 3 and 4 into a 0. 
# What the code should do is take items that are negatively scored and turn a 4 into a 2, 
# turn a 3 into a 1, and turn 1 and 2 into a 0. 

alldata <- alldata %>% mutate(SRS2_tidy = case_when 
                              (SRS2_scoring_type == 'positive' &  SRS2_resp == 1 ~ 0, 
                                SRS2_scoring_type == 'positive' &  SRS2_resp == 2 ~ 1,
                                SRS2_scoring_type == 'positive' &  SRS2_resp == 3 ~ 2,
                                SRS2_scoring_type == 'positive' &  SRS2_resp == 4 ~ 3,
                                SRS2_scoring_type == 'negative' &  SRS2_resp == 1 ~ 3,
                                SRS2_scoring_type == 'negative' &  SRS2_resp == 2 ~ 2,
                                SRS2_scoring_type == 'negative' &  SRS2_resp == 3 ~ 1,
                                SRS2_scoring_type == 'negative' &  SRS2_resp == 4 ~ 0,))
#view(alldata)

# It worked!!!!!

# Then we can add this column for each participant to get their individual RAW SRS2 score by treatment 
#subScale and total score and DSM-5 compatible subscales

# Total score first
alldata <- alldata %>% group_by(participant) %>%
  mutate(total_RAW_score = sum(SRS2_tidy))

# Now individual treatment subscale raw scores

alldata <- alldata %>% group_by(participant, SRS2_treatment_subscale) %>% 
  mutate(treatment_RAW_score = sum(SRS2_tidy))

#Now DSM-5 compatible subscale scores
#Sort into your subscales of RRB and SCI DSM5 subscales
alldata <- alldata%>%
  mutate(DSM5_group = SRS2_treatment_subscale == 'rrb')
# Rename TRUE FALSE to more meaningful labels.
alldata$DSM5_group [alldata$DSM5_group == 'TRUE'] <- "RRB"
alldata$DSM5_group [alldata$DSM5_group == 'FALSE'] <- "SCI"
#Now get the RAW DSM5 scores
alldata <- alldata %>% group_by(participant, DSM5_group) %>% 
  mutate(DSM5_RAW_score = sum(SRS2_tidy))

#view(alldata)

# It worked!!!!!

# Now let's try the total t- scores. We need to create a variable 'total_t_scores' changing the total_raw_score of
# participants to a specified value based on a range as seen below.

# Let'd find the minimum vale so we don't do any extra work
min(alldata$total_RAW_score)
max(alldata$total_RAW_score)
#output min is 18 and the max is 149 so we don't need to input values below for anything less

#Why has it only done this for one participants data???????? grrrrrr
#alldata['total_t_score'][alldata['total_t_score']> 17 & alldata['total_t_score'] <19] <- 42
# Also Doesn't work!!! 
#alldata <- alldata %>% mutate(total_t_score = case_when(total_RAW_score > 17 & alldata$total_raw_score < 19 ~ '42'))

alldata <- alldata %>%
  mutate(total_t_score = case_when(total_RAW_score >= 17 & total_RAW_score <= 19 ~ '42',
                                   total_RAW_score >= 20 & total_RAW_score <= 21 ~ '43',
                                   total_RAW_score >= 22 & total_RAW_score <= 24 ~ '44',
                                   total_RAW_score >= 25 & total_RAW_score <= 27 ~ '45',
                                   total_RAW_score >= 28 & total_RAW_score <= 30 ~ '46',
                                   total_RAW_score >= 31 & total_RAW_score <= 33 ~ '47',
                                   total_RAW_score >= 34 & total_RAW_score <= 36 ~ '48',
                                   total_RAW_score >= 37 & total_RAW_score <= 38 ~ '49',
                                   total_RAW_score >= 39 & total_RAW_score <= 41 ~ '50',
                                   total_RAW_score >= 42 & total_RAW_score <= 44 ~ '51',
                                   total_RAW_score >= 45 & total_RAW_score <= 47 ~ '52',
                                   total_RAW_score >= 48 & total_RAW_score <= 50 ~ '53',
                                   total_RAW_score >= 51 & total_RAW_score <= 53 ~ '54',
                                   total_RAW_score >= 54 & total_RAW_score <= 56 ~ '55',
                                   total_RAW_score >= 57 & total_RAW_score <= 58 ~ '56',
                                   total_RAW_score >= 59 & total_RAW_score <= 61 ~ '57',
                                   total_RAW_score >= 62 & total_RAW_score <= 64 ~ '58',
                                   total_RAW_score >= 65 & total_RAW_score <= 67 ~ '59',
                                   total_RAW_score >= 68 & total_RAW_score <= 70 ~ '60',
                                   total_RAW_score >= 71 & total_RAW_score <= 73 ~ '61',
                                   total_RAW_score >= 74 & total_RAW_score <= 75 ~ '62',
                                   total_RAW_score >= 76 & total_RAW_score <= 78 ~ '63',
                                   total_RAW_score >= 79 & total_RAW_score <= 81 ~ '64',
                                   total_RAW_score >= 82 & total_RAW_score <= 84 ~ '65',
                                   total_RAW_score >= 85 & total_RAW_score <= 87 ~ '66',
                                   total_RAW_score >= 88 & total_RAW_score <= 90 ~ '67',
                                   total_RAW_score >= 91 & total_RAW_score <= 92 ~ '68',
                                   total_RAW_score >= 93 & total_RAW_score <= 95 ~ '69',
                                   total_RAW_score >= 96 & total_RAW_score <= 98 ~ '70',
                                   total_RAW_score >= 99 & total_RAW_score <= 101 ~ '71',
                                   total_RAW_score >= 102 & total_RAW_score <= 104 ~ '72',
                                   total_RAW_score >= 105 & total_RAW_score <= 107 ~ '73',
                                   total_RAW_score >= 108 & total_RAW_score <= 109 ~ '74',
                                   total_RAW_score >= 110 & total_RAW_score <= 112 ~ '75',
                                   total_RAW_score >= 113 & total_RAW_score <= 115 ~ '76',
                                   total_RAW_score >= 116 & total_RAW_score <= 118 ~ '77',
                                   total_RAW_score >= 119 & total_RAW_score <= 121 ~ '78',
                                   total_RAW_score >= 122 & total_RAW_score <= 124 ~ '79',
                                   total_RAW_score >= 125 & total_RAW_score <= 127 ~ '80',
                                   total_RAW_score >= 128 & total_RAW_score <= 129 ~ '81',
                                   total_RAW_score >= 130 & total_RAW_score <= 132 ~ '82',
                                   total_RAW_score >= 133 & total_RAW_score <= 135 ~ '83',
                                   total_RAW_score >= 136 & total_RAW_score <= 138 ~ '84',
                                   total_RAW_score >= 139 & total_RAW_score <= 141 ~ '85',
                                   total_RAW_score >= 142 & total_RAW_score <= 144 ~ '86',
                                   total_RAW_score >= 145 & total_RAW_score <= 146 ~ '87',
                                   total_RAW_score >= 147 & total_RAW_score <= 149 ~ '88',
                                   total_RAW_score >= 150 & total_RAW_score <= 152 ~ '89',
                                   total_RAW_score >= 153 ~ '90'))


# It Worked!!!!!!#

#Now lets see according to the t score where do these participants traits sit on the autism spectrum condition
alldata <- alldata %>%
  mutate(overall_clinical_range = case_when(total_t_score>= 0 & total_t_score <= 59 ~ 'Within Normal Limits',
                                            total_t_score >= 60 & total_t_score <= 65 ~ 'Mild',
                                            total_t_score >= 66 & total_t_score <= 75 ~ 'Moderate',
                                            total_t_score >= 76 & total_t_score <= 90 ~ 'Severe'))

#view(alldata)
#It Worked!!!

#Now Individual t scores for treatment subscales
# minimum value = 0 RRB & 15 SCI, maximum value = 34 RRB & 117 SCI
aggregate(treatment_RAW_score ~ SRS2_treatment_subscale, alldata, function(x) min(x))
aggregate(treatment_RAW_score ~ SRS2_treatment_subscale, alldata, function(x) max(x))

#SRS2_treatment_subscale treatment_RAW_score
#1                     awr                   1
#2                     cog                   2
#3                     com                   2
#4                     mot                   3
#5                     rrb                   0
#1                     awr                  18
#2                     cog                  29
#3                     com                  52
#4                     mot                  28
#5                     rrb                  34

alldata <- alldata %>%
  mutate(treatment_t_score = case_when(SRS2_treatment_subscale == "awr" & treatment_RAW_score == 0 ~ '32',
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 1 ~ '35', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 2 ~ '38', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 3 ~ '41', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 4 ~ '44', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 5 ~ '47', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 6 ~ '49',
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 7 ~ '52',
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 8 ~ '55',
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 9 ~ '58', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 10 ~ '61', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 11 ~ '64', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 12 ~ '66', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 13 ~ '69', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 14 ~ '72',
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 15 ~ '75', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 16 ~ '78',
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 17 ~ '81', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 18 ~ '83', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 19 ~ '86', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score == 20 ~ '89', 
                                       SRS2_treatment_subscale == "awr" & treatment_RAW_score >= 21 ~ '90', 
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 0 ~ '37',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 1 ~ '39',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 2 ~ '41',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 3 ~ '42',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 4 ~ '44',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 5 ~ '46',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 6 ~ '48',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 7 ~ '49',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 8 ~ '51',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 9 ~ '53',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 10 ~ '55',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 11 ~ '56',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 12 ~ '58',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 13 ~ '60',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 14 ~ '62',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 15 ~ '63',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 16 ~ '65',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 17 ~ '67',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 18 ~ '69',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 19 ~ '70',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 20 ~ '72',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 21 ~ '74',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 22 ~ '76',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 23 ~ '77',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 24 ~ '79',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 25 ~ '81',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 26 ~ '83',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 27 ~ '84',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 28 ~ '86',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 29 ~ '88',
                                       SRS2_treatment_subscale == "cog" & treatment_RAW_score == 30 ~ '90',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 0 ~ '37',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 1 ~ '38',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 2 ~ '39',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 3 ~ '40',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 4 ~ '41',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 5 ~ '42',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 6 ~ '43',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 7 ~ '44',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 8 ~ '45',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 9 ~ '46',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 10 ~ '47',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 11 ~ '48',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 12 ~ '49',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 13 ~ '50',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 14 ~ '51',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 15 ~ '52',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 16 ~ '53',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 17 ~ '54',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 18 ~ '55',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 19 ~ '56',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 20 ~ '57',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 21 ~ '58',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 22 ~ '59',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 23 ~ '60',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 24 ~ '61',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 25 ~ '62',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 26 ~ '63',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 27 ~ '64',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 28 ~ '65',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 29 ~ '66',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 30 ~ '67',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 31 ~ '68',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 32 ~ '69',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 33 ~ '70',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 34 ~ '71',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 35 ~ '72',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 36 ~ '73',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 37 ~ '74',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 38 ~ '75',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 39 ~ '75',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 40 ~ '76',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 41 ~ '77',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 42 ~ '78',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 43 ~ '79',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 44 ~ '80',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 45 ~ '81',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 46 ~ '82',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 47 ~ '83',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 48 ~ '84',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 49 ~ '85',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 50 ~ '86',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 51 ~ '87',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 52 ~ '88',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 53 ~ '89',
                                       SRS2_treatment_subscale == "com" & treatment_RAW_score == 54 ~ '90',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 0 ~ '37',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 1 ~ '39',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 2 ~ '41',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 3 ~ '42',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 4 ~ '44',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 5 ~ '46',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 6 ~ '47',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 7 ~ '49',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 8 ~ '51',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 9 ~ '52',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 10 ~ '54',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 11 ~ '56',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 12 ~ '57',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 13 ~ '59',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 14 ~ '61',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 15 ~ '62',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 16 ~ '64',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 17 ~ '66',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 18 ~ '67',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 19 ~ '69',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 20 ~ '71',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 21 ~ '72',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 22 ~ '74',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 23 ~ '76',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 24 ~ '77',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 25 ~ '79',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 26 ~ '81',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 27 ~ '82',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 28 ~ '84',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 29 ~ '86',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 30 ~ '87',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 31 ~ '89',
                                       SRS2_treatment_subscale == "mot" & treatment_RAW_score == 32 ~ '90',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 0 ~ '40',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 1 ~ '42',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 2 ~ '43',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 3 ~ '45',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 4 ~ '47',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 5 ~ '48',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 6 ~ '50',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 7 ~ '52',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 8 ~ '53',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 9 ~ '55',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 10 ~ '57',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 11 ~ '58',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 12 ~ '60',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 13 ~ '62',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 14 ~ '63',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 15 ~ '65',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 16 ~ '67',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 17 ~ '68',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 18 ~ '70',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 19 ~ '72',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 20 ~ '73',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 21 ~ '75',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 22 ~ '77',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 23 ~ '78',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 24 ~ '80',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 25 ~ '82',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 26 ~ '83',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 27 ~ '85',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 28 ~ '87',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score == 29 ~ '88',
                                       SRS2_treatment_subscale == "rrb" & treatment_RAW_score >= 30 ~ '90'))

alldata <- alldata %>%
  mutate(treatment_clinical_range = case_when(treatment_t_score >= 0 & treatment_t_score <= 59 ~ 'Within Normal Limits',
                                              treatment_t_score >= 60 & treatment_t_score <= 65 ~ 'Mild',
                                              treatment_t_score >= 66 & treatment_t_score <= 75 ~ 'Moderate',
                                              treatment_t_score >= 76 & treatment_t_score <= 90 ~ 'Severe'))

#view(alldata)                                        
# Treatment clinical ranges



#Now Individual t scores for DSM-5 compatible subscales
# minimum value = 0 RRB & 15 SCI, maximum value = 34 RRB & 117 SCI
aggregate(DSM5_RAW_score ~ DSM5_group, alldata, function(x) min(x))
aggregate(DSM5_RAW_score ~ DSM5_group, alldata, function(x) max(x))


alldata <- alldata %>%
  mutate(DSM5_t_score = case_when(DSM5_group == "SCI" & DSM5_RAW_score == 0 ~ '35',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 1 & DSM5_RAW_score <= 2 ~ '36',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 3 & DSM5_RAW_score <= 5 ~ '37',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 6 & DSM5_RAW_score <= 7 ~ '38',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 8 & DSM5_RAW_score <= 9 ~ '39',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 10 & DSM5_RAW_score <= 12 ~ '40',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 13 & DSM5_RAW_score <= 14 ~ '41',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 15 & DSM5_RAW_score <= 16 ~ '42',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 17 & DSM5_RAW_score <= 19 ~ '43',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 20 & DSM5_RAW_score <= 21 ~ '44',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 22 & DSM5_RAW_score <= 23 ~ '45',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 24 & DSM5_RAW_score <= 26 ~ '46',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 27 & DSM5_RAW_score <= 28 ~ '47',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 29 & DSM5_RAW_score <= 30 ~ '48',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 31 & DSM5_RAW_score <= 33 ~ '49',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 34 & DSM5_RAW_score <= 35 ~ '50',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 36 & DSM5_RAW_score <= 37 ~ '51',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 38 & DSM5_RAW_score <= 39 ~ '52',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 40 & DSM5_RAW_score <= 42 ~ '53',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 43 & DSM5_RAW_score <= 44 ~ '54',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 45 & DSM5_RAW_score <= 46 ~ '55',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 47 & DSM5_RAW_score <= 49 ~ '56',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 50 & DSM5_RAW_score <= 51 ~ '57',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 52 & DSM5_RAW_score <= 53 ~ '58',  
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 54 & DSM5_RAW_score <= 56 ~ '59',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 57 & DSM5_RAW_score <= 58 ~ '60',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 59 & DSM5_RAW_score <= 60 ~ '61',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 61 & DSM5_RAW_score <= 63 ~ '62',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 64 & DSM5_RAW_score <= 65 ~ '63',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 66 & DSM5_RAW_score <= 67 ~ '64',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 68 & DSM5_RAW_score <= 70 ~ '65',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 71 & DSM5_RAW_score <= 72 ~ '66',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 73 & DSM5_RAW_score <= 74 ~ '67',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 75 & DSM5_RAW_score <= 77 ~ '68',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 78 & DSM5_RAW_score <= 79 ~ '69',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 80 & DSM5_RAW_score <= 81 ~ '70',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 82 & DSM5_RAW_score <= 84 ~ '71',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 85 & DSM5_RAW_score <= 86 ~ '72',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 87 & DSM5_RAW_score <= 88 ~ '73',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 89 & DSM5_RAW_score <= 91 ~ '74',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 92 & DSM5_RAW_score <= 93 ~ '75',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 94 & DSM5_RAW_score <= 95 ~ '76',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 96 & DSM5_RAW_score <= 97 ~ '77',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 98 & DSM5_RAW_score <= 100 ~ '78',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 101 & DSM5_RAW_score <= 102 ~ '79',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 103 & DSM5_RAW_score <= 104 ~ '80',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 105 & DSM5_RAW_score <= 107 ~ '81',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 108 & DSM5_RAW_score <= 109 ~ '82',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 110 & DSM5_RAW_score <= 111 ~ '83',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 112 & DSM5_RAW_score <= 114 ~ '84',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 115 & DSM5_RAW_score <= 116 ~ '85',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 117 & DSM5_RAW_score <= 118 ~ '86',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 119 & DSM5_RAW_score <= 121 ~ '87',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 122 & DSM5_RAW_score <= 123 ~ '88',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 124 & DSM5_RAW_score <= 125 ~ '89',
                                  DSM5_group == "SCI" & DSM5_RAW_score >= 126 ~ '90', 
                                  DSM5_group == "RRB" & DSM5_RAW_score == 0 ~ '40',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 1 ~ '42',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 2 ~ '43',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 3 ~ '45',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 4 ~ '47',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 5 ~ '48',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 6 ~ '50',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 7 ~ '52',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 8 ~ '53',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 9 ~ '55',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 10 ~ '57',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 11 ~ '58',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 12 ~ '60',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 13 ~ '62',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 14 ~ '63',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 15 ~ '65',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 16 ~ '67',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 17 ~ '68',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 18 ~ '70',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 19 ~ '72',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 20 ~ '73',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 21 ~ '75',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 22 ~ '77',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 23 ~ '78',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 24 ~ '80',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 25 ~ '82',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 26 ~ '83',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 27 ~ '85',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 28 ~ '87',
                                  DSM5_group == "RRB" & DSM5_RAW_score == 29 ~ '88',
                                  DSM5_group == "RRB" & DSM5_RAW_score >= 30 ~ '90'))

alldata <- alldata %>%
  mutate(DSM5_clinical_range = case_when(DSM5_t_score >= 0 & DSM5_t_score <= 59 ~ 'Within Normal Limits',
                                         DSM5_t_score >= 60 & DSM5_t_score <= 65 ~ 'Mild',
                                         DSM5_t_score >= 66 & DSM5_t_score <= 75 ~ 'Moderate',
                                         DSM5_t_score >= 76 & DSM5_t_score <= 90 ~ 'Severe'))


view(alldata)                                

#Export a CSV of the new data set...
write.csv(alldata,"//nask.man.ac.uk/home$/Desktop/ASC_large/SRS2_data\\alldata.csv", row.names = TRUE)

# All data extracted and new CSV created called all data created. WHOOP WHOOP!!!!!!!!!

#Let's examine the data a little 

# ***************** HELP FROM HERE DOWN *********************** THANKS

#Let's look at the difference between the two groups
#WHY ARE YOU NOT WORKING because dbl not num. import the exxported dataset and change dbl to num

alldata %>% 
  group_by(Group_Status) %>%
  summarise(mean(total_t_score), sd(total_t_score))
##OUTPUT##
# A tibble: 2 × 3
#Group_Status `mean(total_t_score)` `sd(total_t_score)`
#<chr>                        <dbl>               <dbl>
#  1 ASC                      72.5                7.85
#2 TD                         50.3                6.01

#Average ASC in the moderate autistic traits category, average TD in the within normal limits category

#Much lower empathy scores for the ASC group compared to the TD group Let's have a look at this with a t test
ASC_SRS2_mean <- rnorm(30, mean = 72.5, sd = 7.85)
TD_SRS2_mean <- rnorm(29, mean = 50.3, sd = 6.01)
t.test(ASC_SRS2_mean, TD_SRS2_mean, var.equal = TRUE) 
##OUTPUT##
#Two Sample t-test

#data:  ASC_SRS2_mean and TD_SRS2_mean
#t = 13.375, df = 57, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  19.20707 25.97127
#sample estimates:
#  mean of x mean of y 
#73.58801  50.99884 