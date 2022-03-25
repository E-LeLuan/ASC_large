#SRS2 auto tidy script (attempt)


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



# Combining the individual data spreadsheets into one data frame.

alldata <- rbind (P1_ASC_LARGE_SRS2,P2_ASC_LARGE_SRS2,P3_ASC_LARGE_SRS2,P4_ASC_LARGE_SRS2,P5_ASC_LARGE_SRS2,P6_ASC_LARGE_SRS2,
                  P7_ASC_LARGE_SRS2,P8_ASC_LARGE_SRS2,P9_ASC_LARGE_SRS2,P10_ASC_LARGE_SRS2)
                

view(alldata)

#alldata <- alldata%>%
 # mutate(Group_Status = participant <= 15)

# Rename TRUE FALSE to more meaningful labels.
#alldata$Group_Status[alldata$Group_Status == 'TRUE'] <- "ASC"
#alldata$Group_Status[alldata$Group_Status == 'FALSE'] <- "TD"
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
view(alldata)

# It worked!!!!!

# Then we can add this column for each participant to get their individual SRS2 score
SRS2_score <- alldata %>%
  group_by(participant) %>%
  summarise_at(vars(SRS2_tidy), list(SRS2_score = sum))

alldata <- inner_join(alldata, SRS2_score, by = "participant")

view(alldata)