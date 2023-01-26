#Remove extra data to put all ID's together and reduce file Size
#Remove extra data to put all ID's together and reduce file Size
library(readr)
alldata_SRS2 <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_large/SRS2_data/alldataSRS2.csv")
View(alldata_SRS2)

library(tidyverse)

#total scores first
SRS2totalscore <- alldata_SRS2[ , c("participant", "total_RAW_score", "total_t_score", "overall_clinical_range")]
view(SRS2totalscore)

SRS2totalscoresimp <- SRS2totalscore %>% 
  distinct(participant, total_RAW_score, total_t_score, overall_clinical_range, .keep_all = TRUE)
view(SRS2totalscoresimp)

write.csv (SRS2totalscoresimp,"//nask.man.ac.uk/home$/Desktop/ASC_large/SRS2_data\\SRS2totaltscore", row.names = TRUE)

#This leaves us with 120 lines instead of 7800 Now we can simply transfer this over to our spreadsheet with copy paste. Removes 
# any human error element. 

#Now lets do treatment subscales

SRS2treatmentscore <- alldata_SRS2[ , c("participant","SRS2_treatment_subscale", "treatment_RAW_score", "treatment_t_score", "treatment_clinical_range")]
view(SRS2treatmentscore)

# Here I want to select only MOT no other treatment subscales. HOW TO DO IT?????? Let's think about it.... 
#Might be worth doing this bit in OpenRefine. Then bring it back here as individual csv files to remove duplicates. 
SRS2MOTscore <- SRS2treatmentscore[]


#Now DSM-5 compatible subscale scores
#Sort into your subscales of RRB and SCI DSM5 subscales
alldata_SRS2 <- alldata_SRS2%>%
  mutate(DSM5_group = SRS2_treatment_subscale == 'rrb')
# Rename TRUE FALSE to more meaningful labels.
alldata_SRS2$DSM5_group [alldata_SRS2$DSM5_group == 'TRUE'] <- "RRB"
alldata_SRS2$DSM5_group [alldata_SRS2$DSM5_group == 'FALSE'] <- "SCI"
#Now get the RAW DSM5 scores
alldata_SRS2 <- alldata_SRS2 %>% group_by(participant, DSM5_group) %>% 
  mutate(DSM5_RAW_score = sum(SRS2_tidy))
view(alldata_SRS2)


