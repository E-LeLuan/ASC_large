#Remove extra data to put all ID's together and reduce file Size
library(readr)
alldata_EQ <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_large/EQ_data/alldata_EQ.csv")
View(alldata_EQ)

library(tidyverse)

EQscore <- alldata_EQ[ , c("participant", "EQ_score")]

EQscore <- EQscore %>% 
  distinct(participant, EQ_score, .keep_all = TRUE)

write.csv (EQscore,"//nask.man.ac.uk/home$/Desktop/ASC_large/EQ_data\\EQscore", row.names = TRUE)

view(EQscore)

#This leaves us with 120 lines instead of 7200 Now we can simply transfer this over to our spreadsheet with copy paste. Removes 
# any human error element. 