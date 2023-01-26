#Tidy comprehension accuracy data
library(readr)
alldata_Pred_RT <- read_csv("Tidy_RT_data/Prediction/alldata_Pred_RT.csv")

library(tidyverse)

#Comp_Question_Resp column tidy and extract
alldata_Pred_RT_comp <- alldata_Pred_RT %>%
  mutate(comp_tidy = case_when(Comp_Question_Resp == "space" ~ '0',
                               Comp_Question_Resp == "y" & item_number == 1 ~ '1',
                               Comp_Question_Resp == "n" & item_number == 1 ~ '0',
                               Comp_Question_Resp == "y" & item_number == 2 ~ '1',
                               Comp_Question_Resp == "n" & item_number == 2 ~ '0',
                               Comp_Question_Resp == "y" & item_number == 3 ~ '1',
                               Comp_Question_Resp == "n" & item_number == 3 ~ '0',
                               Comp_Question_Resp == "y" & item_number == 4 ~ '1',
                               Comp_Question_Resp == "n" & item_number == 4 ~ '0',
                               Comp_Question_Resp == "y" & item_number == 5 ~ '1',
                               Comp_Question_Resp == "n" & item_number == 5 ~ '0',
                               Comp_Question_Resp == "y" & item_number == 6 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 6 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 7 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 7 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 8 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 8 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 9 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 9 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 10 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 10 ~ '1'))
view(alldata_Pred_RT_comp)


#code below shows us that we have 0 NA's we need to check noone entereed the wrong key by accident at anypoint. e.g.
# space for a no or yes question
sum(is.na(alldata_Pred_RT_comp$comp_tidy))
#Let's replace it with 0

# Change column comp_tidy to numeric
#find out what it is listed as...
sapply(alldata_Pred_RT_comp, class)
#it is a character lets make it numeric
alldata_Pred_RT_comp$comp_tidy <- as.numeric(as.character(alldata_Pred_RT_comp$comp_tidy))
# did it work?
sapply(alldata_Pred_RT_comp, class)
#Yes it did 

# Total comprehension score
alldata_Pred_RT_comp <- alldata_Pred_RT_comp %>% group_by (participant) %>%
  mutate(total_comp = sum(comp_tidy))

view(alldata_Pred_RT_comp)
#Whoop whoop it worked 

#Let's create percentage correct
alldata_Pred_RT_comp <- alldata_Pred_RT_comp %>% group_by (participant) %>%
  mutate(total_perc = (total_comp / 10 * 100))



#write to CSV file location on laptop is .... C:\Users\eliza\Desktop\ASC_small\Tidy_RT_data
#write.csv(alldata_Pred_RT_comp,"//C:/Users/eliza/Desktop/ASC_small/Tidy_RT_data/Prediction\\alldata_Pred_RT_comp.csv", row.names = TRUE)
#C:\Users\eliza\Desktop\ASC_small\Tidy_RT_data
write.csv (alldata_Pred_RT_comp,"//nask.man.ac.uk/home$/Desktop/ASC_large/Tidy_RT_data/Prediction\\alldata_Pred_RT_comp.csv", row.names = TRUE)


# now lets just get one number of accuracy to upload to spreadsheet

library(readr)
library(tidyverse)
alldata_Pred_RT_comp <- read_csv("Tidy_RT_data/Prediction/alldata_Pred_RT_comp.csv", 
                                 col_types = cols(total_comp = col_number(), 
                                                  total_perc = col_number()))
#View(alldata_Pred_RT_comp)


#Prediction accuracy file size reduced
Pred_accuracy <- alldata_Pred_RT_comp[ , c("participant", "total_comp" , "total_perc")]
#view(Pred_accuracy)

Pred_accuracyimp <- Pred_accuracy %>% 
  distinct(participant, total_comp, total_perc, .keep_all = TRUE)
#view(Pred_accuracyimp)

#write.csv (Pred_accuracyimp,"//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/Prediction\\Pred_accuracyimp", row.names = TRUE)


