#Tidy comprehension accuracy data
library(readr)
alldata_IR_RT <- read_csv("Tidy_RT_data/Indirect_Replies/alldata_IR_RT.csv")

library(tidyverse)

#Comp_Question_Resp column tidy and extract
alldata_IR_RT_comp <- alldata_IR_RT %>%
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
                               Comp_Question_Resp == "y" & item_number == 6 ~ '1',
                               Comp_Question_Resp == "n" & item_number == 6 ~ '0',
                               Comp_Question_Resp == "y" & item_number == 7 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 7 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 8 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 8 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 9 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 9 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 10 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 10 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 11 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 11 ~ '1',
                               Comp_Question_Resp == "y" & item_number == 12 ~ '0',
                               Comp_Question_Resp == "n" & item_number == 12 ~ '1'))
view(alldata_IR_RT_comp)

# Change column comp_tidy to numeric
#find out what it is listed as...
sapply(alldata_IR_RT_comp, class)
#it is a character lets make it numeric
alldata_IR_RT_comp$comp_tidy <- as.numeric(as.character(alldata_IR_RT_comp$comp_tidy))
# did it work?
sapply(alldata_IR_RT_comp, class)
#Yes it did 

# Total comprehension score
alldata_IR_RT_comp <- alldata_IR_RT_comp %>% group_by (participant) %>%
  mutate(total_comp = sum(comp_tidy))

#Whoop whoop it worked 

alldata_IR_RT_comp <- alldata_IR_RT_comp %>% group_by (participant) %>%
  mutate(total_perc = (total_comp / 12 * 100))

view(alldata_IR_RT_comp)

#write to CSV file location on laptop is .... C:\Users\eliza\Desktop\ASC_large\Tidy_RT_data
#write.csv(alldata_IR_RT_comp,"//C:/Users/eliza/Desktop/ASC_large/Tidy_RT_data/Indirect_Replies\\alldata_IR_RT_comp.csv", row.names = TRUE)
#C:\Users\eliza\Desktop\ASC_large\Tidy_RT_data
#write.csv (alldata_IR_RT_comp,"//nask.man.ac.uk/home$/Desktop/ASC_large/Tidy_RT_data/Indirect_Replies\\alldata_IR_RT_comp.csv", row.names = TRUE)

# now lets just get one number of accuracy to upload to spreadsheet

library(readr)
library(tidyverse)
alldata_IR_RT_comp <- read_csv("Tidy_RT_data/Indirect_Replies/alldata_IR_RT_comp.csv", 
                               col_types = cols(total_comp = col_number(), 
                                                total_perc = col_number()))
#View(alldata_IR_RT_comp)


#Indirect_Replies accuracy file size reduced
IR_accuracy <- alldata_IR_RT_comp[ , c("participant", "total_comp" , "total_perc")]
#view(IR_accuracy)

IR_accuracyimp <- IR_accuracy %>% 
  distinct(participant, total_comp, total_perc, .keep_all = TRUE)
view(IR_accuracyimp)

#write.csv (IR_accuracyimp,"//nask.man.ac.uk/home$/Desktop/ASC_large/Tidy_RT_data/Indirect_Replies\\IR_accuracyimp", row.names = TRUE)

