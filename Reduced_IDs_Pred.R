#ID reduction down to only used variables no exploratory

library(Matrix)
library(tidyverse)
library(readr)

all_IDs <- read_csv("//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data/All_IDs.csv")
view(all_IDs)


Reduced_IDs_Pred <- all_IDs[ , c("participant", "Age", "Gender", "Comprehension_accuracy_IR", "SRS_total_score_raw", "SRS_total_score_t", 
                                 "Total_SRS_Category", "EQ", "Total_RAN", "Total_reading_cluster")]

Reduced_IDs_Pred <- Reduced_IDs_Pred %>%
  distinct(participant, Age, Gender, Comprehension_accuracy_IR, SRS_total_score_raw, SRS_total_score_t, Total_SRS_Category, EQ, Total_RAN, 
           Total_reading_cluster, .keep_all = TRUE)

view(Reduced_IDs_Pred)


write.csv (Reduced_IDs_Pred,"//nask.man.ac.uk/home$/Desktop/ASC_small/Tidy_RT_data\\Reduced_IDs_Pred", row.names = TRUE)
