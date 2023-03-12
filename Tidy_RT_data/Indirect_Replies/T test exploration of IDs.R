library(readr)
Reduced_IDs_IR <- read_csv("Tidy_RT_data/Indirect_Replies/Reduced_IDs_IR.csv")
#View(Reduced_IDs_IR)
alldata_IR_RT_comp <- read_csv("Tidy_RT_data/Indirect_Replies/alldata_IR_RT_comp.csv")
#View(alldata_IR_RT_comp)

alldata_IR_RT_comp <- alldata_IR_RT_comp %>%
  distinct(participant, .keep_all = TRUE)
library(tidyverse)

Reduced_ID <- inner_join(Reduced_IDs_IR, alldata_IR_RT_comp, by = "participant")

library(Matrix)

Reduced_ID <- Reduced_ID [,c("participant", "Group_Status", "total_perc", "total_t_score", "EQ_score")]

view(Reduced_ID)

#T.tests, mean, and sd
#EQ
compare_means(EQ_score ~ Group_Status, data = Reduced_ID, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_ID, x = "Group_Status", y = "EQ_score",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Overall score on EQ", x = "Group", y = "EQ")

t.test(EQ_score ~ Group_Status, data = Reduced_ID)

#EQ
Reduced_ID %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(EQ_score),
            sdASC = sd(EQ_score))

#Comp Accuracy
compare_means(total_perc ~ Group_Status, data = Reduced_ID, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_ID, x = "Group_Status", y = "total_perc",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Comprehension Accuracy", x = "Group", y = "Percentage Correct")


t.test(total_perc ~ Group_Status, data = Reduced_ID)
Reduced_ID %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(total_perc),
            sdASC = sd(total_perc))


#SRS-2
compare_means(total_t_score ~ Group_Status, data = Reduced_ID, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(Reduced_ID, x = "Group_Status", y = "total_t_score",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test") %>% 
  labs(title = "Overall score on SRS-2", x = "Group", y = "Total t Score")


(t.test(total_t_score ~ Group_Status, data = Reduced_ID))
#p + stat_compare_means(aes(group = Group_Status), label = "p.signif")

Reduced_ID %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(total_t_score),
            sdASC = sd(total_t_score))

#p + stat_compare_means(aes(group = Group_Status), label = "p.signif")

