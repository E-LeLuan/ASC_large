library(tidyverse)

set.seed(1235)

# Importing the data into R.

library(readr)

alldataSRS2 <- read_csv("C:/Users/eliza/Desktop/ASC_large/SRS2_data/SRS2totalscoresimp.csv")

view(alldataSRS2)

library(ggpubr)

#SRS2
compare_means(total_t_score ~ Group_Status, data = alldataSRS2, method = "t.test")

#stat_compare_means(mapping = NULL, comparisons = "Group_Status" hide.ns = FALSE,
#                   label = p.signif,  label.x = NULL, label.y = NULL,  ...)

p <- ggboxplot(alldataSRS2, x = "Group_Status", y = "total_t_score",
               color = "Group_Status", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means(method = "t.test")
p + labs(title = "Overall score on SRS-2", x = "Group", y = "Total t Score")

t.test(total_t_score ~ Group_Status, data = alldataSRS2)
alldataSRS2 %>% 
  group_by(Group_Status) %>%
  summarize(meanASC = mean(total_t_score),
            sdASC = sd(total_t_score))
