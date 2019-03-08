# Dplyr workshop notes

## March 8th, 2019

# install.packages("dplyr")
library(dplyr)

# data acquired from %>% 
# https://drive.google.com/drive/folders/1Gy8iI9lt6cjYVBA3KEPFTBb_d6dQ6-df

setwd("~/Development/github/Dispader/r_dplyr_workshop")
load("ATUSdata_subset.Rdata")
#View(atus)

atus.subset1 <- select(atus, YEAR, ACT_WORK, ACT_SOCIAL)
atus.subset1 <- filter(atus.subset1, YEAR == "2012")
atus.subset1

atus.subset <- atus %>% 
               #select(YEAR, ACT_WORK, ACT_SOCIAL) %>% 
               filter(YEAR == "2012") %>% 
               mutate(WORK_HOURS = ACT_WORK/60,
                      SOCIAL_HOURS = ACT_SOCIAL/60) %>% 
               arrange(desc(ACT_WORK)) %>% 
               group_by(REGION, EDUC) %>% 
               summarize(mean_work_hours = mean(WORK_HOURS),
                         stdv_work_hours = sd(WORK_HOURS),
                         mean_social_hours = mean(SOCIAL_HOURS),
                         stdv_social_hours = sd(SOCIAL_HOURS),
                         count = n())
atus.subset %>% head(10)

atus %>% 
  group_by(respondent_sex = SEX,
           children_under_18 = HH_CHILD) %>% 
  summarize(working_hours = mean(mean(ACT_WORK/60)),
            socializing_hours = mean(mean(ACT_SOCIAL/60)),
            respondents = n())

atus.long <- atus %>% 
  select(EDUC, REGION) %>% 
  group_by(EDUC, REGION) %>% 
  summarize(count = n())

atus.wide <- dcast(atus.long,
                   EDUC ~ REGION,
                   value.var = "count")
atus.wide %>% head(10)
