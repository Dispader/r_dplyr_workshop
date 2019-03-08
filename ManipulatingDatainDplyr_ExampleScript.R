#######################################
## R Script for Dplyr Workshop
##  2019-03-08
##
######################################

#dplyr is a package in the tidyverse umbrella package - can load dplyr directly or use tidyverse
install.packages("dplyr")
library(dplyr)

# OR:
# install.packages("tidyverse")
# library(tidyverse)


#set working directory
setwd("~/Documents/WorkshopMaterials/LATIS_Workshops/Data_Wrangling_R_Sp19")

#load in the Rdata file
load("ATUSdata_subset.Rdata")
View(atus)


## SUBSET ##

#Select only year, region, and age:
atus.sub <- select(atus, YEAR, REGION, AGE)
head(atus.sub)

#Select variables within a range:
atus.sub <- select(atus, YEAR:RACE)
head(atus.sub)

#Select all the activity variables (start with "ACT_")
atus.act <- select(atus, starts_with("ACT"))
head(atus.act)


## FILTER ##

#Keep only data for the Midwest
atus.sub <- select(atus, YEAR, REGION, AGE)

atus.midwest <- filter(atus.sub, REGION == "Midwest")
summary(atus.midwest)

#Try it
 # First, create a subset of the atus data that contains year the columns you are interested in using select(). Call this new data "atus.subset1".
atus.subset1 <- select(atus, YEAR, AGE, SEX, ACT_WORK, ACT_SOCIAL)
# Then, filter the rows of atus.subset1 to contain only the data for the year you selected using filter(). Call this new data object "atus.subset2".
atus.subset2 <- filter(atus.subset1, YEAR==2010)

# Instead of nesting or creating intermediate objects like we did above, you can "chain" together multiple commands with pipes ("%>%") in dplyr to do it in one step. 

atus.subset <- atus %>%
  select(YEAR, AGE, SEX, ACT_WORK, ACT_SOCIAL) %>%
  filter(YEAR == 2010)

summary(atus.subset) 

## Arrange ## 

#Sort by age asending
atus.subset.ar <- arrange(atus.subset, AGE)
head(atus.subset.ar, n=3)

#age descending
atus.subset.ar <- arrange(atus.subset, desc(AGE))
head(atus.subset.ar, n=3)


## Mutate ##

#using base R  
atus$EARNMONTH <- atus$EARNWEEK*4
summary(atus$EARNMONTH)

#using dplyr with mutate()
atus <- mutate(atus, EARNMONTH = EARNWEEK*4)
summary(atus$EARNMONTH)


#Try it: The "ACT_" variables in the atus data currently capture the number of **minutes** spent performing each activity. 
# For the activity variables in your "atus.subset": 1. Use mutate to create two new variables for the activities in **hours** (divide by 60) 2. Sort the dataset by one of these variables. 

atus.subset <- atus.subset %>% 
  mutate(ACT_SOCIAL_hr = ACT_SOCIAL/60, ACT_WORK_hr = ACT_WORK/60) %>% 
  arrange(ACT_SOCIAL_hr)
head(atus.subset)


## Summarizing ##

#For example, let's say we want to take the average and sd of age 
summarize(atus, mean_age = mean(AGE), sd_age = sd(AGE))

#Try it 
#Use summarize() to calculate the mean and standard deviation for each of your activity variables
summarize(atus.subset, meanWork = mean(ACT_WORK_hr), sdWork = sd(ACT_WORK_hr), meanSocial = mean(ACT_SOCIAL_hr), sdSocial = sd(ACT_SOCIAL_hr))


## Group_by ##

#Want means of age for each region.
atus %>%
  group_by(REGION) %>% 
  summarize(mean_age = mean(AGE), sd_age = sd(AGE), count = n())



#Try it: Now try taking the mean and standard deviation you calculated earlier grouped by one of your demographic variables. Add in count (**n()**) in your summarize() command to return the count of respondents in each level of your demographic variable.  Next, try grouping by both demographic variables.


#Challenge
#Starting with the atus dataset, create a dataset that contains summary data by sex and whether there are children under 18 in the house. 
#The data should contain: 
# average hours worked that day 
# average hours spent socializing
# correlation between hours spent working & socializing
# counts of respondents in each group

#One possible solution:
#first, specify the data
atus %>% 
  #then use mutate() to create new varibles for hours (rather than minutes) worked
  mutate(ACT_WORK_HR = ACT_WORK/60, ACT_SOCIAL_HR = ACT_SOCIAL/60) %>% 
  #the group by sex and whether they have children
  group_by(SEX, HH_CHILD) %>%
  #finally, create averages for the hours worked, socialized, their correlation, and counts
  summarize(Avg_HRwork = mean(ACT_WORK_HR), AvgHRsocial = mean(ACT_SOCIAL_HR), CorWorkSocial = cor(ACT_WORK_HR, ACT_SOCIAL_HR), Count = n())


## Reshaping Data ##

#install and load the reshape2 package
install.packages("reshape2")
library(reshape2)


#Create a subset of the atus data as our long dataset
atus.long <- atus %>%
  select(EDUC, REGION) %>%
  group_by(EDUC, REGION) %>%
  summarize(count = n())


head(atus.long)


#Going from long to wide: dcast()
# Reshape so that there are separate columns for counts of region. 
atus.wide <- dcast(atus.long, 
                   EDUC ~ REGION, 
                   value.var = "count")

head(atus.wide)

#Can also just tag on using pipes:
atus %>%
  select(EDUC, REGION) %>%
  group_by(EDUC, REGION) %>%
  summarize(count = n()) %>% 
  dcast(EDUC ~ REGION, value.var = "count") %>% 
  head()

#what columns would the following code contain?
atus %>%
  select(EDUC, REGION, SEX) %>%
  group_by(EDUC, REGION, SEX) %>%
  summarize(count = n()) %>% 
  dcast(EDUC ~ REGION + SEX, value.var = "count")

#how will this differ?
atus %>%
  select(EDUC, REGION, SEX) %>%
  group_by(EDUC, REGION, SEX) %>%
  summarize(count = n()) %>% 
  dcast(EDUC + REGION ~ SEX, value.var = "count")


#Reshape columns for education back into rows:
head(atus.wide)

#Going from wide to long: melt()
atus.long.again <- melt(atus.wide, id.vars = "EDUC")
head(atus.long.again)

#Can also specify the names of the newly created variables:

atus.long.again <- melt(atus.wide, id.vars = "EDUC", variable.name = "REGION", value.name = "count")
head(atus.long.again)

