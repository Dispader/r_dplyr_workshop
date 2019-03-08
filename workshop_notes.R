# Dplyr workshop notes

## March 8th, 2019

# install.packages("dplyr")
library(dplyr)

# data acquired from
# https://drive.google.com/drive/folders/1Gy8iI9lt6cjYVBA3KEPFTBb_d6dQ6-df

setwd("~/Development/github/Dispader/r_dplyr_workshop")
load("ATUSdata_subset.Rdata")
View(atus)

atus.subset1 <- select(atus, YEAR, ACT_WORK, ACT_SOCIAL)