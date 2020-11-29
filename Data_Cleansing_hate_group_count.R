library(plyr) # to rename columns
library(tidyverse)
library(dplyr)


hc_full_df <- read.csv('hate_crimes_full.csv', header = TRUE, fileEncoding='UTF-8-BOM')

# Add in the 2016 Data
hate_group_2016_df <- read.csv('hate_group_count_2016.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hate_group_2016_df<-rename(hate_group_2016_df, hate_group_count_2016=HateGroupCount)
hate_group_2016_df<-rename(hate_group_2016_df, state_full=State)
hate_group_2016_df <- hate_group_2016_df[ -c(1)]

hc_full_df<-left_join(hc_full_df, hate_group_2016_df, by = 'state_full')

hc_full_df$hate_group_count_2016[is.na(hc_full_df$hate_group_count_2016)] <- 0

# Add in the 2019 Data
hate_group_2019_df <- read.csv('hate_group_count_2019.csv', header = TRUE, fileEncoding='UTF-8-BOM')
hate_group_2019_df<-rename(hate_group_2019_df, hate_group_count_2019=HateGroupCount)
hate_group_2019_df<-rename(hate_group_2019_df, state_full=State)
hate_group_2019_df <- hate_group_2019_df[ -c(1)]

hc_full_df<-left_join(hc_full_df, hate_group_2019_df, by = 'state_full')

hc_full_df$hate_group_count_2019[is.na(hc_full_df$hate_group_count_2019)] <- 0

write.csv(hc_full_df, 'hate_crimes_full_v2.csv', row.names=F)