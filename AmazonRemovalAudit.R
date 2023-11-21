library(readxl)
library(tidyverse)
library(xlsx)
library(dplyr)
library(tidyr)

amazon_report_path <- '/Users/liviobeqiri/Documents/R/R-Programming/Amazon.csv'
netsuite_report_path <- '/Users/liviobeqiri/Documents/R/R-Programming/AmazonRemovalNetsuite.xlsx'

#Get Raw Data
amazon_raw_data <- read_csv(amazon_report_path)
netsuite_raw_data <- read_excel(netsuite_report_path)
#Create Data Frames
amazon_df <- data.frame(amazon_raw_data)
netsuite_df <- data.frame(netsuite_raw_data)

split_frame <- amazon_df %>% separate(tracking.number, c('Tracking1', 'Tracking2'))

merged_split_frame1 <- merge(split_frame, netsuite_df, by.x = 'Tracking1', by.y = 'Amazon.Fulfillment')
merged_split_frame2 <- merge(split_frame, netsuite_df, by.x = 'Tracking2', by.y = 'Amazon.Fulfillment')

final_data_set <- rbind(merged_split_frame1, merged_split_frame2)

errors <- filter(final_data_set, (fnsku==FNSKU & shipped.quantity != Quantity))

write.csv(errors, '/Users/liviobeqiri/Documents/R/R-Programming/splitTrackings.csv')

