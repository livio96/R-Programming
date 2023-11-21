library(readxl)
library(tidyverse)
library(xlsx)
library(dplyr)

amazon_report_path <- '/Users/liviobeqiri/Documents/R/R-Programming/Amazon.csv'
netsuite_report_path <- '/Users/liviobeqiri/Documents/R/R-Programming/AmazonRemovalNetsuite.xlsx'

amazon_raw_data <- read_csv(amazon_report_path)
netsuite_raw_data <- read_excel(netsuite_report_path)

amazon_df <- data.frame(amazon_raw_data)
netsuite_df <- data.frame(netsuite_raw_data)

amazon_tracking_number_vector <- amazon_df[9]
amazon_tracking_number_vector <- strsplit(as.character(amazon_tracking_number_vector), split = ", ")
print(amazon_tracking_number_vector)
write.csv(amazon_tracking_number_vector, '/Users/liviobeqiri/Documents/R/R-Programming/splitTrackings.csv')

#print(netsuite_df)

