library(tidyverse)
library(xlsx)
library(readxl)
library(dplyr)

path = "/Users/liviobeqiri/Documents/R/ZillowRentalData.xlsx"
raw_data <- read_excel(path)
#print(raw_data)

newark_nj <- filter(raw_data, State == 'NJ' & RegionName == 'Newark' )
subsetted_newark_nj = subset(newark_nj, select = -c(RegionID,SizeRank,RegionName,RegionType,StateName,State, Metro, CountyName) )
print(subsetted_newark_nj)
#barplot(as.matrix(subsetted_newark_nj), ylim = c(0,2000))

library(shiny)
library(shinythemes)
library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)
library(tidyverse)
library(raster)



file_path <- '/Users/liviobeqiri/Documents/R/R-Programming/Revenue.xlsx'
raw_data <- read_excel(file_path)
data_df <- as.data.frame(raw_data)
data_filtered_by_revenue <- filter(data_df, Revenue>200000)



# Barplot
ggplot(data_filtered_by_revenue, aes(x=Brand, y=Revenue)) + 
  geom_bar(stat = "identity") + coord_cartesian(ylim = c(500000, 5000000))

