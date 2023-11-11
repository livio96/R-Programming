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
barplot(as.matrix(subsetted_newark_nj), ylim = c(0,2000))
