library(xlsx)
library(tidyverse)
library(dplyr)
library(readr)

#Pull data from datasets
Received <- read_csv("/Users/liviobeqiri/Documents/R/Purchase Order Profitability/Received.csv")
Inventory <- read_csv("/Users/liviobeqiri/Documents/R/Purchase Order Profitability/Inventory.csv")
Fulfilled <- read_csv("/Users/liviobeqiri/Documents/R/Purchase Order Profitability/Fulfilled.csv")
ReceivedAndSold <- read_csv("/Users/liviobeqiri/Documents/R/Purchase Order Profitability/ReceivedAndSold.csv")


merged_data <- merge(Received, Fulfilled, by="Serial Number")

#add profit Column
merged_data <- transform(
  merged_data, profit = ifelse(merged_data$`Item Rate`!= '' & merged_data$Rate != '', merged_data$Rate - merged_data$`Item Rate` - merged_data$Fees)
)


merged_data <- merged_data %>% 
  group_by(Sales.Rep) %>% 
  summarise(Total_Profit = sum(profit), Revenue = sum(Rate), Cost = sum(Item.Rate), Fees = sum(Fees))

view(merged_data)

  





