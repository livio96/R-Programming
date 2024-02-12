library(xlsx)
library(tidyverse)
library(dplyr)
library(readr)

#Pull data from datasets
#Received <- read_csv("/Users/liviobeqiri/Documents/R/Purchase Order Profitability/Received.csv")

ReceivedAndSold <- read_csv("C:/Users/LivioBeqiri/Desktop/R/PO Profitability/ReceivedAndSold.csv")
Fulfilled <- read_csv("C:/Users/LivioBeqiri/Desktop/R/PO Profitability/Fulfilled.csv")
Received <- read_csv("C:/Users/LivioBeqiri/Desktop/R/PO Profitability/Received.csv")
Inventory <- read_csv("C:/Users/LivioBeqiri/Desktop/R/PO Profitability/Inventory.csv")


merged_data <- merge(Received, Fulfilled, by="Serial Number")

#add profit Column
merged_data <- transform(
  merged_data, profit = ifelse(merged_data$`Item Rate`!= '' & merged_data$Rate != '', merged_data$Rate - merged_data$`Item Rate` - merged_data$Fees)
)

#Sales Rep Summary
merged_data_rep <- merged_data %>% 
  group_by(Sales.Rep) %>% 
  summarise(Total_Profit = sum(profit), Revenue = sum(Rate), Cost = sum(Item.Rate), Fees = sum(Fees))

#Item Summary
merged_data_item <- merged_data %>% 
  group_by(Name) %>% 
  summarise(Total_Profit = sum(profit), Revenue = sum(Rate), Cost = sum(Item.Rate), Fees = sum(Fees))

#Vendor Summary
merged_data_vendor <- merged_data %>% 
  group_by(Main.Line.Name) %>% 
  summarise(Total_Profit = sum(profit), Revenue = sum(Rate), Cost = sum(Item.Rate), Fees = sum(Fees))

#PO Summary
merged_data_po <- merged_data %>% 
  group_by(Document.Number) %>% 
  summarise(Total_Profit = sum(profit), Revenue = sum(Rate), Cost = sum(Item.Rate), Fees = sum(Fees))


write.xlsx(merged_data_rep, file="C:/Users/LivioBeqiri/Desktop/R/PO Profitability/TotalPOProfitability.xlsx", sheetName = 'PO Profitability - Rep')
write.xlsx(merged_data_item, file="C:/Users/LivioBeqiri/Desktop/R/PO Profitability/TotalPOProfitability.xlsx", sheetName = 'PO Profitability - Item', append = TRUE)
write.xlsx(merged_data_vendor, file="C:/Users/LivioBeqiri/Desktop/R/PO Profitability/TotalPOProfitability.xlsx", sheetName = 'PO Profitability - Vendor', append = TRUE)
write.xlsx(merged_data_po, file="C:/Users/LivioBeqiri/Desktop/R/PO Profitability/TotalPOProfitability.xlsx", sheetName = 'PO Profitability - PO', append = TRUE)





