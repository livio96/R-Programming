library(xlsx)
library(tidyverse)
library(dplyr)
library(readr)

#Pull data from datasets
  ReceivedAndSold <- read_csv("C:/Users/LivioBeqiri/Desktop/Tableau Files/ReceivedAndSold.csv")
  Fulfilled <- read_csv("C:/Users/LivioBeqiri/Desktop/Tableau Files/Fulfilled.csv")
  Received <- read_csv("C:/Users/LivioBeqiri/Desktop/Tableau Files/Received.csv")
  Inventory <- read_csv("C:/Users/LivioBeqiri/Desktop/Tableau Files/Inventory.csv")
  
  sold_per_po <- ReceivedAndSold %>% 
    group_by(`Document Number`) %>% 
    summarise(QTY_SOLD = length(`Serial Number`))
  #view(sold_per_po)
  
  sold_per_po <- as.data.frame(sold_per_po)
  
  total_received_per_po <- Received %>% 
    group_by(`Document Number`) %>% 
    summarise(QTY_RECEIVED = length(`Serial Number`))
  #view(total_received_per_po)
  
  total_received_per_po <- as.data.frame(total_received_per_po)
  
  merge_closed_po_data <- merge(total_received_per_po, sold_per_po, by="Document Number")
  #view(merge_closed_po_data)
  
  closed_pos <- filter(merge_closed_po_data, QTY_RECEIVED == QTY_SOLD)
  #view(closed_pos)
  
  closed_pos_complete_view <- merge(Received, closed_pos, by="Document Number")
  #view(closed_pos_complete_view)
  
  final_data_set <- merge(closed_pos_complete_view, Fulfilled, by="Serial Number")
  #view(final_data_set)
  
  #Add Profit Column
  final_data_set <- transform(
    final_data_set, profit = ifelse(final_data_set$`Item Rate` != '' & final_data_set$Rate != '', final_data_set$Rate - final_data_set$`Item Rate` - final_data_set$Fees)
  )
  
  profit_by_Vendor <- final_data_set %>% 
    group_by(Main.Line.Name) %>% 
    summarise(Profit = sum(profit))
  view(profit_by_Vendor) 
  
  profit_by_item <- final_data_set %>% 
    group_by(Name) %>% 
    summarise(Profit = sum(profit))
  view(profit_by_item)
  
  profit_by_po <- final_data_set %>% 
    group_by(Sales.Rep, Document.Number) %>% 
    summarise(Profit = sum(profit))
  view(profit_by_po)
    
  profit_by_rep <- final_data_set %>% 
    group_by(Sales.Rep) %>%
    summarise(Profit = sum(profit))
    
  view(profit_by_rep)
  





