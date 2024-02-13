library(xlsx)
library(tidyverse)
library(dplyr)
library(readr)

ReceivedAndSold <- read_csv("C:/Users/LivioBeqiri/Desktop/R/PO Profitability/ReceivedAndSold.csv")
Fulfilled <- read_csv("C:/Users/LivioBeqiri/Desktop/R/PO Profitability/Fulfilled.csv")
Received <- read_csv("C:/Users/LivioBeqiri/Desktop/R/PO Profitability/Received.csv")
Inventory <- read_csv("C:/Users/LivioBeqiri/Desktop/R/PO Profitability/Inventory.csv")

Fulfilled <- filter(Fulfilled, Rate>0 & (Type == 'Invoice' | Type == "Cash Sale"))
merged_fulfilled_received <- merge(Received, Fulfilled, by = "Serial Number")

#Calculate quantity Received per PO and total amount received
quantity_received_po <- Received %>% 
  group_by(`Document Number`, `Sales Rep`) %>% 
  summarise(QTY_Received = length(`Serial Number`), amount_purchased = sum(`Item Rate`))
#Calculate Quantity Sold per PO and total revenue
quantity_fulfilled_po <- merged_fulfilled_received %>% 
  group_by(`Document Number`) %>% 
  summarise(QTY_Fulfilled = length(`Serial Number`), revenue = sum(`Rate`), Inventory_cost = sum(`Item Rate`), fees = sum(`Fees`))

#Calculate Remaining Inventory
remaining_inventory <- merge(quantity_received_po,quantity_fulfilled_po, by = "Document Number")
remaining_inventory <- transform(
  remaining_inventory, profit = remaining_inventory$revenue-remaining_inventory$Inventory_cost-remaining_inventory$fees, remaining_quantity = remaining_inventory$QTY_Received-remaining_inventory$QTY_Fulfilled
)
view(remaining_inventory)





