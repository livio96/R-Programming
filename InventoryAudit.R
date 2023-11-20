library(xlsx)
library(readxl)
library(tidyverse)
library(dplyr)

path = '/Users/liviobeqiri/Documents/R/R-Programming/SerializedInventory.xlsx'
raw_data <- read_excel(path)

serials <- raw_data[2] # Get Serial Number column
only_serialized_inventory <- filter(raw_data, `Inventory Number` != '') #Remove non-serialized rows


#_______ Start of Duplicate Detection Code _______

duplicated_serial_numbers <- only_serialized_inventory[duplicated(only_serialized_inventory$`Inventory Number`) ,] #Get All Duplicated Rows - where serial number is duplicated
merged_data <- merge(raw_data, duplicated_serial_numbers, by.x = 'Inventory Number', by.y = 'Inventory Number')
write.csv(merged_data, '/Users/liviobeqiri/Documents/R/R-Programming/DuplicatedSerials.csv') #create a csv file with all duplicated serials

#_______ End of Dulicate Detection Code _________


#_______ Start of SN Status Check Code ___________

only_serialized_inventory_df <- data.frame(only_serialized_inventory)
sn_in_good_status <- filter(only_serialized_inventory_df, Status == 'Good') #Pull a list of all sn that are in good status
sn_in_good_status_wrong_bin <- filter(sn_in_good_status, (Bin.Number == 'A-RTV-01' | Bin.Number == 'Trash' | Bin.Number == 'StockCount1' | Bin.Number == 'Not Counted', | Bin.Number == 'Refurbishing', | Bin.Number == 'Testing'))
print(sn_in_good_status_wrong_bin)
write.csv(sn_in_good_status_wrong_bin, '/Users/liviobeqiri/Documents/R/R-Programming/GoodStatusWrongBin.csv') #create a csv file with all duplicated serials





