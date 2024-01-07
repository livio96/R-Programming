library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)

path = '/cloud/project/R/InventoryDetailOnHandBinStatusResults.csv'
raw_data <- read.csv(path)
print(raw_data)
serials <- raw_data[2] # Get Serial Number column
only_serialized_inventory <- filter(raw_data, `Inventory.Number` != '') #Remove non-serialized rows

#_______ Start of Duplicate Detection Code _______

duplicated_serial_numbers <- only_serialized_inventory[duplicated(only_serialized_inventory$`Inventory Number`) ,] #Get All Duplicated Rows - where serial number is duplicated
merged_data <- merge(raw_data, duplicated_serial_numbers, by.x = 'Inventory.Number', by.y = 'Inventory.Number')
write.csv(merged_data, '/cloud/project/R/DuplicatedSerials.csv') #create a csv file with all duplicated serials

#_______ End of Dulicate Detection Code _________

#_______ Start of SN Status Check Code ___________

only_serialized_inventory_df <- data.frame(only_serialized_inventory)
sn_in_good_status <- filter(only_serialized_inventory_df, Status == 'Good') #Pull a list of all sn that are in good status
sn_in_good_status_wrong_bin <- filter(sn_in_good_status, (Bin.Number == 'A-RTV-01' | Bin.Number == 'Trash' | Bin.Number == 'StockCount1' | Bin.Number == 'Not Counted' | Bin.Number == 'Refurbishing' | Bin.Number == 'Testing' | Bin.Number == 'Amazon Removal' | Bin.Number == 'Parts'))
print(sn_in_good_status_wrong_bin)
write.csv(sn_in_good_status_wrong_bin, '/cloud/project/R/GoodStatusWrongBin.csv') #create a csv file with all duplicated serials

#______ End of SN Status Check code _______

sn_not_in_good_status <- filter(only_serialized_inventory_df, Status != 'Good')
sn_not_in_good_status_cd_bin <- filter(sn_not_in_good_status, (startsWith(Bin.Number, "CD-")))
print(sn_not_in_good_status_cd_bin)
write.csv(sn_not_in_good_status_cd_bin, '/cloud/project/R/NotGoodStatusCDBin.csv')



