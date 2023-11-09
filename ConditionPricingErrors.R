library(xlsx)
library(readxl)
library(dplyr)
library(tidyverse)

#Define Excel Path
path <- "C:/Users/LivioBeqiri/Desktop/R/ConditionPricingErrors.xlsx"
raw_data_set = read_excel(path)
data_set_frame = data.frame(raw_data_set)


new_condition_set = filter(data_set_frame, WebStore.Condition=='New')
like_new_condition_set = filter(data_set_frame, WebStore.Condition=='Like New')
recertified_condition_set = filter(data_set_frame, WebStore.Condition=='Recertified')
refresh_condition_set = filter(data_set_frame, WebStore.Condition=='Refresh')

#If Like New price > New Price
merged_set <- merge(new_condition_set,like_new_condition_set, by.x='MPN', by.y='MPN')
merged_set_mod <- filter(merged_set, Main.Brokerbin.Price.x < Main.Brokerbin.Price.y)
write.csv(merged_set_mod,"C:/Users/LivioBeqiri/Desktop/R/LikeNewGreaterThanNew.csv")

#If Recertified price > New Price
merged_set <- merge(new_condition_set,recertified_condition_set, by.x='MPN', by.y='MPN')
merged_set_mod <- filter(merged_set, Main.Brokerbin.Price.x < Main.Brokerbin.Price.y)
write.csv(merged_set_mod,"C:/Users/LivioBeqiri/Desktop/R/RecertifiedGreaterThanNew.csv")

#If Like New > recertified
merged_set <- merge(like_new_condition_set,recertified_condition_set, by.x='MPN', by.y='MPN')
merged_set_mod <- filter(merged_set, Main.Brokerbin.Price.x > Main.Brokerbin.Price.y)
write.csv(merged_set_mod,"C:/Users/LivioBeqiri/Desktop/R/LikeNewGreaterThanRecertified.csv")

#If refresh > New
merged_set <- merge(refresh_condition_set,new_condition_set, by.x='MPN', by.y='MPN')
merged_set_mod <- filter(merged_set, Main.Brokerbin.Price.x > Main.Brokerbin.Price.y)
write.csv(merged_set_mod,"C:/Users/LivioBeqiri/Desktop/R/RefreshGreaterThanNew.csv")

