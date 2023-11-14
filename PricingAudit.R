library(xlsx)
library(dplyr)
library(readxl)
library(tidyverse)

path = 'C:/Users/LivioBeqiri/Desktop/R/PricingAudit.xlsx'
data_file <- read_excel(path)

data_frame = data.frame(data_file)
#print(data_frame)

business_bb_df <- filter(data_frame, as.numeric(Main.Brokerbin.Price) > as.numeric(Business.Price))  #Business < Brokerbin
ebay_business_df <- filter(data_frame, as.numeric(Set.eBay.Price) < as.numeric(Business.Price) & as.numeric(Set.eBay.Price)>0)   #Ebay < Business
amazon_business_df <- filter(data_frame, as.numeric(Amazon.FBM.Price) < as.numeric(Business.Price) & as.numeric(Amazon.FBM.Price) > 0) #Amazon < Business
newegg_business_df <- filter(data_frame, as.numeric(Set.Newegg.Price) < as.numeric(Business.Price) & as.numeric(Set.Newegg.Price)>0) #Newegg < Business
newegg_b_business_df <- filter(data_frame, as.numeric(Set.Newegg.Business.Price) < as.numeric(Business.Price) & as.numeric(Set.Newegg.Business.Price > 0)) #Newegg Business < Business

error_message_list <- list() #This list will store all error messages

if(nrow(business_bb_df) > 0) {
for(i in 1:nrow(business_bb_df)-1){
  error_message_list <- append(error_message_list, 'Business Price Less than BB Price')
}
}
if(nrow(ebay_business_df) > 0) {
for (i in 1:nrow(ebay_business_df)-1){
  error_message_list <- append(error_message_list, "Business Price less than ebay price")
}
}

if(nrow(amazon_business_df) > 0) {
for (i in 1:nrow(amazon_business_df)-1){
  error_message_list <- append(error_message_list,'Business Price Less than Amazon Price')
}
}

if(nrow(newegg_business_df) > 0) {
for (i in 1:nrow(newegg_business_df)-1){
  error_message_list <- append(error_message_list,'Business Price Less than Newegg Price')
}
}

if(nrow(newegg_b_business_df) > 0) {
for (i in 1:nrow(newegg_b_business_df)-1){
  error_message_list <- append(error_message_list,'Business Price Less than Newegg Business Price')
}
}

final_df <- bind_rows(business_bb_df, ebay_business_df, amazon_business_df, newegg_business_df, newegg_b_business_df) #bind all df together
if(length(error_message_list)>0){
final_df$error_message_list <- error_message_list #append error message list to final data frame before writing to csv
final_df <- apply(final_df,2,as.character)
}

      
write.csv(final_df, "C:/Users/LivioBeqiri/Desktop/R/PricingErrorReport.csv") #write into csv
