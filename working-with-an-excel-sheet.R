library(xlsx)
library(readxl)

#Get Excel File Data - Excel_Data.xlsx
excel_data <- read_xlsx("C:/Users/LivioBeqiri/Desktop/R/excel_data.xlsx", sheet = "Sheet1")

#Define Excel Path
excel_path <- "C:/Users/LivioBeqiri/Desktop/R/excel_data.xlsx"

#Get Excel Columns and store them into a variable
title <- read.xlsx(excel_path, sheetName = "Sheet1", colIndex = 1)
price <- read.xlsx(excel_path, sheetName = "Sheet1", colIndex = 2)

#Create a data-Frame for all excel columns
DataFrame <- data.frame(title,price)

print(DataFrame)
