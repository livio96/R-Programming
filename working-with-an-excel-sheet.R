library(xlsx)
library(readxl)

#Get Excel File Data - Excel_Data.xlsx
excel_data <- read_xlsx("C:/Users/LivioBeqiri/Desktop/R/excel_data.xlsx", sheet = "Sheet1")
print(excel_data)
