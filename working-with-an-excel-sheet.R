library(xlsx)
library(readxl)

#Get Excel File Data - Excel_Data.xlsx

#Define Excel Path
First_Data_Set_Path <- "C:/Users/LivioBeqiri/Desktop/R/FirstDataSet.xlsx"
Second_Data_Set_Path <- "C:/Users/LivioBeqiri/Desktop/R/SecondDataSet.xlsx"

#Get Excel Columns and store them into a variable
title_first_data_set <- read.xlsx(First_Data_Set_Path, sheetName = "Sheet1", colIndex = 1)
price_price_first_data_set <- read.xlsx(First_Data_Set_Path, sheetName = "Sheet1", colIndex = 2)
quantity_first_data_set <- read.xlsx(First_Data_Set_Path, sheetName = "Sheet1", colIndex = 3)

#Get Excel Columns and store them into a variable
title_second_data_set <- read.xlsx(Second_Data_Set_Path, sheetName = "Sheet1", colIndex = 1)
price_second_data_set <- read.xlsx(Second_Data_Set_Path, sheetName = "Sheet1", colIndex = 2)


#Create a data-Frame for all excel columns
DataFrame_FirstDataSet <- data.frame(title_first_data_set,price_price_first_data_set, quantity_first_data_set)
DataFrame_SecondDataSet <- data.frame(title_second_data_set,price_second_data_set)
print(DataFrame_FirstDataSet)
print(DataFrame_SecondDataSet)

merged_data_frame = merge(DataFrame_SecondDataSet, DataFrame_FirstDataSet, by="Title")
print(merged_data_frame)

#Plotting the dataframes defined abouve
#first convert to matrix
data_matrix = data.matrix(DataFrame_SecondDataSet)

barplot(data_matrix, xlab="x-asix" , ylab = 'y-axis", main="Bar-Chart")  #BarPlot
plot(data_matrix, type="l", xlab="x-axis", ylab="yaxis", main="Line-Chart")  #LineChart

