library(DBI)
library(RODBC)
library(sqldf)
conn <- odbcConnect("CData NetSuite Source")

query = "Select averageCost from SerializedInventoryItem where SerializedInventoryItem.internalid=21865"
result <- sqlQuery(conn,query)
odbcClose(conn)
print(result)
