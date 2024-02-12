library(xlsx)
library(dplyr)
library(tidyr)
library(readr)
library(RCurl)

#Read file from FTP server
url <- "http://telquestftp.com/config.csv"
text_data <- getURL(url, userpwd = "r@telquestftp.com:*********", connecttimeout = 60)
df <- read.csv(text = text_data)

print(df)
