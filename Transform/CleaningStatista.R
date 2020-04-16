library(readxl)
#setwd("C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Statista")
rm(list = ls())
statista <- read_excel("C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Statista\\statistic_id798422_data-breach-costs-per-capita-in-health-care-vs-other-industries-2018.xls",sheet = 2)
names(statista) <- c("Industry","PerCapitaCost")

#Remove the first 2 unwanted rows
statista <- statista[-c(1,2), ]
statista$IndustryID <- seq.int(nrow(statista))
statista <- statista[c(3,1,2)]
statista$Industry <- gsub("Health","Healthcare",statista$Industry)
statista$PerCapitaCost <- as.numeric(statista$PerCapitaCost)
#write.csv(statista,"C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Statista\\After Cleaning\\RawDataCost.csv",row.names = FALSE)
write.csv(statista,"C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\RawDataFiles\\RawDataCost.csv", row.names = FALSE)
