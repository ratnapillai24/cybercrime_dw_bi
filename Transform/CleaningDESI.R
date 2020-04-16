library(scales)
library(readxl)
desi <- read_excel("C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Structured Data\\DESI.xlsx")
#Clean Indicator column, remove the number assigned to indicator
desi$Indicator <- substring(desi$Indicator,2)
#Round the weignthed score
desi$`Weighted Score` <- round(desi$`Weighted Score`,0)

write.csv(desi,"C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\RawDataFiles\\RawDataDESI.csv",row.names = FALSE)
