library(scales)
library(readxl)
hesa <- read.csv("C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Structured Data\\HE Student Enrollment by Subject.csv")

#Remove all other columns except 2017/18 data
hesa <- hesa[-c(1:13), ]
hesa <- hesa[ ,-c(2:9)]
hesa <- hesa[-c(1,2), ]
names(hesa) <- c("Subject Area","Female","Male","Other","Total")
hesa$Country <- "UK"

write.csv(hesa,"C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\RawDataFiles\\RawDataStudents.csv",row.names = FALSE)
