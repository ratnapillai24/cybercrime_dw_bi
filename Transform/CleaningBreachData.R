
#Read & Merge both 2018 and 2017 data for breach data

remove(allbreachdata)  
mergefiles <- function(mypath){
  #setwd("C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Unstructured Data\\Before Cleaning\\BreachData\\")
  filenames <- list.files(path=mypath, full.names = TRUE)
  datalist <- lapply(filenames,
                     function(x){read.csv(file = x,
                                          header = TRUE,
                                          stringsAsFactors = FALSE)})
  Reduce(function(x,y){merge(x,y,all=TRUE)}, datalist)
  
}

breachdatapath <- "C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Unstructured Data\\Before Cleaning\\BreachData\\"

allbreachdata <- mergefiles(breachdatapath)
length(allbreachdata)

#Format the Breach_Date column
library(lubridate)
#all <- allbreachdata$Date.of.Breach
allbreachdata$Date.of.Breach <- mdy(allbreachdata$Date.of.Breach)



#Drop 2nd and Rank Column as it stores the serial number for both files on merge
allbreachdata <- allbreachdata[-c(1,2)]


#Assign column name to serial number created on merge & set names with proper naming convention for other columns
names(allbreachdata) <- c("Risk_Score","Industry","Records_Breached","Breach_Date","Breach_Type","Breach_Source","Location")
#allbreachdata$Breach_Date <- mdy(allbreachdata$Breach_Date)

#Create a new column to indicate the unique id for breach data
allbreachdata$Breach_ID <- seq.int(nrow(allbreachdata))

#Re-order the Breach ID column to from last column to first column
allbreachdata <- allbreachdata[c(8,1,2,3,4,5,6,7)]

#Write to csv file to create the final raw data file for breach data
write.csv(allbreachdata,"C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Unstructured Data\\After Cleaning\\RawBreachData.csv",row.names = FALSE)

#DATA MANIPULATION AND CLEANING
#Read raw Data file
rawdata <- read.csv("C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Unstructured Data\\After Cleaning\\RawBreachData.csv",na.strings = c("","NA"))
#Check for NA values in each column
sapply(rawdata,function(x) sum(is.na(x)))

#Map values for breach type and breach source
rawdata$BreachTypeNo <- ''
rawdata$BreachSourceNo <- ''
for(t in 1:lastrow){
  if(rawdata$Breach_Type[t] == "Nuisance"){rawdata$BreachTypeNo[t] <- 1}
  else if(rawdata$Breach_Type[t] == "Account Access"){rawdata$BreachTypeNo[t] <- 2}
  else if(rawdata$Breach_Type[t] == "Financial Access"){rawdata$BreachTypeNo[t] <- 3}
  else if(rawdata$Breach_Type[t] == "Identity Theft"){rawdata$BreachTypeNo[t] <- 4}
  else if(rawdata$Breach_Type[t] == "Existential Data"){rawdata$BreachTypeNo[t] <- 1}
  else {rawdata$BreachTypeNo[t] <- 1}
}

for(s in 1:lastrow){
  if(rawdata$Breach_Source[s] == "Accidental Loss"){
    rawdata$BreachSourceNo[s] <- 1
  } else if(rawdata$Breach_Source[s] == "Lost Device"){
    rawdata$BreachSourceNo[s] <- 1
  } else if(rawdata$Breach_Source[s] == "Malicious Insider"){
    rawdata$BreachSourceNo[s] <- 3
  } else if(rawdata$Breach_Source[s] == "Malicious Outsider"){
    rawdata$BreachSourceNo[s] <- 4
  } else if(rawdata$Breach_Source[s] == "State Sponsored"){
    rawdata$BreachSourceNo[s] <- 5
  } else if(rawdata$Breach_Source[s] == "Stolen Device"){
    rawdata$BreachSourceNo[s] <- 2
  } else {
    rawdata$BreachSourceNo[s] <- 1
  }
}

#Replace NA calues with 1 in Records Breached column
rawdata$Records_Breached[is.na(rawdata$Records_Breached)] <- 1
sapply(rawdata,function(x) sum(is.na(x)))


rawdata$BreachSourceNo
rawdata$BreachTypeNo

rawdata$newriskscore <- ''
rawdata$xvlaue <- ''

recordfact <- rawdata$Records_Breached
recordcount <- ''

#Find the new risk score based on the formula
for(j in 1:lastrow){
  
  recordcount[j] <- gsub(",","",recordfact[j])
  rawdata$xvalue[j] <- as.numeric((as.numeric(recordcount[j]) * as.numeric(rawdata$BreachSourceNo[j]) * as.numeric((rawdata$BreachTypeNo[j]))))
  rawdata$newriskscore[j] <- log10(rawdata$xvalue[j])
}
class(rawdata$newriskscore)
format(round(as.numeric(rawdata$newriskscore),2),nsmall = 2)



par(mfrow = c(2,2))
#Histogram of original Risk score of the data
hist(rawdata$Risk_Score)

#Histogram of new risk score
hist(as.numeric(rawdata$newriskscore))


#Compute Risk Severity based on the severity calculation given on the Breachlevel Index site
rawdata$Risk_Severity <- NA

lastrow <- length(rawdata$Risk_Score)
#class(lastrow)

for (i in 1:lastrow) {
  
  #Compute Risk Score
  if(rawdata$Risk_Score[i] >=0 & rawdata$Risk_Score[i] <=0.9){
    rawdata$Risk_Severity[i] <- 'Nominal'
  }else if(rawdata$Risk_Score[i] >=1 & rawdata$Risk_Score[i] <=2.9){
    rawdata$Risk_Severity[i] <- 'Minimal'
  } else if(rawdata$Risk_Score[i] >=3 & rawdata$Risk_Score[i] <=4.9){
    rawdata$Risk_Severity[i] <- 'Moderate'
  } else if(rawdata$Risk_Score[i] >=5 & rawdata$Risk_Score[i] <=6.9){
    rawdata$Risk_Severity[i] <- 'Critical'
  } else if(rawdata$Risk_Score[i] >=7 & rawdata$Risk_Score[i] <=8.9){
    rawdata$Risk_Severity[i] <- 'Severe' 
  } else{
    rawdata$Risk_Severity[i] <- 'Catastrophic'
  }
}


#Remove all calculated fields for cleaning except Risk Severity
rawdata <- rawdata[-c(9:13)]

#write to csv file
write.csv(rawdata,"C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Unstructured Data\\After Cleaning\\RawDataBreach.csv",row.names = FALSE)
