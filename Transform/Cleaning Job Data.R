jobs <- read.csv("C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\Unstructured Data\\Before Cleaning\\March3AllJobs.csv")
#Remove all columns except live vacancies and Skills
jobs <- jobs[,-c(1,2,4:7) ]
jobs$Country <- "UK"
#Rename columns
names(jobs) <- c("Skills","HistoricalAds","LiveJobVacancy","Country")

#Check for NA values in each column
sapply(jobs,function(x) sum(is.na(x)))

#Since the live vacancy count is NA, we can eliminate them
newjobs <- jobs[!is.na(jobs$LiveJobVacancy), ]

#Categorize the skills

skills <- newjobs$Skills
for (i in 1:nrow(newjobs)){
  if(grepl("Blockchain",skills[i])){
    newjobs$SkillCategory[i] <- "Blockchain Developer"
  }else if(grepl("Compliance",skills[i]) || grepl("Regulatory",skills[i]) || grepl("Governance",skills[i])){
    newjobs$SkillCategory[i] <- "IT Compliance Analyst"
  }else if(grepl("crpyt",skills[i])){
    newjobs$SkillCategory[i] <- "Cyptography Engineer"
  }else if(grepl("disaster",skills[i])){
    newjobs$SkillCategory[i] <- "Disaster Recovery"
  }else if(grepl("penetration",skills[i])||grepl("ethical",skills[i])){
    newjobs$SkillCategory[i] <- "Ethical Hacker"
  }else if(grepl("Firewall",skills[i])){
    newjobs$SkillCategory[i] <- "Firewall Analyst"
  }else if(grepl("Information",skills[i])){
    newjobs$SkillCategory[i] <- "Information Security"
  }else if(grepl("Detection",skills[i])){
    newjobs$SkillCategory[i] <- "Intrusion Detection"
  }else if(grepl("Malware",skills[i])){
    newjobs$SkillCategory[i] <- "Malware Analyst"
  }else if(grepl("Incident",skills[i])){
    newjobs$SkillCategory[i] <- "Security Incident Analyst"
  }else if(grepl("Risk",skills[i])){
    newjobs$SkillCategory[i] <- "Security Risk Specialist"
  }else if(grepl("Vulnerability",skills[i])){
    newjobs$SkillCategory[i] <- "Vulnerability Assessor"
  }else if(skills[i] == "C" || grepl("JavaScript",skills[i]) || grepl("Python",skills[i]) || grepl("Software Engineering",skills[i]) || grepl("PHP",skills[i]) || grepl("SQL",skills[i])){
    newjobs$SkillCategory[i] <- "Cyber Security Programming"
  }else if(grepl("cyber",skills[i]) || grepl("support",skills[i]) || grepl("Systems Administrator",skills[i]) || grepl("Forensics",skills[i]) || grepl("Encryption",skills[i]) || grepl("Privacy",skills[i]) || grepl("Governance",skills[i]) || grepl("Protection",skills[i]) || grepl("SOC",skills[i]) || grepl("Threat",skills[i]) || grepl("Web Services",skills[i])){
    newjobs$SkillCategory[i] <- "Cyber Security"
  }else if(grepl("SCADA",skills[i]) || grepl("Network",skills[i]) || grepl("CCSP",skills[i]) || grepl("^CC",skills[i]) || grepl("web services",skills[i]) || grepl("REST",skills[i]) || grepl("SOAP",skills[i]) || grepl("CISM",skills[i]) || grepl("Linux",skills[i]) || grepl("Java",skills[i])){
    newjobs$SkillCategory[i] <- "Security Specialist"
  }else{
    newjobs$SkillCategory[i] <- "Other IT jobs"
  }
}

#Write to csv file
write.csv(newjobs,"C:\\Data Analytics\\Sem 1\\DWBI\\Project\\Final DWBI Project\\Cyber Crime\\RawDataFiles\\RawDataJob.csv",row.names = FALSE)



