#Comments or questions to:
#John West
#john@tacc.utexas.edu
#Texas Advanced Computing Center, U of Texas at Austin
#18 Aug 2015
#
#RStudio Version 0.99.441 – © 2009-2015 RStudio, Inc.
#Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_4) AppleWebKit/600.7.12 (KHTML, like Gecko)

#This program reads in the CSV file created by the Qualtrics survey software and prints out
#cross-tabulated summaries of the quantities of interest to the screen. I know this isn't the best
#R code ever; I sacrificed speed in getting the job done for elegance of the code.
#
#Inputs: file named TACC_Demographics_Results.csv in the same directory as this file.
#Outputs: summary stats printed to the console
#

library(plyr)
library(dplyr)
library(reshape2)
library(data.table)

#These three variables determine the mapping of group names to staffing categories (leadership,
#tech, admin).
leadershipc <- c("Strategic Initiatives",
                 "TACC Leadership Team \\(LT\\)", #the assignment is done using grep so we need to escape regex special characters
                 "Industrial Programs",
                 "Center Programs",
                 "Director's Office"
)
adminc <- c("Education and Outreach",
            "Administrative Support",
            "Facilities"
)
#I included communications in tech because that discipline requires HPC-specific domain knowledge. It is a
#judgment call as to whether it belongs here or with Education and Outreach in admin.
techc <- c("Communications, Media and Design",
           "Data Mining and Statistics",
           "Data Management and Collections",
           "Life Sciences Computing",
           "Large Scale Systems",
           "HPC Applications",
           "HPC Software Tools",
           "HPC Performance & Architectures",
           "Portal and Gateway Infrastructure",
           "User Services",
           "Scalable Visualization Technologies",
           "Technology Infrastructure",
           "Visualization Interfaces and Applications",
           "Web and Cloud Services",
           "Web and Mobile Applications"
)

rawData <- read.csv("./TACC_Demographics_Results.csv",header = TRUE,na.strings = "", stringsAsFactors = FALSE,skip = 1)

#Get rid of columns we don't need
del <- c(1:11,19:22)
tidyData <- rawData[,-del]

#Set pretty names for the columns we'll use
names(tidyData)[1] <- "gender"
names(tidyData)[2] <- "asian"
names(tidyData)[3] <- "black"
names(tidyData)[4] <- "hispanic"
names(tidyData)[5] <- "white"
names(tidyData)[6] <- "other"
names(tidyData)[7] <- "TACC.group"

#Convert columns from character strings to factors
tidyData$gender <- as.factor(tidyData$gender)
tidyData$asian <- as.factor(tidyData$asian)
tidyData$white <- as.factor(tidyData$white)
tidyData$black <- as.factor(tidyData$black)
tidyData$hispanic <- as.factor(tidyData$hispanic)
tidyData$other <- as.factor(tidyData$other)

#Map the TACC group to leadership, technical, admin
#
#Yes, I know this is ugly code; don't hate. If you know of a more R way to do a logical or on
#elements of a list let me know. Probably something with ldply
a <- ldply(techc,function(x) {grepl(x,tidyData$TACC.group)})
tech <- a[1,] | a[2,] | a[3,] | a[4,] | a[5,] | a[6,] | a[7,] | a[8,] |
    a[9,] | a[10,] | a[11,] | a[12,] | a[13,] | a[14,] | a[15,]
tidyData[tech,"Type"] <- "Technical"

a <- ldply(leadershipc,function(x) {grepl(x,tidyData$TACC.group)})
leaders <- a[1,] | a[2,] | a[3,] | a[4,] | a[5,]
tidyData[leaders,"Type"] <- "Leadership"

a <- ldply(adminc,function(x) {grepl(x,tidyData$TACC.group)})
admin <- a[1,] | a[2,] | a[3,]
tidyData[admin,"Type"] <- "Admin"

#Convert to factor and delete the group column
tidyData$Type <- as.factor(tidyData$Type)
tidyData$TACC.group <- NULL

#Create the table summarizing the overal fraction of men and women at TACC
genderTable <- prop.table(table(tidyData$gender))
print("Overall gender") 
print(format(genderTable,digits = 3))

#Create the table summarizing the gender fraction by position type
genderType <- xtabs(~tidyData$gender+tidyData$Type)
typeTotals <- colSums(genderType)
prop.genderType <- sweep(genderType[,1:3],2,typeTotals,`/`)

#genderType <- as.data.frame(prop.genderType)

#Convert this from factor to numeric; this has the effect of enabling a simple sum over the columns
#in order to get a count of each category; something that would be harder had I not converted to 
#a factor first. Note that the NAs resulting from this coercion have to be thrown away in the sum
tidyData$asian <- as.numeric(tidyData$asian)
tidyData$white <- as.numeric(tidyData$white)
tidyData$black <- as.numeric(tidyData$black)
tidyData$hispanic <- as.numeric(tidyData$hispanic)
tidyData$other <- as.numeric(tidyData$other)

#Converting to table allows us to use .SD to get the multicolumn sum; this would be hard to get 
#otherwise
tidyData.table <- as.data.table(tidyData)
ethnicity <- tidyData.table[, lapply(.SD, sum, na.rm=TRUE),.SDcols=c("asian","white","black","hispanic","other") ]
print("Overall Ethnicity")
print(format(ethnicity/rowSums(ethnicity),digits=3))

print("Gender by Role")
print(format(prop.genderType,digits=3))

print("Fraction of roles comprised of each ethnicity")
ethnicityRoles <- (tidyData.table[, lapply(.SD, sum, na.rm=TRUE),by=Type,.SDcols=c("asian","white","black","hispanic","other") ])
ethnicityRoles[,2:6] <- sweep(ethnicityRoles[,2:6,with=FALSE],2,unlist(ethnicity),`/`)
print(format(ethnicityRoles,digits=3))

print("Fraction of ethnicity assigned to each role (this is the stat that is reported)")
roleEthnicity <- (tidyData.table[, lapply(.SD, sum, na.rm=TRUE),by=Type,.SDcols=c("asian","white","black","hispanic","other") ])
roles <- rowSums(roleEthnicity[,2:6,with=FALSE])
roleEthnicity[,2:6] <- sweep(roleEthnicity[,2:6,with=FALSE],1,unlist(roles),`/`)
print(format(roleEthnicity,digits=3))

