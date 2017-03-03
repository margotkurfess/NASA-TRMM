rm(list = ls())
# set working directory
setwd("/Users/bjornwisaeus/Desktop/Current datawork")

library(RNetCDF)
# load packages
pacman::p_load(foreign, plyr, data.table, readstata13)

#loading the sets
df1998 <- read.dta("monthly_data_Ethiopia_1998.dta")
df1999 <- read.dta("monthly_data_Ethiopia_1999.dta")
df2000 <- read.dta("monthly_data_Ethiopia_2000.dta")
df2001 <- read.dta("monthly_data_Ethiopia_2001.dta")
df2002 <- read.dta("monthly_data_Ethiopia_2002.dta")
df2003 <- read.dta("monthly_data_Ethiopia_2003.dta")
df2004 <- read.dta("monthly_data_Ethiopia_2004.dta")
df2005 <- read.dta("monthly_data_Ethiopia_2005.dta")
df2006 <- read.dta("monthly_data_Ethiopia_2006.dta")
df2007 <- read.dta("monthly_data_Ethiopia_2007.dta")
df2008 <- read.dta("monthly_data_Ethiopia_2008.dta")
df2009 <- read.dta("monthly_data_Ethiopia_2009.dta")
df2010 <- read.dta("monthly_data_Ethiopia_2010.dta")

#cleaning away some columns
df1998$Var1 <- NULL
df1998$Var2 <- NULL
df1998$id <- NULL
df1999$Var1 <- NULL
df1999$Var2 <- NULL
df1999$id <- NULL
df2000$Var1 <- NULL
df2000$Var2 <- NULL
df2000$id <- NULL
df2001$id <- NULL
df2002$id <- NULL
df2003$id <- NULL
df2004$id <- NULL
df2005$id <- NULL
df2006$id <- NULL
df2007$id <- NULL
df2008$id <- NULL
df2009$id <- NULL
df2010$id <- NULL

#Summing months to years
df1998$yr98 <- rowSums(subset(df1998, select=3:14))
df1999$yr99 <- rowSums(subset(df1999, select=3:14))
df2000$yr00 <- rowSums(subset(df2000, select=3:14))
df2001$yr01 <- rowSums(subset(df2001, select=3:14))
df2002$yr02 <- rowSums(subset(df2002, select=3:14))
df2003$yr03 <- rowSums(subset(df2003, select=3:14))
df2004$yr04 <- rowSums(subset(df2004, select=3:14))
df2005$yr05 <- rowSums(subset(df2005, select=3:14))
df2006$yr06 <- rowSums(subset(df2006, select=3:14))
df2007$yr07 <- rowSums(subset(df2007, select=3:14))
df2008$yr08 <- rowSums(subset(df2008, select=3:14))
df2009$yr09 <- rowSums(subset(df2009, select=3:14))
df2010$yr10 <- rowSums(subset(df2010, select=3:14))

#Removing excess columns 
yearly1998 <- df1998[c(1, 2, 15)]
yearly1999 <- df1999[c(1, 2, 15)]
yearly2000 <- df2000[c(1, 2, 15)]
yearly2001 <- df2001[c(1, 2, 15)]
yearly2002 <- df2002[c(1, 2, 15)]
yearly2003 <- df2003[c(1, 2, 15)]
yearly2004 <- df2004[c(1, 2, 15)]
yearly2005 <- df2005[c(1, 2, 15)]
yearly2006 <- df2006[c(1, 2, 15)]
yearly2007 <- df2007[c(1, 2, 15)]
yearly2008 <- df2008[c(1, 2, 15)]
yearly2009 <- df2009[c(1, 2, 15)]
yearly2010 <- df2010[c(1, 2, 15)]

#Summing them all in one dataframe. Should have done it with a loop...
All_yrs <- merge(yearly1998, yearly1999, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2000, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2001, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2002, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2003, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2004, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2005, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2006, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2007, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2008, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2009, by=c("latitude", "longitude"))
All_yrs <- merge(All_yrs, yearly2010, by=c("latitude", "longitude"))
#Adding a column with average rainfall to genereate long term average rainfall
All_yrs$avg_rf <- rowMeans(subset(All_yrs, select=3:15))

#Selecting relevant years and reformatting as relative time-variables

#DHS 2000 Note that I create three colums with missing observations-this is since we do not have 
# rainfall data 3-5 yrs back, but we need to have the same amount of columns.
All_yrs_2000 <- All_yrs[c(1:5)]
All_yrs_2000$t_5 <-  NA
All_yrs_2000$t_4 <-  NA
All_yrs_2000$t_3 <-  NA
names(All_yrs_2000)[names(All_yrs_2000)=="yr98"] <- "t_2"
names(All_yrs_2000)[names(All_yrs_2000)=="yr99"] <- "t_1"
names(All_yrs_2000)[names(All_yrs_2000)=="yr00"] <- "t"

#DHS 2005
All_yrs_2005 <- All_yrs[c(1:2, 5:10)]
names(All_yrs_2005)[names(All_yrs_2005)=="yr00"] <- "t_5"
names(All_yrs_2005)[names(All_yrs_2005)=="yr01"] <- "t_4"
names(All_yrs_2005)[names(All_yrs_2005)=="yr02"] <- "t_3"
names(All_yrs_2005)[names(All_yrs_2005)=="yr03"] <- "t_2"
names(All_yrs_2005)[names(All_yrs_2005)=="yr04"] <- "t_1"
names(All_yrs_2005)[names(All_yrs_2005)=="yr05"] <- "t"

#DHS 2011
All_yrs_2010 <- All_yrs[c(1:2, 10:15)]
names(All_yrs_2010)[names(All_yrs_2010)=="yr05"] <- "t_5"
names(All_yrs_2010)[names(All_yrs_2010)=="yr06"] <- "t_4"
names(All_yrs_2010)[names(All_yrs_2010)=="yr07"] <- "t_3"
names(All_yrs_2010)[names(All_yrs_2010)=="yr08"] <- "t_2"
names(All_yrs_2010)[names(All_yrs_2010)=="yr09"] <- "t_1"
names(All_yrs_2010)[names(All_yrs_2010)=="yr10"] <- "t"

# Loading the spatial join files
Join2000 <- read.csv("Sp_join_2000.csv")
Join2005 <- read.csv("Sp_join_2005.csv")
Join2011 <- read.csv("Sp_join_2011.csv")  

# Merging them to the three sets of relative years rainfall
Merged_DHS2000 <- merge(Join2000, All_yrs_2000, by=c("latitude", "longitude"))
Merged_DHS2005 <- merge(Join2005, All_yrs_2005, by=c("latitude", "longitude"))
Merged_DHS2011 <- merge(Join2011, All_yrs_2010, by=c("latitude", "longitude"))

### Merging with DHS ###

#Loading the DHS sets
library(readstata13)
DHS2000 <- readstata13::read.dta13("2000_kr.dta")
DHS2005 <- readstata13::read.dta13("2005_kr.dta")
DHS2011 <- readstata13::read.dta13("2011_kr.dta")

#First, rename the cluster id- column so they can be merged on it
names(Merged_DHS2000)[names(Merged_DHS2000)=="DHSCLUST"] <- "v001"
names(Merged_DHS2005)[names(Merged_DHS2005)=="DHSCLUST"] <- "v001"
names(Merged_DHS2011)[names(Merged_DHS2011)=="DHSCLUST"] <- "v001"

#Then merge them by the cluster id
MERGE_2000 <- merge(DHS2000, Merged_DHS2000, by=c("v001"))
MERGE_2005 <- merge(DHS2005, Merged_DHS2005, by=c("v001"))
MERGE_2011 <- merge(DHS2011, Merged_DHS2011, by=c("v001"))

#Appending it all together
twosurveys <- rbind(MERGE_2000, MERGE_2005)
COMPLETE_SET <- rbind(twosurveys, MERGE_2011)


write.dta(COMPLETE_SET, "COMPLETE_SET.dta")
