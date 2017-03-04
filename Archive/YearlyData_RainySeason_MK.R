rm(list = ls())
# set working directory
setwd("/Users/margotkurfess/Desktop/Thesis/Data/Full directory")

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

#Summing the months in the rainy season (We are using March to September)
df1998$rsyr98 <- rowSums(subset(df1998, select=5:11))
df1999$rsyr99 <- rowSums(subset(df1999, select=5:11))
df2000$rsyr00 <- rowSums(subset(df2000, select=5:11))
df2001$rsyr01 <- rowSums(subset(df2001, select=5:11))
df2002$rsyr02 <- rowSums(subset(df2002, select=5:11))
df2003$rsyr03 <- rowSums(subset(df2003, select=5:11))
df2004$rsyr04 <- rowSums(subset(df2004, select=5:11))
df2005$rsyr05 <- rowSums(subset(df2005, select=5:11))
df2006$rsyr06 <- rowSums(subset(df2006, select=5:11))
df2007$rsyr07 <- rowSums(subset(df2007, select=5:11))
df2008$rsyr08 <- rowSums(subset(df2008, select=5:11))
df2009$rsyr09 <- rowSums(subset(df2009, select=5:11))
df2010$rsyr10 <- rowSums(subset(df2010, select=5:11))

#Removing excess columns 
RS_1998 <- df1998[c(1, 2, 15)]
RS_1999 <- df1999[c(1, 2, 15)]
RS_2000 <- df2000[c(1, 2, 15)]
RS_2001 <- df2001[c(1, 2, 15)]
RS_2002 <- df2002[c(1, 2, 15)]
RS_2003 <- df2003[c(1, 2, 15)]
RS_2004 <- df2004[c(1, 2, 15)]
RS_2005 <- df2005[c(1, 2, 15)]
RS_2006 <- df2006[c(1, 2, 15)]
RS_2007 <- df2007[c(1, 2, 15)]
RS_2008 <- df2008[c(1, 2, 15)]
RS_2009 <- df2009[c(1, 2, 15)]
RS_2010 <- df2010[c(1, 2, 15)]

#Summing them all in one dataframe. Should have done it with a loop...
RS_All_yrs <- merge(RS_1998, RS_1999, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2000, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2001, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2002, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2003, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2004, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2005, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2006, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2007, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2008, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2009, by=c("latitude", "longitude"))
RS_All_yrs <- merge(RS_All_yrs, RS_2010, by=c("latitude", "longitude"))
#Adding a column with average rainfall to genereate long term average rainfall
RS_All_yrs$avg_rf <- rowMeans(subset(RS_All_yrs, select=3:15))

#Selecting relevant years and reformatting as relative time-variables

#DHS 2000 Note that I create three colums with missing observations-this is since we do not have 
# rainfall data 3-5 yrs back, but we need to have the same amount of columns.
RS_All_yrs_2000 <- RS_All_yrs[c(1:16)]

#DHS 2005
RS_All_yrs_2005 <- RS_All_yrs[c(1:16)]

#DHS 2011
RS_All_yrs_2010 <- RS_All_yrs[c(1:16)]

# Loading the spatial join files
Join2000 <- read.csv("Sp_join_2000.csv")
Join2005 <- read.csv("Sp_join_2005.csv")
Join2011 <- read.csv("Sp_join_2011.csv")  

# Merging them to the three sets of relative years rainfall
Merged_RS_DHS2000 <- merge(Join2000, RS_All_yrs_2000, by=c("latitude", "longitude"))
Merged_RS_DHS2005 <- merge(Join2005, RS_All_yrs_2005, by=c("latitude", "longitude"))
Merged_RS_DHS2011 <- merge(Join2011, RS_All_yrs_2010, by=c("latitude", "longitude"))

### Merging with DHS ###

#Loading the DHS sets
library(readstata13)
DHS2000 <- readstata13::read.dta13("2000_kr.dta")
DHS2005 <- readstata13::read.dta13("2005_kr.dta")
DHS2011 <- readstata13::read.dta13("2011_kr.dta")

#First, rename the cluster id- column so they can be merged on it
names(Merged_RS_DHS2000)[names(Merged_RS_DHS2000)=="DHSCLUST"] <- "v001"
names(Merged_RS_DHS2005)[names(Merged_RS_DHS2005)=="DHSCLUST"] <- "v001"
names(Merged_RS_DHS2011)[names(Merged_RS_DHS2011)=="DHSCLUST"] <- "v001"

#Then merge them by the cluster id
RS_MERGE_2000 <- merge(DHS2000, Merged_RS_DHS2000, by=c("v001"))
RS_MERGE_2005 <- merge(DHS2005, Merged_RS_DHS2005, by=c("v001"))
RS_MERGE_2011 <- merge(DHS2011, Merged_RS_DHS2011, by=c("v001"))

#Appending it all together
twosurveys <- rbind(RS_MERGE_2000, RS_MERGE_2005)
RS_COMPLETE_SET <- rbind(twosurveys, RS_MERGE_2011)


write.dta(RS_COMPLETE_SET, "RS_COMPLETE_SET.dta")
