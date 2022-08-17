library(readxl)
library(maps)
library(dplyr)
#Reading misdemeanour and felony datasets
misd <- read_excel("mis.xlsx") 
View(misd)

misd = misd[,c(1,3,4,5,9,10,11,12,13,16,17)]

feld <- read_excel("fel.xlsx")
View(feld)



feld = feld[,c(1,3,4,5,9,10,11,12,13,16,17)]



View(misdfel)
dim(misdfel)
#joining both using rbind
misdfel <- rbind(misd, feld)

exp = misdfel



harris <- read.csv("15-20-all-Census-2.csv")


misfel_no_miss <- misdfel[!is.na(misdfel$Latitude_Defendant_Address),]
 
View(misfel_no_miss)


no_201 <- misfel_no_miss %>% filter(misfel_no_miss$CensusCountyFips_Defendant_Address != 201)
View(no_201)
library(maps)

no_201$State <- maps::map.where(database = "state", no_201$Longitude_Defendant_Address, no_201$Latitude_Defendant_Address)
View(no_201)


#keeping only , state = texas
library(dplyr)
no_201 <- no_201 %>% filter(State == 'texas')


no_201$CensusTract_Defendant_Address <- as.numeric(no_201$CensusTract_Defendant_Address)
no_201$CensusCountyFips_Defendant_Address <- as.numeric(no_201$CensusCountyFips_Defendant_Address)


#Getting the geoid by preprocessing and concatenating columns
no_201$CensusTract_Defendant_Address <- sprintf("%07.2f",no_201$CensusTract_Defendant_Address)
no_201$CensusTract_Defendant_Address <- gsub("\\.","",no_201$CensusTract_Defendant_Address)

no_201$CensusCountyFips_Defendant_Address <- str_pad(no_201$CensusCountyFips_Defendant_Address, 3, side = "left", pad = 0)




View(no_201)

library(stringi)
library(stringr)
#Joing the modified census tract and block group columns to obtain geoid
no_201$key <- str_c(no_201$CensusCountyFips_Defendant_Address,'', no_201$CensusTract_Defendant_Address, '', no_201$CensusBlockGroup_Defendant_Address) 

#2015

no_201_15 <- no_201 %>% filter(CaseFilingYear == 2015)
View(no_201_15)


View(y2015_ac)

y2015_ac$key <- str_sub(y2015_ac$GEOID, 10, 19)


no_201_15_all <- merge(no_201_15, y2015_ac, on = "key", all.x = T)

View(no_201_15_all)

write.csv(no_201_15_all, "C:\\Users\\Sourav Ray\\Other Counties\\2015_no_201.csv", row.names = FALSE)

#2016

no_201_16 <- no_201 %>% filter(CaseFilingYear == 2016)
View(no_201_15)


View(y2015_ac)

y2016_ac$key <- str_sub(y2016_ac$GEOID, 10, 19)


no_201_16_all <- merge(no_201_16, y2016_ac, on = "key", all.x = T)

View(no_201_15_all)

write.csv(no_201_16_all, "C:\\Users\\Sourav Ray\\Other Counties\\2016_no_201.csv", row.names = FALSE)

#2017

no_201_17 <- no_201 %>% filter(CaseFilingYear == 2017)
View(no_201_17)


View(y2017_ac)

y2017_ac$key <- str_sub(y2017_ac$GEOID, 10, 19)


no_201_17_all <- merge(no_201_17, y2017_ac, on = "key", all.x = T)

View(no_201_17_all)

write.csv(no_201_17_all, "C:\\Users\\Sourav Ray\\Other Counties\\2017_no_201.csv", row.names = FALSE)

#2018

no_201_18 <- no_201 %>% filter(CaseFilingYear == 2018)
View(no_201_18)


View(y2017_ac)

y2018_ac$key <- str_sub(y2018_ac$GEOID, 10, 19)


no_201_18_all <- merge(no_201_18, y2018_ac, on = "key", all.x = T)

View(no_201_18_all)

write.csv(no_201_18_all, "C:\\Users\\Sourav Ray\\Other Counties\\2018_no_201.csv", row.names = FALSE)


#2019

no_201_19 <- no_201 %>% filter(CaseFilingYear == 2019)
View(no_201_19)


View(y2019_ac)

y2019_ac$key <- str_sub(y2019_ac$GEOID, 10, 19)


no_201_19_all <- merge(no_201_19, y2019_ac, on = "key", all.x = T)

View(no_201_19_all)

write.csv(no_201_19_all, "C:\\Users\\Sourav Ray\\Other Counties\\2019_no_201.csv", row.names = FALSE)

#2020

#Search for variables in decennial census
vars <- load_variables(2020, "pl")

View(vars)
census_api_key("apikey", install = TRUE)
library(dplyr)




no_201_all_cnt <- rbind(no_201_15_all, no_201_16_all, no_201_17_all, no_201_18_all, no_201_19_all)

req <- na.omit(no_201_all_cnt)
View(req)

write.csv(req, "C:\\Users\\Sourav Ray\\Other Counties\\no_201_all.csv", row.names = FALSE)


harris <- read.csv("15-20-all-Census-2.csv")
harris$GEOID <- "US48201"
harris$Key <- paste(harris$GEOID, harris$key, sep = "")
harris = harris[,-c(1,25,26,27)]


req <- read.csv("no_201_all.csv")
View(req)
library(stringr)
req$Key <- str_sub(req$GEOID, - 14, - 1)   
#req = req[,-c(9:14)]
req = req[,-1]
req$Total_Insurance = req$LT19.34_1TI + req$LT35.64_1TI + req$MT65_1TI
req$Percent_Insurance = req$Total_Insurance / req$Total_Population

req = req[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,22)]
main_all <- rbind(harris, req)

options(scipen = 999)
library(dplyr)

main_all2 <- dplyr::left_join(main_all, misdfel2, by = "CaseID")

main_all2 = main_all2[,-c(28, 32:40)]

write.csv(misdfel, "C:\\Users\\Sourav Ray\\misdfel2.csv", row.names = FALSE)

misdfel2 <- read.csv("misdfel2.csv")

write.csv(main_all2, "C:\\Users\\Sourav Ray\\MAIN_ALL.csv", row.names = FALSE)
View(main_all2)

colnames(main_all2)[1] <- "CaseFilingYear"
main

library(ggplot2)
pu_data <- main_all2 %>% 
  group_by(CaseFilingYear) %>%
  summarise_at(vars(Percent_Unemp), list(Percent_Unemployed = mean)) 
  
pu_data$Percent_Unemployed = pu_data$Percent_Unemployed * 100

pu_fig <- ggplot(data = pu_data, aes(x = CaseFilingYear, y = Percent_Unemployed)) + 
  geom_col(fill = "steelblue") + theme(a)

pu_fig

#code for zip code
library("ggmap")




 pu_data$Percent_Unemployed = pu_data$Percent_Unemployed * 100

gender_groupby <- main_all2 %>%
  group_by(CaseFilingYear.x) %>%
  count(Gender)


View(gender_groupby)



hmil_all <- rbind(harris, req)

View(harris)
harris$Percent_HSMF <- harris$HSMF / harris$Total_Population

View(req)

req$Percent_HSMF <- req$HSMF / req$Total_Population


library(dplyr)

harris %>%
  group_by(CaseFilingYear) %>%
  summarise_at(vars(Percent_HSMF), list(name = mean))

View(harris)

req %>%
  group_by(CaseFilingYear) %>%
  summarise_at(vars(Percent_HSMF), list(Education_Attainment = mean))

harris %>%
  group_by(CaseFilingYear) %>%
  summarise_at(vars(Percent_Insurance), list(Percent_Insured = mean))


req %>%
  group_by(CaseFilingYear) %>%
  summarise_at(vars(Percent_Insurance), list(Percent_Insured = mean))

harris %>%
  group_by(CaseFilingYear) %>%
  summarise_at(vars(Percent_MT50k), list(Percent_MT50k = mean))

req %>%
  group_by(CaseFilingYear) %>%
  summarise_at(vars(Percent_MT50k), list(Percent_MT50k = mean))

all <- req[,-c(9:14)]

req$Total_Insurance <- req$LT19.34_1TI + req$LT35.64_1TI + req$MT65_1TI
req$Percent_Insurance <- req$Total_Insurance / req$Total_Population

View(all)


all_counties <- rbind(abc,harris)

str(harris)
str(all)

colnames(all)[20] <- "LT19.34_1TI"
colnames(all)[21] <- "LT35.64_1TI"

harris = harris[,-1]
all = all[,-1]

write.csv(all, "C:\\Users\\Sourav Ray\\counties_all_no_201.csv", row.names = FALSE)

write.csv(all_counties, "C:\\Users\\Sourav Ray\\ALL_Counties.csv", row.names = FALSE)

abc <- read.csv("counties_all_no_201.csv")

str(abc)
View(all_counties)

library(readxl)
extra_fiel <- read_excel("main-extra-fields.xlsx")

View(extra_fiel)
extra_all_counties <- merge(all_counties, extra_fiel, on = "CaseID", all.x = T)
#filtering out only after 2018
#misfel201567 <- misdfel %>% filter(CaseFilingYear >= 2015 & CaseFilingYear < 2018)
#View(misfel201567)


#misfel <- misfel201918[,c(1,3,10,22,27:31,34,35)]#Choosing necessary columns
#View(misfel)

#Creating 2 datasets : for null and non null values
misfel_miss <- misdfel[is.na(misdfel$Latitude_Defendant_Address),] #missing values
dim(misfel_miss)
misfel_no_miss <- misdfel[!is.na(misdfel$Latitude_Defendant_Address),] #non missing values

View(misfel_miss)
View(misfel_no_miss)

#Working with no_miss data

#Getting state for rows which have longitude and latitude (non miss points)
library(maps)
misfel_no_miss$State <- maps::map.where(database = "state", misfel_no_miss$Longitude_Defendant_Address, misfel_no_miss$Latitude_Defendant_Address)
View(misfel_no_miss)


#keeping only county = 201, state = texas
library(dplyr)
misfel_no_miss1 <- misfel_no_miss %>% filter(State == 'texas')
View(misfel_no_miss1)




library(stringr)
library(stringi)

#convert tract to numeric 
misfel_no_miss1$CensusTract_Defendant_Address <- as.numeric(misfel_no_miss1$CensusTract_Defendant_Address)

#Getting the geoid by preprocessing and concatenating columns
misfel_no_miss1$CensusTract_Defendant_Address <- sprintf("%07.2f",misfel_no_miss1$CensusTract_Defendant_Address)
misfel_no_miss1$CensusTract_Defendant_Address <- gsub("\\.","",misfel_no_miss1$CensusTract_Defendant_Address)

View(misfel_no_miss1)
#Joing the modified census tract and block group columns to obtain geoid
misfel_no_miss1$key <- str_c(misfel_no_miss1$CensusTract_Defendant_Address, '', misfel_no_miss1$CensusBlockGroup_Defendant_Address) 
View(misfel_no_miss1)

#Checking for those which do not have census tract information leading to no keys

misfel_no_cen <- misfel_no_miss1[is.na(misfel_no_miss1$key),]
misfel_no_cen <- misfel_no_cen[,-c(8:12)]
View(misfel_no_cen)

misfel_cen <- misfel_no_miss1[!is.na(misfel_no_miss1$key),]
misfel_cen <- misfel_cen[,-c(8:12)]
View(misfel_cen)

#Writing this to csv to get geoid from python code
write.csv(misfel_no_cen, "C:\\Users\\Sourav Ray\\no_key_data_157.csv", row.names = FALSE)

bind1 <- read.csv("no_key_added_157.csv")
misfel_no_cen <- cbind(bind1, misfel_no_cen)
View(misfel_no_cen)

#modifying the geoid obtained from python code
misfel_no_cen$key <- stri_sub(misfel_no_cen$geoid,6,12)
misfel_no_cen = misfel_no_cen[,-c(1:6)]


misfel_main <- rbind(misfel_cen, misfel_no_cen)
View(misfel_main)



#Creating subsets for each year
mm2015 <- misfel_main %>% filter(CaseFilingYear == 2015)
mm2016 <- misfel_main %>% filter(CaseFilingYear == 2016)
mm2017 <- misfel_main %>% filter(CaseFilingYear == 2017)
mm2018 <- misfel_main %>% filter(CaseFilingYear == 2018)
mm2019 <- misfel_main %>% filter(CaseFilingYear == 2019)
mm2020 <- misfel_main %>% filter(CaseFilingYear == 2020)
View(mm2015)


library(totalcensus)

download_census("acs5", 2020, states = "TX")
set_path_to_census("C:/cen-data-counties")


y2015_ac <- read_acs5year(
  year = 2015,
  states = "TX",
  table_contents = c("B01003_001","B23025_005", "B19001_001", "B19001_002","B19001_003","B19001_004","B19001_005","B19001_006","B19001_007","B19001_008","B19001_009"
                     ,"B19001_010","B15002_011", "B15002_028", "B23025_005", "B01003_001", "B27010_003", "B27010_019", "B27010_035", "B27010_052"),
  summary_level = "block group"
)

View(block_groups)

y2015_ac = block_groups
View(y2015_ac)
View(y2016_ac)
View(y2017_ac)




#Reading data from 2015 census, manipulating geoid and removing some rows

ult_2015 <- read.csv("2015_cen.csv")
View(ult_2015)
ult_2015$key <- stri_sub(ult_2015$GEO_ID, - 7)
#ult_2018 = ult_2018[,-c(5,6)]
View(ult_2015)

#Merging mm2018 with ult_2015
mm15 <- merge(mm2015, ult_2015, on = "key", all.x = T)
View(mm15)

mm15 = mm15[,-9]

dim(na.omit(mm15))
View(mm15)

#Imputing with mean values
for(i in 1:ncol(mm15)) {
  mm15[,i][is.na(mm15[,i])] <- mean(mm15[,i], na.rm = T)
}



#Same process for year 2016

ult_2016 <- read.csv("2016_cen.csv")
View(ult_2016)
ult_2016$key <- stri_sub(ult_2016$GEO_ID,-7)

View(ult_2016)

#Merging mm2019 with ult_2019
mm16 <- merge(mm2016, ult_2016, on = "key", all.x = T)
View(mm16)


mm16 = mm16[,-9]

#Imputing with mean values
for(i in 1:ncol(mm16)) {
  mm16[,i][is.na(mm16[,i])] <- mean(mm16[,i], na.rm = T)
}

View(mm16)


#2017

ult_2017 <- read.csv("2017_cen.csv")
View(ult_2017)
ult_2017$key <- stri_sub(ult_2017$GEO_ID,-7)
#ult_2020 = ult_2020[,-c(5,6)]
View(ult_2017)

ult_2017 = ult_2017[,-1]

mm17 <- merge(mm2017, ult_2017, on = "key", all.x = T)
View(mm17)



#Imputing with mean values
for(i in 1:ncol(mm17)) {
  mm17[,i][is.na(mm17[,i])] <- mean(mm17[,i], na.rm = T)
}

View(mm17)

#combining the 3 using rbind
ult_mm <- rbind(mm15, mm16, mm17)

#cleaning to remove entries with homeless or unkown in the ult_mm dataset

ult_mm <- ult_mm[-grep("HOMELES", ult_mm$defendantAddressAtCaseFiling),]

#Unknown filtering
ult_mm <- ult_mm[-grep("UNKNOW", ult_mm$defendantAddressAtCaseFiling),]

View(ult_mm)

# Working with miss data

View(misfel_miss)

misfel_miss = misfel_miss[,-c(8:11)]
View(misfel_miss)

#misfel_miss[misfel_miss == ""] <- NA
misfel_miss = misfel_miss[,-c(6,7)]

#misfel_miss = na.omit(misfel_miss)

misfel_miss = misfel_miss[!is.na(misfel_miss$defendantAddressAtCaseFiling),]

dim(misfel_miss)
#Removing the homeless and unknown in the missing dataset
misfel_miss <- misfel_miss[-grep("HOMELES", misfel_miss$defendantAddressAtCaseFiling),]

#Unknown filtering
misfel_miss <- misfel_miss[-grep("UNKNOW", misfel_miss$defendantAddressAtCaseFiling),]
View(misfel_miss)
dim(misfel_miss)
#Geocoding these addresses
library(ggmap)
ggapi <- "your api"

register_google(key = ggapi)

View(misfel_miss)
dim(misfel_miss)
add_ltg201567 <- geocode(location = misfel_miss$defendantAddressAtCaseFiling, output = "more", source = "google")
dim(add_ltg201567)
View(add_ltg201567)

write.csv(add_ltg201567, "C:\\Users\\Sourav Ray\\misfel_missltg2.csv", row.names = FALSE)

misfel_miss_ltg <- read.csv("misfel_miss_ltg2.csv")
View(misfel_miss_ltg)
dim(misfel_miss_ltg)

mno <- na.omit(misfel_miss_ltg)
dim(mno)

misfel_miss <- cbind(misfel_miss, misfel_miss_ltg)
View(misfel_miss)

#misfel_miss = misfel_miss[,-c(8:15, 19, 20)]
View(misfel_miss)

#Filter by state and county
misfel_miss <- misfel_miss %>% filter(state == 48 & county == 201)
misfel_miss = misfel_miss[,-c(6,8,9)]


misfel_miss$key <- stri_sub(misfel_miss$geoid,6,12)
View(misfel_miss)
#Filtering and joining as per year

m2015 <- misfel_miss %>% filter(CaseFilingYear == 2015)
m2016 <- misfel_miss %>% filter(CaseFilingYear == 2016)
m2017 <- misfel_miss %>% filter(CaseFilingYear == 2017)

#Year 2015

m2015 <- merge(m2015, ult_2015, on = "key", all.x = T)
View(m2015)

m2015 = m2015[,-c(7,10)]

for(i in 1:ncol(m2015)) {
  m2015[,i][is.na(m2015[,i])] <- mean(m2015[,i], na.rm = T)
}

View(m2015)
#m2018 = m2018[,c(1,6,7,8,9,10,11,12,2,3,4,5)]

#2016
m2016 <- merge(m2016,ult_2016, on = "key", all.x = T)
View(m2016)

m2016 = m2016[,-c(7,10)]

for(i in 1:ncol(m2016)) {
  m2016[,i][is.na(m2016[,i])] <- mean(m2016[,i], na.rm = T)
}

View(m2016)

#2017

m2017 <- merge(m2017, ult_2017, on = "key", all.x = T)
View(m2017)


m2017 = m2017[,-7]

for(i in 1:ncol(m2017)) {
  m2017[,i][is.na(m2017[,i])] <- mean(m2017[,i], na.rm = T)
}

View(m2017)




m_ult <- rbind(m2018, m2019, m2020)
View(m_ult)
colnames(m_ult)[7] <- "Longitude_Defendant_Address"
colnames(m_ult)[8] <- "Latitude_Defendant_Address"

View(ult_mm)

m_ult = m_ult[,c(1,2,3,4,5,6,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]

census_all <- rbind(ult_mm, m_ult)
View(census_all)

census_all <- census_all[order(census_all$CaseFilingYear),] #done for 2018-2020 

write.csv(census_all, "C:\\Users\\Sourav Ray\\18-20-Mod-Census", row.names = FALSE)

mod_census1820 <- read.csv("18-20-Mod-Census.csv")

#for 2015 - 2017
m_ult2 <- rbind(m2015,m2016,m2017)
View(m_ult2)

m_ult2 = m_ult2[,c(1,2,3,4,5,6,8,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]

colnames(m_ult2)[7] <- "Longitude_Defendant_Address"
colnames(m_ult2)[8] <- "Latitude_Defendant_Address"

census_all2 <- rbind(ult_mm, m_ult2)
View(census_all2)

ult_census <- rbind(mod_census1820, census_all2)

ult_census <- ult_census[order(ult_census$CaseFilingYear),]

View(ult_census)

write.csv(ult_census, "C:\\Users\\Sourav Ray\\15-20-all-Census.csv", row.names = FALSE)



add_1_cen <- read.csv("15-20-all-Census.csv")



View(add_1_cen)
library(stringi)

add_1_cen$block_group <- stri_sub(add_1_cen$key,-1)
add_1_cen$census_tract1 <- stri_sub(add_1_cen$key, 1, 4)
add_1_cen$census_tract2 <- stri_sub(add_1_cen$key, 5, 6)  
add_1_cen$census_tract <- paste(add_1_cen$census_tract1, add_1_cen$census_tract2, sep = ".")
add_1_cen = add_1_cen[,-c(26,27)]


write.csv(add_1_cen, "C:\\Users\\Sourav Ray\\15-20-all-Census-2.csv", row.names = FALSE)


#Reading file for merging with new variables

library("readxl")
mergedif <- read_excel("mergedif.xlsx")
View(mergedif)

library(tidyverse)

#Getting Case filing year to sort it

mergedif$CaseFilingYear <- format(mergedif$CaseFilingDT, format = "%Y")



View(mergedif)
mi <- mergedif %>% filter(CaseFilingYear >= 2018)

View(mi)

#Joining by caseid

merged_f <- merge(census_all, mi, by = "CaseID", all.x = T)
View(merged_f)

merged_f = merged_f[,-c(13,14,35)]

write.csv(merged_f, "C:\\Users\\Sourav Ray\\4-census-mod.csv", row.names = FALSE)
