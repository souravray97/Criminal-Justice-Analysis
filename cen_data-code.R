#cen_data <- read.csv("add_misd.csv")
#ccd <- read.csv("ccd-new.csv")
#case_match = ccd[,c(4,2)]
#View(case_match)

misd <- read.delim("misd.txt") 
feld <- read.delim("fel.txt")
View(misdfel)
misdfel <- rbind(misd, feld)

misdfel_spn_id = misdfel[,c(3,10,22)]
View(misdfel_spn_id)
misdfel_add_caseid <- misdfel[,c(3,27)]


misd_check <- misdfel[,c(3,36,28,29)]
misdfel$DefendantAddress
View(misd_check)
View(misdfel_geocode)

homless_sub <- misdfel[grep("HOMELESS", misdfel$defendantAddressAtCaseFiling), ]
View(homless_sub)

#misdfel <- misdfel[!duplicated(misdfel$CaseID),]

#View(ccd)
grouped1 <- aggregate(misdfel, by = list(misdfel$CaseFilingYear, misdfel$Latitude_Defendant_Address), FUN = count)
grouped1


misfel1 <- misdfel[,c(1,3,27:31,34,35)]
View(misfel1)


library(dplyr)
grouped1 <- aggregate(misfel1, by = list(misfel1$CaseFilingYear, misfel1$Latitude_Defendant_Address), FUN = count)
grouped1

misfel1 <- na.omit(misfel1)

aggregate(misfel1$Latitude_Defendant_Address, by = list(misfel1$CaseFilingYear), FUN = length)





library(handymaps)
misfel1$State <- maps::map.where(database = "state", misfel1$Longitude_Defendant_Address, misfel1$Latitude_Defendant_Address)
View(misfel1)

dim(misfel1)

##code for 2020

library(dplyr)
misfel2020 <- misfel1 %>% filter(State == 'texas' & CensusCountyFips_Defendant_Address == 201 & CaseFilingYear == 2018)
View(misfel2020)

str(misfel2020)

# below code was used for census tract formatting
misfel2020$CensusTract_Defendant_Address <- sprintf("%07.2f",misfel2020$CensusTract_Defendant_Address)
misfel2020$CensusTract_Defendant_Address <- gsub("\\.","",misfel2020$CensusTract_Defendant_Address)

library(stringr)

misfel2020$GEO_ID <- str_c(misfel2020$CensusTract_Defendant_Address, '', misfel2020$CensusBlockGroup_Defendant_Address) 
View(misfel2020)

colnames(misfel2020)[11] <- "key"

pop20_c <- read.csv("2018pop.csv")
pop20_c = pop20_c[-1,]
pop20_c = pop20_c[,-4]
View(pop20_c)
minc20_c <- read.csv("2018inc.csv")
View(minc20_c)
minc20_c <- minc20_c[,-c(3:34,36)]
minc20_c = minc20_c[-1,]
ed20_c <- read.csv("2018edu.csv")
ed20_c = ed20_c[,-c(3:31,36)]
ed20_c = ed20_c[,-c(4)]
ed20_c = ed20_c[-1,]


emp20_c <- read.csv("2020emp.csv")
emp20_c = emp20_c[,-c(2:14,16)]
emp20_c = emp20_c[-1,]
library(stringi)
emp20_c$GEO_ID <- stri_sub(emp20_c$GEO_ID, - 7)

colnames(emp20_c)[1] <- "Employment_Status_Estimate"
colnames(emp20_c)[2] <- "key"

View(emp18_c)

emp19_c <- read.csv("2019emp.csv")
emp19_c = emp19_c[,-c(2:14,16)]
emp19_c = emp19_c[-1,]
library(stringi)
emp19_c$GEO_ID <- stri_sub(emp19_c$GEO_ID, - 7)

colnames(emp19_c)[1] <- "Employment_Status_Estimate"
colnames(emp19_c)[2] <- "key"

emp18_c <- read.csv("2018emp.csv")
emp18_c = emp18_c[,-c(2:14,16)]
emp18_c = emp18_c[-1,]
library(stringi)
emp18_c$GEO_ID <- stri_sub(emp18_c$GEO_ID, - 7)

colnames(emp18_c)[1] <- "Employment_Status_Estimate"
colnames(emp18_c)[2] <- "key"



med <- merge(minc20_c, ed20_c, by = "GEO_ID", all.x = T)
main <- merge(pop20_c, med, by = "GEO_ID", all.x = T)

View(main)
library(stringi)
main$GEO_ID <- stri_sub(main$GEO_ID,-7)

colnames(main)[1] <- "key"
colnames(main)[2] <- "2018_Population_Estimate"
colnames(main)[3] <- "2018_Population_MOE"
colnames(main)[4] <- "2018_Median_Income_Estimate"
colnames(main)[5] <- "2018_Median_Income_MOE"
colnames(main)[6] <- "2018_Education_Estimate"
colnames(main)[7] <- "2018_Education_MOE"










data_2020 <- merge(misfel2020, main, by = "key", all.x = T)
View(data_2020)

dim(data_2020)
write.csv(data_2020, "C:\\Users\\Sourav Ray\\2018_populated", row.names = FALSE)

y2018 <- read.csv("year_2018.csv")
View(y2018)

colnames(y2018)[12] <- "Population_Estimate"
colnames(y2018)[13] <- "Population_MOE"
colnames(y2018)[14] <- "Medin_Income_Estimate"
colnames(y2018)[15] <- "Median_Income_MOE"
colnames(y2018)[16] <- "Education_Estimate"
colnames(y2018)[17] <- "Education_MOE"

y2019 <- read.csv("year_2019.csv")
View(y2019)

colnames(y2019)[12] <- "Population_Estimate"
colnames(y2019)[13] <- "Population_MOE"
colnames(y2019)[14] <- "Medin_Income_Estimate"
colnames(y2019)[15] <- "Median_Income_MOE"
colnames(y2019)[16] <- "Education_Estimate"
colnames(y2019)[17] <- "Education_MOE"

y2020 <- read.csv("year_2020.csv")

colnames(y2020)[12] <- "Population_Estimate"
colnames(y2020)[13] <- "Population_MOE"
colnames(y2020)[14] <- "Medin_Income_Estimate"
colnames(y2020)[15] <- "Median_Income_MOE"
colnames(y2020)[16] <- "Education_Estimate"
colnames(y2020)[17] <- "Education_MOE"
View(y2020)
populated_df <- rbind(y2018, y2019, y2020)


View(populated)
colnames(populated_df)[14] <- "Median_Income_Estimate"


final_main <- merge(misdfel, populated_df, by = "CaseID", all.x = T)

write.csv(final_main, "ccd_updated", row.names = FALSE)

write.csv(populated_df, "populated_3_yrs", row.names = FALSE)

populated <- read.csv("populated.csv")
View(populated)

populated2020 <- populated %>% filter(CaseFilingYear == 2020)
populated2020 <- merge(populated2020, emp20_c, by = "key", all.x = T)


populated2019 <- populated %>% filter(CaseFilingYear == 2019)
populated2019 <- merge(populated2019, emp19_c, by = "key", all.x = T)

populated2018 <- populated %>% filter(CaseFilingYear == 2018)
populated2018 <- merge(populated2018, emp18_c, by = "key", all.x = T)

variables4 <- rbind(populated2018, populated2019, populated2020)
View(variables4)


variables4 = variables4[,-c(13,15,17)]

write.csv(variables4, "C:\\Users\\Sourav Ray\\4-census-variables.csv", row.names = FALSE)



aggregate(final$Latitude_Defendant_Address, by = list(final$CaseFilingYear), FUN = length)

View(misdfel_exp1) 



#### This part of the code was used for analyzing missing data


library(dplyr)
misdfel_exp = misdfel[,c(1,3,27,28,29)]
View(misdfel_exp)
misdfel_exp1 <- misdfel_exp %>% filter(CaseFilingYear >= 2018)

#Other years data use for 2015-2017


#misdfel_exp2 <- misdfel_exp %>% filter(CaseFilingYear == 2017)
#misdfel_exp3 <- misdfel_exp %>% filter(CaseFilingYear == 2016)
View(misdfel_exp1)
a <- misdfel_exp1[is.na(misdfel_exp1$Latitude_Defendant_Address),]
View(a)
View(a_new)

a_new <- a
a_new[a_new == ""] <- NA

geo_code_data = a_new[!is.na(a_new$defendantAddressAtCaseFiling),]

View(geo_code_data)
dim(geo_code_data)
#Homeless filtering
geo_code_data <- geo_code_data[-grep("HOMELES", geo_code_data$defendantAddressAtCaseFiling),]

#Unknown filtering
geo_code_data <- geo_code_data[-grep("UNKNOW", geo_code_data$defendantAddressAtCaseFiling),]


caseid <- geo_code_data[,c(1,2)]
View(caseid)
View(geo_code_data)
library(ggmap)

mod_geo_code_data = geo_code_data[,c(1,2,3)]
View(mod_geo_code_data)

dim(mod_geo_code_data)

add_ltg201918 <- geocode(location = mod_geo_code_data$defendantAddressAtCaseFiling, output = "more", source = "google")
View(add_ltg201918)

dim(add_ltg201918)
View(add_ltg201918)


write.csv(add_ltg201918, "C:\\Users\\Sourav Ray\\updated-missing.csv", row.names = FALSE)

b <- read.csv("Adress_201918.csv")


View(b)
options(scipen = 999)
c <- cbind(mod_geo_code_data, b)
View(c)

c <- na.omit(c)
View(f)
c$gid <- stri_sub(c$geoid,6,12)
library(dplyr)
View(c)
c2018 <- c %>% filter(state == 48 &  CaseFilingYear == 2018 & county == 201)
c2019 <- c %>% filter(state == 48 &  CaseFilingYear == 2019 & county == 201)
c2020 <- c %>% filter(state == 48 &  CaseFilingYear == 2020 & county == 201)
View(c2018)


f <- read.csv("2018ultimate.csv")
View(f)
y2019 <- read.csv("2019ultimate.csv")
y2020 <- read.csv("2020ultimate.csv")
f$gid <- stri_sub(f$id,-7)
y2019$gid <- stri_sub(y2019$id,-7)
y2020$gid <- stri_sub(y2020$GEO_ID,-7)
View(y2020)

f = f[,c(1,2,3,4,7)]
y2019f = y2019[,c(1,2,3,4,7)]
y2020f = y2020[,c(1,2,3,4,7)]

View(f)
View(y2020f)
z <- merge(c2018, f, by = "gid", all.x = T)##IMP
View(z)

for(i in 1:ncol(z)) {
  z[,i][is.na(z[,i])] <- mean(z[,i], na.rm = T)
}

View(z)


z = z[,-c(5,6,7,8)]
colnames(z)[1] <- "key"
colnames(z)[5] <- "Latitude_Defendant_Case_Filing_Address"
colnames(z)[6] <- "Longitude_Defendant_Case_Filing_Address"
colnames(z)[7] <- "Population_Estimate"
colnames(z)[8] <- "Median_Income_Estimate"
colnames(z)[9] <- "Employment_Status_Estimate"
colnames(z)[10] <- "Education_Estimate"

z = z[,c(1,2,3,4,5,6,7,8,10,9)]

#populated2018 <- merge(populated2018, emp18_c, by = "key", all.x = T)

z2019 <- merge(c2019, y2019f, by = "gid", all.x = T)##IMP
View(z2019)

for(i in 1:ncol(z2019)) {
  z2019[,i][is.na(z2019[,i])] <- mean(z2019[,i], na.rm = T)
}

View(z2019)

z2019 = z2019[,-c(5,6,7,8)]
colnames(z2019)[1] <- "key"
colnames(z2019)[5] <- "Latitude_Defendant_Case_Filing_Address"
colnames(z2019)[6] <- "Longitude_Defendant_Case_Filing_Address"
colnames(z2019)[7] <- "Population_Estimate"
colnames(z2019)[8] <- "Median_Income_Estimate"
colnames(z2019)[9] <- "Employment_Status_Estimate"
colnames(z2019)[10] <- "Education_Estimate"

z2019 = z2019[,c(1,2,3,4,5,6,7,8,10,9)]


z2020 <- merge(c2020, y2020f, by = "gid", all.x = T)##IMP
View(z2020)

for(i in 1:ncol(z2020)) {
  z2020[,i][is.na(z2020[,i])] <- mean(z2020[,i], na.rm = T)
}

z2020 = z2020[,-c(5,6,7,8)]
colnames(z2020)[1] <- "key"
colnames(z2020)[5] <- "Latitude_Defendant_Case_Filing_Address"
colnames(z2020)[6] <- "Longitude_Defendant_Case_Filing_Address"
colnames(z2020)[7] <- "Population_Estimate"
colnames(z2020)[8] <- "Median_Income_Estimate"
colnames(z2020)[9] <- "Employment_Status_Estimate"
colnames(z2020)[10] <- "Education_Estimate"

z2020 = z2020[,c(1,2,3,4,5,6,7,8,10,9)]
View(z2020)



main_merge = read.csv("4-census-variables.csv")
View(main_merge)
colnames(main_merge)[5] <- "Latitude_Defendant_Case_Filing_Address"
colnames(main_merge)[6] <- "Longitude_Defendant_Case_Filing_Address"

main_merge <- main_merge[,-c(4,7:10)]


#main_merge = merge(main_merge, misdfel_add_caseid, by = "CaseID", all.x = T)

View(main_merge)

main_merge = main_merge[,-c(7:11)]

mm2018 <- main_merge %>% filter(CaseFilingYear == 2018)
View(mm2018)

a = na.omit(mm2020)
dim(mm2020)
dim(a)

for(i in 1:ncol(mm2020)) {
  mm2020[,i][is.na(mm2020[,i])] <- mean(mm2020[,i], na.rm = T)
}
View(mm2020)







main_merge = main_merge[,-6]
main_merge <- na.omit(main_merge)

main_z <- rbind(z, z2019, z2020)
main_mm <- rbind(mm2018, mm2019, mm2020)


View(main_z)
View(main_mm)

main_mm_z <- rbind(main_z, main_mm)
View(main_mm_z)

#main_z <- main_z[,-4]

#merged_main <- merge(main_zmerge, misdfel_add_caseid, by = "CaseID", all.x = T)
#View(merged_main)
#main_zmerge <- rbind(main_z, main_merge)

#main_merge1[main_merge1 == ""] <- NA
#main_merge1 <- na.omit(main_merge1)

library("readxl")
mergedif <- read_excel("mergedif.xlsx")

str(mergedif)
library(tidyverse)

mergedif$CaseFilingYear <- format(mergedif$CaseFilingDT, format = "%Y")



View(mergedif)
mi <- mergedif %>% filter(CaseFilingYear >= 2018)

View(mi)

mi = mi[,-24]

View(mergedif)
xyz <- na.omit(mergedif)
dim(mergedif)
dim(xyz)
length(mergedif$CaseID)
length(unique(mergedif$CaseID))
View(mergedif)

merged_f <- merge(main_mm_z, mi, by = "CaseID", all.x = T)
View(merged_f)

write.csv(merged_f, "C:\\Users\\Sourav Ray\\Main_Merged_3_Years.csv", row.names = FALSE)

mm3y <- read.csv("Main_Merged_3_Years.csv")
View(mm3y)

length(mm3y$CaseID)
length(unique(mm3y$CaseID))

options(scipen = 999)

mm3y_mod <- merge(mm3y, misdfel_spn_id, by = "CaseID", all.x = T)
View(mm3y_mod)

write.csv(mm3y, "C:\\Users\\Sourav Ray\\Main_2_Merged_3_Years.xlsx", row.names = FALSE)


