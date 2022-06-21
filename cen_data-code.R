#cen_data <- read.csv("add_misd.csv")
#ccd <- read.csv("ccd-new.csv")
#case_match = ccd[,c(4,2)]
#View(case_match)

misd <- read.delim("misd.txt")
feld <- read.delim("fel.txt")

misdfel <- rbind(misd, feld)
View(misdfel)
dim(misdfel)

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

write.csv(final_main, "C:\\Users\\Sourav Ray\\ccd_updated", row.names = FALSE)

write.csv(populated_df, "C:\\Users\\Sourav Ray\\populated_3_yrs", row.names = FALSE)

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

misdfel_exp = misdfel[,c(1,3,36,28,29)]
misdfel_exp1 <- misdfel_exp %>% filter(CaseFilingYear >= 2018)


a <- misdfel_exp1[is.na(misdfel_exp1$Latitude_Defendant_Address),]
View(a_new)

a_new <- a
a_new[a_new == ""] <- NA

geo_code_data = a_new[!is.na(a_new$DefendantAddress),]
View(geo_code_data)

#Homeless filtering
geo_code_data <- geo_code_data[-grep("HOMELES", geo_code_data$DefendantAddress),]

#Unknown filtering
geo_code_data <- geo_code_data[-grep("UNKNOW", geo_code_data$DefendantAddress),]

#AIzaSyC7AVl1bnkdALGQIm7CpgI8Nud4xA8GQgY mapsapi

library(ggmap)

ggapi <- "AIzaSyC7AVl1bnkdALGQIm7CpgI8Nud4xA8GQgY"

register_google(key = ggapi)

tmap <- get_map("Texas", zoom = 1)
ggmap(tmap)

add_ltg <- geocode(location = geo_code_data$DefendantAddress, output = "more", source = "google")
View(add_ltg)

write.csv(add_ltg, "C:\\Users\\Sourav Ray\\missing-add-lat-long.csv", row.names = FALSE)
