#acs 5 year estimate structuring

View(y2017_ac)


#Household Income
y2019_ac$LT25k <- y2019_ac$B19001_002 + y2019_ac$B19001_003 + y2019_ac$B19001_004 + y2019_ac$B19001_005
y2019_ac$LT50k <- y2019_ac$LT25k + y2019_ac$B19001_006 + y2019_ac$B19001_007 + y2019_ac$B19001_008 + y2019_ac$B19001_009 + y2019_ac$B19001_010
y2019_ac$MT50k <- y2019_ac$B19001_001 - y2019_ac$LT50k

#employment
y2019_ac$HSMF <- y2019_ac$B15002_011 + y2019_ac$B15002_028




#Percentage household income
y2019_ac$Total_Household <- y2019_ac$LT25k + y2019_ac$LT50k + y2019_ac$MT50k
y2019_ac <- transform(y2019_ac, Percent_LT25k = LT25k / Total_Household)
y2019_ac <- transform(y2019_ac, Percent_LT50k = LT50k / Total_Household)
y2019_ac <- transform(y2019_ac, Percent_MT50k = MT50k / Total_Household)

#Percentage insurance
y2019_ac$Total_Insurance <- y2019_ac$B27010_019 + y2019_ac$B27010_035 + y2019_ac$B27010_052

y2019_ac <- transform(y2019_ac, Percent_Insurance = y2019_ac$Total_Insurance / y2019_ac$population)


colnames(y2019_ac)[18] <- "LT19-34_1TI"
colnames(y2019_ac)[19] <- "LT35-64_1TI"
colnames(y2019_ac)[20] <- "MT65_1TI"
colnames(y2019_ac)[4] <- "Total_Unemployed"
colnames(y2019_ac)[3] <- "Total_Population"
View(y2019_ac)

#Percentage unemployed
y2019_ac <- transform(y2019_ac, Percent_Unemp = Total_Unemployed / Total_Population)

y2019_ac = y2019_ac[,-c(5:17)]
y2019_ac = y2019_ac[,-c(2,8:13)]

y2019_ac = y2019_ac[,c(1,2,3,10,7,8,9,17,11,12,13,14,4,5,6)]
View(y2017_ac)




#2020
#Median Income Census Variable

inc <- read.csv("ACSDT5Y2015.B19001_data_with_overlays_2022-06-30T105718.csv")
View(inc)
inc = inc[,-c(20:34,36)]
inc = inc[,-c(2,4,6,8,10,12,14,16,18)]
inc = inc[-1,]

str(inc)

for(i in 1:(ncol(inc)-1)) {
  inc[,i] <- as.integer(inc[,i])
}

inc$LT25k <- inc$B19001_002E + inc$B19001_003E + inc$B19001_004E + inc$B19001_005E
inc$LT50k <- inc$LT25k + inc$B19001_006E + inc$B19001_007E + inc$B19001_008E + inc$B19001_009E + inc$B19001_010E
inc$MT50k <- inc$B19001_001E - inc$LT50k

inc = inc[,-c(1:10)]
View(inc)

#Education attainment census variable

edu <- read.csv("ACSDT5Y2015.B15002_data_with_overlays_2022-06-30T105604.csv")
View(edu)

edu_odd <- edu[,seq_len(ncol(edu)) %% 2 == 1]
View(edu_odd)

for(i in 1:(ncol(edu_odd)-1)) {
  edu_odd[,i] <- as.integer(edu_odd[,i])
}

edu_odd = edu_odd[-1,]

edu_odd$HSMF <- edu_odd$B15002_011E + edu_odd$B15002_028E

edu_odd = edu_odd[,-c(1:35)]

#Employment Status 

emp <- read.csv("ACSDT5Y2015.B23025_data_with_overlays_2022-06-30T105448.csv")
View(emp)


emp = emp[,c(9,15)]
emp = emp[-1,]

for(i in 1:(ncol(emp)-1)) {
  emp[,i] <- as.integer(emp[,i])
}


#Health 

health <- read.csv("ACSDT5Y2015.B27010_data_with_overlays_2022-06-30T105249.csv")
View(health)


health = health[-1, c(5,37,69,103,133)]


#pop

pop <- read.csv("ACSDT5Y2015.B01003_data_with_overlays_2022-06-30T105527.csv")
View(pop)

pop = pop[-1,c(1,3)]


cen1 <- merge(pop, emp, on = "GEO_ID", all.x = T)
View(cen1)

cen2 <- merge(edu_odd, inc, on = "GEO_ID", all.x = T)

cen_15 <- merge(cen1, cen2, on = "GEO_ID", all.x = T)
View(cen_15)


cen_15$B01003_001E = as.integer(cen_15$B01003_001E)
cen_15 <- transform(cen_15, Percent_Unemp = B23025_005E / B01003_001E)
str(cen_15)


cen_15$Total_Household <- cen_15$LT25k + cen_15$LT50k + cen_15$MT50k
cen_15 <- transform(cen_15, Percent_LT25k = LT25k / Total_Household)
cen_15 <- transform(cen_15, Percent_LT50k = LT50k / Total_Household)
cen_15 <- transform(cen_15, Percent_MT50k = MT50k / Total_Household)

cen_15 = merge(cen_15, health, on = "GEO_ID", all.x = T)
View(cen_15)

colnames(cen_15)[2] <- "Total_Population"
colnames(cen_15)[3] <- "Total_Unemployed"
colnames(cen_15)[13] <- "LT19_1TI"
colnames(cen_15)[14] <- "LT19-34_1TI"
colnames(cen_15)[15] <- "LT35-64_1TI"
colnames(cen_15)[16] <- "MT65_1TI"

cen_15 = cen_15[,-13]
cen_15$`LT19-34_1TI` = as.integer(cen_15$`LT19-34_1TI`)
cen_15$`LT35-64_1TI` = as.integer(cen_15$`LT35-64_1TI`)
cen_15$MT65_1TI = as.integer(cen_15$MT65_1TI)

cen_15$Total_Insurance <- cen_15$`LT19-34_1TI` + cen_15$`LT35-64_1TI` + cen_15$MT65_1TI

cen_15 <- transform(cen_15, Percent_Insurance = cen_15$Total_Insurance / cen_15$Total_Population)

str(cen_15)
write.csv(cen_15, "C:\\Users\\Sourav Ray\\census_mod_final_file\\2015_cen.csv", row.names = FALSE)
