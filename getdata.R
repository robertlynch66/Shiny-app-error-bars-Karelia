# get data


library(dplyr)

#load("m.Rda")
s<-readRDS("person_data.rds")

cols<- c("blue","darkgreen","black","darkred","darkgoldenrod4","purple","orange","yellow","lightblue")
# load data and fix variables
#s <- person_data  %>% filter (birthregion=="karelia")

#s <- s  %>% filter (birthregion=="karelia")

s$census_1950<- s$'1950_census'
s$'1950_census'<- NULL
# dump the 'other' category in statistics Finland
s$statistics_finland<- gsub("6", "5", s$statistics_finland)
s$statistics_finland<- gsub("7",NA,s$statistics_finland)
s$statistics_finland<-gsub("8",NA,s$statistics_finland)
#s$outbred <- factor(s$outbred)
s$education  <- factor(s$education, labels=c("Uneducated", "Educated"))
s$birthpopulation <- gsub("2559000", NA, s$birthpopulation)
s$social_class <- factor(s$social_class, labels=c("Wealthy","Rich","Upper Middle","Middle","Lower Middle", "Lower",
                                                  "Poor"))
s$sex <- factor(s$sex, labels=c("Female","Male"))
s$fdf_population <- as.numeric(s$fdf_population)
s$fdf_population <- log(s$fdf_population)
s$rdk_population <- as.numeric (s$rdk_population)
s$rdk_population <- log(s$rdk_population)
s$birthpopulation <- as.numeric(s$birthpopulation)
s$birthpopulation <- log(s$birthpopulation)
s$farmtotalarea <- log(s$farmtotalarea)
s$servedduringwar <- as.integer(s$servedduringwar)
s$lotta <- as.integer(s$lotta)
s$injuredinwar <- as.integer(s$injuredinwar)


# make the drop down categorial variables factors
s$servedduringwar <- factor (s$servedduringwar, labels = c("Did not serve","Served"))
s$lotta <- factor (s$lotta, labels= c("Did not serve","Served"))
s$injuredinwar <- factor (s$injuredinwar, labels = c("Not injured","Injured"))
s$man_labor <- factor(s$man_labor, labels=c("Non-manual labor", "Manual labor"))
s$agriculture <- factor(s$agriculture, labels = c("Non-agricultural", "Agricultural"))
s$statistics_finland <- factor(s$statistics_finland, labels=c ("Self-employed and\n employer farmers",
                                                               "Self-employed or\n employers", 
                                                               "Upper level\n employees", 
                                                               "Lower level\n employees", 
                                                               "Manual workers"))
s$census_1950<- factor(s$census_1950, labels=c("Technical \nprofessionals\n and teachers",
                                               "Directors,\n office workers\n and typers",
                                               "Business\n and selling", "Agriculture and \nforestry related",
                                               "Mining and\n industry", "Transportation", "Factory and\n craftsmen",
                                                "Handicraft \nworkers","Service"))

s$outbreed <- s$outbred
s$outbreed [is.na(s$spouse_id) ] <- 2 
#include only people who were over 19 in 1939
#s$outbreed [s$age_1970<49] <- NA
s$outbreed <- factor(s$outbreed, labels= c("0"="Married a\n Karelian", "1"="Married a Finn","2"="Never married"))

#s$returnedkarelia_factor <- s$returnedkarelia
#s$returnedkarelia_factor <- factor(s$returnedkarelia_factor, labels=c("0"="Did not return", "1"="Returned"))
attach(s)
s$returnedkarelia_factor[returnedkarelia == 0] <- 0
s$returnedkarelia_factor[returnedkarelia == 1] <- 1

detach(s)
s$returnedkarelia_factor <- factor(s$returnedkarelia_factor, labels=c("0"="Did not return","1"="Returned"))

#s$total_migrations<- as.numeric(m$total_migrations)
s$peacetime_migrations<- as.numeric(s$peacetime_migrations)
s$movesbefore1940 <- as.numeric(s$movesbefore1940)
s$movesafter1945 <- as.numeric(s$movesafter1945)

attach(s)
s$wedyear[weddingyear < 1940] <- 0
s$wedyear[weddingyear > 1939 & weddingyear <= 1944] <- 1
s$wedyear[weddingyear > 1945] <- 2
detach(s)
s$wedyear <- factor(s$wedyear, labels=c("0"="Before the war","1"="During the war","2"="After the war"))

attach(s)
s$byear[birthyear < 1900] <- 0
s$byear[birthyear > 1899 & birthyear <= 1905] <- 1
s$byear[birthyear > 1905 & birthyear <= 1910] <- 2
s$byear[birthyear > 1910 & birthyear <= 1915] <- 3
s$byear[birthyear > 1915 & birthyear <= 1920] <- 4
s$byear[birthyear > 1920 & birthyear <= 1925] <- 5
s$byear[birthyear > 1925 & birthyear <= 1930] <- 6
s$byear[birthyear > 1930 & birthyear <= 1935] <- 7
s$byear[birthyear > 1935 & birthyear <= 1940] <- 8
s$byear[birthyear > 1940] <- 9
detach(s)
s$byear <- factor(s$byear, labels=c("0"="Before 1900","1"="1900-1905","2"="1906-1910",
                                    "3"="1911-1915","4"="1916-1920","5"="1921-1925",
                                    "6"="1926-1930","7"="1931-1935","8"="1936-1940",
                                    "9"="After 1940"))


#s<- na.omit(s)
dataset <- s
