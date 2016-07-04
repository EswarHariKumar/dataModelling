# install.packages("ggmap")
install.packages("plyr")
install.packages("dplyr")
install.packages("stringr")

# library(maptools)
require(ggmap)
require(ggplot2)
require(plyr)
require(dplyr)
require(ggalt)
require(ggthemes)
require(rgdal)
# library(leaflet)
require(gridExtra)
require(stringr)
require(MASS)
require(shiny)

timesData <- read.csv("timesData.csv", stringsAsFactors = FALSE,na.strings = c("", "-"))
timesData$country =  gsub("Unted Kingdom","United Kingdom",timesData$country)
timesData$country = gsub('Republic of Ireland','Ireland',timesData$country)
timesData$country = gsub('Russian Federation','Russia',timesData$country)
timesData$country = gsub("Unisted States of America",'United States of America',timesData$country)
unique_country <- distinct(select(timesData,country))
lonlat_all <- geocode(as.character(unique_country$country))
country_with_lonlat <- cbind(unique_country,lonlat_all)
#maintains the orignal order of col
timesData_lonlat <- join(timesData,country_with_lonlat,by="country")
#Transform the Rank to Integer -> rank_int
#and then Dividing the rank_int -> ranking_range (25 records per break) 
timesData_lonlat[c("rank_int","ranking_range")] <- NA
timesData_lonlat$rank_int <- as.integer(sapply(strsplit(
  str_replace(timesData_lonlat$rank,"=",''),"-"),"[[",1))
timesData_lonlat$ranking_range <- timesData_lonlat$rank
timesData_lonlat$ranking_range[timesData_lonlat$rank_int > 0 & 
                                 timesData_lonlat$rank_int <26] <- c("0-25")
timesData_lonlat$ranking_range[timesData_lonlat$rank_int > 25 & 
                                 timesData_lonlat$rank_int < 51] <- c("26-50")
timesData_lonlat$ranking_range[timesData_lonlat$rank_int > 50 & 
                                 timesData_lonlat$rank_int <76] <- c("51-75")
timesData_lonlat$ranking_range[timesData_lonlat$rank_int > 75 & 
                                 timesData_lonlat$rank_int <101] <- c("76-100")
timesData_lonlat$ranking_range[timesData_lonlat$rank_int > 100 & 
                                 timesData_lonlat$rank_int <126] <- c("101-125")
timesData_lonlat$ranking_range[timesData_lonlat$rank_int > 125 & 
                                 timesData_lonlat$rank_int < 151] <- c("126-150")
timesData_lonlat$ranking_range[timesData_lonlat$rank_int > 150 & 
                                 timesData_lonlat$rank_int <176] <- c("151-175")
timesData_lonlat$ranking_range[timesData_lonlat$rank_int > 175 & 
                                 timesData_lonlat$rank_int <201] <- c("176-200")

#remove missing data and compute mean values
timesData_lonlat[c("research_new","teaching_new","international_new",
           "citations_new","income_new","overall_new")] <- 0

#dividing the data by year
td.2011 <- filter(timesData_lonlat, timesData_lonlat$Year==2011)
td.2012 <- filter(timesData_lonlat, timesData_lonlat$Year==2012)
td.2013 <- filter(timesData_lonlat, timesData_lonlat$Year==2013)
td.2014 <- filter(timesData_lonlat, timesData_lonlat$Year==2014)
td.2015 <- filter(timesData_lonlat, timesData_lonlat$Year==2015)
td.2016 <- filter(timesData_lonlat, timesData_lonlat$Year==2016)

#calculating the mean values by year
td.2011 <-  ddply(td.2011, .(country), mutate,
                  summarize,researchMean=mean(research,na.rm=TRUE),
                  teachingMean=mean(teaching,na.rm=TRUE),
                  internationalMean = mean(international,
                                           na.rm=TRUE),
                  citationsMean = mean(citations,na.rm=TRUE),
                  incomeMean = mean(income, na.rm = TRUE))
td.2012 <-  ddply(td.2012, 
                  .(country), mutate,
                  summarize,researchMean=mean(research,na.rm=TRUE),
                  teachingMean=mean(teaching,na.rm=TRUE),
                  internationalMean = mean(international,
                                           na.rm=TRUE),
                  citationsMean = mean(citations,na.rm=TRUE),
                  incomeMean = mean(income,na.rm=TRUE))

td.2013 <-  ddply(td.2013, 
                  .(country), mutate,
                  summarize,researchMean=mean(research,na.rm=TRUE),
                  teachingMean=mean(teaching,na.rm=TRUE),
                  internationalMean = mean(international,
                                           na.rm=TRUE),
                  citationsMean = mean(citations,na.rm=TRUE),
                  incomeMean = mean(income,na.rm=TRUE))

td.2014 <-  ddply(td.2014, 
                  .(country), mutate,
                  summarize,researchMean=mean(research,na.rm=TRUE),
                  teachingMean=mean(teaching,na.rm=TRUE),
                  internationalMean = mean(international,
                                           na.rm=TRUE),
                  citationsMean = mean(citations,na.rm=TRUE),
                  incomeMean = mean(income,na.rm=TRUE))

td.2015 <-  ddply(td.2015, 
                  .(country), mutate,
                  summarize,researchMean=mean(research,na.rm=TRUE),
                  teachingMean=mean(teaching,na.rm=TRUE),
                  internationalMean = mean(international,
                                           na.rm=TRUE),
                  citationsMean = mean(citations,na.rm=TRUE),
                  incomeMean = mean(income,na.rm=TRUE))


td.2016 <-  ddply(td.2016, 
      .(country), mutate,
      summarize,researchMean=mean(research,na.rm=TRUE),
      teachingMean=mean(teaching,na.rm=TRUE),
      internationalMean = mean(international,
                               na.rm=TRUE),
      citationsMean = mean(citations,na.rm=TRUE),
      incomeMean = mean(income,na.rm=TRUE))

#Bring the data back together and sorting them by year and rank
timesData.all <-  rbind(td.2011,td.2012,td.2013,td.2014,td.2015,td.2016)
timesData.all <- arrange(timesData.all,timesData.all$Year,timesData.all$rank_int)

timesData.all$research_new <- ifelse(is.na(timesData.all$research),
                                round(timesData.all$researchMean,1),
                                timesData.all$research)
timesData.all$teaching_new <- ifelse(is.na(timesData.all$teaching),
                                round(timesData.all$teachingMean,1),
                                timesData.all$teaching)
timesData.all$international_new <- ifelse(is.na(timesData.all$international),
                                     round(timesData.all$internationalMean,1),
                                     timesData.all$international)
timesData.all$citations_new <- ifelse(is.na(timesData.all$citations),
                                 round(timesData.all$citationsMean,1),
                                 timesData.all$citations)
timesData.all$income_new <- ifelse(is.na(timesData.all$income),
                              round(timesData.all$incomeMean,1),
                              timesData.all$income)
timesData.all$overall_new <- ifelse(is.na(timesData.all$overall),
                               round(0.3*timesData.all$teaching_new +
                                       0.3*timesData.all$research_new +
                                       0.3*timesData.all$citations_new +
                                       0.075*timesData.all$international_new +
                                       0.025*timesData.all$income_new,1 ),
                               timesData.all$overall)
# write.csv(file = "timesData_all.csv",timesData.all)
write.table(file="timesData_allTbl.csv", timesData.all)
timesDataTable <- read.table("timesData_allTbl.csv", stringsAsFactors = FALSE)
timesData.all <- data.frame(timesDataTable)

td.2011 <- filter(timesData.all, timesData.all$Year==2011)
td.2012 <- filter(timesData.all, timesData.all$Year==2012)
td.2013 <- filter(timesData.all, timesData.all$Year==2013)
td.2014 <- filter(timesData.all, timesData.all$Year==2014)
td.2015 <- filter(timesData.all, timesData.all$Year==2015)
td.2016 <- filter(timesData.all, timesData.all$Year==2016)

firstRankedUniPerCounty <- ddply(td.2016, .(country), 
                                 function (x) as.character(x$school_name[1]))
names(firstRankedUniPerCounty)[2] <- "school_name"

theRest <- subset(td.2016,
                  !td.2016$school_name %in% firstRankedUniPerCounty$school_name)
theRest.avg <- ddply(theRest,.(country),mutate,
                     summarize,researchMean2=mean(research_new,na.rm=TRUE),
                     teachingMean2=mean(teaching_new,na.rm=TRUE),
                     internationalMean2 = mean(international_new,na.rm=TRUE),
                     citationsMean2 = mean(citations_new,na.rm=TRUE),
                     incomeMean2 = mean(as.numeric(income_new) ,na.rm=TRUE),
                     overallMean2 = mean(overall_new,na.rm=TRUE))

avgResult <- distinct(select(theRest.avg,teachingMean2,internationalMean2,citationsMean2,incomeMean2,overallMean2, researchMean2,country))

firstRankedUniPerCounty_allinfo <- 
  filter(select(td.2016,rank:overall_new), 
         td.2016$school_name %in% firstRankedUniPerCounty$school_name)

firstRanked.avg <- ddply(firstRankedUniPerCounty_allinfo,.(country),mutate,
                         summarize,researchMean2=mean(research_new,na.rm=TRUE),
                         teachingMean2=mean(teaching_new,na.rm=TRUE),
                         internationalMean2 = mean(international_new,na.rm=TRUE),
                         citationsMean2 = mean(citations_new,na.rm=TRUE),
                         incomeMean2 = mean(income_new),
                         overallMean2 = mean(overall_new))

firstRanked <- select(firstRanked.avg,school_name,country,Year,rank_int,ranking_range,
                      teaching_new,research_new,
                      international_new,citations_new,income_new,overall_new,lon,lat)
firstRanked.theRest <- join(firstRanked,avgResult, by="country")


write.table(file="firstRankedandtheRest.csv", firstRanked.theRest)
firstRanked.theRestTbl <- read.table("firstRankedandtheRest.csv", stringsAsFactors = FALSE)
firstRanked.theRest <- data.frame(firstRanked.theRestTbl)

school.country_lookup <- read.csv("school_and_country_table.csv")