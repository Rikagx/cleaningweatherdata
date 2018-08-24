#"Cleaning Data in R" Datacamp Course: Putting it all together section
# By: Rika Gorn

library(haven)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(janitor)

#import RDS dataset
weather <- readRDS("weather.rds")

#understanding the structure of your data
class(weather) 
dim(weather)
names(weather)
glimpse(weather) #dplyr function

#looking at your data shows lots of NAs
head(weather)
tail(weather)

#Columns represent days of the month. Gather column names using tidyr package
weather2 <- gather(weather, day, value, X1:X31, na.rm = TRUE)

#remove first column of row names by subsetting
without_x <- weather2[,-1]

#spread the measure data which has variables as rows
weather3 <- spread(without_x, measure, value)

#get rid of X in day column
weather3$day <- str_replace(weather3$day, "X", "")

#create new column "date" uniting year, month, and day 
weather4 <- unite(weather3, date, year, month, day, sep = "-")

#coerce date column into proper date format
weather4$date <- ymd(weather4$date)

#rearrange columns
weather5 <- select(weather4, date, Events, CloudCover:WindDirDegrees)

#check out our data and see that its all characters and strings!!EEEEEKKK
str(weather5)

#replace "T"'s in precipitation with "0" 
weather5$PrecipitationIn <- str_replace(weather5$PrecipitationIn, "T", "0")

#convert  all but the first 2 columns to numerics using scoped variant of mutate 
#https://dplyr.tidyverse.org/reference/summarise_all.html
weather6 <- mutate_at(weather5, vars(CloudCover:WindDirDegrees), funs(as.numeric))

#looking for missing/incorrect values  
sum(is.na(weather6)) #6 NAs in the Max. Gust. SpeedMPH column
summary(weather6) #maximum humidity is 1,000%, -1 min mean visibility

#find and look at rows with missing and incorrect values
ind <- which(is.na(weather6$Max.Gust.SpeedMPH)) #give you indices of NAs 
weather6[ind, ] #subsets rows, keeping NAs for now

ind <- which(weather6$Max.Humidity == 1000) #can also use which.max
weather6[ind, ]

#change 1000 to 100 to fix humidity data error
weather6$Max.Humidity[ind] <- 100
summary(weather6$Max.Humidity) #check your work!

ind <- which(weather6$Mean.VisibilityMiles == -1) #can also use which.min
weather6[ind,]

#change -1 to 10
weather6$Mean.VisibilityMiles[ind] <- 10
summary(weather6$Mean.VisibilityMiles) #did not work for some reason :(

hist(weather6$MeanDew.PointF)
hist(weather6$Min.TemperatureF)
hist(weather6$Mean.TemperatureF)

#Clean up bad names and empty cells
weather7 <- clean_names(weather6) #didnt have datacamp's new colnames so used janitor package 
weather7$events[weather7$events == ""] <- "None"
