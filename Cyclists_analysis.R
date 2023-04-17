library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)

tripdata_2020_Q1 <- read.csv(("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/Divvy_Trips_2020_Q1.csv"))
tripdata_202004 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202004-divvy-tripdata.csv")
tripdata_202005 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202005-divvy-tripdata.csv")
tripdata_202006 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202006-divvy-tripdata.csv")
tripdata_202007 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202007-divvy-tripdata.csv")
tripdata_202008 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202008-divvy-tripdata.csv")
tripdata_202009 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202009-divvy-tripdata.csv")
tripdata_202010 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202010-divvy-tripdata.csv")
tripdata_202011 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202011-divvy-tripdata.csv")
tripdata_202012 <- read.csv("C:/Users/Divy Raj Singh/Desktop/Capstone Google DA/202012-divvy-tripdata.csv")

str(tripdata_2020_Q1)
str(tripdata_202004)
str(tripdata_202005)
str(tripdata_202006)
str(tripdata_202007)
str(tripdata_202008)
str(tripdata_202009)
str(tripdata_202010)
str(tripdata_202011)
str(tripdata_202012)



## Changing start_station_id from chr to int

options(warn=-1)

tripdata_202012 <- mutate(tripdata_202012, start_station_id = as.integer(start_station_id))


## changing end_station_id from chr to int
0
tripdata_202012 <- mutate(tripdata_202012, end_station_id = as.integer(end_station_id))

#combining all data
tripdata_2020 <- rbind(tripdata_2020_Q1, tripdata_202004, tripdata_202005, tripdata_202006, tripdata_202007, tripdata_202008, tripdata_202009, tripdata_202010, tripdata_202011, tripdata_202012)


## Confirming the total number of rows for the individual dataframes

rowtotal <- sum(
  nrow(tripdata_2020_Q1),
  nrow(tripdata_202004), 
  nrow(tripdata_202005), 
  nrow(tripdata_202006), 
  nrow(tripdata_202007), 
  nrow(tripdata_202008), 
  nrow(tripdata_202009), 
  nrow(tripdata_202010),
  nrow(tripdata_202011), 
  nrow(tripdata_202012))
print(rowtotal)

## Confirming the total number of rows for the combined dataframe

print(nrow(tripdata_2020))

#Examining the combined dataset

str(tripdata_2020)

head(tripdata_2020)


#data cleaning

tripdata_2020$date <- as.Date(tripdata_2020$started_at)
tripdata_2020$month <- format(as.Date(tripdata_2020$date), "%b")
tripdata_2020$day <- format(as.Date(tripdata_2020$date), "%d")
tripdata_2020$year <- format(as.Date(tripdata_2020$date), "%Y")
tripdata_2020$day_of_week <- format(as.Date(tripdata_2020$date), "%A")


## Removing NA's

tripdata_2020 <- drop_na(tripdata_2020)


## Remove duplicates from dataframe

tripdata_2020_no_duplicates <- tripdata_2020[!duplicated(tripdata_2020$ride_id), ]
print(paste("Removed", nrow(tripdata_2020) - nrow(tripdata_2020_no_duplicates), "duplicate rows"))

## Creating a column to determine the ride length 

tripdata_2020_v2 <- mutate(tripdata_2020_no_duplicates, ride_length = difftime(ended_at, started_at, units = "mins"))
str(tripdata_2020_v2)



## filtering out trips with a ride length less than 0.

nrow(tripdata_2020_v2[tripdata_2020_v2$ride_length < 0,])
tripdata_2020_v3 <- tripdata_2020_v2[!tripdata_2020_v2$ride_length <0,]
glimpse(tripdata_2020_v3)

## determining the amount of members vs casual riders

rider_type_total <- table(tripdata_2020_v3$member_casual)
View(rider_type_total)

## Statistical analysis

trip_stats <- tripdata_2020_v3 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length), median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))
head(trip_stats)

## Determine the mode for the day of the week (code learnt from tutorialspoint.com)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

weekday_mode <- getmode(tripdata_2020_v3$day_of_week)

print(weekday_mode)


## Determining the most popular day by rider type

tripdata_2020_v3$day_of_week <- ordered(tripdata_2020_v3$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

tripdata_2020_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(rider_type_total = n(), average_ride_length = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)


## Determining the most popular months during 2020

popular_month <- tripdata_2020_v3 %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(-number_of_rides)

View(popular_month)

## Determine the most popular start station

station_mode <- getmode(tripdata_2020_v3$start_station_name)

print(station_mode)

## Determine the most popular start station for members

popular_start_stations_member <- tripdata_2020_v3 %>% 
  filter(member_casual == 'member') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_member)

## Determine the most popular start station for casual riders

popular_start_stations_casual <- tripdata_2020_v3 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_casual)

##VIZ. for analyzing and sharing


#Rider Types

tripdata_2020_v3 %>% 
  group_by(member_casual) %>% 
  summarise(total_rider_type = n()) %>% 
  ggplot(aes(x = member_casual, y = total_rider_type, fill = member_casual)) + 
  geom_col(position = "dodge") + geom_text(aes(label = total_rider_type, vjust = -0.25))


#rider types and ride duration

rider_type_average_duration <- tripdata_2020_v3 %>% 
  group_by(member_casual) %>% 
  summarize(average_ride_length = mean(ride_length))

rider_type_average_duration %>% 
  ggplot(aes(x = member_casual, y = average_ride_length, fill = member_casual)) +
  geom_col(position = "dodge") + geom_text(aes(label = average_ride_length, vjust = -0.25))

## Visualization of the usage by members and casual riders by the weekday

tripdata_2020_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


## Visualization of the number of trips by members and casual riders by the weekday


tripdata_2020_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


## Visualization of the usage by members and casual riders by the month

tripdata_2020_v3$month <- ordered(tripdata_2020_v3$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

tripdata_2020_v3 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length) ) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides, angle = 90)) +
  facet_wrap(~member_casual)


## Visualization of the number of trips by members and casual riders by the month


tripdata_2020_v3 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides, angle = 90)) +
  facet_wrap(~member_casual)



