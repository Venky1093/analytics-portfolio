# Loading library files

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(here)
library(janitor)
library(tidyr)
install.packages("metR")
library(metR)

#Setting up the directory

setwd("F:/Data Analyst/Coursera/Portfolio/Track Cycle Project/Data")

#Importing csv files

Mar <- read.csv("202203-divvy-tripdata.csv")
Apr <- read.csv("202204-divvy-tripdata.csv")
May <- read.csv("202205-divvy-tripdata.csv")
Jun <- read.csv("202206-divvy-tripdata.csv")
Jul <- read.csv("202207-divvy-tripdata.csv")
Aug <- read.csv("202208-divvy-tripdata.csv")
Sep <- read.csv("202209-divvy-tripdata.csv")
Oct <- read.csv("202210-divvy-tripdata.csv")
Nov <- read.csv("202211-divvy-tripdata.csv")
Dec <- read.csv("202212-divvy-tripdata.csv")
Jan <- read.csv("202301-divvy-tripdata.csv")
Feb <- read.csv("202302-divvy-tripdata.csv")

# Inspecting the structure of all the data files to ensure equal columns and appropriate datatypes.

colnames(Feb)
str(Feb)

# Combining all csv files into single data frame

track_data <- bind_rows(Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, Jan)
str(track_data)
show(track_data)

# Processing the data for analysis

head(track_data)
colnames(track_data)
nrow(track_data)
dim(track_data)
summary(track_data) #statistical summary of data mainly for numerics
str(track_data) #list of columns and datatypes

# Adding columns for date, month, year, day of the week into the data frame

track_data$date <- as.Date(track_data$started_at)
track_data$month <- format(as.Date(track_data$date),"%m")
track_data$day <- format(as.Date(track_data$date),"%d")
track_data$year <- format(as.Date(track_data$date),"%Y")
track_data$day_of_week <- format(as.Date(track_data$date),"%A")


colnames(track_data)

head(track_data)

# Adding ride_length column into the data frame

track_data$ride_length <- difftime(track_data$ended_at, track_data$started_at)
str(track_data)
glimpse(track_data)

# Converting ride_length to numeric

track_data$ride_length <- as.numeric(as.character(track_data$ride_length))
is.numeric(track_data$ride_length)
track_data$ride_length_m <- track_data$ride_length/60
glimpse(track_data)

# Inspecting the bad ride length

sum(track_data$ride_length <=0)

# Removing bad ride length data

track_datav1 <- track_data[!(track_data$ride_length <= 0),]
sum(track_datav1$ride_length_m <=0)

# Assigning seasons to month
track_datav1$month <- as.numeric(track_datav1$month) # Converting month to numeric
is.numeric(track_datav1$month)

track_datav1$season <- season(track_datav1$month)

track_datav1$month <- format(as.Date(track_datav1$date),"%b") # For better anlysis

glimpse(track_datav1)

# Performing Statistical Analysis

track_datav1 %>% 
  group_by(member_casual) %>% summarise(average_ride_length = mean(ride_length), median_length = median(ride_length),
                                        max_ride_length = max(ride_length), min_ride_length = min(ride_length))

# For total number of rides

track_datav1 %>%
  group_by(member_casual) %>%
  summarise(ride_count = n())

# Calculating average ride length and no. of rides as per day of the week.

track_datav1 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(),
            average_ride_length = mean(ride_length),.groups = "drop")

# For total number of rides season ise

track_datav1 %>%
  group_by(member_casual, season) %>%
  summarise(ride_count = n())

# Share through Visualisation

# Visualizing total rides taken by members and casual riders
track_datav1 %>%
  group_by(member_casual) %>%
  summarise(ride_count = length(ride_id)) %>%
  ggplot() + geom_col(mapping = aes(x = member_casual, y = ride_count, fill = member_casual), show.legend = FALSE) +
  labs(title = "TOTAL NO. OF RIDES")

# Visualising the days of the week with no. of rides taken by riders
track_datav1 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title = "TOTAL RIDES VS DAY OF THE WEEK") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x,scientific = FALSE))

# Visualising average ride by day of the week
track_datav1 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "AVERAGE RIDE LENGTH VS DAY OF THE WEEK")

# Visualising Season ise rides

track_datav1%>%
  group_by(member_casual, season, day_of_week) %>%   
  summarise(number_of_rides = n()						 
            ,avg_ride_length = mean(ride_length_m)) %>% 
  ggplot() + geom_col(mapping = aes(x = day_of_week, y = number_of_rides, fill = member_casual), position = "dodge") + facet_wrap(~season) + scale_y_continuous(breaks = seq(0, 400000, by = 50000))

# Ride_lenght by week_day and rider type and season

track_datav1%>%
  group_by(season, day_of_week, member_casual) %>%   
  summarise(number_of_rides = n()						 
            ,avg_ride_length = mean(ride_length_m),.groups = "drop") %>% 
  ggplot() + geom_col(mapping = aes(x = day_of_week, y = avg_ride_length, fill = member_casual), position = "dodge") + facet_wrap(~season) + scale_y_continuous(breaks = seq(0, 50, by = 10))

# Visualising average rides by month
track_datav1 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = "drop") %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "AVERAGE RIDE LENGTH VS MONTH")

# Visualising and comparing casual and member rides by distance
track_datav1 %>%
  group_by(member_casual) %>%
  summarise(average_ride_distance = mean(ride_length)) %>%
  ggplot() + geom_col(mapping = aes(x = member_casual, y = average_ride_distance, fill = member_casual), show.legend = FALSE) +
  labs(title = "AVERAGE DISTANCE TRAVELLED")

# Visualising comparison of total rides with the type of ride
track_datav1 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot() + geom_col(mapping = aes(x = rideable_type, y = number_of_rides, fill = member_casual), show.legend = TRUE) +
  labs(title = "TOTAL No OF RIDES VS RIDE TYPE")

