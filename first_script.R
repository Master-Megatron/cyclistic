install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("dplyr")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(scales)


#--------------
# A. COLLECT DATA
#----------------
q1_2020<- read.csv("new_data/202110-divvy-tripdata.csv")
q2_2020<- read.csv("new_data/202111-divvy-tripdata.csv")
q3_2020<- read.csv("new_data/202112-divvy-tripdata.csv")
q4_2020<- read.csv("new_data/202201-divvy-tripdata.csv")
q5_2020<- read.csv("new_data/202202-divvy-tripdata.csv")
q6_2020<- read.csv("new_data/202203-divvy-tripdata.csv")
q7_2020<- read.csv("new_data/202204-divvy-tripdata.csv")
q8_2020<- read.csv("new_data/202205-divvy-tripdata.csv")
q9_2020<- read.csv("new_data/202206-divvy-tripdata.csv")
q10_2021<- read.csv("new_data/202207-divvy-tripdata.csv")
q11_2021<- read.csv("new_data/202208-divvy-tripdata.csv")
q12_2021<- read.csv("new_data/202209-divvy-publictripdata.csv")
#---------------------------------------------
#B. WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#---------------------------------------------
# 1. Check column names each of the files

colnames(q1_2020)
colnames(q2_2020)
colnames(q3_2020)
colnames(q4_2020)
colnames(q5_2020)
colnames(q6_2020)
colnames(q7_2020)
colnames(q8_2020)
colnames(q9_2020)
colnames(q10_2021)
colnames(q11_2021)
colnames(q12_2021)


# 2. Inspect the data frames and look for incongruencies
str(q1_2020)
str(q2_2020)
str(q3_2020)
str(q4_2020)
str(q5_2020)
str(q6_2020)
str(q7_2020)
str(q8_2020)
str(q9_2020)
str(q10_2021)
str(q11_2021)
str(q12_2021)
#Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2020,q2_2020,q3_2020,q4_2020,q5_2020,
                      q6_2020,q7_2020,q8_2020,q9_2020,q10_2021,
                      q11_2021,q12_2021)

#-----------------------------------------------------
# C.CLEAN UP DATA AND ADD DATA TO PREPARE FOR ANALYSIS
#-----------------------------------------------------
# 1. inspect the new dataframe
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

# 2. cek value in a row
distinct(all_trips,rideable_type, .keep_all = FALSE)
distinct(all_trips,member_casual, .keep_all = FALSE)
any(is.na(all_trips) )
#Change membership value and rideable_type
all_trips["member_casual"][all_trips["member_casual"] == "casual"] <- "Casual"
all_trips["member_casual"][all_trips["member_casual"] == "member"] <- "Member"
all_trips["rideable_type"][all_trips["rideable_type"] == "docked_bike"] <- "Docked Bike"
all_trips["rideable_type"][all_trips["rideable_type"] == "classic_bike"] <- "Classic Bike"
all_trips["rideable_type"][all_trips["rideable_type"] == "electric_bike"] <- "Electric Bike"
#get only the names of the data frame columns that contain missing values
mv_all_trips <- as.data.frame(
  cbind(
    lapply(
      lapply(all_trips, is.na), sum)
  )
)
rownames(subset(mv_all_trips, mv_all_trips$V1 != 0))
# There are NA value in  end_lat and end_lng
#delete all missing value and insert into new dataframe
all_trips_v2 <-all_trips[rowSums(is.na(all_trips))==0 ,]
all_trips_v2 <- all_trips_v2[!all_trips_v2$start_station_name == "", ]
all_trips_v2 <- all_trips_v2[!all_trips_v2$end_station_name == "", ] 
str(all_trips_v2)

# 3. Add columns that list the date, month, day, and year of each ride by started_ad 
all_trips_v2$date <- as.Date(all_trips_v2$started_at) # the default format yyyy-mm-dd
all_trips_v2$month<- format(as.Date(all_trips_v2$date), "%m")
all_trips_v2$day<- format(as.Date(all_trips_v2$date), "%d")
all_trips_v2$year <- format(as.Date(all_trips_v2$date), "%Y")
all_trips_v2$day_of_week <- format(as.Date(all_trips_v2$date), "%A")

# 4.  Add a "ride_length" calculation to all_trips (in seconds)
all_trips_v2$ride_length <- difftime(all_trips_v2$ended_at, all_trips_v2$started_at)

# 5. inspect new dataframe structure
str(all_trips_v2)
is.factor(all_trips_v2$ride_length)
is.numeric(all_trips_v2$ride_length)

# 6. Remove Bad data and change data type ride_length
all_trips_v2$ride_length <- as.numeric(as.character(all_trips_v2$ride_length))
all_trips_v3 <- all_trips_v2[!( all_trips_v2$ride_length <0),]
all_trips_v3<-distinct(all_trips_v3,ride_id,.keep_all = TRUE)
duplicated(all_trips_v3)
# save for analyze
write.csv(all_trips_v3, file = 'C:/Users/akung/Documents/R/Projek/projek_1/all_trips_v3.csv')