#importing packages
install.packages("tidyverse") 
install.packages("ggplot2")
install.packages("lubridate") #for manupilating dates and time
install.packages("geosphere") #for geographic coordinates
install.packages("gridExtra") #for grid graphics
install.packages("ggmap") # to visualize spatial data and models on top of static maps 
install.packages("sqldf")
install.packages("maps")
install.packages("rgdal")
install.packages("RColorBrewer")
install.packages("plotly")
install.packages("ggpubr")

library("maps")
library("rgdal")
library("RColorBrewer")
library("dplyr")
library("data.table")
library("sqldf")
library("tidyverse")
library("ggplot2")
library("lubridate")
library("geosphere")
library("gridExtra") 
library("ggmap")
library("plotly")
library("ggpubr")

#importing data
trips_data_2020_04 <- read.csv("../bike/Data/202004-divvy-tripdata.csv")
trips_data_2020_05 <- read.csv("../bike/Data/202005-divvy-tripdata.csv")
trips_data_2020_06 <- read.csv("../bike/Data/202006-divvy-tripdata.csv")
trips_data_2020_07 <- read.csv("../bike/Data/202007-divvy-tripdata.csv")
trips_data_2020_08 <- read.csv("../bike/Data/202008-divvy-tripdata.csv")
trips_data_2020_09 <- read.csv("../bike/Data/202009-divvy-tripdata.csv")
trips_data_2020_10 <- read.csv("../bike/Data/202010-divvy-tripdata.csv")
trips_data_2020_11 <- read.csv("../bike/Data/202011-divvy-tripdata.csv")
trips_data_2020_12 <- read.csv("../bike/Data/202012-divvy-tripdata.csv")

trips_data_2021_01 <- read.csv("../bike/Data/202101-divvy-tripdata.csv")
trips_data_2021_02 <- read.csv("../bike/Data/202102-divvy-tripdata.csv")
trips_data_2021_03 <- read.csv("../bike/Data/202103-divvy-tripdata.csv")
trips_data_2021_04 <- read.csv("../bike/Data/202104-divvy-tripdata.csv")
trips_data_2021_05 <- read.csv("../bike/Data/202105-divvy-tripdata.csv")
trips_data_2021_06 <- read.csv("../bike/Data/202106-divvy-tripdata.csv")
trips_data_2021_07 <- read.csv("../bike/Data/202107-divvy-tripdata.csv")
trips_data_2021_08 <- read.csv("../bike/Data/202108-divvy-tripdata.csv")
trips_data_2021_09 <- read.csv("../bike/Data/202109-divvy-tripdata.csv")
trips_data_2021_10 <- read.csv("../bike/Data/202110-divvy-tripdata.csv")
trips_data_2021_11 <- read.csv("../bike/Data/202111-divvy-tripdata.csv")
trips_data_2021_12 <- read.csv("../bike/Data/202112-divvy-tripdata.csv")

trips_data_2022_01 <- read.csv("../bike/Data/202201-divvy-tripdata.csv")

#inspecting data
str(trips_data_2020_04)
str(trips_data_2020_05)
str(trips_data_2020_06)
str(trips_data_2020_07)
str(trips_data_2020_08)
str(trips_data_2020_09)
str(trips_data_2020_10)
str(trips_data_2020_11)
str(trips_data_2020_12)
str(trips_data_2021_01)
str(trips_data_2021_02)
str(trips_data_2021_03)
str(trips_data_2021_04)
str(trips_data_2021_05)
str(trips_data_2021_06)
str(trips_data_2021_07)
str(trips_data_2021_08)
str(trips_data_2021_09)
str(trips_data_2021_10)
str(trips_data_2021_11)
str(trips_data_2021_12)
str(trips_data_2022_01)

#Convert inconsistent start_station_id and end_station_id  fiels from int to char datatype
trips_data_2020_04 <- trips_data_2020_04 %>% mutate(start_station_id=as.character(start_station_id), end_station_id=as.character(end_station_id))
trips_data_2020_05 <- trips_data_2020_05 %>% mutate(start_station_id=as.character(start_station_id), end_station_id=as.character(end_station_id))
trips_data_2020_06 <- trips_data_2020_06 %>% mutate(start_station_id=as.character(start_station_id), end_station_id=as.character(end_station_id))
trips_data_2020_07 <- trips_data_2020_07 %>% mutate(start_station_id=as.character(start_station_id), end_station_id=as.character(end_station_id))
trips_data_2020_08 <- trips_data_2020_08 %>% mutate(start_station_id=as.character(start_station_id), end_station_id=as.character(end_station_id))
trips_data_2020_09 <- trips_data_2020_09 %>% mutate(start_station_id=as.character(start_station_id), end_station_id=as.character(end_station_id))
trips_data_2020_10 <- trips_data_2020_10 %>% mutate(start_station_id=as.character(start_station_id), end_station_id=as.character(end_station_id))
trips_data_2020_11 <- trips_data_2020_11 %>% mutate(start_station_id=as.character(start_station_id), end_station_id=as.character(end_station_id))

#Combine all the datasets into one single dataframe
all_trips_data <- bind_rows(trips_data_2020_04,trips_data_2020_05,trips_data_2020_06,trips_data_2020_07,trips_data_2020_08,trips_data_2020_09,trips_data_2020_10,trips_data_2020_11,trips_data_2020_12,trips_data_2021_01,trips_data_2021_02,trips_data_2021_03,trips_data_2021_04,trips_data_2021_05,trips_data_2021_06,trips_data_2021_07,trips_data_2021_08,trips_data_2021_09,trips_data_2021_10,trips_data_2021_11,trips_data_2021_12,trips_data_2022_01)
str(all_trips_data)

#remove duplicates
distinct(all_trips_data)

#convert started_at & ended_at field in datetime datatype
all_trips_data[['started_at']] <- ymd_hms(all_trips_data[['started_at']])
all_trips_data[['ended_at']] <- ymd_hms(all_trips_data[['ended_at']])

##column for day of the week the trip started - abbriviated notation
all_trips_data$day_of_the_week <- format(as.Date(all_trips_data$started_at),'%a')
all_trips_data$month <- format(as.Date(all_trips_data$started_at),'%b_%y')

#checkin duplicates in the ride_id colunm
bikes<-unique(all_trips_data$ride_id)
length(bikes)

#add colunm for ride duration
all_trips_data$duration<-(as.double(difftime(all_trips_data$ended_at, all_trips_data$started_at)))/60
glimpse(all_trips_data)

#counting negative duration trips : 10695
nrow(subset(all_trips_data,duration < 0))

#deleting rows where duration <0
all_trips_data_v2 <- all_trips_data[!(all_trips_data$duration < 0),]
nrow(subset(all_trips_data_v2,duration < 0))

#adding time columns
all_trips_data_v2$time <- format(all_trips_data_v2$started_at, format = "%H:%M")
all_trips_data_v2$time <- as.POSIXct(all_trips_data_v2$time, format = "%H:%M")

#checking for testrides that were made by company for quality checks
nrow(subset(all_trips_data, start_station_name %like% "TEST"))
nrow(subset(all_trips_data, start_station_name %like% "test"))
nrow(subset(all_trips_data, start_station_name %like% "Test"))

#remove test rides
all_trips_data_v2<- all_trips_data_v2[!((all_trips_data_v2$start_station_name %like% "TEST" | all_trips_data_v2$start_station_name %like% "test" | all_trips_data_v2$start_station_name %like% "Test")),]
glimpse(all_trips_data_v2)

# checking count of distinct values
table(all_trips_data_v2$member_casual)

#aggregating total trip duration by customer type
setNames(aggregate(duration ~ member_casual, all_trips_data_v2, mean), c("customer_type", "total_trip_duration(mins)"))


#Calculating and visualize cusual riders and members rides
myPalette <- brewer.pal(5, "Set2") 
prop <- all_trips_data_v2%>% group_by(member_casual)%>%summarise(n=n())
labels <- c("casual","member")
piepercent<-round(100*prop$n/sum(prop$n),1)
png(file="Rider type percentage.jpg")
pie(prop$n, labels=piepercent, main="Costumer type percentage" , col = myPalette)
legend("topright",  c("casual","member"),  cex=0.8, fill=myPalette)
dev.off()


#trips by month and rider type
all_trips_data_v2 %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total trips by customer type per Month") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+scale_fill_brewer(palette="Dark2")


#trips by day and rider type
all_trips_data_v2 %>%  
  group_by(member_casual, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+scale_fill_brewer(palette="Dark2")

#duration by customer type
df1<- group_by(all_trips_data_v2, member_casual) 
df1<- summarise(df1, total_duration = sum(duration)) 
p1<-  ggplot(df1, aes(x = member_casual, y = total_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+scale_fill_brewer(palette="Dark2")+coord_flip()+labs(y= "Minutes", x = "Customer type")

#mean duration by customr type
df2<- group_by(all_trips_data_v2, member_casual) 
df2<- summarise(df2, Mean_duration = mean(duration)) 
p2<-  ggplot(df2, aes(x = member_casual, y = Mean_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+scale_fill_brewer(palette="Dark2")+coord_flip()+labs(y= "Minutes", x = "Customer type")


ggarrange(p1, p2, 
          labels = c("Total duration per customer type", "Mean duration per customer type"),
          ncol = 1, nrow = 2,common.legend = TRUE)


#mean trip duration by month
dfd1<- group_by(all_trips_data_v2, member_casual, month)
dfd1<- summarise(dfd1, average_trip_duration = mean(duration))
fig1<-ggplot(dfd1, aes(x = month, y = average_trip_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  theme(axis.text.x = element_text(angle = 90))+scale_fill_brewer(palette="Dark2")


#mean trip duration by day
dfd2<- group_by(all_trips_data_v2, member_casual, day_of_the_week) 
dfd2<- summarise(dfd2, average_trip_duration = mean(duration)) 
fig2<- ggplot(dfd2, aes(x = day_of_the_week, y = average_trip_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5))+
  theme(axis.text.x = element_text(angle = 90))+scale_fill_brewer(palette="Dark2")


ggarrange(fig1, fig2, 
          labels = c("Mean trip duration by month", "Mean trip duration by day"),
          ncol = 1, nrow = 2,common.legend = TRUE)



#trip by hour
all_trips_data_v2 %>%  
  group_by(member_casual, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = member_casual, group = member_casual)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")

