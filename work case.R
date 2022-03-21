install.packages("tidyverse")
install.packages("skimr")
install.packages("lubridate")
install.packages("janitor")


library("tidyverse")
library(lubridate)
library(skimr)
library(janitor)

Library ()
divvy202101 <- read_csv("202101-divvy-tripdata.csv")
divvy202102 <- read_csv("202102-divvy-tripdata.csv")
divvy202103 <- read_csv("202103-divvy-tripdata.csv")
divvy202104 <- read_csv("202104-divvy-tripdata.csv")
divvy202105 <- read_csv("202105-divvy-tripdata.csv")
divvy202106 <- read_csv("202106-divvy-tripdata.csv")
divvy202107 <- read_csv("202107-divvy-tripdata.csv")
divvy202108 <- read_csv("202108-divvy-tripdata.csv")
divvy202109 <- read_csv("202109-divvy-tripdata.csv")
divvy202110 <- read_csv("202110-divvy-tripdata.csv")
divvy202111 <- read_csv("202111-divvy-tripdata.csv")
divvy202112 <- read_csv("202112-divvy-tripdata.csv")

str(divvy202101)
install.packages("janitor")
library(janitor)
compare_df_cols(
  divvy202101,
  divvy202102,
  divvy202103,
  divvy202104,
  divvy202105,
  divvy202106,
  divvy202107,
  divvy202108,
  divvy202109,
  divvy202110,
  divvy202111,
  divvy202112, return = "mismatch"
  
)
divvy2021 <- bind_rows(
                       divvy202101,
                       divvy202102,
                       divvy202103,
                       divvy202104,
                       divvy202105,
                       divvy202106,
                       divvy202107,
                       divvy202108,
                       divvy202109,
                       divvy202110,
                       divvy202111,
                       divvy202112 )


install.packages("skimr")
library(skimr)

view(divvy202101)
col(divvy2021)
summary(divvy202101)
head(divvy2021)
dim(divvy202101)
skim(divvy2021)

divvy2021 <- divvy2021 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
skim(divvy2021)


install.packages("lubridate")
library(lubridate)

divvy2021$date <- as.Date(divvy2021$started_at)
divvy2021$month <- format(as.Date(divvy2021$date), "%m")
divvy2021$day <- format(as.Date(divvy2021$started_at), "%d")
divvy2021$day_in_week <- format(as.Date(divvy2021$started_at), "%A")

divvy2021$ride_time_length <- difftime(divvy2021$ended_at, divvy2021$started_at)


skim(divvy2021$ride_time_length)
divvy2021 <- divvy2021[!(divvy2021$ride_time_length < 0),]
skim(divvy2021$ride_time_length)
table(divvy2021$member_casual)



install.packages("ggplot2")
library(ggplot2)

ggplot(data = divvy2021) + stat_count(mapping = aes(x= day_in_week, fill = member_casual), position = "dodge") + 
  ggtitle("Figure 2: Number of Rides by Rider Type and Day of the Week") + ylab("Number of Rides (1e + 05 = 100,000)") +
  xlab("Day of the Week") + labs(fill = "Legend") + theme(axis.text.x = element_text(angle = 45))



ggplot( data = divvy2021  ) +  aes(x = month, y = average_duration, fill = member_casual) +
  geom_col(position = "dodge") + 
  labs(title = "Average duration by rider type", 
       subtitle = "Sorted by month")



divvy202101 <-  divvy202101 %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))


aggregate(divvy2021$ride_time_length ~ divvy2021$member_casual, FUN = median)
aggregate(divvy2021$ride_time_length ~ divvy2021$member_casual + divvy2021$day_in_week, FUN = mean)
# Notice that the days of the week are out of order. Let's fix that.
divvy2021$day_in_week <- ordered(divvy2021$day_in_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

counts <- aggregate(divvy2021$ride_time_length ~ divvy2021$member_casual + divvy2021$day_in_week, FUN = mean)
write.csv(counts, file = "/Volumes/Hard/R projects/counts.csv")

type_median <- aggregate(divvy2021$n() ~ divvy2021$member_casual)
write.csv(type_median, file = "/Volumes/Hard/R projects/member_casual_time_ave.csv")



# analyze ridership data by type and weekday

divvy2021 %>% 
  group_by(member_casual, rideable_type) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n() ,average_duration = mean(ride_time_length)) %>%    #calculates the number of rides and average duration 
  arrange(member_casual, day_in_week)


rides_number <- divvy2021 %>% 
  group_by(member_casual, day_in_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n() ,average_duration = mean(ride_time_length)) %>%    #calculates the number of rides and average duration 
  arrange(member_casual, day_in_week)

write.csv(rides_number, file = "/Volumes/Hard/R projects/rides_number_by_type.csv")




divvy2021 %>% 
  group_by(member_casual, day_in_week) %>%  
  summarise(number_of_rides = n() ,average_duration = mean(ride_time_length)) %>% 
  arrange(member_casual, day_in_week) %>% 
  ggplot(aes(x = day_in_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average duration by rider type",
       subtitle = "Sorted by weekday") + 
  theme(axis.text.x = element_text(angle = 45))


view(ccc) <- divvy2021 %>% 
  group_by(members_casual, ride_id) %>%  
  summarise(number_of_rides = n() ,ride_id) %>% 
  arrange(member_casual, month) %>% 
ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average duration by rider type", subtitle = "Sorted by month")



# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of rides by rider type",
    subtitle = "Sorted by weekday"
  )


# Let's create a visualization for average duration
divvy2021 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  average_duration = mean(divvy2021$ride_time_length ) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average duration by rider type",
       subtitle = "Sorted by weekday")


average_duration <- mean(ride_time_length)
# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average duration by rider type", 
       subtitle = "Sorted by month")










ggplot(data = divvy202101) + 
geom_point(mapping = aes(x = member_casual , y = ride_time_length ))





write.csv(divvy2021, file = "/Volumes/Hard/R projects/Data.csv")



divvy202101$ride_time_length <- difftime(divvy202101$ended_at, divvy202101$started_at)
divvy2021$ride_time_length  <- as.numeric(as.character( divvy2021$ride_time_length))

is.numeric(divvy2021$ride_time_length)

divvy202101$month <- format(as.Date(divvy202101$date), "%m")
divvy202101$dayweek <- format(as.Date(divvy202101$started_at), "%A")
divvy202101 <- divvy202101 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
view(divvy202101)
divvy <- divvy202101 %>%
  add_column(Add_Column = difftime(ended_at, started_at, units = "minutes"))
difftime("2020-5-16", "2020-1-15", units = "days")
