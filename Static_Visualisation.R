library(ggplot2)
library(dplyr)


ped_count <- read.csv('Pedestrian_Counting_System_2019.csv')
ped_sensor_location <- read.csv('Pedestrian_Counting_System_-_Sensor_Locations.csv')

pedestrain <- merge(ped_count, ped_sensor_location, by.x = 'Sensor_Name', by.y = 'sensor_name', all.x = FALSE, all.y = FALSE)


pedestrain_mean<-ped_count %>% 
  group_by(Day, Time) %>%
  summarise_at(vars(Hourly_Counts), funs(mean(., na.rm=TRUE)))


ggplot(data = pedestrain_mean, aes(x=Time, y=Hourly_Counts)) + 
  geom_line()+labs(title = "Plotting Pedestrian Counting System")+ facet_wrap(~Day)
