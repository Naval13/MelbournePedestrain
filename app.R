library(shiny)
library(leaflet)
library(sp)
#library(sf)
library(maps)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(leaflet)
library(maps)


#Reading the dataset and storing in a variable named ped_count and ped_sensor_location
ped_count <- read.csv('Pedestrian_Counting_System_2019.csv')
ped_sensor_location <- read.csv('Pedestrian_Counting_System_-_Sensor_Locations.csv')

# Joining both the datasets, by doing a left outer join 
pedestrain <- merge(ped_count, ped_sensor_location, by.x = 'Sensor_Name', by.y = 'sensor_name', all.x = FALSE, all.y = FALSE)


#Arranging the days of week according to the week days

dayLabs<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") 
ped_count$Day <- factor(ped_count$Day, levels= dayLabs)
ped_count<-ped_count[order(ped_count$Day), ]

ui <- fluidPage(
  # Creating a drop down menu for all Sensor_Name and All Sensors 
  selectInput("Sensor_Name", "Sensor_Name",choices = c("All Sensors", c(unique(pedestrain$Sensor_Name)))),
  
  #Main panel 
  mainPanel(
    plotOutput("Pedestrain_Graph"),
    #SiteMap
    leafletOutput("map", height="500px")
    )
)
server <- function(input, output, session) {
  # Aggregating the Hourly_Counts as per their mean values
  sensor_hourly_count<-aggregate(ped_count$Hourly_Counts, by=list(Sensor_Name = ped_count$Sensor_Name), FUN=mean)
  colnames(sensor_hourly_count)<-c("sensor_name","Hourly_Counts")
  merged_df <- merge(ped_sensor_location,sensor_hourly_count,by.y="sensor_name")
  
  #Changing the dataset as numeric
  merged_df$latitude <- as.numeric(merged_df$latitude)
  merged_df$longitude <- as.numeric(merged_df$longitude)
  merged_df$Hourly_Counts<-merged_df$Hourly_Counts/100
  
  #Filtering data
  required_data <- reactive({
    click<-input$Sensor_Name
    # Checking the dropdown menu click and updating the plot as per the selected Sensor_Name
        if(is.null(click)){
         return(merged_df)
        }else {
          # Filtering the data as per the selected Sensor_Name
        select_field <- pedestrain_mean_by_name<-ped_count %>%
          filter(Sensor_Name == input$Sensor_Name) %>%
          group_by(Day, Time) %>%
          summarise_at(vars(Hourly_Counts), funs(mean(., na.rm=TRUE)))
        
          return(select_field)
    }

    
    })
  
  #Pedestrain graph Output
  output$Pedestrain_Graph <- renderPlot({

    # Setting data of line charts as per all data or selected sensor data
    if (input$Sensor_Name == "All Sensors") {
      pedestrain_mean<-ped_count %>%
          group_by(Day, Time) %>%
          summarise_at(vars(Hourly_Counts), funs(mean(., na.rm=TRUE)))
    
    ggplot(data = pedestrain_mean, aes(x=Time, y=Hourly_Counts)) +
      geom_line() + labs(title = "Plotting Pedestrian Counting System") + facet_wrap(~Day)
    } else {
    ggplot(data = required_data(), aes(x=Time, y=Hourly_Counts)) +
     geom_line()+labs(title = "Plotting Pedestrian Counting System")+ facet_wrap(~Day)
    }
  })
  
  output$map <- renderLeaflet({
    # Ploting the map of Pedestrain Counting Sysstem
    mapp<-leaflet(data = merged_df) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%  
    
      # layerId: is used to get the click of the map event and then updating the line charts accordingly 
      # radius=~Hourly_Counts to show the proportional values
      addCircleMarkers(lat = ~latitude, lng = ~longitude, 
                     popup = ~sensor_name,
                     radius=~Hourly_Counts, 
                     color="green",
                     fillOpacity = 0.3, 
                     layerId=~sensor_name,
                     labelOptions = labelOptions(noHide = T))
  })
  

}

# Running the Shiny App
shinyApp(ui, server)
