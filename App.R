library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(remotes)
#install_github("yonghah/esri2sf")
library(esri2sf)
setwd("C:/Users/p99br/Desktop/CSE281/EW_Shiny_App/")

#energy data frame(s)
energy_df <- read.csv("AnnualData.csv")
sample_energy_df <- read.csv("sampleEnergyData.csv")
sample_energy_df$time <- ymd_hms(sample_energy_df$time)

#building metadata
bldng_metadata_all <- esri2sf(
  "https://services.arcgis.com/VXxCfMpXUKuFKFvE/arcgis/rest/services/Building/FeatureServer/0",
)
bldng_metadata <- bldng_metadata_all[, c("BUILDINGID", "BUILDINGTY", "GROSSAREA","NETAREA", "YEARBUILT",  "Shape__Area")]

energy_building_metadata <- merge(sample_energy_df, bldng_metadata, by="BUILDINGID" )

# demoing group support in the `choices` arg
shinyApp(
  ui = fluidPage(
    selectizeInput('BUILDINGID', 'Select BUILDINGID', choices = c("choose" = "", unique(sample_energy_df$BUILDINGID))),
    sliderInput("slider", "Number of observations:", 1800, max(energy_building_metadata$YEARBUILT), 1950),
    checkboxGroupInput("variable", "Variables to show:",
                       unique(energy_building_metadata$BUILDINGTY)),
    mainPanel(
      plotOutput("ts_plot"),
      plotOutput("ts_plot_line"),
      #tableOutput("table")
    )
  ),
  server = function(input, output) {
    
    filter_BID <- reactive({
      
      sample_energy_df %>% filter(BUILDINGID == input$BUILDINGID)
    })
    
    # plot time series
    output$ts_plot <- renderPlot({
      
      sample_energy_df <- filter_BID()
      ggplot(sample_energy_df, aes(x = time, y=WattHours)) + geom_area()
       
      #+ scale_x_date(date_labels = "%b %Y")
      
    })
    output$ts_plot_line <- renderPlot({
      
      sample_energy_df <- filter_BID()
      ggplot(sample_energy_df, aes(x = time, y=WattHours)) + geom_line()
      #+ geom_area() 
      #+ scale_x_date(date_labels = "%b %Y")
      
    })
    
    output$table <- renderTable({
      filter_BID()
    })
      
    #output$areaChart <- renderPlot({
    #  plot(mtcars$wt, mtcars$mpg)
    #})
  }
)