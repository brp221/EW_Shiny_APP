library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(remotes)
#install_github("yonghah/esri2sf")
library(esri2sf)
#setwd("C:/Users/p99br/Desktop/CSE281/EW_Shiny_App/")
setwd("/Users/bratislavpetkovic/Desktop/CSE_COURSES/CSE281/EW_Shiny_App/")


#energy data frame(s)
#energy_df <- read.csv("AnnualData.csv")
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
    sliderInput("YearBuilt", "Number of observations:", 1800, max(energy_building_metadata$YEARBUILT), 1950),
    checkboxGroupInput("BTYPES", "Building Types:",
                       unique(energy_building_metadata$BUILDINGTY)),
    mainPanel(
      plotOutput("ts_plot"),
      plotOutput("ts_plot_line"),
      plotOutput("btype_avg_wh"),
      #tableOutput("table")
    )
  ),
  server = function(input, output) {
    
    filter_BID <- reactive({
      
      sample_energy_df %>% filter(BUILDINGID == input$BUILDINGID)
    })
    filter_BTYPE <- reactive({
      
      energy_building_metadata %>% filter(BUILDINGTY == input$BTYPES)
      
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
    output$btype_avg_wh <- renderPlot({
      energy_building_metadata <- filter_BTYPE()
      avg_watt_btype <- energy_building_metadata %>% group_by(BUILDINGTY)%>% summarise(WattHours = mean(WattHours))
      
      # 3: Using RColorBrewer
      ggplot(energy_building_metadata, aes(x=BUILDINGTY, y = WattHours,  fill=BUILDINGTY )) + 
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Set1") +
        theme(legend.position="none")
    })
    
    output$table <- renderTable({
      filter_BID()
    })
      
    #output$areaChart <- renderPlot({
    #  plot(mtcars$wt, mtcars$mpg)
    #})
  }
)