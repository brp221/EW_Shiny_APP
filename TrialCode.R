library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(remotes)
install_github("yonghah/esri2sf", force = 'TRUE')
library(esri2sf)
library(shinydashboard)
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

  
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("lightbulb", tabName = "lightbulb", icon = icon("lightbulb"))
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("ts_plot", height = 250)),
                
                box(
                  title= "Controls",
                  #radio to select by buildingID
                  selectizeInput('BUILDINGID', 'Select BUILDINGID', choices = c("choose" = "", unique(sample_energy_df$BUILDINGID))),
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "lightbulb",
              fluidRow(
                box(
                  plotOutput("scatter_plot_year", height = 250),
                ),
                box(
                  title= "Controls",
                  #slider to select buildings by yearbuilt 
                  sliderInput("YEARBUILT", "Year Built:", 1800, max(energy_building_metadata$YEARBUILT), value = c(1800,2017)),
                )
              )
      )
    )
  )
)
server <- function(input, output) {
  
  filter_BID <- reactive({
    energy_building_metadata %>% filter(BUILDINGID == input$BUILDINGID)
  })
  filter_year_built <- reactive({
    energy_building_metadata %>% filter(YEARBUILT > input$YEARBUILT[1], YEARBUILT < input$YEARBUILT[2])
  })
  # plot time series
  output$ts_plot <- renderPlot({
    energy_building_metadata <- filter_BID()
    ggplot(energy_building_metadata, aes(x = time, y=WattHours)) + geom_area()
    #+ scale_x_date(date_labels = "%b %Y")
  })
  output$scatter_plot_year <- renderPlot({
    energy_building_metadata <- filter_year_built()
    sum_watts_date_range <- energy_building_metadata %>% group_by(BUILDINGID)%>% summarise(WattHours = mean(WattHours))
    # A basic scatterplot with color depending on Species
    ggplot(energy_building_metadata, aes(x=YEARBUILT, y=WattHours, color=BUILDINGTY)) + 
      geom_point(size=2) 
  })
    
}
shinyApp(ui,server)

