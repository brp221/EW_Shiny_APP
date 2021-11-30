library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(remotes)
install_github("yonghah/esri2sf", force = 'TRUE')
library(esri2sf)
library(shinydashboard)
library(arcgisbinding)

setwd("C:/Users/p99br/Desktop/CSE281/EW_Shiny_App/")
#setwd("/Users/bratislavpetkovic/Desktop/CSE_COURSES/CSE281/EW_Shiny_App/")

arc.check_product()

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
ui <- dashboardPage(
  dashboardHeader(title = "Lehigh Energy Dashboard"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("lightbulb", tabName = "lightbulb", icon = icon("lightbulb")),
    menuItem("clock", tabName = "clock", icon = icon("clock")),
    menuItem("plug", tabName = "plug", icon = icon("plug"))
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title= textOutput('ts_BID_title'),
                  plotOutput("ts_plot", height = 250)
                ),
                
                box(
                  title= "Controls",
                  #radio to select by buildingID
                  selectizeInput('BUILDINGID', 'Select BUILDINGID', choices = c("choose" = "", unique(sample_energy_df$BUILDINGID)),selected= "0019"),
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
      ),
      # Third tab content
      tabItem(tabName = "clock",
              fluidRow(
                box(plotOutput("btype_avg_wh", height = 250),
                ),
                box(
                  title= "Controls",
                  #radio group to select buildings by their building type
                  checkboxGroupInput("BTYPES", "Building Types:", unique(energy_building_metadata$BUILDINGTY), selected =unique(energy_building_metadata$BUILDINGTY)),
                )
              )
      ),
      # Fourth tab content
      tabItem(tabName = "plug",
              fluidRow(
                box(
                  plotOutput("boxplot_btype", height = 250),
                ),
                box(
                  title= "Controls",
                  #radio to select by BUILDINGTY
                  selectizeInput('BUILDINGTY', 'Select BUILDINGTY', choices = c("choose" = "", unique(energy_building_metadata$BUILDINGTY))),
                  actionButton("Filter_0s", "Filter 0 Values"),
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
  filter_BTYPE <- reactive({
    energy_building_metadata %>% filter(BUILDINGTY == input$BTYPES)
  })
  filter_BTYPE_group <- reactive({
    energy_building_metadata %>% filter(BUILDINGTY == input$BUILDINGTY)
  })
  filter_0_vals <- eventReactive(input$Filter_0s, {
    energy_building_metadata %>% filter(WattHours != 0,BUILDINGTY == input$BUILDINGTY )
    #runif(input$BUILDINGTY)
  })
  # plot time series
  output$ts_plot <- renderPlot({
    energy_building_metadata <- filter_BID()
    ggplot(energy_building_metadata, aes(x = time, y=WattHours)) + geom_area()
    #+ scale_x_date(date_labels = "%b %Y")
  })
  #dynamically changing title of the ts plot based on BID value
  output$ts_BID_title <- renderText(bldng_metadata_all$SHORTNAME[bldng_metadata_all$BUILDINGID==input$BUILDINGID])
  
  output$scatter_plot_year <- renderPlot({
    energy_building_metadata <- filter_year_built()
    sum_watts_date_range <- energy_building_metadata %>% group_by(BUILDINGID)%>% summarise(WattHours = mean(WattHours))
    # A basic scatterplot with color depending on Species
    ggplot(energy_building_metadata, aes(x=YEARBUILT, y=WattHours, color=BUILDINGTY)) + 
      geom_point(size=2) 
  })
  # avg watts by building type bar chart 
  output$btype_avg_wh <- renderPlot({
    energy_building_metadata <- filter_BTYPE()
    avg_watt_btype <- energy_building_metadata %>% group_by(BUILDINGTY)%>% summarise(WattHours = mean(WattHours))
    
    # 3: Using RColorBrewer
    ggplot(energy_building_metadata, aes(x=BUILDINGTY, y = WattHours,  fill=BUILDINGTY )) + 
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position="none")
  })
  
  output$boxplot_btype <- renderPlot({
    energy_building_metadata <- filter_BTYPE_group()
    energy_building_metadata <- filter_0_vals()
    ggplot(energy_building_metadata, aes(x=BUILDINGTY, y=WattHours)) + 
      geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2))
    
  })
  
}
shinyApp(ui,server)