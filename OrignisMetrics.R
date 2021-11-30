library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(remotes)
#install_github("yonghah/esri2sf", force = 'TRUE')
library(esri2sf)
library(shinydashboard)
library(arcgisbinding)
library(sf)
library(sp)

setwd("C:/Users/p99br/Desktop/CSE281/EW_Shiny_App/")
#setwd("/Users/bratislavpetkovic/Desktop/CSE_COURSES/CSE281/EW_Shiny_App/")

arc.check_product()

# energy dataframes 
originis <- arc.select(arc.open("C:/Users/p99br/Desktop/CSE281/EW_Shiny_APP/EW_Shiny_App/EW_Shiny_App.gdb/Originis"))
#initium <- arc.select(arc.open("C:/Users/p99br/Desktop/CSE281/EW_Shiny_APP/EW_Shiny_App/EW_Shiny_App.gdb/Initium"))
sample_energy_df <- read.csv("sampleEnergyData.csv")
#https://services.arcgis.com/VXxCfMpXUKuFKFvE/arcgis/rest/services/Initium/FeatureServer/0
#https://services.arcgis.com/VXxCfMpXUKuFKFvE/arcgis/rest/services/Originis/FeatureServer/0


#convert time into appropriate date format 
sample_energy_df$time <- ymd_hms(sample_energy_df$time)
originis$time <-ymd_hms(originis$time)
#initium$time <-ymd_hms(initium$time)


#building metadata directly from AGOL
bldng_metadata_all <- esri2sf(
  "https://services.arcgis.com/VXxCfMpXUKuFKFvE/arcgis/rest/services/Building/FeatureServer/0",
)
bldng_metadata <- bldng_metadata_all[, c("BUILDINGID", "BUILDINGTY", "GROSSAREA","NETAREA", "YEARBUILT",  "Shape__Area")]

#merge the dataframes to create energy building metadata
originis_bldg_md <- merge(originis, bldng_metadata, by="BUILDINGID" )



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
                  selectizeInput('BUILDINGID', 'Select BUILDINGID', choices = c("choose" = "", unique(originis_bldg_md$BUILDINGID)),selected= "0019"),
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
                  sliderInput("YEARBUILT", "Year Built:", 1800, max(originis_bldg_md$YEARBUILT), value = c(1800,2017)),
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
                  checkboxGroupInput("BTYPES", "Building Types:", unique(originis_bldg_md$BUILDINGTY), selected =unique(originis_bldg_md$BUILDINGTY)),
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
                  selectizeInput('BUILDINGTY', 'Select BUILDINGTY', choices = c("choose" = "", unique(originis_bldg_md$BUILDINGTY))),
                  actionButton("Filter_0s", "Filter 0 Values"),
                )
              )
      )
    )
  )
)
server <- function(input, output) {
  
  filter_BID <- reactive({
    originis_bldg_md %>% filter(BUILDINGID == input$BUILDINGID)
  })
  filter_year_built <- reactive({
    originis_bldg_md %>% filter(YEARBUILT > input$YEARBUILT[1], YEARBUILT < input$YEARBUILT[2])
  })
  filter_BTYPE <- reactive({
    originis_bldg_md %>% filter(BUILDINGTY == input$BTYPES)
  })
  filter_BTYPE_group <- reactive({
    originis_bldg_md %>% filter(BUILDINGTY == input$BUILDINGTY)
  })
  filter_0_vals <- eventReactive(input$Filter_0s, {
    originis_bldg_md %>% filter(wathtot != 0,BUILDINGTY == input$BUILDINGTY )
    #runif(input$BUILDINGTY)
  })
  # plot time series
  output$ts_plot <- renderPlot({
    originis_bldg_md <- filter_BID()
    ggplot(originis_bldg_md, aes(x = time, y=wathtot)) + geom_area()
    #+ scale_x_date(date_labels = "%b %Y")
  })
  #dynamically changing title of the ts plot based on BID value
  output$ts_BID_title <- renderText(bldng_metadata_all$SHORTNAME[bldng_metadata_all$BUILDINGID==input$BUILDINGID])
  
  output$scatter_plot_year <- renderPlot({
    originis_bldg_md <- filter_year_built()
    sum_wathtot_date_range <- originis_bldg_md %>% group_by(BUILDINGID)%>% summarise(wathtot = mean(wathtot))
    # A basic scatterplot with color depending on Species
    ggplot(originis_bldg_md, aes(x=YEARBUILT, y=wathtot, color=BUILDINGTY)) + 
      geom_point(size=2) 
  })
  # avg wathtot by building type bar chart 
  output$btype_avg_wh <- renderPlot({
    originis_bldg_md <- filter_BTYPE()
    avg_watt_btype <- originis_bldg_md %>% group_by(BUILDINGTY)%>% summarise(wathtot = mean(wathtot))
    
    # 3: Using RColorBrewer
    ggplot(originis_bldg_md, aes(x=BUILDINGTY, y = wathtot,  fill=BUILDINGTY )) + 
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position="none")
  })
  
  output$boxplot_btype <- renderPlot({
    originis_bldg_md <- filter_BTYPE_group()
    originis_bldg_md <- filter_0_vals()
    ggplot(originis_bldg_md, aes(x=BUILDINGTY, y=wathtot)) + 
      geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2))
    
  })
  
}
shinyApp(ui,server)

