wd <- "~/Documents/hackdavis2023" # kyra pop wd
setwd(wd)
source("./1_data_explore.R")
library(shiny)
if (!require(leaflet))remotes::install_github("rstudio/leaflet")
if(!require(magrittr))install.packages("magittr")
if(!require(htmltools))remotes::install_github("rstudio/htmltools")
if(!require(bslib))install.packages("bslib")

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "quartz"),
  # titlePanel("US State Data Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("general_vars", "General:", choices = unique(cats$label[cats$category == "general"]), multiple = TRUE),
      selectInput("income_vars", "Income:", choices = unique(cats$label[cats$category == "income"]), multiple = TRUE),
      selectInput("housing_vars", "Housing:", choices = unique(cats$label[cats$category == "housing"]), multiple = TRUE),
      selectInput("living_costs_vars", "Living Costs:", choices = unique(cats$label[cats$category == "living.costs"]), multiple = TRUE),
      selectInput("law_enforcement_vars", "Policing and crime:", choices = unique(cats$label[cats$category == "law.enforcement"]), multiple = TRUE),
      selectInput("policy_vars", "Safety and policy:", choices = unique(cats$label[cats$category == "politics"]), multiple = TRUE),
      selectInput("health_vars", "Health:", choices = unique(cats$label[cats$category == "health"]), multiple = TRUE),
      selectInput("clim_vars", "Climate:", choices = unique(cats$label[cats$category == "climate"]), multiple = TRUE),
      actionButton("update_map", "update map")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # reading data
  data <- states
  
  
  # initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(data = states, stroke = T, fillOpacity = 0.5, color = "lightpink",
                  popup = paste0("<b>",data$State,"</b>","<br>") ) %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 2.5)
  })
  
  # update information based on selected state and variables
  observeEvent(input$update_map, {
    
    # get the selected variables
    selected_general <- input$general_vars
    selected_income <- input$income_vars
    selected_housing <- input$housing_vars
    selected_living_costs <- input$living_costs_vars
    selected_law_enforcement <- input$law_enforcement_vars
    selected_policy <- input$policy_vars
    selected_climate <- input$clim_vars
    selected_health <- input$health_vars
    
    # Combine all selected variables into one vector
    selected_labels <- c(selected_general, selected_income, selected_housing,
                         selected_living_costs, selected_law_enforcement, 
                         selected_policy, selected_climate,selected_health)
    selected_variables <- cats$name[cats$label %in% selected_labels] 
    
    # Filter the data based on selected variables
    filtered_data <- data[,selected_variables]
    pop <- paste0("<b>",data$State,"</b>","<br>") 
    for (v in selected_variables) {
      variable_value <- data[[v]]
      l <- cats$label[cats$name == v] 
      pop <- paste(pop,l, ": ", variable_value, "<br>")
    }
    
    # Update the map
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = filtered_data, stroke = TRUE, fillOpacity = 0.5, color = "lightpink",
                  popup = pop) %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 2.5)
  })
  
  
}

# Run the app
shinyApp(ui, server)

