ui <- fluidPage(
  
  titlePanel("Sensor Dashboard Prototype"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("site", "Site:", site_list, multiple = TRUE), # Dropdown Selector for Sensor Dataset
      # Dropdown for datasets available overall
      # Does not currently update dynamically: All sensors will be listed if a site is selected that doesn't have all possible sensors
      selectInput("sensor", "Sensor:", c("none selected", sensor_list)), 
      selectInput("timeSpan", "View data from previous:",
                  c("3 hours" = 3, "6 hours" = 6, "12 hours" = 12, "1 day" = 24, "2 days" = 2*24, "3 days" = 3*24, 
                    "4 days" = 4*24, "5 days" = 5*24, "1 Week" = 7*24, "10 days" = 10*24, "2 Weeks" = 2*7*24, 
                    "3 Weeks" = 3*7*24, "1 Month" = 31*24, "6 Weeks" = 6*7*24, "2 Months" = 31*2*24, 
                    "year" = 24 * 365, "2 years" = 48 * 365), selected = 5*24),
      uiOutput('parameterSelector') # conditional dropdown for sensor parameters at selected site and sensor
    ),
    
    mainPanel(
      plotOutput("plot", height = "800px")
    )
  )
)
