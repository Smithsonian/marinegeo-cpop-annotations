ui <- fluidPage(
  
  titlePanel("Sensor Dashboard Prototype"),
  
  sidebarLayout(
    
    sidebarPanel(
      # Dropdown selector for sites
      selectInput("site", "Select one or more sites to display", 
                  unique(key$site), multiple = TRUE), 
      
      # Does not currently update dynamically: All sensors will be listed if a site is selected that doesn't have all possible sensors
      selectInput("sensor", "Select a sensor", c("none selected", unique(key$sensor))), 
      
      # select a time range 
      dateRangeInput("date_range", "Select a time range",
                     start = "2017-03-18"),
      
      # conditional dropdown for sensor parameters at selected site and sensor
      uiOutput('parameterSelector'),
      
      # button that triggers data import, if necessary, and triggers plot creation 
      actionButton("runQuery", "Visualize data"),
      
      width = 3 # the default is 4 (of 12 total units)
    ),
    
    mainPanel(
      plotOutput("plot", height = "800px")
    )
  )
)
