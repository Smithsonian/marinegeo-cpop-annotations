# shiny app demo showing SERC and STRI water quality and met data

function(input, output) {
  
  # source functions employed by the app
  source("./R/app-functions.R", local = TRUE)
  
  # Turn key into a reactive dataframe to monitor which datasets have been imported
  data_tracker <- reactiveValues(df = key)
  
  # Initiate empty objects that will store each sensor type's data as they're imported
  MET_data <- reactiveValues(df = data.frame())
  WaterLevel_data <- reactiveValues(df = data.frame())
  WaterQuality_data <- reactiveValues(df = data.frame())
  
  # builds ui dropdown selector for the sensor variables from all the valid dataframe column names
  output$parameterSelector = renderUI({
    
    if(input$sensor == "none selected" | is.null(input$site)){
      
      div(
        "Select a site and sensor to view available parameters",
      
        selectInput('parameter', 'Parameter', choices=c(), 
                   multiple = TRUE)
        )
      
    } else{

      # select all the column names in csv that are not in ignore list
      # parameter_list <- colnames(getSensorData())[!names(getSensorData()) %in% ignore_list]
      parameter_list <- getParameters()
      
      # create the select input UI element with only the relevant parameters listed
      # the selected argument refers back to itself in order to remember what was selected if and when 
      # the input has to reactively update to another changing input (such as time frame) 
      
      selectInput('parameter', 'Parameter', choices=parameter_list, 
                  selected = input$parameter, 
                  multiple = TRUE)
      
    }
  })
  
  # Action to take if run query button pressed
  observeEvent(input$runQuery, {
    
    year_start <- year(input$date_range[1])
    year_end <- year(input$date_range[2])
    
    subset <- data_tracker$df %>%
      filter(sensor %in% input$sensor) %>%
      filter(site %in% input$site) %>%
      filter(year >= year_start & year <= year_end)
    
    # if any files are not yet imported
    if(nrow(subset) > 0 & !all(subset$imported)) {
      importFiles(subset)
    }
    
  })
  
  # use tabsetPanel 'id' argument to change tabs when plot data is selected
  # observeEvent(input$runQuery, {
  #   updateTabsetPanel(session, "dataviz", "dataPlot")
  #   })
  
  plotInput <- reactive({
    # Generates a facet grid plot for one to many parameters
    p <- getSensorData() %>%
      select(Timestamp, Site, input$parameter) %>%
      # melt the data to long form
      gather(key="variable", value = "measurement", -Timestamp, -Site, na.rm = TRUE) %>%
      ggplot(aes(Timestamp, measurement, color = Site)) + 
      geom_line() + 
      facet_grid(variable ~ ., scales = "free") + 
      theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
      ylab("")
  })
  
  # Generate a plot of the data 
  output$plot <- renderPlot({
    #if(is.null(input$parameter) | input$sensor == "none selected"){return(NULL)}
    if(input$runQuery == 0){return(NULL)}
    
    # Trigger plot creation when run query button is clicked
    input$runQuery
    
    # Isolate prevents graph from updating whenever other inputs change
    isolate({
      print(plotInput())
    })
  }, height = function(){
    input$GetScreenHeight * .6
  })
  
  output$availability <- renderPlot({

      # Generates a facet grid plot for one to many parameters
    date_log %>%
      select(Site, Sensor, Date_Rounded) %>%
      distinct() %>%
      ggplot(mapping = aes(x = Date_Rounded, y = Sensor, fill = Sensor)) +
      geom_tile(color = "white") +
      # coord_equal() +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            # plot.title = element_text(hjust = 0.5, size=24),
            # panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position="none", # position of legend or none
            # legend.direction="vertical", # orientation of legend
            legend.title= element_blank(), # no title for legend
            # axis.text.x=element_text(size=10, angle=90, vjust=0.5),
            # axis.text.y=element_text(size=10)
      ) +
      facet_wrap(~Site, dir = "v")
    
  }, height = function(){
    input$GetScreenHeight * .6
  })
  
  # this will be translated into making the date slider reactive
  output$info <- renderText({
    res <- brushedPoints(date_log, input$plot_brush)
    paste0("Date min = ", min(res$Date_Rounded), 
           "\nDate max = ", max(res$Date_Rounded))
  })
  
  # Download handler creates filename and prepares data for download
  output$download <- downloadHandler(
    filename = function(){
      paste0("marinegeo_cpop-", Sys.Date(), ".csv")
    },
    content = function(file){
      write.csv(getSensorData(), file)
    }
  )
  
  # Download the figure
  output$downloadFigure <- downloadHandler(
    filename = function(){
      paste0("marinegeo_cpop-", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    })
}