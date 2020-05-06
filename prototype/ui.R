# shiny app demo showing SERC and STRI water quality, water level, and met data
# UI dashboard uses shinydashboard package 

header <- dashboardHeader(
  title = "MarineGEO Sensor Dashboard Prototype",
  titleWidth = 450
)

body <- dashboardBody(
  tags$script(jscode),
  fluidRow(
    box(status = "primary",
        solidHeader = F,
        collapsible = T,
        width = 12,
        fluidRow(
          column(width = 2, align = "center",
                 div(img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250))),
          column(width = 8, offset = 2,  align = "center", "Beta version: Please contact marinegeo@si.edu with any questions or issues."))),
    
    # column(width = 3,
           box(width = 3, status = "primary",
               # Dropdown selector for sites
               selectInput("site", "Select one or more sites to display",
                           unique(key$site), multiple = TRUE),

               # Does not currently update dynamically: All sensors will be listed if a site is selected that doesn't have all possible sensors
               selectInput("sensor", "Select a sensor", c("none selected", unique(key$sensor))),

               # select a date range
               # dateRangeInput("date_range", "Select a date range",
               #                start = "2013-01-01"),
               uiOutput("date_range"),

               # # button that triggers data import, if necessary, and triggers plot creation
               # actionButton("runQuery2", "Display Data Availability"),
               
               # conditional dropdown for sensor parameters at selected site and sensor
               uiOutput('parameterSelector'),

               # button that triggers data import, if necessary, and triggers plot creation
               actionButton("runQuery", "Visualize data"), 
               
               div(hr()),
               
               downloadButton("download", "Download plotted data"),
               
               div(hr()),
               
               downloadButton("downloadFigure", "Download figure")
           )
           # box(width = NULL, status = "primary",
           #     downloadButton("downloadData", "Download Data") # Button to download the data for selected time span
           # )

    # )
    ,
           tabBox(
             title = "Data Visualization",
             width = 9,
             # The id lets us use input$dataviz on the server to find the current tab
             id = "dataviz", height = "550px",
             tabPanel(value = "dataAvailability",
                      "Available Data",
                      plotOutput("availability",
                                 brush = brushOpts(id = "plot_brush",
                                                   direction = "x")),
                      verbatimTextOutput("info")),
             tabPanel(value = "dataPlot",
                      "Plot Data",
                      plotOutput("plot"))
           )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)