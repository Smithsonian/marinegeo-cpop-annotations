library(shiny)
library(plotly)
library(tidyverse)

df <- read_csv("./MGEO_SMS_ExoTable_bundle.csv") 
df$site <- c(rep("-3", 10000), rep("0", 50000), rep("2", 23906))
df$site <- factor(df$site, levels =c("-3","0","2"))

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  
  #nms <- row.names(mtcars)
  
  output$plot <- renderPlotly({
    # df <- data.frame(value = c(rnorm(100,1,1), rnorm(100,2,1), rnorm(100,3,1),
    #                            rnorm(100,3,1), rnorm(100,1,1), rnorm(100,2,1),
    #                            rnorm(100,2,1), rnorm(100,3,1), rnorm(100,1,1)),
    #                  class = c(rep("c1",100), rep("c2",100), rep("c3",100),
    #                            rep("c2",100), rep("c4",100), rep("c1",100),
    #                            rep("c4",100), rep("c3",100), rep("c2",100)),
    #                  group = c(rep("g1",300), rep("g2",300), rep("g3",300)))
    # 
    # df$class <- factor(df$class, levels =c("c1","c2","c3","c4"))
    # df$group <- factor(df$group, levels =c("g1","g2","g3"))
    # 
    # #library(dplyr)
    # #library(ggplot2)
    # #library(plotly)
    # 
    # plot.list <- lapply(c("g1","g2","g3"), function(g){
    #   density.df <- do.call(rbind,lapply(unique(dplyr::filter(df, group == g)$class),function(l)
    #     ggplot_build(ggplot(dplyr::filter(df, group == g & class == l),aes(x=value))+geom_density(adjust=1,colour="#A9A9A9"))$data[[1]] %>%
    #       dplyr::select(x,y) %>% dplyr::mutate(class = l)))
    #   
    #   p <- plot_ly(data = density.df, x = ~x, y = ~y, type = 'scatter', color = ~class, legendgroup = ~class, showlegend = FALSE) %>%
    #     layout(xaxis = list(zeroline = F), yaxis = list(zeroline = FALSE)) %>%
    #     add_annotations(
    #       text = g,
    #       x = 0.5,
    #       y = 1.1,
    #       yref = "paper",
    #       xref = "paper",
    #       xanchor = "middle",
    #       yanchor = "top",
    #       showarrow = FALSE,
    #       font = list(size = 15)
    #     )
    #   if(g == "g1"){
    #     dummy_df <- data.frame(class = unique(df$class))
    #     dummy_df$x <- density.df$x[1]
    #     dummy_df$y <- density.df$y[1]
    #     p <- add_trace(p, data = dummy_df, x = ~x, y = ~y, color = ~class, type = "scatter", mode = "lines", showlegend = TRUE, legendgroup = ~class, hoverinfo = 'none')
    #   }
    #   p
    # })
    # 
    # subplot(plot.list, nrows = length(plot.list), shareX = TRUE)
    
    a <- plot_ly(df, x = ~TIMESTAMP, y = ~Temp_C,
            color = ~site, # Format Codes or Flags to code or flag, respectively
            #key=~RECORD, type = "scatter") %>% #, legendgroup = ~site) %>%
            key=~RECORD, type = "scatter", legendgroup = ~site,
            showlegend = F) %>%

      # rangeslider(type = "date"
      #             #borderwidth = 1,
      #             #thickness = .15) %>%
      #             #yaxis = list(
      #             #range = c(40,60)
      # ) %>%
      #layout(legend = list(orientation = 'h'), # https://plotly.com/python/reference/layout/#layout-legend
             #y = -.6),
             #yaxis = list(title = input$parameter_qc),
             # xaxis = list(title = "", # https://plotly.com/python/reference/layout/xaxis/
             #              range = c(input$start_date, as.Date(date_range_max()))
             #              # rangeselector = list(
             #              #   buttons = list(
             #              #     list(count = 24, label = "1 day", step = "hour", stepmode = "todate"),
             #              #     list(count = 3, label = "3 days", step = "day", stepmode = "todate"),
             #              #     list(count = 7, label = "1 wk", step = "day", stepmode = "todate"),
             #              #     list(count = 1, label = "1 mo", step = "month", stepmode = "todate")
             #              #   )
             #              # )
             # ),
             # rangeslider = list(bgcolor = '#000',
             #                    type = "date")),
      #       showlegend = TRUE) %>%
        #layout(showlegend = F) %>%
    toWebGL()

    dummy_df <- data.frame(site = unique(df$site))
    dummy_df$x <- df$TIMESTAMP[1]
    dummy_df$y <- df$Temp_C[1]
    dummy_df$RECORD <- df$RECORD[1]

    b <- plot_ly(df, x = ~TIMESTAMP, y = ~Chlorophyll_RFU,
                          color = ~site, # Format Codes or Flags to code or flag, respectively
                          #key=~RECORD, type = "scatter") %>%#, legendgroup = ~site, showlegend = F) %>%
                          key=~RECORD, type = "scatter", legendgroup = ~site,
                 showlegend = F) %>%

      # rangeslider(type = "date"
      #             #borderwidth = 1,
      #             #thickness = .15) %>%
      #             #yaxis = list(
      #             #range = c(40,60)
      # ) %>%
      # layout(legend = list(orientation = 'h'), # https://plotly.com/python/reference/layout/#layout-legend
      #        #y = -.6),
      #        yaxis = list(title = input$parameter_qc),
      #        xaxis = list(title = "", # https://plotly.com/python/reference/layout/xaxis/
      #                     range = c(input$start_date, as.Date(date_range_max()))
    #                     # rangeselector = list(
    #                     #   buttons = list(
    #                     #     list(count = 24, label = "1 day", step = "hour", stepmode = "todate"),
    #                     #     list(count = 3, label = "3 days", step = "day", stepmode = "todate"),
    #                     #     list(count = 7, label = "1 wk", step = "day", stepmode = "todate"),
    #                     #     list(count = 1, label = "1 mo", step = "month", stepmode = "todate")
    #                     #   )
    #                     # )
    #        ),
    #        # rangeslider = list(bgcolor = '#000',
    #        #                    type = "date")),
    #     layout(showlegend = F) %>%
    toWebGL()

    a <- add_trace(a, data = dummy_df, x = ~x, y = ~y, color = ~site, type = "scatter", showlegend = TRUE, legendgroup = ~site, hoverinfo = 'none')

    subplot(a,b, nrows = 2, shareX = T)
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (!is.null(d)) d
  })

  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (!is.null(d)) d
  })

}

shinyApp(ui, server)