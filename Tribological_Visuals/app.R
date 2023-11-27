library(shiny)
library(plotly)
library(DT)
library(tidyverse)

expsum <- readRDS("..//RDS_files//experiment_summaries.rds")
Data <- readRDS(file = "../RDS_files/data_df.rds")
# Sample data (replace this with your actual data)
sample_data <- data.frame(
  Distance = seq(1, 100, by = 5), #testDistance or TotalDistance in Data
  VolumeLost = rnorm(20, mean = 0, sd = 5), # 
  FrictionCoefficient = rnorm(20, mean = 0.5, sd = 0.1), #mu in Data or mu1 in APS
  CycleNumber = rep(1:5, each = 4), #time in APS
  Ktotal = rnorm(20, mean = 0, sd = 5), # all in Data
  K_MC1 = rnorm(20, mean = 0, sd = 5),
  K_MC2 = rnorm(20, mean = 0, sd = 5),
  K_MC3 = rnorm(20, mean = 0, sd = 5),
  K_MC4 = rnorm(20, mean = 0, sd = 5),
  K_MC5 = rnorm(20, mean = 0, sd = 5)
)

# Define UI
ui <- fluidPage(
  
  tags$style(HTML("
    body {
      background-color: #FDF7F7; 
    }
  ")),
  
  titlePanel("Data Visualizations for Tribological Experimental Data"),
  
  tabsetPanel(
    tabPanel("Experiment Summary Explorer Table", 
             mainPanel(
               DTOutput("table")  
    )),
    
    tabPanel("Experiment APS Scatter Plot Visualizations", 
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("plot_types", "Select Plot Types", c("Scatter Plot", "Bar Chart")),
          selectInput("sample", "Select Sample", choices = unique(sample_data$CycleNumber)),
          selectInput("scale2", "Select Scale", choices = c("Linear", "Log-Log", "Semi-Log")),
          checkboxInput("add_legend", "Add Legend", value = TRUE),
          textInput("point_shape", "Point Shape", value = 16),
          textInput("point_color", "Point Color", value = "blue"),
          textInput("line_color", "Line Color", value = "red")
        ),
        mainPanel(
          plotlyOutput("plot")
        )
      )
    ),
    
    tabPanel("Experiment Cycle Number Scatter Visualizations", 
             plotOutput("plot3")), ## Replace "plot3" with name of plot
    
    tabPanel("Experiment Bar Chart Visualizations",
          sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "experiment",
                           label = "Experiment",
                           choices=expsum$Experiment_Name),
               selectInput(inputId = "metric",
                           label = "Metric",
                           choices=c("COF", "Ktotal", "K_MC2", "K_MC3", "K_MC4", "K_MC5")),
               selectInput("scale", "Select Scale", choices = c("Linear", "Log")),
               textInput("bar_color", "Bar Color", value = "#B61E2E")
             ),
             mainPanel(
               plotlyOutput(outputId = "barPlot")
             )
          )
    )
))


metricMap <- list("COF" = "mu", "Ktotal" = "KtotalMonte", "K_MC2" = "KtestMonteN2", "K_MC3" = "KtestMonteN3", "K_MC4" = "KtestMonteN4", "K_MC5" = "KtestMonteN5")

# Define server
server <- function(input, output) {
  
  
  output$table <- renderDT({
    datatable(expsum) %>%
      formatRound(columns=c('mu', "Fn"), digits = 3) %>%
      formatSignif(columns = "wear_rate", digits =  2)
  })  
  
  
  output$plot <- renderPlotly({
    # Filter data based on selected sample
    filtered_data <- subset(sample_data, CycleNumber == input$sample)
    
    if ("Scatter Plot" %in% input$plot_types) {
      # Scatter plot
      scatter_plot <- plot_ly(data = filtered_data, x = ~Distance)
      scatter_plot <- add_trace(scatter_plot, y = ~VolumeLost, mode = "markers", type = "scatter", name = "Volume Lost")
      scatter_plot <- add_trace(scatter_plot, y = ~FrictionCoefficient, mode = "markers", type = "scatter", name = "Friction Coefficient")
      
    
      # Add error bars if applicable
      if ("VolumeLost" %in% colnames(filtered_data)) {
        scatter_plot <- scatter_plot %>% add_trace(y = ~VolumeLost, type = "scatter", mode = "lines",
                                                   line = list(color = input$line_color),
                                                   error_y = list(type = "data", array = c(1, 2, 1, 3)),
                                                   name = "Volume Lost (Error)")
      }
      
      if ("FrictionCoefficient" %in% colnames(filtered_data)) {
        scatter_plot <- scatter_plot %>% add_trace(y = ~FrictionCoefficient, type = "scatter", mode = "lines",
                                                   line = list(color = input$line_color),
                                                   error_y = list(type = "data", array = c(0.05, 0.1, 0.07, 0.15)),
                                                   name = "Friction Coefficient (Error)")
      }
      
      # Customize the plot
      scatter_plot <- scatter_plot %>% layout(
        title = "Scatter Plot",
        xaxis = list(title = "Distance Slid"),
        yaxis = list(title = "Values"),
        showlegend = input$add_legend,
        shapes = list(
          list(
            type = "circle",
            xref = "paper", yref = "paper",
            x0 = 0, x1 = 1, y0 = 0, y1 = 1,
            line = list(color = input$line_color)
          )
        ),
        markers = list(color = input$point_color, symbol = input$point_shape)
      )
    }
    
    if ("Bar Chart" %in% input$plot_types) {
      # Bar chart
      bar_chart <- plot_ly(data = filtered_data, x = ~CycleNumber, type = "bar")
      bar_chart <- add_trace(bar_chart, y = ~Ktotal, name = "Ktotal")
      bar_chart <- add_trace(bar_chart, y = ~FrictionCoefficient, name = "Average Friction Coefficient")
      bar_chart <- add_trace(bar_chart, y = ~K_MC2, name = "K_MC2")
      bar_chart <- add_trace(bar_chart, y = ~K_MC3, name = "K_MC3")
      bar_chart <- add_trace(bar_chart, y = ~K_MC4, name = "K_MC4")
      bar_chart <- add_trace(bar_chart, y = ~K_MC5, name = "K_MC5")
      
      # Customize the plot
      bar_chart <- bar_chart %>% layout(
        title = "Bar Chart",
        xaxis = list(title = "Cycle Number"),
        yaxis = list(title = "Values"),
        showlegend = input$add_legend,
        barmode = "group",
        shapes = list(
          list(
            type = "circle",
            xref = "paper", yref = "paper",
            x0 = 0, x1 = 1, y0 = 0, y1 = 1,
            line = list(color = input$line_color)
          )
        ),
        markers = list(color = input$point_color, symbol = input$point_shape)
      )
    }
    
    # Apply scale
    if (input$scale2 == "Log-Log") {
      scatter_plot <- scatter_plot %>% layout(xaxis = list(type = "log"), yaxis = list(type = "log"))
      bar_chart <- bar_chart %>% layout(xaxis = list(type = "log"), yaxis = list(type = "log"))
    } else if (input$scale == "Semi-Log") {
      scatter_plot <- scatter_plot %>% layout(yaxis = list(type = "log"))
      bar_chart <- bar_chart %>% layout(yaxis = list(type = "log"))
    }
    
    # Combine plots
    if ("Scatter Plot" %in% input$plot_types && "Bar Chart" %in% input$plot_types) {
      subplot(scatter_plot, bar_chart, nrows = 2)
    } else if ("Scatter Plot" %in% input$plot_types) {
      scatter_plot
    } else if ("Bar Chart" %in% input$plot_types) {
      bar_chart
    }
  })
  
  ## Add tab 3 plot3 plots
  
  ## Add tab 4 plot4 plots
  
  output$barPlot <- renderPlotly({
    selectedCol <- metricMap[input$metric][[1]]
    selectedData <- filter(Data, Experiment_Name == input$experiment) %>%
      rename(metric = unlist(selectedCol))
    maxExp <- max(selectedData$Experiment)
    plot <- ggplot(selectedData, aes(x=Experiment, y=metric)) +
      geom_col(fill = input$bar_color) +
      theme_minimal() +
      scale_x_continuous(limits=c(.5, maxExp+.5), breaks=1:maxExp) +
      labs(x="Cycle Number", y=input$metric)
    
    if(input$scale == "Log") {
      plot <- plot +
        scale_y_log10()
    }
    
    plot
  })
}

# Run the Shiny app
shinyApp(ui, server)

