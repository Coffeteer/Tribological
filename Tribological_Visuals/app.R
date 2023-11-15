#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Install necessary packages if not already installed
# install.packages(c("shiny", "plotly"))

library(shiny)
library(plotly)

# Sample data (replace this with your actual data)
sample_data <- data.frame(
  Distance = seq(1, 100, by = 5),
  VolumeLost = rnorm(20, mean = 0, sd = 5),
  FrictionCoefficient = rnorm(20, mean = 0.5, sd = 0.1), #mu in Data
  CycleNumber = rep(1:5, each = 4),
  Ktotal = rnorm(20, mean = 0, sd = 5),
  K_MC1 = rnorm(20, mean = 0, sd = 5),
  K_MC2 = rnorm(20, mean = 0, sd = 5),
  K_MC3 = rnorm(20, mean = 0, sd = 5),
  K_MC4 = rnorm(20, mean = 0, sd = 5),
  K_MC5 = rnorm(20, mean = 0, sd = 5)
)

# Define UI
ui <- fluidPage(
  titlePanel("Data Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("plot_types", "Select Plot Types", c("Scatter Plot", "Bar Chart")),
      selectInput("sample", "Select Sample", choices = unique(sample_data$CycleNumber)),
      selectInput("scale", "Select Scale", choices = c("Linear", "Log-Log", "Semi-Log")),
      checkboxInput("add_legend", "Add Legend", value = TRUE),
      textInput("point_shape", "Point Shape", value = 16),
      textInput("point_color", "Point Color", value = "blue"),
      textInput("line_color", "Line Color", value = "red")
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
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
    if (input$scale == "Log-Log") {
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
}

# Run the Shiny app
shinyApp(ui, server)

