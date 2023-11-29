library(shiny)
library(plotly)
library(tidyverse)

expsum <- readRDS(file = "../RDS_files/experiment_summaries.rds")
Data <- readRDS(file = "../RDS_files/data_df.rds")

ui <- fluidPage(
  titlePanel("Experiment Bar Charts"),
    
  sidebarPanel(
    selectInput(inputId = "experiment",
                label = "Experiment",
                choices=expsum$Experiment_Name),
    selectInput(inputId = "metric",
                label = "Metric",
                choices=c("COF", "Ktotal", "K_MC2", "K_MC3", "K_MC4", "K_MC5")),
    selectInput("scale", "Select Scale", choices = c("Linear", "Log")),
    textInput("bar_color", "Bar Color", value = "black"),
    downloadButton("downloadBar", "Download")
  ),
  
  mainPanel(
    plotlyOutput(outputId = "barPlot")
  )
)

metricMap <- list("COF" = "mu", "Ktotal" = "KtotalMonte", "K_MC2" = "KtestMonteN2", "K_MC3" = "KtestMonteN3", "K_MC4" = "KtestMonteN4", "K_MC5" = "KtestMonteN5")

server <- function(input, output) {
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
  
  output$downloadBar <- downloadHandler(
    filename = function() {
      paste0(input$experiment, ".csv")
    },
    content = function(file) {
      selectedCol <- metricMap[input$metric][[1]]
      selectedData <- filter(Data, Experiment_Name == input$experiment) %>%
        select(Experiment_Name, Experiment, unlist(selectedCol))
      write.csv(selectedData, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
