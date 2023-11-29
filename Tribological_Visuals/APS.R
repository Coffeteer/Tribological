#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

APSmetrics <- readRDS("..//RDS_files//aps_metrics.rds")

APSmetrics <- APSmetrics %>%
    mutate(DistSlid = time*Stroke)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(""),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="exp", label="Select the Experiment (Material)",
                        choices=unique(APSmetrics$Experiment_Name)),
            selectInput(inputId="yvar", label="Select the y axis variable",
                        choices= c("Normal force",
                                   "Friction force", 
                                   "Humidity",
                                   "Average position",
                                   "Motor Direction",
                                   "Friction Coefficient")),
            selectInput(inputId="xvar", label="Select the x axis variable",
                                    choices= c("Distance Slid", 
                                               "Cycle number")
        )),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot2")
        )
    )
    
)

yvarMap <- list("Normal force"= "Fn1",
               "Friction force"="Ff1", 
               "Humidity"="Humidity",
               "Average position"="X",
               "Motor Direction"="MotorRev",
               "Friction Coefficient"= "mu1")

xvarMap <- list("Distance Slid" = "DistSlid", 
                "Cycle number" = "time")

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterPlot2 <- renderPlot({
        selectedyCol <- yvarMap[input$yvar][[1]]
        selectedxCol <- xvarMap[input$xvar][[1]]
        select_exp <- dplyr::filter(APSmetrics, Experiment_Name %in% !!input$exp) %>%
            rename("yvar" = unlist(selectedyCol)) %>%
            rename("xvar" = unlist(selectedxCol))
        
        ggplot(select_exp) + 
            geom_point(aes(x=xvar, y=yvar), size = 0.15) +
            theme_bw() +
            labs(title="", x=input$xvar, y=input$yvar)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


