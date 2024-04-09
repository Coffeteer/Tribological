# Install and load required packages if not already installed
# install.packages(c("shiny", "ggplot2"))

library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("My Shiny App with Tabs"),
  
  # Define tabs
  tabsetPanel(
    tabPanel("Tab 1", 
             plotOutput("plot1")),
    tabPanel("Tab 1", 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("obs", "Number of observations:", 1, 100, 50)
               ),
               mainPanel(
                 plotOutput("plot1")
               )
             )
    ),
    
    tabPanel("Tab 2", 
             plotOutput("plot2")),
    
    tabPanel("Tab 3", 
             plotOutput("plot3")),
    
    tabPanel("Tab 4", 
             plotOutput("plot4"))
  )
)

# Define server logic
server <- function(input, output) {
  # Create sample data for plots
  set.seed(123)
  data <- data.frame(
    x = rnorm(100),
    y = rnorm(100)
  )
  
  # Define plots for each tab
  output$plot1 <- renderPlot({
    ggplot(data, aes(x, y)) + geom_point() + ggtitle("Plot 1")
  })
  
  output$plot2 <- renderPlot({
    ggplot(data, aes(x, y)) + geom_bar(stat = "identity") + ggtitle("Plot 2")
  })
  
  output$plot3 <- renderPlot({
    ggplot(data, aes(x, y)) + geom_boxplot() + ggtitle("Plot 3")
  })
  
  output$plot4 <- renderPlot({
    ggplot(data, aes(x, y)) + geom_line() + ggtitle("Plot 4")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

