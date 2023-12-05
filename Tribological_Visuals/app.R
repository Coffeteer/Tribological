library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(tidyverse)

APSmetrics <- readRDS("..//RDS_files//aps_metrics.rds")
expsum <- readRDS("..//RDS_files//experiment_summaries.rds")
Data <- readRDS(file = "../RDS_files/data_df.rds")
overview.df <- readRDS("..//RDS_files//overview_df.rds")
data.df <- Data
APSmetrics <- APSmetrics %>%
  mutate(DistSlid = 2*time*Stroke)

# read in data



# colors for plots
color.vals <- c("#050505", "#f23838", "#f5903d", "#f5e533", "#09b309", 
                "#0e8feb", "#993bdb", "#e880c2", "#a10a0a", "#b55505", 
                "#c4b402", "#067306", "#182fc7", "#5e069c", "#b00471")
names(color.vals) <- c("Black", "Red", "Orange", "Yellow", "Green", "Blue", 
                       "Purple", "Pink", "Dark Red", "Dark Orange", 
                       "Dark Yellow", "Dark Green", "Dark Blue", "Dark Purple",
                       "Dark Pink")

# shapes for plots
shape.vals <- c("Circle" = 16, "Square" = 15, "Triangle" = 17, 
                "Diamond" = 18, "Open Circle" = 1, "Open Square" = 0, 
                "Open Triangle" = 2, "Open Diamond" = 5, "Plus" = 3,
                "Cross" = 4, "Asterisk" = 8)

# Volume lost as function of sliding distance (scatterplot with error bars)
# Friction Coefficient as function of sliding distance (scatterplot with error bars)

# Define UI
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("Data Visualizations for Tribological Experimental Data"),
  
  tabsetPanel(
    tabPanel("Experiment Summary Explorer Table", 
             mainPanel(
               DTOutput("table")  
    )),
    
    tabPanel("Experiment APS Scatter Plot Visualizations", 
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId="exp2", label="Select the Experiment (Material)",
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
                                        "Cycle number")),
                 selectInput("y_scale", "Select Scale", choices = c("Linear", "Log")),
                 textInput("point_color", "Point Color", value = "#B61E2E"),
                 downloadButton("db_aps", "Download Data")
                 ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotlyOutput("scatterPlot2")
               )
             )
    ),
    
    tabPanel("Experiment Sliding Distance Scatter Visualizations", 
             sidebarLayout(
               sidebarPanel(
                 # select variable for y-axis
                 selectInput(inputId = "y.var", 
                             label = "Variable:",
                             choices = c("Volume Lost", "Friction Coefficient"), 
                             selected = "Volume Lost"),
                 # select experiments
                 selectInput(inputId = "exp", 
                             label = "Sample ID:",
                             choices = unique(data.df$Experiment_Name),
                             selected = unique(data.df$Experiment_Name)[1],
                             multiple = TRUE),
                 # option to add lines
                 checkboxInput(inputId = "add.lines",
                               label = "Add Trendlines",
                               value = FALSE),
                 # option to add color
                 checkboxInput(inputId = "colors",
                               label = "Add Color by Sample ID",
                               value = FALSE),
                 conditionalPanel(
                   condition = "output.colors",
                   # option to change color
                   selectInput(inputId = "colors.select",
                               label = "Change Colors:",
                               choices = color.vals,
                               multiple = TRUE)
                 ),
                 
                 # option to add shapes
                 checkboxInput(inputId = "shapes",
                               label = "Add Shapes by Sample ID",
                               value = FALSE),
                 
                 conditionalPanel(
                   condition = "output.shapes",
                   
                   # option to change shape
                   selectInput(inputId = "shapes.select",
                               label = "Change Shapes:",
                               choices = shape.vals,
                               multiple = TRUE)
                 ),
                 
                 # option to change point size
                 sliderInput(inputId = "point.size",
                             label = "Point Size:",
                             min = 1,
                             max = 3,
                             step = 0.1,
                             value = 1.5,
                             ticks = FALSE),
                 
                 # option to have logarithmic x-axis
                 radioButtons(inputId = "xscale",
                              label = "X-Axis Transformation:",
                              choices = c("No Transformation", "Logarithmic"),
                              inline = TRUE),
                 
                 # option to have logarithmic y-axis
                 radioButtons(inputId = "yscale",
                              label = "Y-Axis Transformation:",
                              choices = c("No Transformation", "Logarithmic"),
                              inline = TRUE),
                 
                 # download data button
                 downloadButton(outputId = "download.data", 
                                label = "Download Plot Data")
               ),
               
               
               # output: display plot
               mainPanel(
                 plotlyOutput(outputId = "scatterplot")
               )
             )
    ), ## Replace "plot3" with name of plot
    
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
                 textInput("bar_color", "Bar Color", value = "#B61E2E"),
                 downloadButton("downloadBar", "Download")),
               mainPanel(
                 plotlyOutput(outputId = "barPlot")
               )
             )
    )
  ))


metricMap <- list("COF" = "mu", "Ktotal" = "KtotalMonte", "K_MC2" = "KtestMonteN2", "K_MC3" = "KtestMonteN3", "K_MC4" = "KtestMonteN4", "K_MC5" = "KtestMonteN5")

yvarMap <- list("Normal force"= "Fn1",
                "Friction force"="Ff1", 
                "Humidity"="Humidity",
                "Average position"="X",
                "Motor Direction"="MotorRev",
                "Friction Coefficient"= "mu1")

xvarMap <- list("Distance Slid" = "DistSlid", 
                "Cycle number" = "time")


# Define server
server <- function(input, output) {
  
  
  output$table <- renderDT({
    datatable(expsum) %>%
      formatRound(columns=c('mu', "Fn"), digits = 3) %>%
      formatSignif(columns = "wear_rate", digits =  2)
  })  
  
  
  output$scatterPlot2 <- renderPlotly({
    selectedyCol <- yvarMap[input$yvar][[1]]
    selectedxCol <- xvarMap[input$xvar][[1]]
    select_exp <- dplyr::filter(APSmetrics, Experiment_Name %in% !!input$exp2) %>%
      rename("yvar" = unlist(selectedyCol)) %>%
      rename("xvar" = unlist(selectedxCol))
    
    p <- ggplot(select_exp, aes(x=xvar, y=yvar)) + 
      geom_point(size = 0.15, color=input$point_color) +
      theme_bw() +
      labs(title="", x=input$xvar, y=input$yvar)
    
    if(input$y_scale == "Log") {
      p <- p +
        scale_y_log10()
    }
    p
  })
  
  
  ## Add tab 3 plot3 plots
  
  
  # COLORS INPUT
  # check if colors is checked
  output$colors <- reactive({
    return(input$colors)
  })
  
  # output if colors is checked
  outputOptions(output, 'colors', suspendWhenHidden=FALSE)
  
  # SHAPES INPUT
  # check if shapes is checked
  output$shapes <- reactive({
    return(input$shapes)
  })
  
  # output if shapes is checked
  outputOptions(output, 'shapes', suspendWhenHidden=FALSE)
  
  # DATA
  # create plot data
  plot.df <- reactive({
    plot.df <- data.df %>% 
      inner_join(overview.df, by=c("Experiment_Name"="SampleID")) %>%
      mutate(vol_lost = (initialmass - mass) / density,
             vol_lost_std = umass / density,
             vol_lost_upper = vol_lost + vol_lost_std,
             vol_lost_lower = vol_lost - vol_lost_std,
             mu_upper = mu + muStd,
             mu_lower = mu - muStd,
             SampleID = as.factor(Experiment_Name)) %>%
      dplyr::select(SampleID, TotalDistance, vol_lost, vol_lost_std, 
                    vol_lost_lower, vol_lost_upper, mu, muStd, mu_lower, 
                    mu_upper)
    return(plot.df)
  })
  
  # download .rds of data
  output$download.data <- downloadHandler(
    filename = "SlidingDistPlotData.rds",
    content = function(file) {
      if (input$y.var == "Volume Lost") {
        df <- plot.df() %>% filter(SampleID %in% input$exp) %>%
          select(-mu, -muStd, -mu_lower, -mu_upper)
      } else {
        df <- plot.df() %>% filter(SampleID %in% input$exp) %>%
          select(-vol_lost, -vol_lost_std, -vol_lost_lower, -vol_lost_upper)
      }
      readr::write_rds(df, file)
    }
  )
  
  # PLOT
  output$scatterplot <- renderPlotly({
    # geom point aes based on color and shape input
    aes.point <- function(x.var, y.var, group.var, text.plot) {
      if (input$colors && input$shapes) { # color and shape
        aes.point <- aes(x = x.var, y = y.var, color = group.var,
                         shape = group.var, text = text.plot)
      } else if (input$colors && !(input$shapes)) { # only color
        aes.point <- aes(x = x.var, y = y.var, color = group.var, 
                         text = text.plot)
      } else if (!(input$colors) && input$shapes) { # only shape
        aes.point <- aes(x = x.var, y = y.var, shape = group.var, 
                         text = text.plot)
      } else { # no color or shape
        aes.point <- aes(x = x.var, y = y.var, text = text.plot)
      }
      return(aes.point)
    }
    
    # geom errorbar aes based on color
    aes.error <- function(y.var.low, y.var.up, x.var, group.var) {
      if (input$colors) { # color
        aes.error <- aes(ymin = y.var.low, ymax = y.var.up, x = x.var, 
                         color = group.var)
      } else { # no color
        aes.error <- aes(ymin = y.var.low, ymax = y.var.up, x = x.var, 
                         group = group.var)
      }
      return(aes.error)
    }
    
    # geom line aes based on color
    aes.line <- function(x.var, y.var, group.var) {
      if (input$colors) { # color
        aes.line <- aes(x = x.var, y = y.var, color = group.var)
      } else { # no color
        aes.line <- aes(x = x.var, y = y.var, group = group.var)
      }
      return(aes.line)
    }
    
    # add logarithm to title if log scale is selected
    if (input$xscale == "Logarithmic") {
      title.log.x = "Logarithm of "
    } else {
      title.log.x = ""
    }
    if (input$yscale == "Logarithmic") {
      title.log.y = "Logarithm of "
    } else {
      title.log.y = ""
    }
    
    # create plot based on y variable selected
    if (input$y.var == "Volume Lost") {
      df <- plot.df() %>% filter(SampleID %in% input$exp) %>% 
        drop_na(SampleID, TotalDistance, vol_lost, vol_lost_lower, 
                vol_lost_upper)
      attach(df)
      plot1 <- ggplot(data = df)+
        geom_point(aes.point(TotalDistance, vol_lost, SampleID,
                             paste0(input$y.var, ": ", round(df$vol_lost, 3),
                                    "\nSliding Distance: ", 
                                    scales::comma(TotalDistance), 
                                    "\nSample ID: ", SampleID)), 
                   size = input$point.size)+
        geom_errorbar(aes.error(vol_lost_lower, vol_lost_upper, TotalDistance,
                                SampleID))+
        labs(title = paste0("Plot of ", title.log.y, "Volume Lost vs. ", 
                            title.log.x, "Total Sliding Distance"), 
             x = paste0(title.log.x, "Total Sliding Distance"), 
             y = paste0(title.log.y, "Volume Lost (mm^3)"))
      if (input$add.lines) {
        plot1 <- plot1 + geom_line(aes.line(TotalDistance, vol_lost, SampleID))
      }
    } else {
      df <- plot.df() %>% filter(SampleID %in% input$exp) %>% 
        drop_na(SampleID, TotalDistance, mu, mu_lower, mu_upper)
      attach(df)
      plot1 <- ggplot(data = df)+
        geom_point(aes.point(TotalDistance, mu, SampleID,
                             paste0(input$y.var, ": ", round(df$mu, 3),
                                    "\nSliding Distance: ", 
                                    scales::comma(TotalDistance), 
                                    "\nSample ID: ", SampleID)), 
                   size = input$point.size)+
        geom_errorbar(aes.error(mu_lower, mu_upper, TotalDistance, SampleID))+
        labs(title = paste0("Plot of ", title.log.y, 
                            "Friction Coefficient vs. ", title.log.x, 
                            "Total Sliding Distance"), 
             x = paste0(title.log.x, "Total Sliding Distance"), 
             y = paste0(title.log.y, "Friction Coefficient"))
      if (input$add.lines) {
        plot1 <- plot1 + geom_line(aes.line(TotalDistance, mu, SampleID))
      }
    }
    
    # add log scale to plot if selected
    if (input$xscale == "Logarithmic") {
      plot1 <- plot1 +
        scale_x_continuous(trans = "log", labels = scales::comma)
    } else {
      plot1 <- plot1 +
        scale_x_continuous(trans = "identity", labels = scales::comma,
                           breaks = scales::breaks_extended(n = 10))
    }
    
    if (input$yscale == "Logarithmic") {
      plot1 <- plot1 +
        scale_y_continuous(trans = "log", 
                           labels = scales::number_format(accuracy = 0.001))
    } else {
      plot1 <- plot1 +
        scale_y_continuous(trans = "identity",
                           labels = scales::number_format(accuracy = 0.001),
                           breaks = scales::breaks_extended(n = 10))
    }
    
    # add colors to plot if selected
    if (input$colors && length(input$colors.select) >= length(input$exp)) {
      plot1 <- plot1 + 
        scale_color_manual(values = input$colors.select[1:length(input$exp)],
                           name = "Sample ID", labels = input$exp)
    } else if (input$colors && length(input$colors.select) < length(input$exp)) {
      plot1 <- plot1 + 
        scale_color_manual(values = rep("black", length(input$exp))) +
        guides(color=FALSE)
    }
    
    # add shapes to plot if selected
    if (input$shapes && length(input$shapes.select) >= length(input$exp)) {
      plot1 <- plot1 + 
        scale_shape_manual(values = input$shapes.select[1:length(input$exp)],
                           name = "Sample ID", labels = input$exp)
    } else if (input$shapes && length(input$shapes.select) < length(input$exp)) {
      plot1 <- plot1 + 
        scale_shape_manual(values = rep(16, length(input$exp))) +
        guides(shape=FALSE)
    }
    
    # add themes to plot
    plot1 <- plot1 + theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, size=12))
    
    # create plotly
    scatterplotly <- ggplotly(plot1, height=700, tooltip="text") %>% 
      layout(font=list(size=12), 
             margin=list(l=50,r=50,b=50,t=50),
             legend = list(orientation = 'h'))
    
    # edit plotly legend
    if (input$colors && input$shapes) {
      scatterplotly <- scatterplotly %>% style(showlegend = FALSE, 
                                               traces = (length(input$exp)+1):(length(input$exp)*2))
    }
    
    return(scatterplotly)
    
  })
  
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
      labs(x="Experiment Number", y=input$metric)
    
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
  
  output$db_aps <- downloadHandler(
    filename = function() {
      paste0(input$exp, "-", input$xvar, " vs ", input$yvar,  ".csv")
    },
    content = function(file) {
      selectedyCol <- yvarMap[input$yvar][[1]]
      selectedxCol <- xvarMap[input$xvar][[1]]
      selectedData <- dplyr::filter(APSmetrics, Experiment_Name %in% !!input$exp) %>%
        select(unlist(selectedxCol), unlist(selectedyCol))
      write.csv(selectedData, file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)

