library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)

# read in data
data.df <- readRDS("../RDS_files/data_df.rds")
overview.df <- readRDS("../RDS_files/overview_df.rds")
# line 11 in app ______________________________________________________________________________________________________________________________________________
tribometer.df <- readRDS("../RDS_files_new/tribometer_df.rds")
#______________________________________________________________________________________________________________________________________________________________

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

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("Sliding Distance Scatterplots"),
  
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
)


server <- function(input, output) {
  
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
    # line 245 in app ______________________________________________________________________________________________________________________________________________
    plot.df <- data.df %>% 
      inner_join(overview.df, by=c("Experiment_Name"="SampleID")) %>% 
      inner_join(tribometer.df, by = c("Experiment_Name" = "experiment"))
      plot.df <- plot.df %>% mutate(mloss = initialmass - mass,
                                    umloss = um + umass,
                                    vol_lost = mloss / density,
             vol_lost_std = abs(vol_lost) * sqrt((umloss/mloss)^2 + (um/initialmass)^2 + (ul/L)^2 + (ul/H)^2 +  (ul/W)^2),
             vol_lost_upper = vol_lost + vol_lost_std,
             vol_lost_lower = vol_lost - vol_lost_std,
             mu_upper = mu + muStd,
             mu_lower = mu - muStd,
             SampleID = as.factor(Experiment_Name)) %>%
      dplyr::select(SampleID, TotalDistance, vol_lost, vol_lost_std, 
                    vol_lost_lower, vol_lost_upper, mu, muStd, mu_lower, 
                    mu_upper)
    #______________________________________________________________________________________________________________________________________________________________
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
  
}

shinyApp(ui = ui, server = server)
