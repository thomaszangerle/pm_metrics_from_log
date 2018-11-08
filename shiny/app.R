library(shiny)
library(tidyverse)
library(lubridate)
library(shinycssloaders)
options(shiny.maxRequestSize=30*1024^2)
# library(Cairo)
# options(shiny.usecairo=T)

source("pm_log_functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("PM Log analyzer"),
   
   fluidRow(
          
          column(3,
                 fileInput("log_file", "Upload log file",
                           multiple = FALSE,
                           accept = ".log"
                 )
          ),
          
          column(3,
                 fileInput("regex_file", "Upload regex file",
                           multiple = FALSE,
                           accept = ".csv"
                 )
          )

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          plotOutput(outputId = "plots", height = 700, width = 1400) %>%
               withSpinner(color="grey")
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    plot_list <- reactive({
        req(input$log_file)
        req(input$regex_file)
        
        log_table <- pm_log_read_log_file(input$log_file$datapath)
        regex_table  <- pm_log_read_regex_table(input$regex_file$datapath)
        sn <- pm_log_extract_sn(log_table)
        metrics_table <- pm_log_extract_metrics(regex_table, log_table, sn)
        pm_log_plot_all(metrics_table, "test")
    })
    
    output$plots <- renderPlot({
        plot_list()[[1]]
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

