library(shiny)
library(tidyverse)
library(lubridate)
library(shinycssloaders)
library(plotly)
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
          ),
          
          column(3, 
                 selectizeInput(
                     inputId = "cat",
                     label = "CATEGORY : ",
                     choices = "",
                     selected = ""
                 )
          )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          plotlyOutput(outputId = "plots", height = 700, width = 1400) %>%
               withSpinner(color="grey")
      )
)


server <- function(input, output, session) {
    
    observe({
        updateSelectizeInput(session, "cat",
                             choices = c(Choose = "", cat_list()),
                             selected = cat_list()[[1]]
        )  
    })
    
    metrics_table <- reactive({
        req(input$log_file)
        req(input$regex_file)
        
        log_table <- pm_log_read_log_file(input$log_file$datapath)
        regex_table  <- pm_log_read_regex_table(input$regex_file$datapath)
        sn <- pm_log_extract_sn(log_table)
        metrics_table <- pm_log_extract_metrics(regex_table, log_table, sn)
        metrics_table
    })
    
    cat_list <- reactive({
        req(input$log_file)
        req(input$regex_file)
        
        metrics_table  <- metrics_table()
        metrics_table %>% 
            distinct(TYPE) %>% 
            pull(TYPE)
    })
    
    plot_list <- reactive({
        req(input$log_file)
        req(input$regex_file)
        
        pm_log_plot_all(metrics_table(), "")
    })
    
    output$plots <- renderPlotly({
        plot_list()[[input$cat]] %>% 
            ggplotly() 
    })
    
    # output$downloadData <- downloadHandler(
    #   filename = function() {
    #     paste('data-', Sys.Date(), '.csv', sep='')
    #   },
    #   content = function(con) {
    #     write.csv(data, con)
    #   }
    # )
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)

