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
                 fileInput("log_file", "Upload log file(s)",
                           multiple = TRUE,
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
          ),
          
          column(3,
                 dateRangeInput('dateRange',
                                label = 'Date range input: yyyy-mm-dd',
                                start = NA,#floor_date(today(), "year"), 
                                end = NA#ceiling_date(today(), "year")
                 )
                 
          )
          
      ),
   
   # Download CSV and PDF row
   fluidRow(
       column(3,
              downloadButton('downloadData', 'CSV')
       ),
       
       column(3,
              downloadButton('report', 'HTML')
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
        
        log_table_list <- map(input$log_file$datapath, pm_log_read_log_file)
        log_table <- log_table_list %>% bind_rows()
        regex_table  <- pm_log_read_regex_table(input$regex_file$datapath)
        sn <- pm_log_extract_sn(log_table)
        metrics_table <- pm_log_extract_metrics(regex_table, log_table, sn)
    })
    
    metrics_table_filtered <- reactive({
        
        metrics_table_filtered <- metrics_table()
        
        
        # filter on date
        if(all(!(is.na(input$dateRange)))){
            metrics_table_filtered <- metrics_table_filtered %>% 
                filter(
                    TIME >= input$dateRange[1] & TIME <= input$dateRange[2] + ddays(1)
                )
        }
        
        metrics_table_filtered
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
        
        pm_log_plot_all(metrics_table_filtered(), "")
    })
    
    output$plots <- renderPlotly({
        plot_list()[[input$cat]] 
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        str_c('data_', format(now(), format="%Y%m%dT%H%M%S"), '.csv', sep='')
      },
      content = function(con) {
        write_csv(metrics_table(), con)
      }
    )
    
    output$report <- downloadHandler(
        
        filename = str_c('metrics_', format(now(), format="%Y%m%dT%H%M%S"), '.html', sep=''),
        
        content = function(file) {
            # Copy the report file to a temporary directory before processing it.
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            plots_copy <- plot_list()
            params <- list(plots = plots_copy)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )}
    )
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)

