
library(tidyverse)
library(lubridate)

pm_log_read_log_file <- function(log_file_path){
    suppressWarnings(
            read_tsv(log_file_path,
                 col_names = c("origin", "datetime", "tag", "module", "txt"),
                 col_types = cols(origin = col_character(),
                                  datetime = col_character(),
                                  tag = col_character(),
                                  module = col_character(),
                                  txt = col_character())
        ) %>% 
            mutate(datetime = dmy_hms(datetime))
    )
}

pm_log_read_regex_table <- function(regex_file_path){
    read_csv(
        regex_file_path, 
        col_types = cols(
            name = col_character(),
            pattern =col_character()
        )
    )
}

pm_log_extract_single_metric <- function(type, name, pattern, log){
    
    # filter rows including the patterns for further extraction
    filtered_rows <- log %>% 
        filter(str_detect(txt, pattern)) 
    
    # extract the info from the filtered rows
    if(filtered_rows %>% nrow() > 0){
        extracted_values <- filtered_rows %>% 
            extract(txt, into = "value", pattern) %>% 
            select(datetime, value) %>% 
            mutate(
                TYPE = type,
                NAME = name,
                VALUE = parse_double(value),
                TIME = datetime
            ) %>%  
            select(TYPE, NAME, TIME, VALUE) %>% 
            arrange(TYPE, NAME, TIME)
    }
    else
        extracted_values <- NA
    
}

pm_log_get_mid <- function(sn){
    str_c(model, sn, sep = "-")
}

model <- "PROMAPPER"
pm_log_extract_metrics <- function(regex_table, log_table, sn){
    
    # loop to extract list of metrics
    metrics_list <- regex_table %>% 
        pmap(pm_log_extract_single_metric, log = log_table)
    
    # add MODEL-SN and bind list to return a single table
    metrics <- metrics_list[!is.na(metrics_list)] %>% 
        bind_rows() %>% 
        mutate(
            MODEL = model,
            SN = sn,
            MID = pm_log_get_mid(sn)
        ) %>% 
        select(MODEL, SN, MID, TIME, TYPE, NAME, VALUE) %>% 
        arrange(TIME)
}

sn_pattern <- "PROMAPPER-([1-3][0-9]SM[0-9]{3})" 
pm_log_extract_sn <- function(log){
     filtered_row <- log %>% 
        filter(str_detect(txt, sn_pattern)) %>% 
         head(1)
    if(filtered_row %>% nrow() > 0){
        filtered_row %>% 
            extract(txt, into = "sn", sn_pattern) %>% 
            head(1) %>% 
            pull(sn)
    }
    else
        NA
}

pm_log_plot_metrics_type <- function(type, metrics_table, plot_title){
    metrics_table %>% 
        filter(TYPE == type) %>%
        ggplot(aes(TIME, VALUE)) +
        facet_wrap(~ NAME, scales = "free_y") +
        geom_line(color = "blue", alpha = .3) +
        geom_point(size = 2, alpha = .5, color = "blue") +
        labs(
            title = plot_title
        ) +
        scale_x_datetime(NULL, date_labels = "%m-%d\n%H:%M")
}

pm_log_plot_all <- function(metrics_table, plot_title){
    types <- metrics_table %>% 
        distinct(TYPE) %>% 
        pull(TYPE)
    
    plots <- map(types, pm_log_plot_metrics_type, metrics_table = metrics_table, plot_title = plot_title)
    names(plots) <- types
    plots
}



    
    
    