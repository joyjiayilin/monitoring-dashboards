
library(echarts4r) #box import only what we need from this when time comes
library(shiny)
library(nrdp.shiny)
library(tidyr)
box::use(
  magrittr[`%>%`, `%<>%`]
)

status_cols <- data.frame(class = c('Closed', 'Met', 'New', 'Recurring'),
                          colors = c(nrdp.shiny::kp_colors$dark$green,  nrdp.shiny::kp_colors$light$green, 
                                     nrdp.shiny::kp_colors$light$orange, nrdp.shiny::kp_colors$dark$orange))

# data format for regional donuts ----
make_table_dat <- function(data, type, quarter, previous_quarter){
  type <- type
  previous_quarter <- previous_quarter
  
  if (length(previous_quarter) > 1) {
    stop()
  }
  
  previous_quarter_dat <- data[data$quarter == previous_quarter,
                               c('State', 'County', 'Specialty', 'kp_region', 'gap')]
  colnames(previous_quarter_dat) <- c("State", "County", "Specialty", "kp_region", "previous_gap" )
  previous_quarter_dat$previous_gap[previous_quarter_dat$previous_gap == 1] <- 10
  
  quarter <- quarter
  current_quarter_dat <- data[data$quarter == quarter,]
  
  #join previous gap
  joined_quarter_dat <- current_quarter_dat %>%
    left_join(y = previous_quarter_dat, by = c("State", "County", "Specialty", "kp_region")) %>%
    mutate(gap_change = (gap - previous_gap),
           gap_class = case_when(gap_change == 0 ~ 'M', #overwrite gap class with change detection logic
                                 gap_change == 1 ~ 'N',
                                 gap_change == -9 ~ 'R',
                                 gap_change == -10 ~ 'C'
           )
    ) %>%
    filter(Type == type)
  
  new_specs <- new_specs <- setdiff(unique(current_quarter_dat$Specialty), unique(previous_quarter_dat$Specialty))
  
  if(length(new_specs) > 0){
    joined_quarter_dat<- joined_quarter_dat %>% dplyr::mutate(gap_class = dplyr::case_when(Specialty %in% new_specs & gap == 1 ~"N",
                                                                                           Specialty %in% new_specs & gap == 0 ~"M",
                                                                                           TRUE ~ gap_class),
                                                              gap_change = dplyr::case_when(Specialty %in% new_specs & gap == 1 ~ 1,
                                                                                            Specialty %in% new_specs & gap == 0 ~ 0,
                                                                                            TRUE ~ gap_change),
                                                              previous_gap = dplyr::case_when(Specialty %in% new_specs ~ 0,
                                                                                              TRUE ~ previous_gap)
    )
  }
  
  return(joined_quarter_dat)
}


# data viz creaters ----
make_regional_donut <- function(df, current_quarter, previous_quarter, region, provider_type){
  
  regional_dat_filtered <- df %>%
    dplyr::filter(kp_region == region,
                  quarter %in% c(current_quarter, previous_quarter))
  
  
  format_dat <- make_table_dat(regional_dat_filtered, type = provider_type, quarter = current_quarter, previous_quarter = previous_quarter)
  
  donut_dat <- format_dat %>%
    filter(quarter == quarter) %>%
    .[, c("kp_region", "County", "Specialty", "quarter", "gap_class")] %>%
    mutate(counter = 1) %>%
    group_by(gap_class) %>%
    summarise(gap_totals = sum(counter), 
              .groups = "drop") %>%
    mutate(gap_class = case_when(gap_class == 'C' ~ 'Closed',
                                 gap_class == 'M' ~ 'Met',
                                 gap_class == 'N' ~ 'New',
                                 gap_class == 'R' ~ 'Recurring'))
  
  #generate colors
  colour <- status_cols %>%
    filter(class %in% donut_dat$gap_class) %>%
    .[, "colors"] %>%
    unlist() %>% 
    unname()
  
  donut_dat %>%
    e_charts(gap_class) %>%
    e_pie(gap_totals,
          radius = c("50%", "80%"),
          name = "Standards Status", legend = F
    ) %>%
    e_color(color = colour) %>%
    e_labels(
      show = TRUE,
      formatter = "{c} \n {d}%", # "{c} \n {d}%", this shows number and percent
      position = "inside"
    ) %>%
    e_tooltip() 
  
}

make_regional_line <- function(df, region, provider_type){
  
  quarter_dat <- df %>%
    dplyr::filter(kp_region == region,
                  Type == provider_type) %>%
    group_by(quarter) %>%
    summarise(Gaps = sum(gap), 
              .groups = "drop")
  
  quarter_dat %>%  
    e_charts(quarter) %>%
    e_area(Gaps, smooth = T) %>%
    e_tooltip() %>%
    e_legend(FALSE)
}



make_exec_line <- function(df){
  data1 <- df[, !names(df) %in% "lob"]
  colnames(data1) <- c('Quarter', 'Gaps')
  data1$Quarter[grep('Pre-Launch', data1)] <- 'Pre-Launch'
  
  data1 %>%  
    e_charts(Quarter) %>%
    e_area(Gaps, smooth = T) %>%
    e_tooltip() %>%
    e_labels(show =T) %>%
    e_title('Enterprise Gap Improvment',
            subtext = 'Pre-Launch to Present')%>%
    e_legend(FALSE)
}

make_regional_table <- function(df, current_quarter, previous_quarter, region, provider_type){
  
  # Filter data for the specified region
  regional_dat_filtered <- df %>%
    dplyr::filter(kp_region == region)
  
  # Format the data for the current quarter
  format_dat <- make_table_dat(regional_dat_filtered, type = provider_type, quarter = current_quarter, previous_quarter = previous_quarter)
  
  # Process current quarter data
  table_dat <- format_dat %>%
    filter(quarter == current_quarter) %>%
    .[, c("kp_region", "County", "Specialty", "quarter", "gap_class")] %>%
    mutate(counter = 1) %>%
    group_by(gap_class) %>%
    summarise(gap_totals = sum(counter), .groups = "drop") %>%
    mutate(gap_class = case_when(
      gap_class == 'C' ~ 'CLOSED',
      gap_class == 'M' ~ 'TOTAL PASSED',
      gap_class == 'N' ~ 'NEW GAPS',
      gap_class == 'R' ~ 'RECURRING GAPS',
      TRUE ~ gap_class
    )) %>%
    complete(gap_class = c("CLOSED", "TOTAL PASSED", "NEW GAPS", "RECURRING GAPS")) %>%
    mutate(gap_totals = replace_na(gap_totals, 0))
  
  # Calculate totals for "New" and "Recurring"
  total_value <- sum(table_dat$gap_totals[table_dat$gap_class %in% c("NEW GAPS", "RECURRING GAPS")])
  
  # Add "Total" row and reorder
  table_dat <- table_dat %>%
    dplyr::filter(gap_class != "CLOSED") %>%
    dplyr::bind_rows(data.frame(gap_class = "TOTAL GAPS", gap_totals = total_value)) %>%
    dplyr::slice(match(c("NEW GAPS", "RECURRING GAPS", "TOTAL GAPS", "TOTAL PASSED"), gap_class))
  
  # Process previous quarter data if applicable
  if (length(tail(unique(format_dat$quarter), 3)[-3]) == 1) {
    format_dat_prev <- make_table_dat(regional_dat_filtered, type = provider_type, quarter = previous_quarter, previous_quarter = tail(unique(df$quarter), 3)[1])
    
    table_dat_prev <- format_dat_prev %>%
      filter(quarter == previous_quarter) %>%
      .[, c("kp_region", "County", "Specialty", "quarter", "gap_class")] %>%
      mutate(counter = 1) %>%
      group_by(gap_class) %>%
      summarise(gap_totals = sum(counter), .groups = "drop")  %>%
      mutate(gap_class = case_when(
        gap_class == 'C' ~ 'CLOSED',
        gap_class == 'M' ~ 'TOTAL PASSED',
        gap_class == 'N' ~ 'NEW GAPS',
        gap_class == 'R' ~ 'RECURRING GAPS',
        TRUE ~ gap_class
      )) %>%
      complete(gap_class = c("CLOSED", "TOTAL PASSED", "NEW GAPS", "RECURRING GAPS")) %>%
      mutate(gap_totals = replace_na(gap_totals, 0))
    
    total_value_prev <- sum(table_dat_prev$gap_totals[table_dat_prev$gap_class %in% c("NEW GAPS", "RECURRING GAPS")])
    
    table_dat_prev <- table_dat_prev %>%
      dplyr::filter(gap_class != "CLOSED") %>%
      dplyr::bind_rows(data.frame(gap_class = "TOTAL GAPS", gap_totals = total_value_prev)) %>%
      dplyr::slice(match(c("NEW GAPS", "RECURRING GAPS", "TOTAL GAPS", "TOTAL PASSED"), gap_class))
    
    # Calculate percentage change
    table_dat <- table_dat %>%
      left_join(
        {
          table_dat_prev[["gap_totals_prev"]] <- table_dat_prev[["gap_totals"]]
          table_dat_prev[, c("gap_class", "gap_totals_prev")]
        }, 
        by = "gap_class"
      ) %>%
      mutate(gap_perc = ifelse(!is.na(gap_totals_prev) & gap_totals_prev != 0,
                               paste0(round((gap_totals - gap_totals_prev) / gap_totals_prev * 100, 2), '%'),
                               '0%')) %>%
      .[, !(names(.) %in% "gap_totals_prev")]
    
  }
  
  table_dat
}

make_network_gaps <- function(df){
  data1 <- df[, !names(df) %in% "lob"]
  colnames(data1) <- c('Quarter', 'Gaps')
  col_select <- c('2018 Q1', data1$Quarter[grep('Q4', data1$Quarter)])
  dat <- data1 %>%
    dplyr::filter(Quarter %in% col_select)
  
  dat %>%  
    e_charts(Quarter) %>%
    e_area(Gaps, smooth = T) %>%
    e_tooltip() %>%
    e_labels(show =T) %>%
    e_legend(show = FALSE) %>%
    e_title('Networks Gaps',
            subtext = 'Since Program Launch in Q1 2018')
}

make_network_gaps_qhp <- function(df){
  data1 <- df[, !names(df) %in% "lob"]
  colnames(data1) <- c('Quarter', 'Gaps')
  col_select <- c(data1$Quarter[grep('Q2', data1$Quarter)])
  dat <- data1 %>%
    dplyr::filter(Quarter %in% col_select)
  
  dat %>%  
    e_charts(Quarter) %>%
    e_area(Gaps, smooth = T) %>%
    e_tooltip() %>%
    e_labels(show =T) %>%
    e_legend(show = FALSE) %>%
    e_title('Networks Gaps')
}

make_quarterly_progress <- function(df){
  data1 <- df[, !names(df) %in% "lob"]
  colnames(data1) <- c('Quarter', 'Gaps')
  dat <- data1 %>%
    dplyr::do(tail(., 5))
  
  dat %>%  
    e_charts(Quarter) %>%
    e_area(Gaps, smooth = T) %>%
    e_tooltip() %>%
    e_legend(show = FALSE) %>%
    e_labels(show =T) %>%
    e_title('Networks Gaps')
  
}

make_quarterly_progress_qhp <- function(df){
  data1 <- df[, !names(df) %in% "lob"]
  colnames(data1) <- c('Quarter', 'Gaps')
  dat <- data1 %>%
    dplyr::do(tail(., 5))
  
  col_select <- c(data1$Quarter[grep('2024', data1$Quarter)])
  dat <- data1 %>%
    dplyr::filter(Quarter %in% col_select)
  
  dat %>%  
    e_charts(Quarter) %>%
    e_area(Gaps, smooth = T) %>%
    e_tooltip() %>%
    e_legend(show = FALSE) %>%
    e_labels(show =T) %>%
    e_title('Networks Gaps')
  
}

# legend for regional graphs
#function for setting font color in cells
status_cols_list <- as.list(c(nrdp.shiny::kp_colors$light$green, nrdp.shiny::kp_colors$light$orange,
                              nrdp.shiny::kp_colors$dark$green, nrdp.shiny::kp_colors$dark$orange))
names(status_cols_list) <- c('M', 'N', 'C', 'R')

status_cell_color <- function(value, index, name) { 
  if(value == 'M'){
    htmltools::div(style = glue::glue("color: {status_cols_list$M};"), value) #background: {status_cols$Y}; 
  } else if(value == 'N'){
    htmltools::div(style = glue::glue("color: {status_cols_list$N};"), value)
  } else if(value == 'C'){
    htmltools::div(style = glue::glue("color: {status_cols_list$C};"), value)
  } else if(value == 'R'){
    htmltools::div(style = glue::glue("color: {status_cols_list$R};"), value)
  }
}

statuses <- c('M', 'N', 'R', 'C')

# legend table ----
legend_data <- data.frame(
  M ='M',
  M_text = 'Met',
  N = 'N',
  N_text = 'New Gap',
  R = 'R',
  R_text = 'Recurring Gap',
  C = 'C',
  C_text = 'Closed Gap'
)

#legend_columns <- list()

defaultcolDef_legend <-
  reactable::colDef(
    style = list(`line-height` = '10px'),
    header= "",
    width = 150
  )


# styling for legend table
legend_columns <- lapply(statuses, function(x){
  reactable::colDef(width=30,
                    header= "",
                    cell = status_cell_color,
                    style = list(background = status_cols_list[[x]],
                                 fontWeight = "bold",
                                 `text-align` = 'center',
                                 `border-radius`= '2px'
                    ))
})

names(legend_columns) <- statuses


legend_reactable <- reactable::reactable(legend_data,
                                         defaultColDef = defaultcolDef_legend,
                                         columns = legend_columns,
                                         theme = reactable::reactableTheme(
                                           borderColor = "white",
                                           style = list(fontFamily = "BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                                           rowStyle = list(height = "25px")
                                         )
)

