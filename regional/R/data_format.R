
#these functions format the data for making tables

make_table_dat <- function(data, type, quarter, previous_quarter){
  type <- type
  previous_quarter <- previous_quarter
  
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

#note: this function requires the output from @make_table_data()
format_table_data <- function(data){
  
  county_status_dat <- data %>%
    group_by(State, County) %>%
    mutate(county_gap_sum = sum(gap),
           county_status = case_when(county_gap_sum == 0 ~ 'Met',
                                     county_gap_sum > 0 ~ 'Not Met')) %>%
    ungroup()
  
  region_results_formatted <- county_status_dat %>%
    arrange(State, County) %>%
    group_by(State, County, Specialty) %>%
    reframe(
      gap_display = unique(gap_class),
      county_status = unique(county_status)
    ) %>%
    tidyr::pivot_wider(names_from = Specialty, values_from = gap_display) %>%
    rename(Status = county_status) %>%
    arrange(desc(Status))
  
  
  summary_dat <- data %>%
    group_by(State, County) %>%
    count(gap_class) %>%
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(., names_from = 'gap_class', values_from = 'n')
  
  if(length(grep('\\bM\\b', names(summary_dat))) == 0){
    summary_dat$M <- NA
  }
  
  if(length(grep('\\bN\\b', names(summary_dat))) == 0){
    summary_dat$N <- NA
  }
  
  if(length(grep('\\bR\\b', names(summary_dat))) == 0){
    summary_dat$R <- NA
  }
  
  if(length(grep('\\bC\\b', names(summary_dat))) == 0){
    summary_dat$C <- NA
  }
  
  summary_dat <- summary_dat %>% replace(is.na(.), 0)
  region_results_w_summary <- left_join(region_results_formatted, summary_dat, by = c('State', 'County'))
  # #reorder columns so summaries are next to status
  region_results_w_summary <- region_results_w_summary %>% relocate(c('M', 'N', 'R', 'C'), .after = 'Status')
  
  return(region_results_w_summary)
  
}

# filters compliant specialties
filter_compliant_specs <- function(data, compliant_specs){
  header_cols <- data %>% select(c("State", "County", "Status", "M", "N", "R", "C"))
  spec_cols <- data[,8:ncol(data)]
  spec_cols <- spec_cols %>% select_if(function(col) length(unique(col))>compliant_specs | length(unique(col))==compliant_specs &&  unique(col)!= 'M')
  final_spec_table_dat <- cbind.data.frame(header_cols, spec_cols)
  
  return(final_spec_table_dat)
}

#makes a list of summarized gaps by State, County and Specialty
make_gap_list <- function(data, gap_classes){
  
  gap_list <- list()
  
  for(i in gap_classes){
    dat <- data %>% 
      filter(gap_class == i) %>%
      arrange(Specialty) %>%
      select(State, County, Specialty) %>%
      group_by(State, County) %>%
      reframe(  # Change from summarise() to reframe()
        Specialty = paste(Specialty, collapse = ', ')
      )
    
    
    
    gap_list[[i]] <- dat
    
  }
  
  return(gap_list)
}


