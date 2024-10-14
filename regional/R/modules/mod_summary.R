# module for Summary tab
source("R/data_format.R")
source("R/utils.R")

summaryUi <- function(id){
  ns <- NS(id)
  #UI elements for summary tab
  tagList(
    div(class="tab-main-content container-xl",
        
        # Providers ----
        # subhed1
        fluidRow(
          column(width = 9, offset = 1,
                 div(
                   style = 'background: #F3F5F9; padding: 12px; margin-bottom: 30px; position: relative;',  # Adjust padding and position
                   h3("Individual Providers", style = 'font-size: 28px; font-style: italic; margin: 0;'),
                   div(style = 'position: absolute; bottom: 0; left: 0; right: 0; width: 100%;',  # Full width with right positioning
                       hr(style = "border: 0.5px solid black; margin: 0; width: 100%; box-sizing: border-box;")  # Ensure border is included in width
                   )
                 )
          ),
          style = "margin-top: 20px;"
        ),
        
        # subhed2 for Recurring Gaps
        fluidRow(
          column(width = 10, offset = 1,
                 div(style = 'display: flex; align-items: center;',
                     h3(htmlOutput(ns("recurring_gaps_with_text")), 
                        style = 'font-size: 24px; margin-bottom: 0px; margin-right: 45px;'), # Small space between text and count
                     h3(htmlOutput(ns("recurring_gaps_count")), 
                        style = 'margin-bottom: 0px;')
                 )
          ),
          column(width = 9, offset = 1,
                 uiOutput(ns("recurring_hr"))  # Conditionally render hr
          )
        ),
        fluidRow(
          column(9, offset = 1,
                 uiOutput(ns("recurring_gap_list_table")))
        ),
        
        # subhed2 for New Gaps
        fluidRow(
          column(width = 10, offset = 1,
                 div(style = 'display: flex; align-items: center; margin-top: 20px;',
                     h3(htmlOutput(ns("new_gaps_with_text")), 
                        style = 'font-size: 24px; margin-bottom: 0px; margin-right: 110px;'), # Small space between text and count
                     h3(htmlOutput(ns("new_gaps_count")), 
                        style = 'margin-bottom: 0px;')
                 )
          ),
          column(width = 9, offset = 1,
                 uiOutput(ns("new_hr"))  # Conditionally render hr
          )
        ),
        
        fluidRow(
          column(9, offset = 1,
                 uiOutput(ns("new_gap_list_table")))
        ),
        
        # subhed2 for Closed Gaps
        fluidRow(
          column(width = 10, offset = 1,
                 div(style = 'display: flex; align-items: center; margin-top: 20px;',
                     h3(htmlOutput(ns("closed_gaps_with_text")), 
                        style = 'font-size: 24px; margin-bottom: 0px; margin-right: 80px;'), # Small space between text and count
                     h3(htmlOutput(ns("closed_gaps_count")), 
                        style = 'margin-bottom: 0px;')
                 )
          ),
          column(width = 9, offset = 1,
                 uiOutput(ns("closed_hr"))  # Conditionally render hr
          )
        ),
        
        fluidRow(
          column(9, offset = 1,
                 uiOutput(ns("closed_gap_list_table")))
        ),
        
        fluidRow(
          column(width = 10, offset = 1,
                 h3(textOutput(ns("placeholder_prac")), style='font-size: 24px; padding-left: 10px; padding-bottom: 38px')
          )
        ),
        
        # Facilities -----
        # subhed1
        fluidRow(
          column(width = 9, offset = 1,
                 div(
                   style = 'background: #F3F5F9; padding: 12px; margin-bottom: 30px; position: relative;',  # Adjust padding and position
                   h3("Facility Providers", style = 'font-size: 28px; font-style: italic; margin: 0;'),
                   div(style = 'position: absolute; bottom: 0; left: 0; right: 0; width: 100%;',  # Full width with right positioning
                       hr(style = "border: 0.5px solid black; margin: 0; width: 100%; box-sizing: border-box;")  # Ensure border is included in width
                   )
                 )
          ),
          style = "margin-top: 20px;"
        ),
        
        # subhed2 for Recurring Gaps (Facility)
        fluidRow(
          column(width = 12, offset = 1,
                 div(style = 'display: flex; align-items: center; margin-top: 20px;',
                     h3(htmlOutput(ns("recurring_gaps_with_text_fac")), 
                        style = 'font-size: 24px; margin-bottom: 0px; margin-right: 45px;'), # Small space between text and count
                     h3(htmlOutput(ns("recurring_gaps_count_fac")), 
                        style = 'margin-bottom: 0px;')
                 )
          ),
          column(width = 9, offset = 1,
                 uiOutput(ns("recurring_hr_fac"))  # Conditionally render hr
          )
        ),
        
        fluidRow(
          column(9, offset = 1,
                 uiOutput(ns("recurring_gap_list_table_fac")))
        ),
        
        # subhed2 for New Gaps (Facility)
        fluidRow(
          column(width = 10, offset = 1,
                 div(style = 'display: flex; align-items: center; margin-top: 20px;',
                     h3(htmlOutput(ns("new_gaps_with_text_fac")), 
                        style = 'font-size: 24px; margin-bottom: 0px; margin-right: 110px;'), # Small space between text and count
                     h3(htmlOutput(ns("new_gaps_count_fac")), 
                        style = 'margin-bottom: 0px;')
                 )
          ),
          column(width = 9, offset = 1,
                 uiOutput(ns("new_hr_fac"))  # Conditionally render hr
          )
        ),
        
        fluidRow(
          column(9, offset = 1,
                 uiOutput(ns("new_gap_list_table_fac")))
        ),
        
        # subhed2 for Closed Gaps (Facility)
        fluidRow(
          column(width = 10, offset = 1,
                 div(style = 'display: flex; align-items: center; margin-top: 20px;',
                     h3(htmlOutput(ns("closed_gaps_with_text_fac")), 
                        style = 'font-size: 24px; margin-bottom: 0px; margin-right: 80px;'), # Small space between text and count
                     h3(htmlOutput(ns("closed_gaps_count_fac")), 
                        style = 'margin-bottom: 0px;')
                 )
          ),
          column(width = 9, offset = 1,
                 uiOutput(ns("closed_hr_fac"))  # Conditionally render hr
          )
        ),
        
        fluidRow(
          column(9, offset = 1,
                 uiOutput(ns("closed_gap_list_table_fac")))
        ),
        
        fluidRow(
          column(width = 10, offset = 1,
                 h3(textOutput(ns("placeholder_fac")), style='font-size: 24px; padding-left: 10px; padding-bottom: 38px')
          )
        )
    )
  )
}

summaryServer <- function(id, data){
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(data(), 
                 {
                   output$recurring_gaps_with_text <- NULL
                   output$recurring_gaps_count <- NULL
                   output$recurring_hr <- NULL
                   output$new_gaps_with_text <- NULL
                   output$new_gaps_count <- NULL
                   output$new_hr <- NULL
                   output$closed_gaps_with_text <- NULL
                   output$closed_gaps_count <- NULL
                   output$closed_hr <- NULL
                   output$recurring_gap_list_table <- NULL
                   output$new_gap_list_table <- NULL
                   output$closed_gap_list_table <- NULL
                   output$placeholder_prac  <- NULL
                   
                   output$recurring_gaps_with_text_fac <- NULL
                   output$recurring_gaps_count_fac <- NULL
                   output$recurring_hr_fac <- NULL
                   output$new_gaps_with_text_fac <- NULL
                   output$new_gaps_count_fac <- NULL
                   output$new_hr_fac <- NULL
                   output$closed_gaps_with_text_fac <- NULL
                   output$closed_gaps_count_fac <- NULL
                   output$closed_hr_fac <- NULL
                   output$recurring_gap_list_table_fac <- NULL
                   output$new_gap_list_table_fac <- NULL
                   output$closed_gap_list_table_fac <- NULL
                   output$placeholder_fac  <- NULL
                   
                   quarter <- tail(unique(data()$quarter), 1)
                   previous_quarter <- tail(unique(data()$quarter), 2)[-2]
                   
                   
                   # gap totals ----
                   # Practitioner
                   gap_data <- make_table_dat(data = data(), type = "Practitioner", quarter = quarter, previous_quarter = previous_quarter)
                   
                   total_gaps <- gap_data %>%
                     group_by(gap_class) %>%
                     summarise(total_gaps = n())
                   
                   #individual gap totals per class
                   first_label <- ifelse(length(unique(data()$quarter)) == 1, "New", "Recurring")

                   message("data()$quarter: ", paste(unique(data()$quarter), collapse = ", "))
                   
                   first_stick_color <- if (length(unique(data()$quarter)) == 1) {
                     message('first_stick_color')
                     nrdp.shiny::kp_colors$light$orange
                   } else {
                     nrdp.shiny::kp_colors$dark$orange
                   }

                   # Determine the correct color based on the quarter comparison
                   first_count_color <- if (length(unique(data()$quarter)) == 1) {
                   message('first_count_color')
                     nrdp.shiny::kp_colors$mid$orange
                   } else {
                     nrdp.shiny::kp_colors$dark$orange
                   }
                   
                   # Check if 'R' (Recurring) exists in total_gaps$gap_class
                   if ('R' %in% total_gaps$gap_class) {
                     output$recurring_gaps_with_text <- renderUI({
                       HTML(paste0(
                         "<div style='display: flex; align-items: center;'>",
                         "<div style='width: 3px; height: 30px; background-color: ", first_stick_color, "; margin-right: 5px;'></div>",
                         "<b>", first_label, "</b>&nbsp;Gaps",
                         "</div>"
                       ))
                     })

                     output$recurring_gaps_count <- renderUI({
                       HTML(paste0(
                         "<span style='font-family: Gotham; font-size: 24px; font-weight: 400; line-height: 24px; letter-spacing: -0.01em; text-align: left; color: ", first_count_color, ";'>",
                         (total_gaps %>% filter(gap_class == 'R'))[, 2],
                         "</span>"
                       ))
                     })

                     output$recurring_hr <- renderUI({
                       hr(style = "border-top: 0.3px solid #C0C0C0; margin-top: -2px; margin-bottom: 10px;")
                     })
                   }


                   # Check if 'N' (New) exists in total_gaps$gap_class
                   if ('N' %in% total_gaps$gap_class) {
                     output$new_gaps_with_text <- renderUI({
                       HTML(paste0(
                         "<div style='display: flex; align-items: center;'>",
                         "<div style='width: 3px; height: 30px; background-color: ", nrdp.shiny::kp_colors$light$orange, "; margin-right: 5px;'></div>",
                         "<b>New</b>&nbsp;Gaps",
                         "</div>"
                       ))
                     })
                     output$new_gaps_count <- renderUI({
                       HTML(paste0(
                         "<span style='font-family: Gotham; font-size: 24px; font-weight: 400; line-height: 24px; letter-spacing: -0.01em; text-align: left; color: ", nrdp.shiny::kp_colors$mid$orange, ";'>",
                         (total_gaps %>% filter(gap_class == 'N'))[, 2], "</span>"
                       ))
                     })
                     output$new_hr <- renderUI({
                       hr(style = "border-top: 0.3px solid #C0C0C0; margin-top: -2px; margin-bottom: 10px;")
                     })
                   }

                   # Check if 'C' (Closed) exists in total_gaps$gap_class
                   if ('C' %in% total_gaps$gap_class) {
                     output$closed_gaps_with_text <- renderUI({
                       HTML(paste0(
                         "<div style='display: flex; align-items: center;'>",
                         "<div style='width: 3px; height: 30px; background-color: ", nrdp.shiny::kp_colors$dark$green, "; margin-right: 5px;'></div>",
                         "<b>Closed</b>&nbsp;Gaps",
                         "</div>"
                       ))
                     })
                     output$closed_gaps_count <- renderUI({
                       HTML(paste0(
                         "<span style='font-family: Gotham; font-size: 24px; font-weight: 400; line-height: 24px; letter-spacing: -0.01em; text-align: left; color: ", nrdp.shiny::kp_colors$dark$green, ";'>",
                         (total_gaps %>% filter(gap_class == 'C'))[, 2], "</span>"
                       ))
                     })
                     output$closed_hr <- renderUI({
                       hr(style = "border-top: 0.3px solid #C0C0C0; margin-top: -2px; margin-bottom: 10px;")
                     })
                   }




                   #getting county/specialty list
                   gap_list <- make_gap_list(gap_data, c('R', "N", 'C'))

                   #making outputs
                   #recurring
                   if(nrow(gap_list$R) != 0){
                     output$recurring_gap_list_table <- renderUI({
                       make_summary_specialty_table(gap_list$R)
                     })
                   }
                   #new
                   if(nrow(gap_list$N) != 0){
                     output$new_gap_list_table <- renderUI({
                       make_summary_specialty_table(gap_list$N)
                     })
                   }
                   #closed
                   if(nrow(gap_list$C) != 0){
                     output$closed_gap_list_table <- renderUI({
                       make_summary_specialty_table(gap_list$C)
                     })
                   }
                   #placeholder text if there are no gaps
                   if(nrow(gap_list$R) == 0 & nrow(gap_list$N) == 0 & nrow(gap_list$C) == 0){
                     output$placeholder_prac <- renderText({"No gaps present"})
                   }

                   
                   #Facilities
                   gap_data_fac <- make_table_dat(data = data(), type = "Facility", quarter = quarter, previous_quarter = previous_quarter)
                   
                   total_gaps_fac <- gap_data_fac %>%
                     group_by(gap_class) %>%
                     summarise(total_gaps = n())
                   
                   #individual gap totals per class
                   first_label <- ifelse(quarter == previous_quarter, "New", "Recurring")
                   
                   if ('R' %in% total_gaps_fac$gap_class) {
                     output$recurring_gaps_with_text_fac <- renderUI({
                       HTML(paste0(
                         "<div style='display: flex; align-items: center;'>",
                         "<div style='width: 3px; height: 30px; background-color: ", first_stick_color, "; margin-right: 5px;'></div>",
                         "<b>", first_label, "</b>&nbsp;Gaps",
                         "</div>"
                       ))
                     })
                     output$recurring_gaps_count_fac <- renderUI({
                       HTML(paste0(
                         "<span style='font-family: Gotham; font-size: 24px; font-weight: 400; line-height: 24px; letter-spacing: -0.01em; text-align: left; color: ", first_count_color, ";'>", 
                         (total_gaps_fac %>% filter(gap_class == 'R'))[, 2], "</span>"
                       ))
                     })
                     output$recurring_hr_fac <- renderUI({
                       hr(style = "border-top: 0.3px solid #C0C0C0; margin-top: -2px; margin-bottom: 10px;")
                     })
                   }
                   
                   if ('N' %in% total_gaps_fac$gap_class) {
                     output$new_gaps_with_text_fac <- renderUI({
                       HTML(paste0(
                         "<div style='display: flex; align-items: center;'>",
                         "<div style='width: 3px; height: 30px; background-color: ", nrdp.shiny::kp_colors$light$orange, "; margin-right: 5px;'></div>",
                         "<b>New</b>&nbsp;Gaps",
                         "</div>"
                       ))
                     })
                     output$new_gaps_count_fac <- renderUI({
                       HTML(paste0(
                         "<span style='font-family: Gotham; font-size: 24px; font-weight: 400; line-height: 24px; letter-spacing: -0.01em; text-align: left; color: ", nrdp.shiny::kp_colors$light$orange, ";'>", 
                         (total_gaps_fac %>% filter(gap_class == 'N'))[, 2], "</span>"
                       ))
                     })
                     output$new_hr_fac <- renderUI({
                       hr(style = "border-top: 0.3px solid #C0C0C0; margin-top: -2px; margin-bottom: 10px;")
                     })
                   }
                   
                   if ('C' %in% total_gaps_fac$gap_class) {
                     output$closed_gaps_with_text_fac <- renderUI({
                       HTML(paste0(
                         "<div style='display: flex; align-items: center;'>",
                         "<div style='width: 3px; height: 30px; background-color: ", nrdp.shiny::kp_colors$dark$green, "; margin-right: 5px;'></div>",
                         "<b>Closed</b>&nbsp;Gaps",
                         "</div>"
                       ))
                     })
                     output$closed_gaps_count_fac <- renderUI({
                       HTML(paste0(
                         "<span style='font-family: Gotham; font-size: 24px; font-weight: 400; line-height: 24px; letter-spacing: -0.01em; text-align: left; color: ", nrdp.shiny::kp_colors$dark$green, ";'>", 
                         (total_gaps_fac %>% filter(gap_class == 'C'))[, 2], "</span>"
                       ))
                     })
                     output$closed_hr_fac <- renderUI({
                       hr(style = "border-top: 0.3px solid #C0C0C0; margin-top: -2px; margin-bottom: 10px;")
                     })
                   }
                   
                   
                   
                   #getting county/specialty list
                   gap_list_fac <- make_gap_list(gap_data_fac, c('R', "N", 'C'))
                   
                   #making outputs
                   #recurring
                   if(nrow(gap_list_fac$R) != 0){
                     output$recurring_gap_list_table_fac <- renderUI({
                       #reactable::renderReactable({
                       make_summary_specialty_table(gap_list_fac$R)
                       #})
                     })
                   }
                   #new
                   if(nrow(gap_list_fac$N) != 0){
                     output$new_gap_list_table_fac <- renderUI({
                       make_summary_specialty_table(gap_list_fac$N)
                     })
                   }
                   
                   #closed
                   if(nrow(gap_list_fac$C) != 0){
                     output$closed_gap_list_table_fac <- renderUI({
                       make_summary_specialty_table(gap_list_fac$C)
                     })
                   }
                   #placeholder text if there are no gaps
                   if(nrow(gap_list_fac$R) == 0 & nrow(gap_list_fac$N) == 0 & nrow(gap_list_fac$C) == 0){
                     output$placeholder_fac <- renderText({"No gaps present"})
                   }
                   
                   
                 }
    )
    
    
    
  })
}