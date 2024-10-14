library(dplyr)

source("R/utils.R")
source("R/pull_data.R")

trendsUi <- function(id){
  ns <- NS(id)
  tagList(
    div(class = 'h1', style = 'margin: 36px 50px 50px 150px; border-bottom: 1px solid grey;',
        span("Yearly Progress", style='color:#000000; font-size: 32px;')),
    
      div(class="tab-main-content container-xl",
        
        fluidRow(
          column(4, div(id = 'summary_notes',
                        style = "padding: 30px; margin: 0px 0px 0px 130px;",
                        #uiOutput(ns('conditional_box1')),
                        div(class= 'info_box', style = 'border-left: 4px solid #0078B3; border-radius: 4px; border-color: #0078B3; box-shadow: 1px 1px; margin: 0px 0px 20px 0px',
                            div(textOutput(ns("box2")), style = 'padding: 18px 16px 18px 24px;' ))
                        )
                 ),
          column(8, echarts4rOutput(ns("exec_gap_reduction")))
          )
      ),
    
      div(class = 'h1', style = 'margin: 36px 50px 50px 150px; border-bottom: 1px solid grey;',
          span("Quarterly Progress", style='color:#000000; font-size: 32px;')),
      
        div(class="tab-main-content container-xl",
          
          fluidRow(
            column(4, div(id = 'summary_notes',
                          style = "padding: 30px; margin: 0px 0px 0px 130px;",
                          div(class= 'info_box', style = 'border-left: 4px solid #0078B3; border-radius: 4px; border-color: #0078B3; box-shadow: 1px 1px; margin: 0px 0px 20px 0px',
                              div(textOutput(ns("box4")), style = 'padding: 18px 16px 18px 24px;' ))
                          )
                   ),
            column(8, echarts4rOutput(ns("exec_quarterly_prog")))
            )
        )
    )
}

trendsServer <- function(id, lob){
  moduleServer(id, function(input, output, session) {
    message('making trendsServer')
    
    if (lob == 'MMP') {
      exec_gaps <- mmp_exec_gaps
    } else {
      exec_gaps <- qhp_exec_gaps
    }
    
    current_quarter <- tail(unique(exec_gaps$quarter), 1)
    #dynamic text variables
    total_gaps <- exec_gaps %>% dplyr::filter(quarter=={{current_quarter}}) %>% dplyr::pull(gaps)
    
    gap_change <- 293-total_gaps
    if(gap_change > 0) {directional_text <- "a decrease"} else {directional_text <- 'an increase'}
    
    current_gap <- exec_gaps$gaps[grep(current_quarter, exec_gaps$quarter)]
    prev_gap_q <- exec_gaps$gaps[grep(current_quarter, exec_gaps$quarter)-1]
    
    q_change <- current_gap - prev_gap_q
    if(q_change < 0) {directional_q_text <- "fewer"} else {directional_q_text <- 'more'}
    if(q_change < 0) {directional_q_text2 <- "a decrease"} else {directional_q_text2 <- 'an increase'}
    q_change <- abs(q_change) #taking absolute value for text
    
    percent_q_change <- abs(round(q_change/prev_gap_q * 100, 1))
    
    if (lob == 'MMP') {
      prev_gap_yr <- exec_gaps$gaps[grep(current_quarter, exec_gaps$quarter)-4]
    } else {
      prev_gap_yr <- exec_gaps$gaps[grep(current_quarter, exec_gaps$quarter)-2]
    }
    
    yr_change <- current_gap - prev_gap_yr
    
    if(yr_change < 0) {directional_yr_text <- "fewer"} else {directional_yr_text <- 'more'}
    if(yr_change < 0) {directional_yr_text2 <- "a decrease"} else {directional_yr_text2 <- 'an increase'}
    yr_change <- abs(yr_change) #taking absolute value for text
    
    percent_yr_change <- abs(round(yr_change/prev_gap_yr * 100, 1))
  
    
    if (lob == 'MMP') {

      # output$conditional_box1 <- renderUI({
      #     div(
      #       class = 'info_box',
      #       style = 'border-left: 4px solid #57A635; border-radius: 4px; border-color: #57A635; box-shadow: 1px 1px; margin: 0px 0px 20px 0px',
      #       div(
      #         textOutput("box1"),
      #         style = 'padding: 18px 16px 18px 24px;'
      #       )
      #     )
      # })

      # output$box1 <- renderText({
      #   'The first Medicare Monitoring Program run in Q1 of 2018 recorded 293 gaps.'
      #   })

      output$exec_gap_reduction <- renderEcharts4r({
        make_network_gaps(exec_gaps)
      })
      
      output$exec_quarterly_prog <- renderEcharts4r({
        make_quarterly_progress(exec_gaps)
      })

    } else {

      output$exec_gap_reduction <- renderEcharts4r({
        make_network_gaps_qhp(exec_gaps)
      })
      
      output$exec_quarterly_prog <- renderEcharts4r({
        make_quarterly_progress_qhp(exec_gaps)
      })

    }

    output$box2 <- renderText({
      glue::glue('Compared to the prior year, there are {yr_change} {directional_yr_text} gaps: {directional_yr_text2} of {percent_yr_change}% for the year.')
    })

    output$box4 <- renderText({
      glue::glue('Compared to the prior quarter, there are {q_change} {directional_q_text} gaps: {directional_q_text2} of {percent_q_change}% for the quarter.')
      })
    
    
  })
}