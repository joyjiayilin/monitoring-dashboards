#load modules
source("R/modules/program_trends.R")
source("R/modules/regional_details.R")

options(shiny.autoreload = TRUE)

ui <- nrdp_page(
  
  title = "National Health Plan Network Adequacy Monitoring",
  
  nav = nrdp_navbar(
    nrdp_navbar_menu(
      nrdp_navbar_menu_item(
        text = "Trends",
        tabName = "program_trends",
        selected = TRUE
      ),
      nrdp_navbar_menu_item(
        text = "Regional Highlights",
        tabName = "regional_details"
      )
    ),
    
    # nrdp_navbar_menu(
    #   div(
    #     selectInput("lob",
    #                 "",
    #                 choices = c('MMP', 'QHP'),
    #                 selected = 'MMP',
    #                 selectize = FALSE,
    #                 width = "100%"),
    #     tags$style(HTML("
    #       /* Style the entire dropdown text */
    #       #lob {
    #         font-size: 32px;
    #         background-color: #0D1C3D;  /* Makes the dropdown background clear */
    #         border: none;  /* Removes the border */
    #         box-shadow: none;  /* Removes any shadow that might be acting as a border */
    #         color: #FFFFFF;  /* Sets the default dropdown text color to #0D1C3D */
    #       }
    #       /* Style the dropdown options */
    #       #lob option {
    #         color: #FFFFFF;  /* Sets the options text color to #0D1C3D */
    #         background-color: #0D1C3D;  /* Makes the background of options transparent */
    #       }
    #       /* Style the selected option */
    #       #lob option:checked {
    #         background-color: transparent;  /* Keeps the background transparent */
    #         color: #FFFFFF;  /* Sets the selected option text color to white */
    #       }
    #     "))
    #   )
    # ),
    
    brand = kp_app_brand(
      "National Health Plan Network Adequacy Monitoring",
      "v0.3.1 | Built at KP by NPDM"
    )
  ),
  
  body = nrdp_body(
    nrdp_tab_items(
      div(
        fluidRow(class = 'input-header', style = 'background: #E9EBEE; width: 100%;',
                 column(width = 2, offset = 1,
                        div(
                          selectInput("lob",
                                      "",
                                      choices = c('MMP', 'QHP'),
                                      selected = 'MMP',
                                      selectize = FALSE),
                          tags$style(HTML("
                                  #lob {
                                  font-size: 34px;
                                  background-color: transparent; 
                                  border-color: transparent;  
                                  border: none; 
                                  box-shadow: none; 
                                  color: #0D1C3D;
                                  }
                                  "))
                        ))
        )
      ),
      
      # div(
      #   fluidRow(class = 'input-header', style = 'background: #E9EBEE;',
      #            column(width = 4, offset = 1,
      #                   div(
      #                     textOutput('dynamicHeading'),
      #                     style = "margin-top: 100px; margin-bottom: 0px; font-family: Gotham; font-size: 16px; font-style: italic; font-weight: 300; line-height: 24px; letter-spacing: 0.01em; text-align: left;"
      #                   )
      #            ),
      #            
      #            column(width = 4, offset = 7,
      #                   div(style = "text-align: right; width: 100%; font-size: 24px; font-weight: 350; line-height: 20px;", 
      #                       nrdp_navbar_menu(
      #                         class = "navbar-menu", 
      #                         nrdp_navbar_menu_item(
      #                           text = "Trends",
      #                           tabName = "program_trends",
      #                           selected = TRUE
      #                         ),
      #                         nrdp_navbar_menu_item(
      #                           text = "Regional Highlights",
      #                           tabName = "regional_details"
      #                         )
      #                       )
      #                   )
      #            )
      #   )
      # ),
      
      nrdp_tab_item(
        tabName = "program_trends",
        div(class = 'tab-header', 
            style = 'height: 100px; background: #E9EBEE; padding: 30px 160px 0px 160px; display: flex; justify-content: space-between; align-items: center;',
            div(img(src="line-chart-outline.png", style = 'height: 30px; margin-top:-15px'),
                span("Program Trends", 
                     style = 'color:#0D1C3D; font-size: 30px; padding-top: 20px; margin-left:15px')
            ),
            div(
              style = "text-align: right;",
              div(
                p("Data last updated:", 
                  style = 'font-family: Gotham; font-size: 16px; font-style: italic; font-weight: 500; line-height: 24px; text-align: right; color:#0D1C3D; margin: 0;'),
                div(textOutput('current_time1'), 
                    style = 'color:#0D1C3D; font-size: 16px; padding:0px; margin: 0;')
              )
            )
        ),
        uiOutput("trendsContent")
      ),
      
      
      nrdp_tab_item(
        tabName = "regional_details",
        div(class = 'tab-header', 
            style = 'height: 100px; background: #E9EBEE; padding: 30px 160px 0px 160px; display: flex; justify-content: space-between; align-items: center;',
            div(img(src="bar-chart-outline.png", style = 'height: 30px; margin-top:-15px'),
                span("Regional Details", 
                     style = 'color:#0D1C3D; font-size: 30px; padding-top: 20px; margin-left:15px')
            ),
            div(
              style = "text-align: right;",
              div(
                p("Data last updated:", 
                  style = 'font-family: Gotham; font-size: 16px; font-style: italic; font-weight: 500; line-height: 24px; text-align: right; color:#0D1C3D; margin: 0;'),
                div(textOutput('current_time2'), 
                    style = 'color:#0D1C3D; font-size: 16px; padding:0px; margin: 0;')
              )
            )
        ),
          uiOutput("regionContent")
        )
    )
  ),
  
  footer = nrdp_footer(left = HTML('<a href="mailto:NPDM_Network.Adequacy@kp.org" style="color: white;">Questions?</a>'),
                       right = div(img(src="Manhole-Cover_NPDM-Round_Outlined_2s_W.svg", style = 'height: 40px; opacity: 0.2')))
)


server <- function(input, output, session) {
  message("server function")
  
  current_time <- reactive({
    current_time <- as.POSIXct("2024-09-03 09:00:00")
    formatted_time <- format(current_time, "%B %d %Y, %I:%M %p")
    local_tz <- lubridate::with_tz(current_time, tzone = "America/Los_Angeles") %>% format("%Z")
    paste0(formatted_time, " ", local_tz)
  })
  
  output$current_time1 <- renderText({ current_time() })
  output$current_time2 <- renderText({ current_time() })
  
  
  output$dynamicHeading <- renderText({
    message('dynamicHeading')
    text <- paste0(input$lob, " Internal Monitoring Results")
    return(text)
  })
  
  
  
  observeEvent(input$lob, {
  
  
    output$trendsContent <- renderUI({
      message('Rendering trendsContent')
      trendsUi("program_trends")
    })

    trendsServer("program_trends", input$lob)


    output$regionContent <- renderUI({
      message('Rendering regionContent')
      regionUi("regional_details")
    })

    regionServer("regional_details", input$lob)
    
    
  })

}


shinyApp(ui, server)
