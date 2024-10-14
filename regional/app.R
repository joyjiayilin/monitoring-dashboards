#load modules
source("R/modules/mod_summary.R")

options(shiny.autoreload = TRUE)


js <- "$(document).on(
      'shiny:idle',
      function() {
       $('.rt-tr-group').css('align-items','flex-end');
    });"

ui <- nrdp_page(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = ("css/npdm-opf.css")),
    tags$script(HTML(js))
  ),
  
  title = "National Health Plan Network Adequacy Monitoring",
  
  nav = nrdp_navbar(
    nrdp_navbar_menu(
      nrdp_navbar_menu_item(
        text = "Summary",
        tabName = "summary",
        selected = TRUE
      ),
      
      nrdp_navbar_menu_item(
        text = "Practitioners",
        tabName = "practitioner"
      ),
      nrdp_navbar_menu_item(
        text = "Facilities",
        tabName = "facilities"
      ),
      uiOutput('address_menu')
    ),
    brand = kp_app_brand(
      "National Health Plan Network Adequacy Monitoring",
      ""
    )
  ),
  
  body = nrdp_body(
    use_print(),
    nrdp_tab_items( 
      div(
        fluidRow(class = 'input-header', style = 'background: #E9EBEE; width: 100%;',
                 column(width = 2.4, offset = 1,
                        uiOutput("region_selector")),
                 column(width = 1,
                        style = "margin-left: 57%;",
                        div(
                          selectInput("lob",
                                      "",
                                      choices = c('MMP', 'QHP'),
                                      selected = 'MMP',
                                      selectize = FALSE),
                          tags$style(HTML("
                                  #lob {
                                  font-size: 26px;
                                  background-color: transparent; 
                                  border-color: transparent;  
                                  border: none; 
                                  box-shadow: none; 
                                  color: #0D1C3D;
                                  }
                                  "))
                        )),
                 column(width = 1.4, style = "margin-left: -10%;",
                        uiOutput("quarter_selector"))
        )
      ),
      
      nrdp_tab_item(
        tabName = "summary",
        div(class = 'tab-header', style = 'height: 50px; background: #E9EBEE; padding:0px 160px 0px 160px; margin-top: -55px;',
            div(img(src="thermometer-outline.svg", 
                    style = 'height: 30px; margin-top: -10px;'), 
                span("Summary", 
                     style='color:#0D1C3D; font-size: 26px;font-style: italic; margin-top: 20px;'))
            
        ),
        
        
        
        div(class="tab-main-content container-xl", style = 'margin-top: 10px;',
            fluidRow(
              column(width=9,
                     summaryUi("summary"),
              ),
              column(width = 2,
                     div(
                       
                       div(
                         p("Release Notes", style='font-family: Gotham; font-size: 18px; font-style: italic; font-weight: 350; line-height: 32px; letter-spacing: -0.01em; text-align: left; color:#0D1C3D; margin: 20px 0px 0px 0px;'),
                         hr(style = "border: 0.5px solid black; margin: 5px 0 0 0; width: calc(100% + 20px); margin-left: -10px; box-sizing: border-box;")
                       ),
                       
                       div(textOutput('notes'), style='font-family: Gotham; font-size: 14px; font-weight: 325; line-height: 20px; letter-spacing: -0.01em; text-align: left; color:#0D1C3D; padding: 20px 0px 30px 0px;'),
                       hr(style = "border-top: 0.3px dashed #C0C0C0;"),
                       
                       div(
                         div(textOutput('regional_notes_title'), style='font-family: Gotham; font-size: 12px; font-weight: 400; line-height: 18px; letter-spacing: -0.01em; text-align: left; color:#0D1C3D; padding: 0px;'),
                         hr(style = "border-top: 0.3px solid #C0C0C0; margin: 0; width: 100%; box-sizing: border-box;")
                       ),
                       
                       div(textOutput('regional_notes'), style='font-family: Gotham; font-size: 14px; font-weight: 325; line-height: 20px; letter-spacing: -0.01em; text-align: left; color:#0D1C3D; padding: 20px 0px 0px 0px;'),
                       hr(style = "border-top: 0.3px dashed #C0C0C0;"),
                       
                       div(
                         style = "text-align: right; margin-top: 40px;",
                         div(
                           p("Data last updated:", 
                             style = 'font-family: Gotham; font-size: 16px; font-style: italic; font-weight: 500; line-height: 24px; text-align: right; color:#0D1C3D; margin: 0;'),
                           div(textOutput('current_time'), 
                               style = "color: #000000; font-family: Gotham; font-size: 14px; line-height: 20px; text-align: right; padding: 0px;"
                           )
                         )
                       )
                     )
              )
            )
        )
      ),
      
      nrdp_tab_item(
        "practitioner",
        div(class = 'tab-header', style = 'height: 50px; background: #E9EBEE; padding: 0px 160px 0px 160px; margin-top: -55px;',
            div(img(src="group-outline.svg", style = 'height: 30px; margin-top:-10px'),
                span("Internal Monitoring Results | Practitioners", 
                     style='color:#0D1C3D; font-size: 26px;'))
        ),
        
        div(class="tab-main-content container-xl", style = 'margin:10px 50px 50px 80px',
            
            fluidRow(
              column(width = 10,
                     div(textOutput("quarter2"), style='color:#57A635; font-size: 14px; margin-bottom: -10px;') # padding-left: 10px; padding-bottom: 20px'
              )
            ),
            
            fluidRow(
              column(width = 3,
                     div(textOutput("region2"), style='color:#0D1C3D; font-size: 24px;')#padding-left: 10px; padding-bottom: 20px'
              ),
              
              column(width = 7,  
                     reactable::reactableOutput("legend_table")),
              column(width = 8, offset = 3,
                     reactable::reactableOutput("legend_table2"))
            ),
            
            fluidRow(
              style = "margin-top: 20px;", 
              column(width = 3, offset = 4,
                     shinyWidgets::prettySwitch(inputId = 'compliant_specs', label = 'Hide Compliant Specialties')
              )
            ),
            
            fluidRow(
              div(style = 'margin-bottom: 10px; overflow-x: scroll;',
                  column(12,
                         reactable::reactableOutput("prac_table"))
              )
              
            )
        )
        
      ),
      
      nrdp_tab_item(
        "facilities",
        #header banner divs
        div(class = 'tab-header', style = 'height: 50px; background: #E9EBEE; padding: 00px 160px 0px 160px; margin-top: -55px;',
            div(img(src="facility-outline.svg", style = 'height: 30px; margin-top:-10px'),
                span("Internal Monitoring Results | Facilities", 
                     style='color:#0D1C3D; font-size: 26px;'))
        ),
        
        div(class="tab-main-content container-xl", style = 'margin:10px 50px 50px 80px',
            fluidRow(
              column(width = 10,
                     div(textOutput("quarter3"), style='color:#57A635; font-size: 14px; margin-bottom: -10px;') # padding-left: 10px; padding-bottom: 20px'
              ),
            ),
            
            fluidRow(
              column(width = 3,
                     div(textOutput("region3"), style='color:#0D1C3D; font-size: 24px;')#padding-left: 10px; padding-bottom: 20px'
              ),
              column(width = 7,  
                     reactable::reactableOutput("legend_table3")),
              column(width = 8, offset = 3,
                     reactable::reactableOutput("legend_table4"))
            ),
            
            fluidRow(
              div(style = 'margin-top: 20px; margin-bottom: 10px; overflow-x: scroll;',
                  column(12,reactable::reactableOutput("fac_table"))
              )
            )
        )
      ),
      
      nrdp_tab_item(
        "address",
        div(class = 'tab-header', style = 'height: 50px; background: #E9EBEE; padding: 0px 160px 0px 160px; margin-top: -55px;',
            div(img(src="group-outline.svg", style = 'height: 30px; margin-top:-10px'),
                span("Number of Providers With Multiple Addresses", 
                     style='color:#0D1C3D; font-size: 24px;'))
        ),
        
        div(class="tab-main-content container-xl", style = 'margin:10px 50px 50px 80px',
            
            fluidRow(
              column(width = 10,
                     div(textOutput("quarter4"), style='color:#57A635; font-size: 14px; margin-bottom: -10px;') # padding-left: 10px; padding-bottom: 20px'
              ),
            ),
            
            fluidRow(
              column(width = 3,
                     div(textOutput("region4"), style='color:#0D1C3D; font-size: 24px;')#padding-left: 10px; padding-bottom: 20px'
              )
            ),
            
            fluidRow(
              style = "margin-top: 20px;",
              column(width = 4, offset = 1,
                     reactable::reactableOutput("address_table", width = "100%")
              ),
              
              column(width = 3, offset = 3,
                     div(
                       div(
                         p("Additional Notes", style='font-family: Gotham; font-size: 18px; font-style: italic; font-weight: 350; line-height: 32px; letter-spacing: -0.01em; text-align: left; color:#0D1C3D; margin: 20px 0px 0px 0px;'),
                         hr(style = "border: 0.5px solid black; margin: 5px 0 0 0; width: calc(100% + 20px); margin-left: -10px; box-sizing: border-box;")
                       ),
                       
                       div(uiOutput('address_text'), style='font-family: Gotham; font-size: 14px; font-weight: 325; line-height: 20px; letter-spacing: -0.01em; text-align: left; color:#0D1C3D; padding: 20px 0px 30px 0px;'),
                       hr(style = "border-top: 0.3px dashed #C0C0C0;"),
                       
                       div(uiOutput('address_text2'), style='font-family: Gotham; font-size: 14px; font-weight: 325; line-height: 20px; letter-spacing: -0.01em; text-align: left; color:#0D1C3D; padding: 20px 0px 0px 0px;'),
                       hr(style = "border-top: 0.3px dashed #C0C0C0;")
                     )
              )
            )
            
        )
        
      )
      
    )
  ),
  
  footer = nrdp_footer(left = HTML('<a href="mailto:NPDM_Network.Adequacy@kp.org" style="color: white;">Questions?</a>'),
                       right = div(img(src="Manhole-Cover_NPDM-Round_Outlined_2s_W.svg", style = 'height: 40px; opacity: 0.2')))
)




server <- function(input, output, session) {
  #message("server function")
  
  output$current_time <- reactive({
    current_time <- as.POSIXct("2024-09-03 09:00:00")
    formatted_time <- format(current_time, "%B %d %Y, %I:%M %p")
    local_tz <- lubridate::with_tz(current_time, tzone = "America/Los_Angeles") %>% format("%Z")
    paste0(formatted_time, " ", local_tz)
  })

  
  lob <- reactive({input$lob})
  
  output$dynamicHeading <- renderText({
    message('dynamicHeading')
    text <- paste0(lob(), " Internal Monitoring Results")
    return(text)
  })
  
  regional_dat <- reactive({
    message('filter lob')
    req(lob())
    if (lob() == "QHP") {
      dat <- qhp_regional_dat %>% as.data.frame()
    } else if (lob() == "MMP") {
      dat <- mmp_regional_dat %>% as.data.frame()
    }
    message("lob filtered")
    return(dat)
  })
  
  # getting user and AD group info ----
  log_info(glue("Current user: {session$user}"))
  log_info(paste("Current user groups: {session$groups}"))
  
  approved_groups <- reactive({
    message("Checking approved groups for lob: ", lob())
    req(lob())
    if (lob() == "MMP") {
      message('approved groups for MMP')
      if (isTruthy(session$groups) && 'NPDM_NRDP_MMP_EXECUTIVE_SUMMARY' %in% session$groups) {
        return(c('Colorado', 'Northern California', 'Southern California', 'Georgia',
                 'Hawaii', 'Mid Atlantic States', 'Northwest', 'Washington'))
      } else {
        return(group_lookup %>% filter(user_group %in% session$groups) %>% pull(region))
      }
      # return(c('Colorado', 'Northern California', 'Southern California', 'Georgia',
      #          'Hawaii', 'Mid Atlantic States', 'Northwest', 'Washington')) #for local testing
      # 
    } else if (lob() == "QHP") {
      message('approved groups for QHP')
      return(c("Hawaii", "Georgia"))
    }
    
    })
  
  
  output$region_selector <- renderUI({
    div(
      selectInput("region",
                  "",
                  selected = approved_groups()[1],
                  choices = approved_groups(),
                  selectize = FALSE,
                  width = "100%"),
      tags$style(HTML("
                                      #region {
                                        font-size: 30px;
                                        background-color: transparent;
                                        border-color: transparent;  
                                        border: none;
                                        box-shadow: none; 
                                        color: #0D1C3D;
                                      }
                                    "))
    )
  })
  
  
  
  quarter_options <- reactive({unique(regional_dat()$quarter)})
  output$quarter_selector <- renderUI({
    div(
      selectInput("quarter",
                  "",
                  selected = tail(quarter_options(), 1),
                  choices = quarter_options(),
                  selectize = FALSE,  
                  width = "100%"),
      tags$style(HTML("#quarter { font-size: 26px; margin-top: 50px; 
                                  background-color: transparent;
                                  border-color: transparent;  
                                  border: none; 
                                  box-shadow: none;
                                  color: #0D1C3D;
                                }
                              "))
    )
  })
  
  
  # filtering for region/quarter ----
  Quarter <- reactive({input$quarter})
  previous_quarter <- reactive({quarter_options()[grep(Quarter(), quarter_options()) - 1]})
  
  region <- reactive({input$region})
  
  regional_data_filtered <- reactive({
    req(region(), regional_dat())
    
    message("filtering regional data")
    
    filtered_data <- regional_dat() %>%
      dplyr::filter(kp_region == region(),
                    quarter %in% c(Quarter(), previous_quarter()))
    
    message("regional data filtered")
    
    return(filtered_data)
  })
  

  output$regional_notes_title <- reactive({
    message("regional_notes_title")
    req(region())
    paste0(region(),
           ' Specific')
  })

  notes_region_filtered <- reactive({
    message("notes_region_filtered")
    req(region())
    notes %>% dplyr::filter(region == region())
  })

  all_notes <- notes %>% filter(region =='all')
  message("all_notes")
  output$notes <- renderText(glue::glue(all_notes$notes))
  output$regional_notes <- renderText({notes_region_filtered()$notes})


  
  # summary tab server ---- 
  message("summary tab")
  summaryServer("summary", data = regional_data_filtered)
  
  # data tables ----
  message("show region and quarter")
  output$region2 <- renderText(region())
  output$region3 <- renderText(region())

  output$quarter2 <- renderText({Quarter()})
  output$quarter3 <- renderText({Quarter()})

  #practitioner table
  prac_data <- reactive({
    message("making pract data")

    make_table_dat(
      data = regional_data_filtered(),
      type = "Practitioner",
      quarter = Quarter(),
      previous_quarter = previous_quarter()
    )

  })


  output$prac_table <- reactable::renderReactable({

      req(prac_data())
      message("loading pract table...")
      prac_data() %>%
        format_table_data() %>%
        filter_compliant_specs(
          data = .,
          compliant_specs = input$compliant_specs
        ) %>%
        make_regional_gap_table()


  })

  #facility table
  fac_data <- reactive({
    message("making fac data")
    make_table_dat(
      data = regional_data_filtered(),
      type = "Facility",
      quarter = Quarter(),
      previous_quarter = previous_quarter()
    )

  })

  output$fac_table <- reactable::renderReactable({

      message("loading fac table...")
      req(fac_data())
      fac_data() %>%
        format_table_data() %>%
        make_regional_gap_table()


  })
  
  #address table
  output$address_menu <- renderUI({
    
    req(Quarter())
    req(lob())
    
    if ((Quarter() == tail(quarter_options(), 1)) & 
        (lob() == 'MMP')
        ) {
      nrdp_navbar_menu_item(
        text = "Address",
        tabName = "address"
      )
    }
    
  })
  
  
  output$address_table <- reactable::renderReactable({
    
    req(region())
    
    mmp_address <- filter(mmp_address_summary, kp_region == region())
    
    mmp_address <- mmp_address[,1:2]
    
    colnames(mmp_address) <- c("Number of Addresses", "Provider Count")
    
    
    if (Quarter() == tail(quarter_options(), 1)) {
      
      reactable::reactable(as.data.frame(mmp_address))
      
    } else {
      
      shinyalert::shinyalert('This is not the most recent quarter. Please select the most recent quarter to view the address summaries, or select other tabs to view adequacy gaps for this quarter selected.')
      reactable::reactable(as.data.frame(mmp_address))
      
    }
    
  })
  
  #address notes
  output$address_text <- renderUI({
    
    HTML(paste0(
      "CMS limits the number of unique practice locations per provider to 12.", 
      "<br/><br/>",
      "CMS suggests that having that number of addresses per provider may exceed the CMS expectation that providers are “regularly available” for office visits."
    ))
  })
  
  output$address_text2 <- renderUI({
    
    req(region())  # Ensure region input exists
    
    # Filter the data based on the region
    mmp_address <- filter(mmp_address_summary, kp_region == region()) %>%
      mutate(add_cnt = as.integer(add_cnt),
             prov_cnt = as.integer(prov_cnt))
    
    # Filter for providers with 11 or more addresses
    high_address_providers <- filter(mmp_address, add_cnt >= 11)
    
    # Count the number of providers with 11 or more addresses
    provider_count <- sum(high_address_providers$prov_cnt, na.rm = TRUE)
    
    # Construct the message
    if (provider_count > 1) {
      HTML(paste0(
        "KP's Medicare Compliance team recommends examining providers with 11 or more addresses. ", 
        "<br/><br/>",
        region(),
        " has ",
        provider_count,
        " providers with 11 or more addresses in the most recent data."
      ))
    } else {
      HTML(paste0(
        "KP's Medicare Compliance team recommends examining providers with 11 or more addresses. ", 
        "<br/><br/>",
        region(),
        " has ",
        provider_count,
        " provider with 11 or more addresses in the most recent data."
      ))
    }
  })

  #legend
  output$legend_table <- reactable::renderReactable({
    legend_reactable
  })
  
  output$legend_table2 <- reactable::renderReactable({
    legend_reactable3
  })
  
  output$legend_table3 <- reactable::renderReactable({
    legend_reactable
  })
  
  output$legend_table4 <- reactable::renderReactable({
    legend_reactable3
  })


}


shinyApp(ui, server)

