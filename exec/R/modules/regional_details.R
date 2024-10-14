
library(shiny)
library(reactable)
library(dplyr)

source("R/utils.R")
source("R/pull_data.R")

regionUi <- function(id){
  ns <- NS(id)
  
  tagList(
    
    tags$head(
      tags$style(HTML("
        .container {
        display: flex;
        width: 40vw;  /* Responsive width */
        height: auto;  /* Allow height to adjust based on content */
        padding: 1.5vh 2.5vw;  /* Slightly reduced padding */
        gap: 0.8vw;  /* Reduced gap */
        border-radius: 0 0 4px 4px;
        background-color: #F4F5F6;
        opacity: 1; /* Ensure visibility */
      }
      
      .section {
        flex: 1;
        background-color: #F4F5F6;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        position: relative; /* For positioning the vertical stick */
      }
      
      .section:not(:last-child)::after {
        content: '';
        position: absolute;
        right: -1.5vw;  /* Adjusted for responsiveness */
        top: 50%;
        width: 0.8vw;  /* Reduced width for vertical stick */
        height: 50%;
        background-color: #D3D6DC;
        transform: translateY(-50%);
      }
      
      .line {
        width: 100%;
        text-align: center;
        margin: 0.3vh 0;  /* Reduced margin */
        padding: 0.3vh 0;  /* Reduced padding */
        box-sizing: border-box;  /* Ensure padding and borders are included in width/height */
        font-family: Gotham, sans-serif;
      }
      
      .gap-perc {
        font-size: 1vw;  /* Reduced text size */
        font-weight: 325;
        line-height: 1.2vw;  /* Reduced line height */
        letter-spacing: -0.01em;
        text-align: center;
        color: #33A437;
        padding: 0.3vh 0.8vw;  /* Reduced padding */
      }
      
      .gap-totals {
        font-size: 1.4vw;  /* Reduced text size */
        font-weight: 400;
        line-height: 1.8vw;  /* Reduced line height */
        letter-spacing: 0.01em;
        text-align: center;
        color: #000000;
        padding: 0.3vh 0.8vw;  /* Reduced padding */
      }
      
      .gap-class {
        font-size: 1vw;  /* Reduced text size */
        font-weight: 350;
        line-height: 1.2vw;  /* Reduced line height */
        text-align: center;
        color: #000000;
        padding: 0.3vh 0.8vw;  /* Reduced padding */
      }
      
      
          "))
    ),
        
    div(class="tab-main-content container-xl", style = "padding: 30px; margin: 0px 0px 0px 130px;",
        fluidRow(
          column(2, 
                 shinyWidgets::pickerInput(ns("region"), "Select Region(s):",
                                           choices = NULL,
                                           multiple = TRUE
                 )
          ),
          column(width = 8,  offset = 1,
                 reactable::reactableOutput(ns("legend_table"))
          )
          
        ),
        
        uiOutput(ns("plots"))
    )
  )
}

regionServer <- function(id, lob){
  moduleServer(id, function(input, output, session) {
    
    output$legend_table <- renderReactable({
      legend_reactable
    })
    
    if (lob == 'MMP') {
      exec_gaps <- mmp_exec_gaps
      regional_dat <- mmp_regional_dat
    } else if (lob == 'QHP') {
      exec_gaps <- qhp_exec_gaps
      regional_dat <- qhp_regional_dat
    }
    
    message('regionServer for ', lob)
    
    # Update pickerInput based on reactive regional_dat
    observe({
      shinyWidgets::updatePickerInput(
        session,
        "region",
        choices = unique(regional_dat$kp_region),
        selected = unique(regional_dat$kp_region)[1]
      )
    }, autoDestroy = TRUE)
    
    message('regionServer for ', input$region)
    
    quarter <- tail(unique(exec_gaps$quarter), 1)
    previous_quarter <- tail(unique(exec_gaps$quarter), 2)[-2]
    
    ns <- NS(id)
    
    observeEvent(input$region, { 
      message("Regions selected: ", paste(input$region, collapse = ", "))
      
      output$plots <- renderUI({
        
        regional_plot_names <- c('regional_donut_prov', 'regional_line_prov', 
                                 'regional_donut_fac', 'regional_line_fac')
        regional_table_names <- c('regional_table_prov',
                                  'regional_table_fac')
        
        prov_regional_plot_names <- ns(c('regional_donut_prov', 'regional_line_prov'))
        fac_regional_plot_names <- ns(c('regional_donut_fac', 'regional_line_fac'))
        prov_regional_table_names <- ns('regional_table_prov')
        fac_regional_table_names <- ns('regional_table_fac')
        
        regional_plot_names_ns <- ns(regional_plot_names)
        regional_table_names_ns <- ns(regional_table_names)
        output_list <- list()
        
        for(j in input$region){
          
          message("Processing region: ", j)
          
          prov_temp <- lapply(paste0(prov_regional_plot_names, "_", j), function(i) {
            message("Creating provider plot for ID: ", i)
            column(3, echarts4rOutput(i))
            
          })
          
          fac_temp <- lapply(paste0(fac_regional_plot_names, "_", j), function(i) {
            message("Creating facility plot for ID: ", i)
            column(3, echarts4rOutput(i))
            
          })
          
          prov_text <- lapply(paste0(prov_regional_table_names, "_", j), function(i) {
            message("Creating provider table for ID: ", i)
            column(6, uiOutput(i))
            
          })
          
          fac_text <- lapply(paste0(fac_regional_table_names, "_", j), function(i) {
            message("Creating facility table for ID: ", i)
            column(6, uiOutput(i))
            
          })
          
          
          output_list[[j]] <- div(
            span(j, style = 'color:#0D1C3D; font-size: 32px; padding: 0px 0px 0px 0px;'),
            hr(style = "border-top: 0.3px solid #C0C0C0; margin-top: 0; margin-bottom: 20px; width: 100%; box-sizing: border-box;"),
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",  # Align items horizontally and center vertically
              div(
                style = "display: flex; align-items: center; width: 50%;",  # Make PROVIDERS tab wider
                div(
                  style = "display: flex; align-items: center; background-color: #002343; width: 95%; color: #FFFFFF; height: 90px; font-size: 32px; 
            border: 0px 0px 2px 0px; border-radius: 8px 8px 0px 0px; border-style: solid; padding: 12px 16px; box-sizing: border-box;",
                  span("PROVIDERS", style = "flex: 1;"),
                  actionButton("toggle_providers_legend", label = "", icon = icon("plus"), 
                               style = "background-color: #002343; color: #FFFFFF; border: none; margin-left: 10px; height: 50px; width: 40px;")
                )
              ),
              div(
                style = "display: flex; align-items: center; width: 50%;",  # Make FACILITIES tab wider
                div(
                  style = "display: flex; align-items: center; background-color: #E9EBEE; width: 95%; color: #0D1C3D; height: 90px; font-size: 32px; 
            border: 0px 0px 2px 0px; border-radius: 8px 8px 0px 0px; border-style: solid; padding: 12px 16px; box-sizing: border-box;",
                  span("FACILITIES", style = "flex: 1;"),
                  actionButton("toggle_facilities_legend", label = "", icon = icon("plus"), 
                               style = "background-color: #E9EBEE; color: #0D1C3D; border: none; margin-left: 10px; height: 50px; width: 40px;")
                )
              )
            ),
            uiOutput("legend_ui_providers"),
            uiOutput("legend_ui_facilities"),
            fluidRow(prov_temp, 
                     fac_temp),
            fluidRow(prov_text,
                     fac_text)
          )
          
        }
        
        do.call(tagList, output_list)
        
      })
      
      
      
      
     
      for(k in input$region){
        local({
          region_loop <- k
          message("Rendering plots for region: ", region_loop)
          
          #providers
          plotname1 <- paste0("regional_donut_prov_", k)
          message("Rendering donut chart for providers: ", plotname1)
          
          output[[plotname1]] <- renderEcharts4r({
            make_regional_donut(regional_dat, current_quarter = quarter, previous_quarter = previous_quarter, region = region_loop, provider_type = 'Practitioner')
          })
          
          plotname2 <- paste0("regional_line_prov_", k)
          message("Rendering line chart for providers: ", plotname2)
          
          output[[plotname2]] <- renderEcharts4r({
            make_regional_line(regional_dat, region = region_loop, provider_type = 'Practitioner')
          })
          
          #facilities
          plotname3 <- paste0("regional_donut_fac_", k)
          message("Rendering donut chart for facilities: ", plotname3)
          
          output[[plotname3]] <- renderEcharts4r({
            make_regional_donut(regional_dat, current_quarter = quarter, previous_quarter = previous_quarter, region = region_loop, provider_type = 'Facility')
          })
          
          plotname4 <- paste0("regional_line_fac_", k)
          message("Rendering line chart for facilities: ", plotname4)
          
          output[[plotname4]] <- renderEcharts4r({
            make_regional_line(regional_dat, region = region_loop, provider_type = 'Facility')
          })
          
          
          
          # Render text
          render_regional_table <- function(dat, provider_type) {
            if ("gap_perc" %in% colnames(dat)) {
              sections <- lapply(1:nrow(dat), function(i) {
                div(class = "section",
                    div(class = "line gap-perc", dat$gap_perc[i]), 
                    div(class = "line gap-totals", dat$gap_totals[i]),
                    div(class = "line gap-class", dat$gap_class[i])
                )
              })
            } else {
              sections <- lapply(1:nrow(dat), function(i) {
                div(class = "section",
                    div(class = "line gap-totals", dat$gap_totals[i]),
                    div(class = "line gap-class", dat$gap_class[i])
                )
              })
            }
            div(class = "container", sections)
          }
          
          
          #providers
          tablename1 <- paste0("regional_table_prov_", k)
          message("Rendering table for providers: ", tablename1)
          
          output[[tablename1]] <- renderUI({
            dat <- make_regional_table(regional_dat, current_quarter = quarter, previous_quarter = previous_quarter, region = region_loop, provider_type = 'Practitioner')
            render_regional_table(dat, provider_type = 'Practitioner')
          })
          
          # facilities
          tablename2 <- paste0("regional_table_fac_", k)
          message("Rendering table for facilities: ", tablename2)
          
          output[[tablename2]] <- renderUI({
            dat <- make_regional_table(regional_dat, current_quarter = quarter, previous_quarter = previous_quarter, region = region_loop, provider_type = 'Facility')
            render_regional_table(dat, provider_type = 'Facility')
          })
          
          
        })
      }
      
    })
    
    
    
  })
}