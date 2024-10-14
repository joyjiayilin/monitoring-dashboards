# utils for dashboard

library(nrdp.shiny)
library(logger)
library(glue)

# lookup table for AD groups ----

group_lookup <- data.frame(user_group = c('NPDM_NRDP_MMP_CO', 'NPDM_NRDP_MMP_NCAL', 'NPDM_NRDP_MMP_SCAL', 'NPDM_NRDP_MMP_GA', 
                                          'NPDM_NRDP_MMP_HI', 'NPDM_NRDP_MMP_MAS', 'NPDM_NRDP_MMP_NW', 'NPDM_NRDP_MMP_WA'), 
                           region = c('Colorado', 'Northern California', 'Southern California', 'Georgia', 
                                      'Hawaii', 'Mid Atlantic States', 'Northwest', 'Washington')
)

statuses <- c('M', 'N', 'R', 'C')


# utils for specialty table ----
with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy::tippy(value, tooltip, ...))
}

col_header_padding <- "170px"

status_colDef <- reactable::colDef(
  width = 50,
  sticky = 'left',
  align = "center",
  style = list(
    background = '#B9B9B9',
    borderRight = "1px solid #FFFFFF",
    borderLeft = "1px solid #FFFFFF",
    borderTop = "1px solid #d3d6dc",
    borderBottom = "1px solid #FFFFFF"
  ),
  
  headerStyle = list(
    `font-size` = "1.0rem",
    `padding-top` = col_header_padding
    
  )
)

#setting status colors for re-use
status_cols <- as.list(c(nrdp.shiny::kp_colors$light$green, nrdp.shiny::kp_colors$light$orange,
                         nrdp.shiny::kp_colors$dark$green, nrdp.shiny::kp_colors$dark$orange))
names(status_cols) <- c('M', 'N', 'C', 'R')

# styling for spec statuses
status_style_func <- function(value){
  if(value == 'M' || value =='Standards Met'){
    color <- status_cols$M
  } else if (value == 'N' || value =='Standards Not Met' || value == 'EN') {
    color <- status_cols$N
  } else if(value == 'C'){
    color <- status_cols$C
  } else if(value == 'R' || value == 'ER'){
    color <- status_cols$R
  }
  
  list(background = color,
       borderRight = "1px solid #FFFFFF",
       borderLeft = "1px solid #FFFFFF",
       borderTop = "1px solid #FFFFFF",
       borderBottom = "1px solid #FFFFFF")
}

#function for setting font color in cells
status_cell_color <- function(value, index, name) { 
  if(value == 'M'){
    htmltools::div(style = glue::glue("color: {status_cols$M};"), value) #background: {status_cols$Y}; 
  } else if(value == 'N'){
    htmltools::div(style = glue::glue("color: {status_cols$N};"), value)
  } else if(value == 'C'){
    htmltools::div(style = glue::glue("color: {status_cols$C};"), value)
  } else if(value == 'R'){
    htmltools::div(style = glue::glue("color: {status_cols$R};"), value)
  } else {
    htmltools::div(style = glue::glue("color: white;"), value)
  }
}

#special styling for county status
county_status_style <- function(value){
  if(value == 'M' || value =='Met'){
    color <- '#57A635'
  } else if (value == 'N' || value =='Not Met') {
    color <- '#D00000'
  } 
  
  list(background = color,  
       color = '#FFFFFF',
       borderRight = "1px solid #FFFFFF",
       borderLeft = "1px solid #FFFFFF",
       borderTop = "1px solid #FFFFFF",
       borderBottom = "1px solid #FFFFFF")
}


#function for creating colDefs for reactable
create_coldefs <- function(data){
  
  numcols <- colnames(data)
  numcols <- numcols[!numcols %in% c('State', 'County')] #removing first columns, which has its own styling
  
  coldefs <- list(
    reactable::colDef(style = status_style_func, 
                      align = "center",
                      width = 35,
                      cell = status_cell_color,
                      headerStyle = list(
                        `font-size` = "1.0rem",
                        width = "fit-content",
                        height = "fit-content", 
                        `white-space` = "nowrap",
                        transform = "rotate(-180deg)",
                        `text-align` = 'left',
                        `writing-mode` = 'vertical-lr',
                        borderRight = "1px solid #d3d6dc"
                      )
    )
  )
  
  
  # replicate list to required length
  coldefs <- rep(coldefs,length(numcols))
  # name elements of list according to cols
  names(coldefs) <- numcols
  
  
  #adding in the styling for the first 3 columns
  #names should remain consistent across region
  coldefs$State <- reactable::colDef(
    style = list(borderBottom = '1px solid #d3d6dc'),
    width = 100,
    sticky = 'left',
    headerStyle = list(
      `font-size` = "1.0rem",
      `padding-top` = col_header_padding
    )
  )
  
  coldefs$County <- reactable::colDef(
    style = list(borderBottom = '1px solid #d3d6dc'),
    width = 100,
    sticky = 'left',
    headerStyle = list(
      `font-size` = "1.0rem",
      `padding-top` = col_header_padding
    )
  )
  
  coldefs$Status <- reactable::colDef(
    width = 100,
    sticky = 'left',
    align = "center",
    headerStyle = list(
      `font-size` = "1.0rem",
      `padding-top` = col_header_padding
    ),
    style = county_status_style
  )
  
  coldefs$M <- status_colDef
  
  coldefs$N <- status_colDef
  
  coldefs$R <- status_colDef
  
  coldefs$C <- reactable::colDef(
    width = 50,
    sticky = 'left',
    align = "center",
    style = list(
      background = '#B9B9B9',
      borderRight = "1px solid #FFFFFF",
      borderLeft = "1px solid #FFFFFF",
      borderTop = "1px solid #FFFFFF",
      borderBottom = "1px solid #FFFFFF"
    ),
    headerStyle = list(
      `font-size` = "1.0rem",
      `padding-top` = col_header_padding
    )
  )
  
  return(coldefs)
}

# make full specialties table----
make_regional_gap_table <- function(data){
  
  #creating colDefs for table
  coldefs <- create_coldefs(data)
  
  highlight_cell <- function(value) {
    if (value == 'E') {
      htmltools::HTML(paste0("<span style='color: white;'>", value, "</span>"))
    } else {
      value
    }
  }
  
  # Get column names that exist in the data
  existing_columns <- intersect(c("Psychiatry", "Ophthalmology", "Physiatry/Rehab Med", "Urology", 
                                  "Critical Care Services", "Primary Care", "Cardiac Catheterization"), 
                                names(data))
  
  # Apply the transformations only to the existing columns
  data2 <- data %>%
    dplyr::mutate(across(all_of(existing_columns), 
                         ~ dplyr::case_when(
                           (State == 'Washington' & County == 'Lewis' & cur_column() == "Psychiatry" & . %in% c('N', 'R')) ~ paste0('E', .),
                           (State == 'Washington' & County == 'Whatcom' & cur_column() %in% c("Ophthalmology", "Physiatry/Rehab Med") & . %in% c('N', 'R')) ~ paste0('E', .),
                           (State == 'Hawaii' & County == 'Honolulu' & cur_column() == "Urology" & . %in% c('N', 'R')) ~ paste0('E', .),
                           (State == 'Virginia' & County == 'Fairfax City' & cur_column() == "Critical Care Services" & . %in% c('N', 'R')) ~ paste0('E', .),
                           (State == 'Virginia' & County == 'Fairfax' & cur_column() == "Primary Care" & . %in% c('N', 'R')) ~ paste0('E', .),
                           (State == 'California' & County == 'Contra Costa' & cur_column() == "Cardiac Catheterization" & . %in% c('N', 'R')) ~ paste0('E', .),
                           TRUE ~ .
                         )))
  
  # Modify columns where needed
  update_coldef_if_exists <- function(col_name, coldefs, highlight_cell) {
    if (col_name %in% names(coldefs)) {
      coldefs[[col_name]] <- reactable::colDef(cell = highlight_cell)
    }
  }
  
  # List of columns to potentially update
  columns_to_update <- c("Psychiatry", 
                         "Ophthalmology", 
                         "Physiatry/Rehab Med", 
                         "Urology", 
                         "Critical Care Services", 
                         "Primary Care", 
                         "Cardiac Catheterization")
  
  # Update only the columns that exist in the original coldefs
  for (col in columns_to_update) {
    update_coldef_if_exists(col, coldefs, highlight_cell)
  }
  
  # create table
  regional_reactable <- reactable::reactable(
    data2,
    defaultPageSize = 50,
    columns = coldefs,
    theme = reactable::reactableTheme(
      style = list(fontFamily = "BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      rowStyle = list(height = "35px"),
      headerStyle = list("align-items" = 'flex-end')
    )
  )
  
  regional_reactable
  
}

# make summary table ----
make_summary_specialty_table <- function(data) {
  
  # Define CSS for softer dashed lines, excluding the header
  css_styles <- "
  
      .main-table .rt-table {
        border-bottom: none; /* Remove the line under the header */
      }
      
      .main-table .rt-tbody .rt-tr {
            border-bottom: none; /* Remove the line between rows */
      }
      
      .main-table .rt-table {
        border-bottom: 0.3px solid #CCCCCC; /* Add solid line under the header */
      }
      
      .main-table .rt-tbody .rt-tr {
        border-bottom: 1px dashed #CCCCCC; /* Softer dashed line between rows */
      }
      
      .main-table .rt-tbody .rt-tr:last-child {
        border-bottom: 1px dashed #CCCCCC; /* Softer dashed line at the end of the table */
      }
      
      
    "
  
  # Include the CSS in the UI
  css_tag <- htmltools::tags$style(css_styles)
  
  spec_summary <- reactable::reactable(
    data,
    defaultColDef = reactable::colDef(
      footerStyle = list(`margin-bottom` = "100px")
    ),
    columns = list(
      State = reactable::colDef(
        width = 100,
        cell = function(value) {
          htmltools::tags$span(style = "font-weight: bold;", value)
        }
      ),
      County = reactable::colDef(
        width = 100,
        cell = function(value) {
          htmltools::tags$span(style = "font-weight: bold;", value)
        }
      )
    ),
    theme = reactable::reactableTheme(
      borderColor = nrdp.shiny::kp_colors$backgrounds$deep_graphite,
      cellPadding = "8px 12px",
      style = list(
        fontFamily = "BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
      )
    )
  )
  
  # Wrap the table in a div with a specific class
  wrapped_table <- htmltools::tags$div(
    class = "main-table",
    spec_summary
  )
  
  # Combine the CSS with the wrapped table
  htmltools::tagList(css_tag, wrapped_table)
}



# legend table ----
# Define legend data and columns
legend_data <- data.frame(
  M = 'M',
  M_text = 'Met',
  N = 'N',
  N_text = 'New Gap',
  R = 'R',
  R_text = 'Recurring Gap',
  C = 'C',
  C_text = 'Closed Gap'
)

defaultcolDef_legend <- reactable::colDef(
  style = list(`line-height` = '10px', `vertical-align` = 'middle'),
  header = "",
  width = 150
)

legend_columns <- lapply(statuses, function(x) {
  
  reactable::colDef(
    width = 32,
    header = "",
    cell = status_cell_color,
    style = list(
      background = status_cols[[x]],
      fontWeight = "bold",
      `text-align` = 'center',
      `border-radius` = '2px',
      `line-height` = '25px',  
      `padding` = '0'  
    )
  )
})

names(legend_columns) <- statuses

legend_reactable <- reactable::reactable(
  legend_data,
  defaultColDef = defaultcolDef_legend,
  columns = legend_columns,
  theme = reactable::reactableTheme(
    borderColor = "white",
    style = list(fontFamily = "BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    rowStyle = list(height = "30px")  
  )
)


##legend 2

legend_data2 <- data.frame(
  EN = 'E',
  EN_text = 'New Gap with CMS Approved Exception',
  ER = 'E',
  ER_text = 'Recurring Gap with CMS Approved Exception'
)

defaultcolDef_legend2 <- reactable::colDef(
  style = list(`line-height` = '10px', `vertical-align` = 'middle'),
  header = "",
  width = 400
)

statuses2 <- c('EN', 'ER')

status_cols2 <- as.list(c(nrdp.shiny::kp_colors$light$orange,
                          nrdp.shiny::kp_colors$dark$orange))
names(status_cols2) <- statuses2

status_cell_color2 <- function(value, index, name) {
  htmltools::div(
    style = "color: white; 
                line-height: 25px; vertical-align: middle; 
                padding: 0;  
                text-align: center; border-radius: 2px; height: 25px; 
                display: flex; align-items: center; justify-content: center;",  
    value
  )
}

legend_columns2 <- lapply(statuses2, function(x) {
  
  reactable::colDef(
    width = 32,
    header = "",
    cell = status_cell_color2,
    style = list(
      background = status_cols2[[x]],
      fontWeight = "bold",
      `text-align` = 'center',
      `border-radius` = '2px',
      `line-height` = '25px',  
      `padding` = '0'  
    )
  )
})

names(legend_columns2) <- statuses2

legend_reactable2 <- reactable::reactable(
  legend_data2,
  defaultColDef = defaultcolDef_legend2,
  columns = legend_columns2,
  theme = reactable::reactableTheme(
    borderColor = "white",
    style = list(fontFamily = "BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    rowStyle = list(height = "30px")  
  )
)


##legend 3

legend_data3 <- data.frame(
  E = 'E = Gap with CMS Approved Exception'
)

defaultcolDef_legend3 <- reactable::colDef(
  style = list(`line-height` = '10px', `vertical-align` = 'middle'),
  header = "",
  width = 350
)

statuses3 <- c('EN', 'ER')
status_cols3 <- as.list(c(nrdp.shiny::kp_colors$light$orange,
                          nrdp.shiny::kp_colors$dark$orange))
names(status_cols3) <- statuses3

status_cell_color3 <- function(value, index, name) {
  htmltools::div(
    style = glue::glue(
      "color: white;
                line-height: 25px; vertical-align: middle; 
                padding: 0;  
                text-align: center; border-radius: 2px; height: 25px; 
                display: flex; align-items: center; justify-content: center;"
    ),  
    value
  )
}

legend_columns3 <- lapply(statuses3, function(x) {
  
  reactable::colDef(
    width = 32,
    header = "",
    cell = status_cell_color3,
    style = list(
      background = status_cols3[[x]],
      fontWeight = "bold",
      `text-align` = 'center',
      `border-radius` = '2px',
      `line-height` = '25px',  
      `padding` = '0'  
    )
  )
})

names(legend_columns3) <- statuses3

legend_reactable3 <- reactable::reactable(
  legend_data3,
  defaultColDef = defaultcolDef_legend3,
  columns = legend_columns3,
  theme = reactable::reactableTheme(
    borderColor = "white",
    style = list(fontFamily = "BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    rowStyle = list(height = "30px")  
  )
)





