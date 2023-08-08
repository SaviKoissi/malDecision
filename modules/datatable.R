DFTableOutput <- function(id, name, icon_type="table") {
  ns <- NS(id)
  tabPanel(
    name,#"Dataset",
    icon = icon(icon_type),
    tags$style(type = "text/css", ".row {margin-top: 15px;}"),
    tags$head(tags$style("tfoot {display: table-header-group;}")), #
    fluidRow(
      column(12, DT::dataTableOutput(ns("table")))
    )
  )
}

DFTable <- function(input, output, session, dataset) {
  # stopifnot(is.reactive(num_removed_na_rows))
  output$table <- DT::renderDataTable(server = FALSE, {
    dataset()
  }, 
  extensions = c('Buttons','KeyTable'),
  
  options = list(
    keys=TRUE,
    paging = TRUE,
    pageLength = 5,
    scrollX = TRUE,
    searching = TRUE,
    dom = 'Bfrltip',
    buttons = list(
      list(extend = "csv", filename = "data", #text = "Download Full Results", 
           exportOptions = list(
             modifier = list(page = "all")
           )
      ),
      list(extend = "excel", filename = "data", #text = "Download Full Results", 
           exportOptions = list(
             modifier = list(page = "all")
           )
      )
    )#c('csv', 'excel')
  ),
  
  class = "display"
  )
}