mainPanelModuleOutput <- function(id){
  ns <- NS(id)
  mainPanel(
    tabsetPanel(
      tabPanel("Urbanization map", leafletOutput(ns("map2")), 
               helpText("Please check variable importance on the next tab!")),
      tabPanel(
        "Variance Importance",
        dataTableOutput(ns("var_imp_table")),
        plotOutput(ns("var_imp_plot")), 
        helpText("Please check summary statistics on the next tab!")
      ),
      tabPanel("Report", dataTableOutput(ns("report")),
               helpText("Please go back!!!"))
    )
  )
}

mainPanelModule <- function(input, output, session, configs){
  
}