#------------------------------------------------------------------------------------
# malDecision App
# Original idea: Savi Koissi
# Edit and Enhancement: Gbedegnon Roseric Azondekon
#
# Atlanta, July 2023
#------------------------------------------------------------------------------------

options(shiny.maxRequestSize = 100 * 1024^2)

suppressPackageStartupMessages({
  libs <- c(
    "shiny", "shinyWidgets", "shinymeta", "DT", "tidyverse", "rmarkdown", 
    "Hmisc", "sf", "rmapshaper", "maps", "knitr", "kableExtra", "tmap", "rlang",
    "tmaptools", "gtsummary", "shinycssloaders", "leaflet", "randomForest",
    "wesanderson", "shinyjs", "leaflet.extras", "htmltools", "tinytex"
  )
  
  if (length(setdiff(libs, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(libs, rownames(installed.packages()))
                     , repos = "http://cran.us.r-project.org")
  }
  
  lapply(libs, library, character.only = TRUE, quietly = TRUE)
  
  if(!tinytex::is_tinytex()){
    tinytex::install_tinytex()
  }
})


options(shiny.maxRequestSize = 100 * 1024 ^ 2)

data(World)

# Load Modules
source("modules/mainPanel.R")
source("modules/sideBarPanel.R")
source("modules/analysis.R")
source("modules/report.R")
source("modules/datatable.R")

# Load ui Components
source("ui/layoutUI.R")
source("ui/ui.R")

## ui -------------------------------------------------------------------------------
ui <- appUI()


## server ---------------------------------------------------------------------------
server <- function(input, output, session){
  configs <- callModule(sideBarModule, "sideBar")
  analysis_results <- callModule(analysisModule, "analysis", configs)

  selected_country <- reactive(World[World$name == configs$country(), ])

  callModule(mainPanelModule, "mainPanel", configs, selected_country, analysis_results)

  callModule(reportModule, "report", configs, analysis_results, selected_country)

  observeEvent(input$gotoapp, {
    message(input$gotoapp)
    updateTabsetPanel(session, "mainNavBar", selected = "analyzer")
  })

}

shinyApp(ui, server)