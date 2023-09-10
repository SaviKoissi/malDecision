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
source("src/modules/mainPanel.R")
source("src/modules/sideBarPanel.R")
source("src/modules/analysis.R")
source("src/modules/report.R")
source("src/modules/datatable.R")

# Load ui Components
source("src/ui/layoutUI.R")
source("src/ui/ui.R")

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
  
  tags$head(tags$script(
    HTML(
      "
        <!-- Google tag (gtag.js) -->
        <script async src='https://www.googletagmanager.com/gtag/js?id=G-VGTSCPCLQQ'></script>
        <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());

          gtag('config', 'G-VGTSCPCLQQ');
        </script>
      "
    )
  ))
  

}

shinyApp(ui, server, options = list(launch.browser = TRUE))