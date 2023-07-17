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
    "Hmisc", "sf", "rmapshaper", "maps", "knitr", "kableExtra", "tmap",
    "tmaptools", "gtsummary", "shinycssloaders", "leaflet", "wesanderson", 
    "randomForest"
  )
  
  if (length(setdiff(libs, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(libs, rownames(installed.packages()))
                     , repos = "http://cran.us.r-project.org")
  }
  
  lapply(libs, library, character.only = TRUE, quietly = TRUE)
})


options(shiny.maxRequestSize = 100 * 1024 ^ 2)

data(World)