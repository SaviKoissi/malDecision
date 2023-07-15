#
# We build this application to help the community of practice to have an informed decisio
# .

library(shiny)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(sf)
library(rgdal)
library(rmapshaper)
library(mice)
library(raster)
library(move)


ui <- fluidPage(
  navbarPage(
    title = "MalUrb",
    tabPanel(
      title = "Input files",
      "Start importing your epidemiological data for the specific format. Check the guide, 
             Then import the shapefile of the country, 
             Then import raster files for the classification of urban areas,
             Then import the covariates you want to evaluate"
    ),
    tabPanel(title = "Summary", "Brief summary and output of the analysis"),
    tabPanel(title = "Maps and plots", "Some maps"),
    navbarMenu(
      title = "Maps and plots",
      tabPanel("Classification of urban areas"),
      tabPanel("Importance of covariates")
    )
  ),
  navlistPanel(
    tabPanel("Epi_file", fileInput(inputId = "Epi_file", label = "Select an epidemiological data file")),
    tabPanel("Country_file", fileInput(inputId = "Country_file", label = "Select a shapefile of the country")),
    tabPanel("Classification_file1", fileInput(inputId = "Classification_file1", label = "Select a classification file 1")),
    tabPanel("Classification_file2", fileInput(inputId = "Classification_file2", label = "Select a classification file 2")),
    tabPanel("Classification_file3", fileInput(inputId = "Classification_file3", label = "Select a classification file 3")),
    tabPanel("Classification_file4", fileInput(inputId = "Classification_file4", label = "Select a classification file 4")),
    tabPanel("Covariate_file1", fileInput(inputId = "Covariate_file1", label = "Select a covariate file 1")),
    tabPanel("Covariate_file2", fileInput(inputId = "Covariate_file2", label = "Select a covariate file 2")),
    tabPanel("Covariate_file3", fileInput(inputId = "Covariate_file3", label = "Select a covariate file 3")),
    tabPanel("Covariate_file4", fileInput(inputId = "Covariate_file4", label = "Select a covariate file 4")),
    tabPanel("Covariate_file5", fileInput(inputId = "Covariate_file5", label = "Select a covariate file 5")),
    tabPanel("Covariate_file5", fileInput(inputId = "Covariate_file6", label = "Select a covariate file 6")),
    tabPanel("Covariate_fileZip", fileInput(inputId = "Covariate_fileZip", label = "Select a ZIP file containing covariates"))
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
}

shinyApp(ui = ui, server = server)
