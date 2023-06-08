library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(randomForest)

options(shiny.maxRequestSize = 100 * 1024^2)

# UI
ui <- fluidPage(
  tags$img(src = "/Users/koissi/Desktop/R_SIR_Beh/Urbanized-malaria/Urban_malaria/pexels-pixabay-86722.jpg", width = "100%", height = "auto"),
  downloadButton("download", "Download.csv"),
  align = "center",
  tags$h2("malDecision: Interactive Tool For Informed-Decision-Making"),
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  sidebarLayout(
    sidebarPanel(
      id = "sidebarPanel",
      style = "background-color: skyblue; color: white;",
      fileInput("epi_file", "Upload Epi Data (CSV)", accept = ".csv"),
      selectInput("cluster_var", "Select Cluster Variable", choices = NULL),
      selectInput("classifier", "Select Classifier Variable(s)", choices = NULL, multiple = TRUE),
      selectInput("dependent_var", "Select Dependent Variable", choices = NULL),
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Variance Importance", dataTableOutput("var_imp_table")),
        tabPanel("Report", dataTableOutput("report"))
      )
    )
  )
)


# Server
# Server
server <- function(input, output, session) {
  # Read epi data
  epi_data <- reactive({
    req(input$epi_file)
    read.csv(input$epi_file$datapath, stringsAsFactors = FALSE)
  })
  
  # Update choices for cluster variable, classifiers, and dependent variable based on epi data columns
  observeEvent(epi_data(), {
    updateSelectInput(session, "cluster_var", choices = names(epi_data()))
    updateSelectInput(session, "classifier", choices = names(epi_data()))
    updateSelectInput(session, "dependent_var", choices = names(epi_data()))
  })
  
  # Run analysis and generate outputs
  analysis_results <- reactive({
    req(input$run_analysis, epi_data(), input$cluster_var, input$classifier, input$dependent_var)
    
    cluster_var <- input$cluster_var
    classifiers <- input$classifier
    dependent_var <- input$dependent_var
    
    cluster_data <- epi_data() %>%
      dplyr::select(all_of(cluster_var))
    
    prevalence_data <- epi_data() %>%
      dplyr::select(all_of(c(classifiers, dependent_var, cluster_var)))
    
    # Perform cluster-specific random forest regression
    clusters <- unique(cluster_data)
    
    var_importance <- lapply(clusters, function(x) {
      cluster_indices <- which(cluster_data == x)
      cluster_prevalence <- prevalence_data[cluster_indices, ] %>%
        dplyr::select(-!!sym(cluster_var))
      
      rf_model <- randomForest(as.formula(paste0(dependent_var, " ~ .")),
                               ntree = 1000, importance = TRUE,
                               data = cluster_prevalence)
      df <- as.data.frame(importance(rf_model))
      data.frame(Cluster = x, Variable = rownames(df), Importance = df$IncNodePurity) %>%
        arrange(Importance)
    })
    
    var_importance_df <- do.call(rbind, var_importance)
    var_importance_df$Cluster <- as.factor(var_importance_df$Cluster)
    
    var_importance_df
  })
  
  # Generate variance importance table
  output$var_imp_table <- renderTable({
    req(analysis_results())
    analysis_results()
  })
  
  # Generate report table
  output$report <- renderTable({
    req(analysis_results())
    analysis_results()
  })
  
  # Download data as CSV
  output$download <- downloadHandler(
    filename = "report.csv",
    content = function(file) {
      write.csv(analysis_results(), file, row.names = FALSE)
    }
  )
}


# Run the app
shinyApp(ui = ui, server = server)
