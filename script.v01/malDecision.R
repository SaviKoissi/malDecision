library(shiny)
library(dplyr)
library(randomForest)

# UI
ui <- fluidPage(
  titlePanel("malDecision"),
  sidebarLayout(
    sidebarPanel(
      fileInput("epi_file", "Upload Epi Data (CSV)", accept = ".csv"),
      selectInput("cluster_var", "Select Cluster Variable", choices = NULL),
      selectInput("classifier1", "Select Classifier 1 Variable", choices = NULL),
      selectInput("classifier2", "Select Classifier 2 Variable", choices = NULL),
      selectInput("classifier3", "Select Classifier 3 Variable", choices = NULL),
      selectInput("dependent_var", "Select Dependent Variable", choices = NULL),
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Variance Importance", tableOutput("var_imp_table"))
      )
    )
  )
)

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
    updateSelectInput(session, "classifier1", choices = names(epi_data()))
    updateSelectInput(session, "classifier2", choices = names(epi_data()))
    updateSelectInput(session, "classifier3", choices = names(epi_data()))
    updateSelectInput(session, "dependent_var", choices = names(epi_data()))
  })
  
  # Run analysis and generate outputs
  analysis_results <- reactive({
    req(input$run_analysis, epi_data(), input$cluster_var,
        input$classifier1, input$classifier2, input$classifier3, input$dependent_var)
    
    cluster_var <- input$cluster_var
    classifiers <- c(input$classifier1, input$classifier2, input$classifier3)
    dependent_var <- input$dependent_var
    
    cluster_data <- epi_data() %>%
      dplyr::select(all_of(cluster_var))
    
    prevalence_data <- epi_data() %>%
      dplyr::select(all_of(c(classifiers, dependent_var, cluster_var)))
    
    # Perform cluster-specific random forest regression
    clusters <- unique(cluster_data)
    results <- list()
    for (cluster in clusters) {
      cluster_indices <- which(cluster_data == cluster)
      cluster_prevalence <- prevalence_data[cluster_indices, ]
      
      rf_model <- randomForest(as.formula(paste0(dependent_var, " ~ .")), data = cluster_prevalence)
      importance <- importance(rf_model)
      
      results[[as.character(cluster)]] <- importance
    }
    
    results
  })
  
  # Generate variance importance table
  output$var_imp_table <- renderTable({
    req(analysis_results())
    
    var_importance <- lapply(analysis_results(), function(x) {
      data.frame(Variable = rownames(x), Importance = x$MeanDecreaseGini)
    })
    
    do.call(rbind, var_importance)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
