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
      selectInput("classifier", "Select Classifier Variable(s)", choices = NULL, 
                  multiple =TRUE),
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
     updateSelectInput(session, "classifier", choices = names(epi_data()))
    updateSelectInput(session, "dependent_var", choices = names(epi_data()))
  })
  
  # Run analysis and generate outputs
  analysis_results <- reactive({
    req(input$run_analysis, epi_data(), input$cluster_var,
        input$classifier, input$dependent_var)
    
    cluster_var <- input$cluster_var
    classifiers <- input$classifier
    dependent_var <- input$dependent_var
    
    cluster_data <- epi_data() %>%
      dplyr::select(all_of(cluster_var))
    
    prevalence_data <- epi_data() %>%
      dplyr::select(all_of(c(classifiers, dependent_var, cluster_var)))
    
    # Perform cluster-specific random forest regression
    clusters <- unique(cluster_data)
    
    # lapply(clusters$cluster, function(x){
    #   cluster_indices <- which(cluster_data == x)
    #   cluster_prevalence <- prevalence_data[cluster_indices, ] %>% 
    #     dplyr::select(-cluster)
    #   
    #   rf_model <- randomForest(as.formula(paste0(dependent_var, " ~ .")),ntree=1000, importance=TRUE, data = cluster_prevalence)
    #   rf_model$importance 
    # })
    
    clusters %>% 
      mutate(importance=lapply(cluster,function(x){
        cluster_indices <- which(cluster_data == x)
        cluster_prevalence <- prevalence_data[cluster_indices, ] %>% 
          dplyr::select(-!!cluster_var)
        
        rf_model <- randomForest(as.formula(paste0(dependent_var, " ~ .")),ntree=1000, importance=TRUE, data = cluster_prevalence)  
        importance(rf_model) 
      })) %>% 
      mutate(var_importance = lapply(importance, function(x){
        df <- as.data.frame(x)
        data.frame(Variable = rownames(df), Importance = df$IncNodePurity) %>% 
          arrange(Importance)
      }))
    
  })
  
  # Generate variance importance table
  output$var_imp_table <- renderTable({
    req(analysis_results())
    
    # var_importance <- lapply(analysis_results(), function(x) {
    #   df <- as.data.frame(x)
    #   data.frame(Variable = rownames(df), Importance = df$IncNodePurity)
    # })
    # 
    # do.call(rbind, var_importance)
    analysis_results() %>% 
      dplyr::select(-importance) %>% 
      unnest(var_importance)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
