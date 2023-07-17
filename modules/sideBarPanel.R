sideBarModuleOutput <- function(id){
  ns <- NS(id)
  sidebarPanel(
    id = ns("sidebarPanel"),
    selectInput(ns("country"), "Select a country:", choices = unique(World$name)),
    fileInput(ns("epi_file"), "Upload Epi Data", accept = c(".csv", ".txt", ".xlsx", ".xls")),
    
    selectInput(ns("latitude_column"), "Select Latitude (x)", choices = NULL),
    selectInput(ns("longitude_column"), "Select Longitude (y)", choices = NULL),
    selectInput(ns("cluster_var"), "Select Degree of urbanization (Cluster Variable)", choices = NULL),
    selectInput(
      ns("classifier"),
      "Select Determinant(s) (Classifier Variable(s))",
      choices = NULL,
      multiple = TRUE
    ),
    selectInput(ns("dependent_var"), "Select Dependent Variable (Prevalence /Incidence)", choices = NULL),
    actionButton(ns("run_analysis"), "Run Analysis")
  )
}

sideBarModule <- function(input, output, session){
  
  # Read epi data
  epi_data <- reactive({
    req(input$epi_file)
    read.csv(input$epi_file$datapath, stringsAsFactors = FALSE)
  })
  
  selected_vars <- reactiveValues(cluster_var = NULL, classifier = NULL)
  
  observeEvent(input$cluster_var, {
    # Store selected cluster_var
    selected_vars$cluster_var <- input$cluster_var
  })
  
  observeEvent(input$classifier, {
    # Store selected classifier
    selected_vars$classifier <- input$classifier
  })
  
  observeEvent(epi_data(), {
    available_vars <- names(epi_data())
    
    updateSelectInput(session, "latitude_column", choices = available_vars, selected = "x")
    updateSelectInput(session, "longitude_column", choices = available_vars, selected = "y")
    
    # Update cluster_var selectInput
    updateSelectInput(
      session,
      "cluster_var",
      choices = available_vars,
      selected = "cluster"
    )
    
    # Update classifier selectInput, excluding selected cluster_var
    available_vars_classifier <- setdiff(available_vars, selected_vars$cluster_var)
    updateSelectInput(
      session,
      "classifier",
      choices = available_vars_classifier,
      selected = c("topo", "vaccinations")
    )
    
    # Update dependent_var selectInput, excluding selected cluster_var and classifier
    available_vars_dependent <- setdiff(available_vars_classifier, selected_vars$classifier)
    updateSelectInput(session, "dependent_var", choices = available_vars_dependent, selected = "PC1")
  })
  
  list(
    country = reactive(input$country),
    lat = reactive(input$latitude_column),
    long = reactive(input$longitude_column),
    cluster = reactive(input$cluster_var),
    classifier = reactive(input$classifier),
    dependent = reactive(input$dependent_var),
    run_analysis_btn = reactive(input$run_analysis),
    epi_data = epi_data
  )
}