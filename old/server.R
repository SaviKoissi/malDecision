#------------------------------------------------------------------------------------
# malDecision App
# Original idea: Savi Koissi
# Edit and Enhancement: Gbedegnon Roseric Azondekon
#
# Atlanta, July 2023
#------------------------------------------------------------------------------------

server <- function(input, output, session) {
  #selected country
  selected_country <- reactive(World[World$name == input$country, ])
  
  # Read epi data
  epi_data <- reactive({
    req(input$epi_file)
    read.csv(input$epi_file$datapath, stringsAsFactors = FALSE)
  })
  
  get_country_map <- reactive({
    map <- tm_shape(selected_country()) +
      tm_borders(lwd = 1, col = "skyblue") +
      tm_fill(alpha = 0.5, col = "skyblue")
    map
  })
  
  get_point <- reactive({ 
    req(epi_data(), input$longitude_column, input$latitude_column)
    epi_data_sf <- st_as_sf(epi_data(),
                            coords = c(input$longitude_column, input$latitude_column), 
                            crs = 4326)
    return(epi_data_sf) 
  })
  
  get_map <- reactive({
    req(selected_country(),  get_point())
    
    tm_shape(selected_country()) +
      tm_borders()+
      tm_shape(get_point()) +
      tm_symbols(col = input$cluster_var, palette ="Paired", size = 0.1)
    
    # map_point #tmap_leaflet(map_point, in.shiny = TRUE)
  })
  
  output$map2 <- renderTmap({
    get_country_map()
  })
  
  
  
  # Keep a reference to the projected points
  projected_points <- reactiveVal(NULL)
  
  # Update the projected points when the "Run Analysis" button is clicked
  observeEvent(input$run_analysis, {
    output$map2 <- renderTmap({
      get_map() 
    })
  })
  
  # Update choices for cluster variable, classifiers, and dependent variable based on epi data columns
  observeEvent(epi_data(), {
    updateSelectInput(session, "latitude_column", choices = names(epi_data()), selected = "x")
    updateSelectInput(session, "longitude_column", choices = names(epi_data()), selected = "y")
    updateSelectInput(session, "cluster_var", choices = names(epi_data()), selected = "cluster")
    updateSelectInput(session, "classifier", choices = names(epi_data()), selected = c("topo", "vaccination"))
    updateSelectInput(session, "dependent_var", choices = names(epi_data()), selected = "PC1")
  })
  
  
  selected_vars <- reactiveValues(cluster_var = NULL,
                                  classifier = NULL)
  
  observeEvent(epi_data(), {
    # Update column choices based on geometry type
    req(input$epi_file)
    df <- epi_data()
    available_vars <- names(epi_data())
    
    # Update cluster_var selectInput
    updateSelectInput(
      session,
      "cluster_var",
      choices = available_vars,
      selected = selected_vars$cluster_var
    )
    
    # Update classifier selectInput, excluding selected cluster_var
    available_vars_classifier <-
      setdiff(available_vars, selected_vars$cluster_var)
    updateSelectInput(
      session,
      "classifier",
      choices = available_vars_classifier,
      selected = selected_vars$classifier
    )
    
    # Update dependent_var selectInput, excluding selected cluster_var and classifier
    available_vars_dependent <-
      setdiff(available_vars_classifier, selected_vars$classifier)
    updateSelectInput(session, "dependent_var", choices = available_vars_dependent)
  })
  
  
  observeEvent(input$cluster_var, {
    # Store selected cluster_var
    selected_vars$cluster_var <- input$cluster_var
  })
  
  observeEvent(input$classifier, {
    # Store selected classifier
    selected_vars$classifier <- input$classifier
  })
  
  
  # Run analysis and generate outputs
  analysis_results <- reactive({
    req(
      input$run_analysis,
      epi_data(),
      input$cluster_var,
      input$classifier,
      input$dependent_var
    )
    
    cluster_var <- input$cluster_var
    classifiers <- input$classifier
    dependent_var <- input$dependent_var
    
    summarise_data <- epi_data() %>% 
      summary()
    #do.call(rbind, s_data)
    
    
    cluster_data <- epi_data() %>%
      dplyr::select(all_of(cluster_var))
    
    prevalence_data <- epi_data() %>%
      dplyr::select(all_of(c(
        classifiers, dependent_var, cluster_var
      )))
    
    # Perform cluster-specific random forest regression
    clusters <- unique(cluster_data)
    
    var_importance <- lapply(clusters$cluster, function(x) {
      cluster_indices <- which(cluster_data == x)
      cluster_prevalence <- prevalence_data[cluster_indices, ] %>%
        dplyr::select(-{
          {
            cluster_var
          }
        })
      
      rf_model <- randomForest(
          as.formula(paste0(dependent_var, " ~ .")),
          ntree = 1000,
          importance = TRUE,
          data = cluster_prevalence
        )
      df <- as.data.frame(importance(rf_model))
      data.frame(
        Cluster = x,
        Variable = rownames(df),
        Importance = df$IncNodePurity
      ) %>%
        arrange(Importance)
    })
    
    var_importance_df <- do.call(rbind, var_importance)
    
    # Plot of Variable Importance for each cluster
    var_imp_plot <-
      ggplot(var_importance_df,
             aes(
               x = reorder(Variable, Importance),
               y = Importance,
               fill = factor(Cluster)
             )) +
      geom_bar(stat = "identity") +
      labs(x = "Variable", y = "Importance", title = "Variable Importance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = wes_palette("Royal1")) + #low = "blue", high = "darkblue" wes_palette("FantasticFox1")
      labs(caption = paste("malDecision", format(Sys.Date(), "%Y")))
    
    list(
      var_importance = var_importance_df,
      var_imp_plot = var_imp_plot,
      summarise_data = summarise_data
    )
  })
  
  
  # Generate variance importance table
  output$var_imp_table <- renderDataTable({
    req(analysis_results())
    datatable(analysis_results()$var_importance)
  })
  
  # Display the plot
  output$var_imp_plot <- renderPlot({
    req(analysis_results())
    analysis_results()$var_imp_plot
  })
  
  
  # Display summary statistics
  output$report <- renderDataTable({
    req(analysis_results())
    # analysis_results()$summarise_data
    datatable(analysis_results()$summarise_data)
    
  })
  
  
  # Render the leaflet map
  # output$map <- renderTmap({
  #   get_country_map()
  # })
  
  report_envir <- reactive({
    new_envir <- rlang::new_environment(
      list(
        epi_data = epi_data(),
        summarise_data = analysis_results()$summarise_data,
        var_importance_df = analysis_results()$var_importance,
        var_imp_plot = analysis_results()$var_imp_plot,
        country_map = get_country_map(),
        projected_points = get_point(),
        country = get_map(), 
        selected_country = selected_country(),
        cluster_var = input$cluster_var,
        dependent = input$dependent_var,
        classifier = input$classifier
      )
    )
    parent.env(new_envir) <- globalenv()
    new_envir
  })
  
  
  # Download CSV
  output$download <- downloadHandler(
    filename = function() {
      paste0("malDecision_Data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(analysis_results()$var_importance, file, row.names = FALSE)
    }
  )
  
  
  # Generate PDF report
  output$download_report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Render the R Markdown report
      rmarkdown::render("report.Rmd",
                        output_file = file,
                        envir = report_envir())
    }
  )
  
}