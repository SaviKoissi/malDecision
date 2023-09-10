#------------------------------------------------------------------------------------
# malDecision App
# Original idea: Savi Koissi
# Edit and Enhancement: Gbedegnon Roseric Azondekon
#
# Atlanta, July 2023
#------------------------------------------------------------------------------------

analysisModule <- function(input, output, session, configs){
  
  summarise_data <- reactive({
    configs$epi_data() %>% summary()
  })
  
  # Run analysis and generate outputs
  var_importance_df <- reactive({
    req(
      configs$run_analysis(),
      configs$epi_data(),
      configs$cluster(),
      configs$classifier(),
      configs$dependent()
    )
    
    cluster_var <- configs$cluster()
    classifiers <- configs$classifier()
    dependent_var <- configs$dependent()
    
    cluster_data <- configs$epi_data() %>%
      dplyr::select(all_of(cluster_var))
    
    prevalence_data <- configs$epi_data() %>%
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
    
    importance_df <- do.call(rbind, var_importance)
    importance_df
  })
  
  # Plot of Variable Importance for each cluster
  var_imp_plot <- reactive({
    var_importance_df() %>% 
      ggplot(
        aes(
          x = reorder(Variable, Importance),
          y = Importance,
          fill = factor(Cluster)
        )
      ) +
      geom_bar(stat = "identity") +
      labs(x = "Variable", y = "Importance", title = "Variable Importance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = wes_palette("Royal1")) + 
      labs(caption = paste("malDecision", format(Sys.Date(), "%Y")))
  })
  
  list(
    var_importance = var_importance_df,
    var_imp_plot = var_imp_plot,
    summarise_data = summarise_data
  )
}