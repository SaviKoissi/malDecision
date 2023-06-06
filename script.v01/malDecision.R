# analysis_results <- reactive({
#   req(input$run_analysis, epi_data(), input$cluster_var,
#       input$classifier1, input$classifier2, input$classifier3, input$dependent_var)
  
  cluster_var <- "cluster" #input$cluster_var
  classifiers <- c("slop", "topo", "slop") #c(input$classifier1, input$classifier2, input$classifier3)
  dependent_var <- 'PC1_prev'#input$dependent_var
  
  cluster_data <- dgt %>%
    dplyr::select(all_of(cluster_var))
  
  prevalence_data <- dgt  %>%
    dplyr::select(all_of(c(classifiers, dependent_var, cluster_var)))
  
  # Perform cluster-specific random forest regression
  clusters <- unique(cluster_data)
  #results <- list()
  clusters <- clusters %>% 
    mutate(importance=lapply(cluster,function(x){
      cluster_indices <- which(cluster_data == x)
      cluster_prevalence <- prevalence_data[cluster_indices, ]
      
      rf_model <- randomForest(as.formula(paste0(dependent_var, " ~ .")),ntree=1000, importance=TRUE, data = cluster_prevalence)  
      importance(rf_model) 
    })) %>% 
    mutate(var_importance = lapply(importance, function(x){
      df <- as.data.frame(x)
      data.frame(Variable = rownames(df), Importance = df$IncNodePurity)  
    }))
  
  
  results <- lapply(clusters$cluster, function(x){
    cluster_indices <- which(cluster_data == x)
    cluster_prevalence <- prevalence_data[cluster_indices, ]
    
    rf_model <- randomForest(as.formula(paste0(dependent_var, " ~ .")), data = cluster_prevalence)
    importance(rf_model) 
  })
  for (cluster in clusters) {
    cluster_indices <- which(cluster_data == cluster)
    cluster_prevalence <- prevalence_data[cluster_indices, ]
    
    rf_model <- randomForest(as.formula(paste0(dependent_var, " ~ .")), data = cluster_prevalence)
    importance <- importance(rf_model)
    
    #results[[paste0("cluster", cluster)]] <- importance
   }
  