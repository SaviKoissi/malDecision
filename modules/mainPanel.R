#------------------------------------------------------------------------------------
# malDecision App
# Original idea: Savi Koissi
# Edit and Enhancement: Gbedegnon Roseric Azondekon
#
# Atlanta, July 2023
#------------------------------------------------------------------------------------

mainPanelModuleOutput <- function(id){
  ns <- NS(id)
  mainPanel(
    tabsetPanel(
      tabPanel("Urbanization map", leafletOutput(ns("map")), 
               helpText("Please check variable importance on the next tab!")),
      tabPanel(
        "Variance Importance",
        dataTableOutput(ns("var_imp_table")),
        plotOutput(ns("var_imp_plot")), 
        helpText("Please check summary statistics on the next tab!")
      ),
      tabPanel("Report", dataTableOutput(ns("report")),
               helpText("Please go back!!!")),
      tabPanel("Data Preview", dataTableOutput(ns("data_prev")),
               helpText("Please go back!!!"))
    )
  )
}

mainPanelModule <- function(input, output, session, configs, selected_country, analysis_results){
  get_country_map <- reactive({
    map <- tm_shape(selected_country()) +
      tm_borders(lwd = 1, col = "black") +
      tm_fill(alpha = 0.1, col = "black")
    map
    #tmap_leaflet(map, in.shiny = TRUE)
  })
  
  get_point <- reactive({ 
    req(configs$epi_data(), configs$long(), configs$lat())
    epi_data_sf <- st_as_sf(configs$epi_data(),
                            coords = c(configs$long(), configs$lat()), 
                            crs = 4326)
    return(epi_data_sf) 
  })
  
  get_map <- reactive({
    req(selected_country(),  get_point())
    
    tm_shape(selected_country()) +
      tm_borders(lwd = 1)+
      tm_shape(get_point()) +
      tm_symbols(col = configs$cluster(), palette ="Paired", size = 0.1)
    
    # map_point #tmap_leaflet(map_point, in.shiny = TRUE)
  })
  
  output$map <- renderTmap({
    get_country_map()
  })
  
  
  # Keep a reference to the projected points
  # projected_points <- reactiveVal(NULL)
  
  # Update the projected points when the "Run Analysis" button is clicked
  observeEvent(configs$run_analysis_btn(), {
    output$map <- renderTmap({
      get_map()
    })
  })
  
  # Generate variance importance table
  output$var_imp_table <- renderDataTable({
    # req(analysis_results())
    datatable(
      analysis_results$var_importance(),
      options = list(scrollX = TRUE, scrollY = "250px")
    )
  })
  
  # Display the plot
  output$var_imp_plot <- renderPlot({
    # req(analysis_results())
    analysis_results$var_imp_plot()
  })
  
  
  # Display summary statistics
  output$report <- renderDataTable({
    # req(analysis_results())
    # analysis_results()$summarise_data
    DT::datatable(
      analysis_results$summarise_data(),
      options = list(scrollX = TRUE, scrollY = "400px")
    )
  })
  
  output$data_prev <- renderDataTable({
    # req(analysis_results())
    # analysis_results()$summarise_data
    DT::datatable(
      configs$epi_data(),
      options = list(scrollX = TRUE, scrollY = "500px")
    )
  })
}
