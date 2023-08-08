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
    width = 10,
    tabsetPanel(
      tabPanel(
        "Urbanization map", 
        icon = icon("map"), 
        leafletOutput(ns("map"), height = 1000)#, 
        # helpText("Please check variable importance on the next tab!")
      ),
      tabPanel(
        "Variance Importance",
        icon = icon("exclamation-triangle"), br(),
        dataTableOutput(ns("var_imp_table")),
        plotOutput(ns("var_imp_plot"))#, 
        # helpText("Please check summary statistics on the next tab!")
      ),
      tabPanel(
        "Summary Report", 
        icon = icon("book"), 
        reportModuleOutput("report"),
        dataTableOutput(ns("report"))#,
        # helpText("Please go back!!!")
      ),
      DFTableOutput(ns("data_prev"), "Data Preview", "table")
    )
  )
}

mainPanelModule <- function(input, output, session, configs, selected_country, analysis_results){
  map_settings <- reactive({
    leaflet(options = leafletOptions(minZoom = 2.5, worldCopyJump = FALSE, maxBounds = list(list(-80, -180), list(90, 180)))) %>%
      leaflet::addProviderTiles("CartoDB", group = "CartoDB") %>% 
      leaflet::addProviderTiles("Esri", group = "Esri") %>% 
      leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% 
      leaflet::addLayersControl(baseGroups = c("CartoDB", "Esri", "OpenStreetMap"), options = layersControlOptions(collapsed = TRUE)) %>% 
      leaflet::addScaleBar(position = "bottomleft") %>% 
      leaflet.extras::addFullscreenControl() %>% 
      leaflet.extras::addResetMapButton()
  })
  
  color_picker <- reactive({
    leaflet::colorFactor(palette = "RdYlGn", configs$epi_data()[[unique(configs$cluster())]])
  })
  
  get_country_map <- reactive({
    map_settings() %>% 
      addPolygons(data = selected_country(), color = "black", opacity = 1, weight = 1, fillOpacity = 0.1)
  })
  
  get_point <- reactive({ 
    req(configs$epi_data(), configs$long(), configs$lat())
    
    configs$epi_data() %>% 
      mutate(
        lat = configs$epi_data()[[configs$lat()]],
        long = configs$epi_data()[[configs$long()]]
      )
  })
  
  get_map <- reactive({
    req(selected_country(),  get_point())
    
    get_country_map() %>% 
      addCircleMarkers(
        data = get_point(), 
        color = "black",
        fillColor = color_picker()(get_point()[[configs$cluster()]]),
        lat = ~lat, lng = ~long,
        radius = 4, opacity = 1, weight = 1, fillOpacity = 1,
        label = ~htmltools::htmlEscape(NAME),
        popup = ~paste0(
          "<b>", NAME, "</b>",
          "<br>Cluster: ", get_point()[[configs$cluster()]]
        )
      ) %>% 
      addLegend(
        "bottomright", 
        pal = color_picker(), 
        values = unique(get_point()[[configs$cluster()]]), 
        title = "Clusters", 
        opacity = 1
      )
  })
  
  output$map <- renderLeaflet({
    get_country_map()
  })
  
  # Update the projected points when the "Run Analysis" button is clicked
  observeEvent(configs$run_analysis_btn(), {
    output$map <- renderLeaflet({
      get_map()
    })
  })
  
  # Generate variance importance table
  output$var_imp_table <- renderDataTable(server = FALSE, {
    datatable(
      analysis_results$var_importance(),
      extensions = c('Buttons','KeyTable'),
      options = list(
        scrollX = TRUE, 
        scrollY = "250px",
        keys=TRUE,
        paging = TRUE,
        dom = 'Bfrltip',
        buttons = list(
          list(extend = "csv", filename = "data", 
               exportOptions = list(
                 modifier = list(page = "all")
               )
          ),
          list(extend = "excel", filename = "data", 
               exportOptions = list(
                 modifier = list(page = "all")
               )
          )
        )
      )
    )
  })
  
  # Display the plot
  output$var_imp_plot <- renderPlot({
    analysis_results$var_imp_plot()
  })
  
  
  # Display summary statistics
  output$report <- renderDataTable(server = FALSE,{
    DT::datatable(
      analysis_results$summarise_data(),
      extensions = c('Buttons','KeyTable'),
      options = list(
        scrollX = TRUE, 
        scrollY = "400px",
        keys=TRUE,
        paging = TRUE,
        dom = 'Bfrltip',
        buttons = list(
          list(extend = "csv", filename = "data", 
               exportOptions = list(
                 modifier = list(page = "all")
               )
          ),
          list(extend = "excel", filename = "data",  
               exportOptions = list(
                 modifier = list(page = "all")
               )
          )
        )
      )
    )
  })
  
  callModule(DFTable,
             "data_prev",
             dataset = configs$epi_data
  )
}
