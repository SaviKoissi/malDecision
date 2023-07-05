library(shiny)
library(shinyWidgets)
library(shinymeta)
library(DT)
library(tidyverse)
library(randomForest)
library(rmarkdown)
library(Hmisc)
library(sf)
library(rgdal)
library(rmapshaper)
library(tmap)
library(leaflet)
library(rmapshaper)
library(maps)
library(wesanderson)

options(shiny.maxRequestSize = 100 * 1024 ^ 2)

data(World)

# UI
ui <- fluidPage(
  titlePanel(
    title = div(
      img(
        src = "FullLogo_Transparent_NoBuffer.png",
        width = "30%",
        height = "20%",
        style = "float: left:"
      ),
      "malDecision: Interactive Tool For Informed-Decision-Making"
    )
  ),
  
  downloadButton("download", "Download.csv"),
  downloadButton("download_report", "Download Report"),
  
  align = "center",
  sidebarLayout(
    sidebarPanel(
      id = "sidebarPanel",
      selectInput("country", "Select a country:", choices = unique(World$name)),
      fileInput("epi_file", "Upload Epi Data (CSV)", accept = ".csv"),
      
      selectInput("latitude_column", "Select Latitude", choices = NULL),
      selectInput("longitude_column", "Select Longitude", choices = NULL),
      selectInput("cluster_var", "Select Cluster Variable", choices = NULL),
      selectInput(
        "classifier",
        "Select Classifier Variable(s)",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput("dependent_var", "Select Dependent Variable", choices = NULL),
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Map", leafletOutput("map"), 
               helpText("Please check urbanization map after running the analysis!")), 
      tabPanel("Urbanization map", leafletOutput("map2"), 
               helpText("Please check variable importance on the next tab!")),
      tabPanel(
        "Variance Importance",
        dataTableOutput("var_imp_table"),
        plotOutput("var_imp_plot"), 
        helpText("Please check summary statistics on the next tab!")
      ),
      tabPanel("Report", dataTableOutput("report"),
              helpText("Please go back!!!"))
    ))
  ),
  
  
  tags$head(tags$script(
    HTML(
      "
        <!-- Google tag (gtag.js) -->
        <script async src='https://www.googletagmanager.com/gtag/js?id=G-VGTSCPCLQQ'></script>
        <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());

          gtag('config', 'G-VGTSCPCLQQ');
        </script>
      "
    )
  ))
)

# Server
server <- function(input, output, session) {
  # Read epi data
  epi_data <- reactive({
    req(input$epi_file)
    read.csv(input$epi_file$datapath, stringsAsFactors = FALSE)
  })
  
  color_palette <- reactive(
    colorFactor(palette = "Dark2", 
                domain = unique(epi_data()[[input$cluster_var]]))
  )
  #wesanderson::wes_palette("BottleRocket2"),
  get_map <- reactive({
    leaflet() %>% 
      addTiles() %>% 
      addCircles(
        lng = epi_data()[[input$longitude_column]],
        lat = epi_data()[[input$latitude_column]],
        radius = 2,
        fillOpacity = 1,
        color = color_palette()(epi_data()[[input$cluster_var]])
      ) %>% 
      addLegend(
        "bottomright",
        pal = color_palette(),
        values = epi_data()[[input$cluster_var]],
        title = "Degree of urbanization"
      )
  })
  
  selected_country <- reactive(World[World$name == input$country, ])
  
  get_country_map <- reactive({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = selected_country(),
                  color = "skyblue",
                  fillOpacity = 0.5)
  })
  
  
  
  output$map2 <- renderLeaflet({
    # req(epi_data(), input$latitude_column,
    #     input$longitude_column, input$cluster_var)
    map <- try(get_map(), silent = TRUE)
    if(all(class(map) == "try-error")){
      get_country_map()
    } else{
      map
    }
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
    updateSelectInput(session,
                      "cluster_var",
                      choices = available_vars,
                      selected = selected_vars$cluster_var)
    
    # Update classifier selectInput, excluding selected cluster_var
    available_vars_classifier <-
      setdiff(available_vars, selected_vars$cluster_var)
    updateSelectInput(session,
                      "classifier",
                      choices = available_vars_classifier,
                      selected = selected_vars$classifier)
    
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
    
    summarise_data <- epi_data() %>% summary()
    
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
      
      rf_model <-
        randomForest(
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
               fill = Cluster
             )) +
      geom_bar(stat = "identity") +
      labs(x = "Variable", y = "Importance", title = "Variable Importance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_gradient(low = "blue", high = "darkblue") +
      labs(caption = "malDecision 2023")
    
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
    datatable(analysis_results()$summarise_data)
  })
  
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    get_country_map()
  })
  
  report_envir <- reactive({
    new_envir <- rlang::new_environment(
      list(
        epi_data = epi_data(),
        summarise_data = analysis_results()$summarise_data,
        var_importance_df = analysis_results()$var_importance,
        var_imp_plot = analysis_results()$var_imp_plot,
        country = selected_country()
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

shinyApp(ui = ui, server = server)