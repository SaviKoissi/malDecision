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
library(knitr)
library(kableExtra)
library(tmaptools)
library(gtsummary)


options(shiny.maxRequestSize = 100 * 1024 ^ 2)

data(World)

# UI
ui <- fluidPage(
  
  tagList(
    # CSS for navbar elements
    tags$style(
      HTML('.navbar {font-size: 17px;}',
           '.navbar-default .navbar-brand {color: #262626; font-weight: bold}',
           '.navbar-default .navbar-brand:hover {color: #262626;}',
           '.navbar-default .navbar-nav > li > a {color: #262626;}')
    ), 
    # CSS for fonts
    tags$style('h3 {font-weight: normal;}',
               'h4 {font-weight: normal;}',
               '* {font-family: Ubuntu;}'),
    # CSS for all buttons (class .btn)
    tags$style('.btn {color: #333333; 
                      background-color: #eeeeee; 
                      border-color: #cccccc;}
                .btn:hover {background-color: #e1e1e1;}'), 
    # CSS for errors and validation messages
    tags$style('.shiny-output-error-validation {
                 color: #e35300;
                 font-weight: bold;}'),
    # CSS for individual elements: note the '#id' syntax
    tags$style('#map-readme {color: #333333;
                             background-color: #ff770050;
                             border-color: #c84407;}
                #map-readme:hover {background-color: #e36a0075;}
                #trend-readme {color: #333333;
                               background-color: #ff770050;
                               border-color: #c84407;}
                #trend-readme:hover {background-color: #e36a0075;}')
  ),
  navbarPage(
    title = "", 
    theme = shinythemes::shinytheme("readable"),
  )
  ,
  
  
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
      fileInput("epi_file", "Upload Epi Data", accept = c(".csv", ".txt", ".xlsx", ".xls")),
      
      selectInput("latitude_column", "Select Latitude (x)", choices = NULL),
      selectInput("longitude_column", "Select Longitude (y)", choices = NULL),
      selectInput("cluster_var", "Select Degree of urbanization (Cluster Variable)", choices = NULL),
      selectInput(
        "classifier",
        "Select Determinant(s) (Classifier Variable(s))",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput("dependent_var", "Select Dependent Variable (Prevalence /Incidence)", choices = NULL),
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(tabsetPanel(
      # tabPanel("Map", leafletOutput("map"), 
               # helpText("Please check urbanization map after running the analysis!")), 
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
     #tmap_leaflet(map, in.shiny = TRUE)
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
    # selected_cols <- c(classifiers, dependent_var)
    # selected_data <- epi_data()[, selected_cols]
    # 
    # s_data <- lapply(split(selected_data, epi_data()$cluster_var), summary)
      
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

shinyApp(ui = ui, server = server)