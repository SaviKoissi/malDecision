#------------------------------------------------------------------------------------
# malDecision App
# Original idea: Savi Koissi
# Edit and Enhancement: Gbedegnon Roseric Azondekon
#
# Atlanta, July 2023
#------------------------------------------------------------------------------------

appUI <- function() {
  fluidPage(
    
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
    ),
    
    
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
    layoutUI,
    # sidebarLayout(
    #   sidebarPanel(
    #     id = "sidebarPanel",
    #     selectInput("country", "Select a country:", choices = unique(World$name)),
    #     fileInput("epi_file", "Upload Epi Data", accept = c(".csv", ".txt", ".xlsx", ".xls")),
    #     
    #     selectInput("latitude_column", "Select Latitude (x)", choices = NULL),
    #     selectInput("longitude_column", "Select Longitude (y)", choices = NULL),
    #     selectInput("cluster_var", "Select Degree of urbanization (Cluster Variable)", choices = NULL),
    #     selectInput(
    #       "classifier",
    #       "Select Determinant(s) (Classifier Variable(s))",
    #       choices = NULL,
    #       multiple = TRUE
    #     ),
    #     selectInput("dependent_var", "Select Dependent Variable (Prevalence /Incidence)", choices = NULL),
    #     actionButton("run_analysis", "Run Analysis")
    #   ),
    #   mainPanel(tabsetPanel(
    #     tabPanel("Urbanization map", leafletOutput("map2"), 
    #              helpText("Please check variable importance on the next tab!")),
    #     tabPanel(
    #       "Variance Importance",
    #       dataTableOutput("var_imp_table"),
    #       plotOutput("var_imp_plot"), 
    #       helpText("Please check summary statistics on the next tab!")
    #     ),
    #     tabPanel("Report", dataTableOutput("report"),
    #              helpText("Please go back!!!"))
    #   ))
    # ),
    
    
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
}