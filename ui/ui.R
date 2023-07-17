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