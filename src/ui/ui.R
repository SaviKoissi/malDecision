#------------------------------------------------------------------------------------
# malDecision App
# Original idea: Savi Koissi
# Edit and Enhancement: Gbedegnon Roseric Azondekon
#
# Atlanta, July 2023
#------------------------------------------------------------------------------------

appUI <- function() {
    # dependent_pkgs <- readRDS(file = "gvsa/data/required-packages.rds")
    # dependent_pkgs <- setdiff(dependent_pkgs, c("tools", "parallel", "shiny"))
    list(
      shinyjs::useShinyjs(),
      tags$head(tags$link(rel = "stylesheet", href = "style.css"),
                tags$script(HTML(
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
  )
   ),
      navbarPage(
        title = a(
          href = "https://github.com/SaviKoissi/malDecision",
          target = "_blank",
          tags$img(src = "logo-white.png", id="logo")
        ),
        id = "mainNavBar",
        windowTitle = "malDecision: Data-Driven Malaria Decision Making",
        collapsible = TRUE,
        inverse = TRUE,
        position = "fixed-top",
        home,
        analyzer,
        about
      )
    )
}
