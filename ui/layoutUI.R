#------------------------------------------------------------------------------------
# malDecision App
# Original idea: Savi Koissi
# Edit and Enhancement: Gbedegnon Roseric Azondekon
#
# Atlanta, July 2023
#------------------------------------------------------------------------------------

home <- tabPanel(
  "Home",
  shiny::includeHTML("static/home.html"),
  div(
    class = "row",
    div(
      class = "col-md-6 col-md-offset-3 text-center",
      hr(class="my-4"),
      actionButton(
        "gotoapp",
        class = "btn btn-success",
        label = "Go to the malDecision Analyzer")
    )
  ),
  p("")
)

analyzer <- tabPanel(
  "Analyzer",
  value = "analyzer",
  sidebarLayout(
    sideBarModuleOutput("sideBar"),
    mainPanelModuleOutput("mainPanel")
  )
)

faq <- tabPanel(
  "FAQs and Quick Guide",
  shiny::includeHTML("static/faq.html")
)

license <- tabPanel(
  "License",
  shiny::includeHTML("static/license.html")
)

feedback <- tabPanel(
  "Feedback",
  shiny::includeHTML("static/feedback.html")
)


about <- navbarMenu(
  "About",
  license,
  faq,
  feedback
)

