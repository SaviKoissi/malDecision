#------------------------------------------------------------------------------------
# malDecision App
# Original idea: Savi Koissi
# Edit and Enhancement: Gbedegnon Roseric Azondekon
#
# Atlanta, July 2023
#------------------------------------------------------------------------------------

reportModuleOutput <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      12, align="center",
      shinyjs::disabled(
        downloadButton(
          ns("download_report"),
          label = "Generate & download report",
          class = "btn-primary"
        )
      )
    )
  )
}

reportModule <- function(input, output, session, configs, analysis_results, selected_country){
  inc_progress <- function(){
    shiny::incProgress()
  }
  
  observeEvent(configs$run_analysis_btn(), {
    shinyjs::toggleState("download_report", condition = configs$run_analysis_btn() > 0)
  })
  
  report_envir <- reactive({
    new_envir <- rlang::new_environment(
      list(
        inc_progress = inc_progress,
        epi_data = configs$epi_data(),
        summarise_data = analysis_results$summarise_data(),
        var_importance_df = analysis_results$var_importance(),
        var_imp_plot = analysis_results$var_imp_plot(),
        selected_country = selected_country(),
        cluster_var = configs$cluster(),
        dependent = configs$dependent(),
        classifier = configs$classifier(),
        configs = configs
      )
    )
    parent.env(new_envir) <- globalenv()
    new_envir
  })
  
  # Generate PDF report
  output$download_report <- downloadHandler(
    filename = paste0("Report_", Sys.Date(), ".pdf"),
    content = function(file) {
      # Render the R Markdown report
      withProgress(
        message = "Generating report...",
        detail = "Please be patient - This may take a while...",
        value = 0, {
          suppressMessages(
            suppressWarnings(
              rmarkdown::render("report/report.Rmd", output_file = file, envir = report_envir())
            )
          )
        }
      )
    }
  )
}