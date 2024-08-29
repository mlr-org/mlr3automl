#' @title Shiny App for Visualizing AutoML Results
#' 
#' @template param_instance
#' @export
visualize = function(instance) {
  ui = bslib::page_navbar(
    title = "Visualization for mlr3automl",
    bslib::nav_panel("Cost over time", shiny::plotOutput("cost_over_time")),
    bslib::nav_panel("Marginal plot"),
    bslib::nav_panel("Parallel coordinates"),
    bslib::nav_panel("Partial dependency plot")
  )

  server = function(input, output, session) {
    session$onSessionEnded(stopApp)
    output$cost_over_time = renderPlot({
      cost_over_time(instance)
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

