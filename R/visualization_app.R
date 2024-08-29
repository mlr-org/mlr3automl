#' @title Shiny App for Visualizing AutoML Results
#' 
#' @template archive
#' @export
visualize = function(archive) {
  param_ids = archive$search_space$data$id

  ui = bslib::page_navbar(
    id = "nav",
    title = "Visualization for mlr3automl",
    sidebar = bslib::sidebar(
      shiny::conditionalPanel(
        "input.nav === 'Marginal Plots'",
        shiny::selectInput("y",
          label = "Select parameter for y-axis",
          choices = param_ids,
        ),
        shiny::selectInput("x",
          label = "Select parameter for x-axis",
          choices = param_ids,
        )
      )
    ),
    bslib::nav_panel(
      "Cost Over Time",
      bslib::card(shiny::plotOutput("cost_over_time"))
    ),
    bslib::nav_panel(
      "Marginal Plots",
      bslib::card(shiny::plotOutput("marginal_plot"))
    ),
    bslib::nav_panel(
      "Parallel Coordinates",
      bslib::card("TBD")
    ),
    bslib::nav_panel(
      "Partial Dependency Plots",
      bslib::card("TBD")
    )
  )

  server = function(input, output, session) {
    session$onSessionEnded(stopApp)

    output$cost_over_time = renderPlot({
      cost_over_time(archive)
    })

    output$marginal_plot = renderPlot({
      marginal_plot(archive, x = input$x, y = input$y)
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
