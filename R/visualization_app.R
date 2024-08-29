#' @title Shiny App for Visualizing AutoML Results
#' 
#' @template archive
#' @export
visualize = function(archive) {
  param_ids = archive$cols_x

  ui = bslib::page_navbar(
    id = "nav",
    title = "Visualization for mlr3automl",
    sidebar = bslib::sidebar(
      shiny::conditionalPanel(
        "input.nav === 'Marginal Plots'",
        shiny::selectInput("y",
          label = "Select parameter for y-axis",
          choices = c(param_ids, "NULL"),
        ),
        shiny::selectInput("x",
          label = "Select parameter for x-axis",
          choices = param_ids,
        )
      ),
      shiny::conditionalPanel(
        "input.nav === 'Parallel Coordinates'",
        shiny::checkboxGroupInput("cols_x",
          label = "Select hyperparameters to plot:",
          choices = param_ids,
          # select all by default
          selected = param_ids
        ),
        shiny::actionButton("unselect_all",
          label = "Unselect all"
        ),
        shiny::actionButton("select_all",
          label = "Select all"
        ),
        shiny::radioButtons("trafo",
          label = "Apply transformation?",
          choices = list("No", "Yes"),
          selected = "No",
          inline = TRUE
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
      bslib::card(shiny::plotOutput("parallel_coordinates"))
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
      if (input$y == "NULL") {
        marginal_plot(archive, x = input$x)
      } else {
        marginal_plot(archive, x = input$x, y = input$y)
      }
    })

    output$parallel_coordinates = renderPlot({
      if (is.null(input$cols_x)) return() # nothing selected
      trafo = input$trafo == "yes"
      parallel_coordinates(archive, cols_x = input$cols_x, trafo = trafo)
    })
    shiny::observeEvent(input$unselect_all, {
      shiny::updateCheckboxGroupInput(session, "cols_x", choices = param_ids, selected = NULL)
    })
    shiny::observeEvent(input$select_all, {
      shiny::updateCheckboxGroupInput(session, "cols_x", choices = param_ids, selected = param_ids)
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
