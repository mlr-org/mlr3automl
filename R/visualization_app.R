#' @title Shiny App for Visualizing AutoML Results
#' 
#' @param instance (`[mlr3tuning::TuningInstanceAsyncSingleCrit]`)
#' @export
visualize = function(instance) {
  archive = instance$archive
  param_ids = archive$cols_x
  branches = unique(archive$data$branch.selection)

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
      ),
      shiny::conditionalPanel(
        "input.nav === 'Partial Dependence Plots'",
        shiny::radioButtons("select_branch",
          label = "Select branch:",
          choices = branches
        ),
        shiny::selectInput("select_x",
          label = "Select parameter for x-axis:",
          choices = param_ids,
          selected = param_ids[[1]]
        ),
        shiny::selectInput("select_y",
          label = "Select parameter for y-axis:",
          choices = param_ids,
          selected = param_ids[[2]]
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
      "Partial Dependence Plots",
      bslib::card(shiny::plotOutput("pdp"))
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
      trafo = input$trafo == "Yes"
      parallel_coordinates(archive, cols_x = input$cols_x, trafo = trafo)
    })
    shiny::observeEvent(input$unselect_all, {
      shiny::updateCheckboxGroupInput(session, "cols_x", choices = param_ids, selected = NULL)
    })
    shiny::observeEvent(input$select_all, {
      shiny::updateCheckboxGroupInput(session, "cols_x", choices = param_ids, selected = param_ids)
    })


    shiny::observeEvent(input$select_branch, {
      selectable_ids = param_ids[startsWith(param_ids, input$select_branch)]
      shiny::updateSelectInput(session, "select_x", choices = selectable_ids, selected = selectable_ids[[1]])
      shiny::updateSelectInput(session, "select_y", choices = selectable_ids, selected = selectable_ids[[2]])
    })
    output$pdp = renderPlot({
      if (is.null(input$select_x)) return()
      partial_dependence_plot(
        instance, x = input$select_x, y = input$select_y,
        type = "default", grid_size = 20
      )      
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
