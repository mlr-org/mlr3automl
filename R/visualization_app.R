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
    # TBD: cost over time: select timestamp_x / timestamp_y / config_id
    sidebar = bslib::sidebar(
      shiny::conditionalPanel(
        # TBD: select branch, then parameter, as in PDP
        "input.nav === 'Marginal Plots'",
        shiny::selectInput("mp_y",
          label = "Select parameter for y-axis",
          choices = c(param_ids, "NULL"),
        ),
        shiny::selectInput("mp_x",
          label = "Select parameter for x-axis",
          choices = param_ids,
        )
      ),
      shiny::conditionalPanel(
        # TBD: select branch, then parameter, as in PDP
        "input.nav === 'Parallel Coordinates'",
        shiny::checkboxGroupInput("pc_cols_x",
          label = "Select hyperparameters to plot:",
          choices = param_ids,
          # select all by default
          selected = param_ids
        ),
        shiny::actionButton("pc_unselect_all",
          label = "Unselect all"
        ),
        shiny::actionButton("pc_select_all",
          label = "Select all"
        ),
        shiny::radioButtons("pc_trafo",
          label = "Apply transformation?",
          choices = list("No", "Yes"),
          selected = "No",
          inline = TRUE
        )
      ),
      shiny::conditionalPanel(
        "input.nav === 'Partial Dependence Plots'",
        shiny::radioButtons("pdp_branch",
          label = "Select branch:",
          choices = branches
        ),
        shiny::selectInput("pdp_x",
          label = "Select parameter for x-axis:",
          choices = param_ids,
          selected = param_ids[[1]]
        ),
        shiny::selectInput("pdp_y",
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
    ),
    bslib::nav_panel(
      "Pareto Front",
      bslib::card(shiny::plotOutput("pf"))
    )
  )

  server = function(input, output, session) {
    session$onSessionEnded(stopApp)

    output$cost_over_time = renderPlot({
      cost_over_time(archive)
    })

    output$marginal_plot = renderPlot({
      if (input$mp_y == "NULL") {
        marginal_plot(archive, x = input$mp_x)
      } else {
        marginal_plot(archive, x = input$mp_x, y = input$mp_y)
      }
    })

    output$parallel_coordinates = renderPlot({
      if (is.null(input$pc_cols_x)) return() # nothing selected
      trafo = input$pc_trafo == "Yes"
      parallel_coordinates(archive, cols_x = input$pc_cols_x, trafo = trafo)
    })
    shiny::observeEvent(input$pc_unselect_all, {
      shiny::updateCheckboxGroupInput(session, "pc_cols_x", choices = param_ids, selected = NULL)
    })
    shiny::observeEvent(input$pc_select_all, {
      shiny::updateCheckboxGroupInput(session, "pc_cols_x", choices = param_ids, selected = param_ids)
    })


    shiny::observeEvent(input$pdp_branch, {
      selectable_ids = param_ids[startsWith(param_ids, input$pdp_branch)]
      shiny::updateSelectInput(session, "pdp_x", choices = selectable_ids, selected = selectable_ids[[1]])
      shiny::updateSelectInput(session, "pdp_y", choices = selectable_ids, selected = selectable_ids[[2]])
    })
    output$pdp = renderPlot({
      if (is.null(input$pdp_x)) return()
      partial_dependence_plot(
        instance, x = input$pdp_x, y = input$pdp_y,
        type = "default"
      )      
    })

    
    output$pf = renderPlot({
      pareto_front(instance)
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
