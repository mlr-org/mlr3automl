#' @title Shiny App for Visualizing AutoML Results
#' 
#' @param instance (`[mlr3tuning::TuningInstanceAsyncSingleCrit]`)
#' @export
visualize = function(instance) {
  archive = instance$archive
  param_ids = archive$cols_x
  learner_ids = unique(archive$data$branch.selection)

  ui = bslib::page_navbar(
    id = "nav",
    title = "Visualization for mlr3automl",

    sidebar = bslib::sidebar(
      shiny::conditionalPanel(
        "input.nav === 'Cost Over Time'",
        shiny::radioButtons("cot_x",
          label = "Select x-axis:",
          choices = c("configuration ID", "timestamp_xs", "timestamp_ys")
        )
      ),
      param_panel(
        "input.nav === 'Marginal Plots'",
        "mp",
        learner_ids,
        param_ids
      ),
      shiny::conditionalPanel(
        "input.nav === 'Parallel Coordinates'",
        shiny::selectInput("pc_branch",
          label = "Select branch:",
          choices = learner_ids
        ),
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
      param_panel(
        "input.nav === 'Partial Dependence Plots'",
        "pdp",
        learner_ids,
        param_ids,
        shiny::actionButton("pdp_process",
          label = "Process"
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


    # Cost over time
    output$cost_over_time = renderPlot({
      if (input$cot_x == "configuration ID") {
        cost_over_time(instance)
      } else {
        cost_over_time(instance, time = input$cot_x)
      }
    })


    # Marginal plots
    shiny::observeEvent(input$mp_branch, {
      selectable_ids = param_ids[startsWith(param_ids, input$mp_branch)]
      shiny::updateSelectInput(session, "mp_x", choices = selectable_ids, selected = selectable_ids[[1]])
      shiny::updateSelectInput(session, "mp_y", choices = selectable_ids, selected = selectable_ids[[2]])
    })

    output$marginal_plot = shiny::renderPlot({
      if (input$mp_y == "NULL") {
        marginal_plot(instance, x = input$mp_x)
      } else {
        marginal_plot(instance, x = input$mp_x, y = input$mp_y)
      }
    })


    # Parallel Coordinates
    shiny::observeEvent(input$pc_branch, {
      selectable_ids = param_ids[startsWith(param_ids, input$pc_branch)]
      shiny::updateCheckboxGroupInput(session,
        "pc_cols_x",
        choices = selectable_ids,
        # select all by default
        selected = selectable_ids
      )
    })

    output$parallel_coordinates = shiny::renderPlot({
      if (is.null(input$pc_cols_x)) return() # nothing selected
      trafo = input$pc_trafo == "Yes"
      parallel_coordinates(instance, cols_x = input$pc_cols_x, trafo = trafo)
    })

    shiny::observeEvent(input$pc_unselect_all, {
      selectable_ids = param_ids[startsWith(param_ids, input$pc_branch)]
      shiny::updateCheckboxGroupInput(session, "pc_cols_x", choices = selectable_ids, selected = NULL)
    })

    shiny::observeEvent(input$pc_select_all, {
      selectable_ids = param_ids[startsWith(param_ids, input$pc_branch)]
      shiny::updateCheckboxGroupInput(session, "pc_cols_x", choices = selectable_ids, selected = selectable_ids)
    })


    # Partial Dependence Plots
    shiny::observeEvent(input$pdp_branch, {
      selectable_ids = param_ids[startsWith(param_ids, input$pdp_branch)]
      shiny::updateSelectInput(session, "pdp_x", choices = selectable_ids, selected = selectable_ids[[1]])
      shiny::updateSelectInput(session, "pdp_y", choices = selectable_ids, selected = selectable_ids[[2]])
    })
    
    # generate plot only after pressing the "Process" button
    # because it takes quite a while...
    output$pdp = shiny::bindEvent(
      shiny::renderPlot({
        if (is.null(input$pdp_x)) return()
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Making plot. Please wait.")
        partial_dependence_plot(
          instance, x = input$pdp_x, y = input$pdp_y,
          type = "default"
        )
      }),
      input$pdp_process
    )
    

    # Pareto Front
    output$pf = shiny::renderPlot({
      pareto_front(instance)
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
