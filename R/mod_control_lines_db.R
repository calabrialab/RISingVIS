#' control_lines_db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_control_lines_db_ui <- function(id) {
  ns <- NS(id)
  div(
    id = id,
    div("Control cell lines", class = "display-4"),
    div(
      class = "text-muted",
      "A database of all available control lines. Use this interface to",
      "add, delete or edit lines."
    ),
    hr(),
    div(
      class = "d-flex w-100 justify-content-end mb-2",
      actionButton(
        inputId = ns(
          ids()$control_lines_db$inputs$add_cell_line_btn
        ),
        label = "New line",
        icon = icon("plus")
      )
    ),
    reactable::reactableOutput(
      ns(ids()$control_lines_db$outputs$table)
    )
  )
}

#' control_lines_db Server Functions
#'
#' @noRd
mod_control_lines_db_server <- function(id, cldb) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # gargoyle flags init -----------------------------------------------------
    gargoyle::init("cl-tbl-refresh")
    # Data displayed in the table ---------------------------------------------
    cl_tbl_data <- reactive({
      gargoyle::watch("cl-tbl-refresh")
      .convert_db_to_table(cldb)
    })
    output[[
      ids()$control_lines_db$outputs$table
    ]] <- reactable::renderReactable({
      req(cl_tbl_data())
      .build_cl_db_table(isolate(cl_tbl_data()), ns)
    })
    observe({
      gargoyle::watch("cl-tbl-refresh")
      reactable::updateReactable(
        outputId = ids()$control_lines_db$outputs$table,
        data = cl_tbl_data()
      )
    })
    # Modals management -------------------------------------------------------
    ## To edit selected cell line ---------------------------------------------
    edit_cl_modal <- .build_cl_modal_ui("edit", ns)
    .attach_modal_cl_logic(input, output, session, cldb, "edit")
    observeEvent(
      input[[ids()$control_lines_db$inputs$show_edit_modal]],
      {
        if (input[[ids()$control_lines_db$inputs$show_edit_modal]]) {
          showModal(edit_cl_modal)
        }
      }
    )
    ## To delete selected cell line -------------------------------------------
    .delete_cl_logic(input, output, session, cldb)
    ## To add new cell line ---------------------------------------------------
    add_cl_modal <- .build_cl_modal_ui("add", ns)
    .attach_modal_cl_logic(input, output, session, cldb, "add")
    observeEvent(
      input[[ids()$control_lines_db$inputs$add_cell_line_btn]],
      {
        showModal(add_cl_modal)
      }
    )
  })
}
