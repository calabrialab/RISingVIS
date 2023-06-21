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
    hr(),
    div(
      class = "text-muted",
      "A database of all available control lines. Use this interface to",
      "add, delete or edit lines.",
      br(),
      tags$details(
        tags$summary(
          class = "h6",
          "What is a control cell line?"
        ),
        "Control cell lines are cell lines with known",
        "integration sites. They are used by RISingVIS",
        "to check for the presence of contamination among samples."
      ),
      tags$details(
        tags$summary(
          class = "h6",
          "How to add a new line?"
        ),
        "Click the 'New line' button below. A new window will open where",
        "you can insert the required info.", br(),
        "Please note that names must",
        "be unique and",
        "that RISingVIS matches the names of the lines against the independent",
        "sample identifier (ISid). For example, if your ISid is composed",
        "by the",
        "metadata fields `SubjectID` and `Tissue` you should name your line",
        "accordingly, by separating the two fields values with an underscore",
        "(e.g. CEM_BM).", br(),
        "The known integration sites table can't be empty and should not",
        "contain duplicates. Please note that",
        "in the processing phase, the names of the columns will be matched",
        "with the chosen ISAnalytics mandatory IS vars options."
      ),
      tags$details(
        tags$summary(
          class = "h6",
          "How to edit or delete a line?"
        ),
        "Click on the line you want to edit or delete in the table below.",
        "To edit the cell line click the 'Edit' button and a new window",
        "will open. There you can modifiy the info of the line. To delete",
        "the line click the 'Delete' button."
      )
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
    ## Used to refresh the reactable
    gargoyle::init("cl-tbl-refresh")
    ## Used to signal other components of the UI that the db has changed
    gargoyle::init("cl-db-change")
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
