#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  shinyOptions(shiny.maxRequestSize = 1024^3)
  # gargoyle flags init -------------------------------------------------------
  gargoyle::init("workflow-name")
  # Server-side modules -------------------------------------------------------
  ## Workflow -----------------------------------------------------------------
  workflow <- reactiveVal(NULL)
  .wf_name_renderer(workflow, "workflow-name", output)
  mod_workflow_start_server(
    ids()$workflow_start$section_id, workflow,
    "workflow-name"
  )
  side_nav <- reactiveVal(NULL)
  mod_side_bar_server(
    ids()$side_bar$section_id, workflow,
    "workflow-name", side_nav
  )
  # mod_data_import_server(
  #   ids()$data_import$section_id, workflow,
  #   "workflow-name", side_nav
  # )
  # mod_recalibration_server(
  #   ids()$recalibration$section_id, workflow,
  #   "workflow-name", side_nav
  # )
  ## Control lines db ---------------------------------------------------------
  cldb <- getOption("RISingVIS.cell_line_db", default = ControlLinesDb$new())
  mod_control_lines_db_server(
    ids()$control_lines_db$section_id,
    cldb = cldb
  )
}
