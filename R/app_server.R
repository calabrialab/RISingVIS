#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  workflow <- reactiveVal(NULL)
  gargoyle::init("workflow-name")
  .wf_name_renderer(workflow, "workflow-name", output)
  mod_workflow_start_server(ids()$workflow_start$section_id, workflow,
                            "workflow-name")
  side_nav <- reactiveVal(NULL)
  mod_side_bar_server(ids()$side_bar$section_id, workflow,
                      "workflow-name", side_nav)
  mod_data_import_server(ids()$data_import$section_id, workflow,
                         "workflow-name", side_nav)
  mod_recalibration_server(ids()$recalibration$section_id, workflow,
                           "workflow-name", side_nav)
}
