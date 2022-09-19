
server <- function(input, output, session) {
  # Workflow nav ----
  workflow <- WorkFlow$new()
  ## Side graph ----
  output[[
    id_list()$side_graph
  ]] <- renderUI({
    HTML(.svg_graph_mini())
  })
  ## Data import workflow section ----
  data_imp_returns <- dataImportServer(
    id_list()$data_import$section_id,
    workflow)
  ## Recalibration section ----
  rec_returns <- RecServer(
    id_list()$recalibration$section_id,
    workflow
  )
}
