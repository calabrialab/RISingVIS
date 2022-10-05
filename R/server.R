
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
    workflow, matrices = data_imp_returns$data
  )
  ## Plots section ----
  plots_returns <- PlotsServer(
    id_list()$plot_section$section_id,
    workflow, data = data_imp_returns$data,
    metadata = data_imp_returns$metadata,
    data_rec = rec_returns,
    indep_sample_id = data_imp_returns$ind_sample_id,
    project_id = data_imp_returns$project
    )
  ## Saving section ----
  saving_returns <- SavingServer(
      id_list()$saving_section$section_id,
      workflow,
      #filtered_data = plots_returns$data
      filtered_data = data_imp_returns$data
  )
}
