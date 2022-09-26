# Data import module tests -----------------------------------------------------

## ISA options sub module tests ------------------------------------------------
test_that("ISA opt module uses default opt", {
  testServer(dataImport.ISAoptServer,
    args = list(workflow = WorkFlow$new()),
    {
      returns <- session$getReturned()
      inputs_vals <- list("Use defaults")
      names(inputs_vals) <- c(
        id_list()$data_import$isa_opt_section$inputs$toggle1
      )
      do.call(session$setInputs, args = inputs_vals)
      expected_af_cols <- withr::with_options(
        list(ISAnalytics.af_specs = "default"),
        ISAnalytics::association_file_columns()
      )
      expect_false(returns$flag_isa_opt_set())
      expect_setequal(returns$af_file_cols(),
                      expected_af_cols)
      expect_setequal(workflow$get_ISA_options()$af_cols$names,
                      expected_af_cols)
    }
  )
})

# test_that("ISA opt module works for correct json file", {
#   app <- shinytest::ShinyDriver$new(shinyApp(ui, server))
#   ns <- NS(id_list()$data_import$isa_opt_section$section_id)
#   json_file <- system.file("testdata",
#                            "reducedAf_ISAsettings.json",
#                            package = "RISingVIS"
#   )
#   inputs_vals <- list("Import configuration")
#   names(inputs_vals) <- c(
#     ns(id_list()$data_import$isa_opt_section$inputs$toggle1),
#   )
#   expected_af_cols <- withr::with_options(
#     list(ISAnalytics.af_specs = "default"),
#     ISAnalytics::association_file_columns()
#   )
#   do.call(app$setInputs, args = inputs_vals)
#   file_pair <- list(json_file)
#   names(file_pair) <- ns(
#     id_list()$data_import$isa_opt_section$inputs$json_file_input)
#   do.call(app$uploadFile, args = file_pair)
#   app$click(ns(id_list()$data_import$isa_opt_section$inputs$upload_config_btn))
#
# })
