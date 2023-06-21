################################################################################
# Tests for metadata submodule
################################################################################
test_that(".meta_additional_checks works as expected", {
  cldb <- ControlLinesDb$new()
  isa_opt <- .get_isa_is_vars(default = TRUE)
  # Compliant
  known_iss_1 <- tibble::tibble(
    chr = c("1", "5", "9"),
    integration_locus = c(234235, 1243242, 65645),
    strand = c("+", "-", "+")
  )
  # Non-compliant
  known_iss_2 <- tibble::tibble(
    chromosome = c("chr1", "chr2", "chr3"),
    strand = c("+", "-", "+"),
    locus = c(234235, 1243242, 65645)
  )
  cldb$add_cell_line(
    name = "test1",
    known_iss = known_iss_1
  )
  cldb$add_cell_line(
    name = "test2",
    known_iss = known_iss_2
  )
  sample_id <- c("SubjectID")
  checks <- .meta_additional_checks(
    imported_meta = sample_metadata,
    control_cell_line = c("CEM37", "test1", "test2"),
    indep_sample_id = sample_id,
    isa_opt = isa_opt,
    cldb = cldb
  )
  expected_cl_checks <- tibble::tibble(
    concatenatePoolIDSeqRun = c(rep_len("POOL1-1", 3), rep_len("POOL2-1", 3)),
    control_line = rep(c("CEM37", "test1", "test2"), 2),
    present = rep(c(TRUE, FALSE, FALSE), 2),
    compliant = rep(c(TRUE, TRUE, FALSE), 2)
  )
  expect_false(checks$meta_empty)
  expect_equal(checks$req_cols_present, character(0))
  expect_true(checks$ind_smpl_id)
  expect_equal(checks$control_cl, expected_cl_checks)
  # When metadata is empty -----------------------------------------------------
  checks <- .meta_additional_checks(
    imported_meta = sample_metadata |> dplyr::filter(FALSE),
    control_cell_line = c("CEM37", "test1", "test2"),
    indep_sample_id = sample_id,
    isa_opt = isa_opt,
    cldb = cldb
  )
  expect_true(length(checks) == 3)
  expect_true(checks$meta_empty)
  expect_equal(checks$req_cols_present, character(0))
  expect_true(checks$ind_smpl_id)
  # When required cols are missing ---------------------------------------------
  checks <- .meta_additional_checks(
    imported_meta = sample_metadata |> dplyr::select(-SubjectID),
    control_cell_line = c("CEM37", "test1", "test2"),
    indep_sample_id = sample_id,
    isa_opt = isa_opt,
    cldb = cldb
  )
  expect_true(length(checks) == 3)
  expect_false(checks$meta_empty)
  expect_equal(checks$req_cols_present, "SubjectID")
  expect_false(checks$ind_smpl_id)
  # When independent sample id is missing -----------------------------------
  checks <- .meta_additional_checks(
    imported_meta = sample_metadata,
    control_cell_line = c("CEM37", "test1", "test2"),
    indep_sample_id = c("SubjectID2"),
    isa_opt = isa_opt,
    cldb = cldb
  )
  expect_true(length(checks) == 3)
  expect_false(checks$meta_empty)
  expect_equal(checks$req_cols_present, character(0))
  expect_false(checks$ind_smpl_id)
})

# testServer(
#   mod_data_import_metadata_server,
#   # Add here your module params
#   args = list()
#   , {
#     ns <- session$ns
#     expect_true(
#       inherits(ns, "function")
#     )
#     expect_true(
#       grepl(id, ns(""))
#     )
#     expect_true(
#       grepl("test", ns("test"))
#     )
#     # Here are some examples of tests you can
#     # run on your module
#     # - Testing the setting of inputs
#     # session$setInputs(x = 1)
#     # expect_true(input$x == 1)
#     # - If ever your input updates a reactiveValues
#     # - Note that this reactiveValues must be passed
#     # - to the testServer function via args = list()
#     # expect_true(r$x == 1)
#     # - Testing output
#     # expect_true(inherits(output$tbl$html, "html"))
# })

# test_that("module ui works", {
#   ui <- mod_data_import_metadata_ui(id = "test")
#   golem::expect_shinytaglist(ui)
#   # Check that formals have not been removed
#   fmls <- formals(mod_data_import_metadata_ui)
#   for (i in c("id")){
#     expect_true(i %in% names(fmls))
#   }
# })
