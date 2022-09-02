# App utils tests --------------------------------------------------------------

test_that(".load_isa_opts returns for malformed file", {
  json_file <- system.file("testdata", "reducedAf_ISAsettings_malformed.json",
    package = "RISingVIS"
  )
  results <- .load_isa_opts(json_file)
  expect_true(all(names(results) == c("status_banner", "flag")))
  expect_false(results$flag)
  expect_equal(results$status_banner$attribs$class, "alert alert-danger")
})

test_that(".load_isa_opts returns for warnings file", {
  json_file <- system.file("testdata", "reducedAf_ISAsettings_warnings.json",
    package = "RISingVIS"
  )
  results <- .load_isa_opts(json_file)
  expected_af_cols <- c(
    "ProjectID", "PoolID", "TagSequence", "SubjectID",
    "VectorID", "Tissue", "TimePoint", "PCRMethod",
    "CellMarker", "NGSTechnology", "ReplicateNumber",
    "Genome", "concatenatePoolIDSeqRun",
    "CompleteAmplificationID", "PathToFolderProjectID",
    "my_column"
  )
  expect_true(all(names(results) == c("status_banner", "flag")))
  expect_true(results$flag)
  expect_equal(results$status_banner$attribs$class, "alert alert-warning")
  expect_setequal(ISAnalytics::association_file_columns(), expected_af_cols)
})

test_that(".load_isa_opts returns for correct file", {
  json_file <- system.file("testdata", "reducedAf_ISAsettings.json",
    package = "RISingVIS"
  )
  results <- .load_isa_opts(json_file)
  expected_af_cols <- c(
    "ProjectID", "FUSIONID", "PoolID", "TagSequence", "SubjectID", "VectorID",
    "Tissue", "TimePoint", "PCRMethod", "CellMarker", "TagID", "NGSTechnology",
    "DNAnumber", "ReplicateNumber", "VCN", "Genome", "concatenatePoolIDSeqRun",
    "CompleteAmplificationID", "PathToFolderProjectID", "my_column"
  )
  expect_true(all(names(results) == c("status_banner", "flag")))
  expect_true(results$flag)
  expect_equal(results$status_banner$attribs$class, "alert alert-success")
  expect_setequal(ISAnalytics::association_file_columns(), expected_af_cols)
})

# TODO other function tests
