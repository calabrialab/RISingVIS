library(RISingVIS)

# Test tables

dummy_af <- tibble::tibble(ProjectID = "Proj1", PoolID = "Pool1", 
             SubjectID = c("CEM37", "CEM37", "CEM37", 
                           "A", "A", "A", "B", "B", "B"),
             CompleteAmplificationID = c("CEM1", "CEM2", "CEM3", 
                                         "A1", "A2", "A3", 
                                         "B1", "B2", "B3"))

share_mat <- tibble::tribble(
  ~chr, ~integration_locus, ~strand, ~CompleteAmplificationID, ~Value,
  "8", 8866486, "+", "CEM1", 713,
  "11", 64537168, "-", "CEM2", 463,
  "2", 73762398, "-", "CEM3", 645,
  "4", 73292632, "+", "A1", 215,
  "19", 8621028, "-", "A2", 309,
  "17", 1632982, "-", "A3", 311,
  "2", 9876543, "+", "B1", 270,
  "5", 83626283, "+", "B2", 753,
  "21", 9243522, "-", "B3", 600,
  "17", 1632982, "-", "CEM3", 11,
  "5", 83626283, "+", "CEM1", 7,
  "11", 64537168, "-", "A2", 3,
  "2", 73762398, "-", "B3", 5
)

ratios_out <- tibble::tribble(
  ~Sample, ~Ratio, ~IS_Source,
  "A", 2, "CEM",
  "B", 2, "CEM", 
  "All samples", 1, "CEM",
  "A", 2, "Samples",
  "B", 2, "Samples",
  "All samples", 1, "Samples"
)

ratios_byIS_out <- tibble::tribble(
  ~chr, ~integration_locus, ~strand, ~A, ~B, ~`All samples`, ~IS_Source,
  "11", 64537168, "-", 1, NA, 1, "CEM",
  "2", 73762398, "-", NA, 1, 1, "CEM", 
  "17", 1632982, "-", 1, NA, 1, "Samples", 
  "5", 83626283, "+", NA, 1, 1, "Samples", 
)

no_share_mat <- tibble::tribble(
  ~chr, ~integration_locus, ~strand, ~CompleteAmplificationID, ~Value,
  "8", 8866486, "+", "CEM1", 713,
  "11", 64537168, "-", "CEM2", 463,
  "2", 73762398, "-", "CEM3", 645,
  "4", 73292632, "+", "A1", 215,
  "19", 8621028, "-", "A2", 309,
  "17", 1632982, "-", "A3", 311,
  "2", 9876543, "+", "B1", 270,
  "5", 83626283, "+", "B2", 753,
  "21", 9243522, "-", "B3", 600
)

no_share_CEM_mat <- tibble::tribble(
  ~chr, ~integration_locus, ~strand, ~CompleteAmplificationID, ~Value,
  "8", 8866486, "+", "CEM1", 713,
  "11", 64537168, "-", "CEM2", 463,
  "2", 73762398, "-", "CEM3", 645,
  "4", 73292632, "+", "A1", 215,
  "19", 8621028, "-", "A2", 309,
  "17", 1632982, "-", "A3", 311,
  "2", 9876543, "+", "B1", 270,
  "5", 83626283, "+", "B2", 753,
  "21", 9243522, "-", "B3", 600,
  "17", 1632982, "-", "CEM3", 11,
  "5", 83626283, "+", "CEM1", 7
)

no_share_others_mat <- tibble::tribble(
  ~chr, ~integration_locus, ~strand, ~CompleteAmplificationID, ~Value,
  "8", 8866486, "+", "CEM1", 713,
  "11", 64537168, "-", "CEM2", 463,
  "2", 73762398, "-", "CEM3", 645,
  "4", 73292632, "+", "A1", 215,
  "19", 8621028, "-", "A2", 309,
  "17", 1632982, "-", "A3", 311,
  "2", 9876543, "+", "B1", 270,
  "5", 83626283, "+", "B2", 753,
  "21", 9243522, "-", "B3", 600,
  "11", 64537168, "-", "A2", 3,
  "2", 73762398, "-", "B3", 5,
)


# Correct output

test_that("replicates_IS_count works well", {
  mat <- share_mat
  af <- dummy_af
  counts <- replicates_IS_count(af, mat)
  row <- tibble::tibble_row(counts %>% 
                              dplyr::filter(chr == "2" & 
                                              integration_locus == 73762398 & 
                                              strand == "-"))
  row2 <- tibble::tibble_row(counts %>% 
                               dplyr::filter(chr == "19" & 
                                               integration_locus == 8621028 & 
                                               strand == "-"))
  expect_true(row$CEM37 == 1 &
                row$A == 0 &
                row$B == 1)
  expect_true(row2$CEM37 == 0 &
                row2$A == 1 & 
                row2$B == 0)
})

test_that("replicates_IS_ratio works correctly", {
  mat <- share_mat
  af <- dummy_af
  r <- replicates_IS_ratio(af, mat)
  expect_equal(tibble::tibble(r), ratios_out, tolerance = 0.0001)
})

test_that("replicates_IS_ratio_byIS works correctly", {
  mat <- share_mat
  af <- dummy_af
  r <- replicates_IS_ratio_byIS(af, mat)
  expect_equal(tibble::tibble(r), ratios_byIS_out, tolerance = 0.0001)
})


# Errors and warnings for no sharing

test_that("replicates_IS_ratio reports error and warnings for no sharing", {
  mat <- no_share_mat
  af <- dummy_af
  expect_error(replicates_IS_ratio(af, mat))
  expect_error(replicates_IS_ratio_byIS(af, mat))
})

test_that("replicates_IS_ratio reports warning for no sharing from CEMs", {
  mat <- no_share_CEM_mat
  af <- dummy_af
  expect_warning(replicates_IS_ratio(af, mat))
  expect_warning(replicates_IS_ratio_byIS(af, mat))
})

test_that("replicates_IS_ratio reports warning for no sharing from samples", {
  mat <- no_share_others_mat
  af <- dummy_af
  expect_warning(replicates_IS_ratio(af, mat))
  expect_warning(replicates_IS_ratio_byIS(af, mat))
})
