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
  "A", 369.33333333, "CEM",
  "B", 221.60000000, "CEM", 
  "All samples", 138.50000000, "CEM",
  "A", 0.05787781, "Samples",
  "B", 0.02390438, "Samples",
  "All samples", 0.01691729, "Samples"
)

ratios_byIS_out <- tibble::tribble(
  ~chr, ~integration_locus, ~strand, ~A, ~B, ~`All samples`, ~IS_Source,
  "11", 64537168, "-", 154.333333, NA, 154.333333, "CEM",
  "2", 73762398, "-", NA, 129, 129, "CEM", 
  "17", 1632982, "-", 0.03536977, NA, 0.03536977, "Samples", 
  "5", 83626283, "+", NA, 0.00929615, 0.00929615, "Samples", 
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

test_that("shared_IS_ratio works correctly", {
  mat <- share_mat
  af <- dummy_af
  r <- shared_IS_ratio(af, mat)
  expect_equal(tibble::tibble(r), ratios_out, tolerance = 0.0001)
})

test_that("shared_IS_ratio_byIS works correctly", {
  mat <- share_mat
  af <- dummy_af
  r <- shared_IS_ratio_byIS(af, mat)
  expect_equal(tibble::tibble(r), ratios_byIS_out, tolerance = 0.0001)
})

# Errors and warnings for no sharing

test_that("shared_IS_ratio reports error and warnings for no sharing", {
  mat <- no_share_mat
  af <- dummy_af
  expect_error(shared_IS_ratio(af, mat))
  expect_error(shared_IS_ratio_byIS(af, mat))
})

test_that("shared_IS_ratio reports warning for no sharing from CEMs", {
  mat <- no_share_CEM_mat
  af <- dummy_af
  expect_warning(shared_IS_ratio(af, mat))
  expect_warning(shared_IS_ratio_byIS(af, mat))
})

test_that("shared_IS_ratio reports warning for no sharing from samples", {
  mat <- no_share_others_mat
  af <- dummy_af
  expect_warning(shared_IS_ratio(af, mat))
  expect_warning(shared_IS_ratio_byIS(af, mat))
})