library(RISingVIS)

# Test tables

data(association_file)

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
    ~Sample, ~CEM37, ~IS_Source,
    "A", 369.33333333, "Control",
    "B", 221.60000000, "Control", 
    "All samples", 138.50000000, "Control",
    "A", 0.05787781, "Samples",
    "B", 0.02390438, "Samples", 
    "All samples", 0.01691729, "Samples"
)

ratios_byIS_out <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~Control, 
    ~A, ~B, ~`All samples`, ~IS_Source,
    "11", 64537168, "-", "CEM37", 154.333333, NA, 154.333333, "Control",
    "2", 73762398, "-", "CEM37", NA, 129, 129, "Control", 
    "17", 1632982, "-", "CEM37", 0.03536977, NA, 0.03536977, "Samples", 
    "5", 83626283, "+", "CEM37", NA, 0.00929615, 0.00929615, "Samples", 
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

no_share_control_mat <- tibble::tribble(
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

mod_af <- tibble::tibble(ProjectID = "Proj1", PoolID = "Pool1", 
                         Subject = c("CEM37", "CEM37", "CEM37", 
                                     "A", "A", "A", "B", "B", "B"),
                         CompleteAmplificationID = c("CEM1", "CEM2", "CEM3", 
                                                     "A1", "A2", "A3", 
                                                     "B1", "B2", "B3"))

ctrl_matrix <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~CompleteAmplificationID, ~Value,
    "11", 64537168, "-", "ctrl1_2", 463,
    "2", 73762398, "-", "ctrl1_3", 645,
    "8", 8866486, "+", "ctrl2_1", 703,
    "11", 64537168, "-", "ctrl2_2", 473,
    "4", 73292632, "+", "A1", 215,
    "19", 8621028, "-", "A2", 309,
    "17", 1632982, "-", "A3", 311,
    "2", 9876543, "+", "B1", 270,
    "5", 83626283, "+", "B2", 753,
    "21", 9243522, "-", "B3", 600,
    "17", 1632982, "-", "ctrl1_3", 11,
    "5", 83626283, "+", "ctrl2_1", 7,
    "11", 64537168, "-", "A2", 3,
    "2", 73762398, "-", "B3", 5
)

ctrl_af <- tibble::tibble(ProjectID = "Proj1", PoolID = "Pool1", 
                     SubjectID = c("ctrl1", "ctrl1", "ctrl1", 
                                   "ctrl2", "ctrl2", "ctrl2", 
                                   "A", "A", "A", "B", "B", "B"),
                     CompleteAmplificationID = c("ctrl1_1", "ctrl1_2", 
                                                 "ctrl1_3", "ctrl2_1", 
                                                 "ctrl2_2", "ctrl2_3", 
                                                 "A1", "A2", "A3", 
                                                 "B1", "B2", "B3"))

ctrl_out <- tibble::tribble(
    ~Sample, ~ctrl1, ~ctrl2, ~`All controls`, ~IS_Source,
    "A", 369.33333333, 1.576667e+02, 527.00000000, "Control",
    "B", 221.60000000, NA+01, 316.20000000, "Control", 
    "All samples", 138.50000000, 1.576667e+02, 197.62500000, "Control",
    "A", 0.03536977, NA-02, 0.05787781, "Samples",
    "All samples", 0.03536977, 9.296149e-03, 0.01691729, "Samples",
    "B", NA, 9.296149e-03, 0.02390438, "Samples"
)

ctrl_byIS_out <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~Control, 
    ~A, ~B, ~`All samples`, ~IS_Source,
    "11", 64537168, "-", "ctrl1", 154.333333, NA, 154.333333, "Control",
    "2", 73762398, "-", "ctrl1", NA, 129.000000, 129.000000, "Control", 
    "11", 64537168, "-", "ctrl2", 157.66666667, NA, 157.66666667, "Control",
    "11", 64537168, "-", "All controls", 312.000000, NA, 312.000000, "Control",
    "2", 73762398, "-", "All controls", NA, 129.000000, 129.000000, "Control",
    "17", 1632982, "-", "ctrl1", 0.03536977, NA, 0.03536977, "Samples", 
    "5", 83626283, "+", "ctrl2", NA, 0.00929615, 0.00929615, "Samples", 
    "17", 1632982, "-", "All controls", 0.03536977, NA, 0.03536977, "Samples", 
    "5", 83626283, "+", "All controls", NA, 0.00929615, 0.00929615, "Samples"
)


ctrl_1_known_is <- tibble::tribble(
    ~chr,	~integration_locus,	~strand, 
    "11",	64537168,	"-",
    "2",	73762398,	"-"
)

ctrl_2_known_is <- tibble::tribble(
    ~chr,	~integration_locus,	~strand, 
    "11",	64537168,	"-",
    "8",	8866486,	"+"
)

controls <- list(ctrl1 = ctrl_1_known_is, 
                 ctrl2 = ctrl_2_known_is)

# Correct output

test_that("shared_control_known_IS_ratio works correctly", {
    mat <- share_mat
    af <- association_file
    r <- shared_control_known_IS_ratio(af, mat)
    expect_equal(r, 138.5, tolerance = 0.0001)
})

test_that("shared_other_IS_ratio works correctly", {
    mat <- share_mat
    af <- association_file
    r <- shared_other_IS_ratio(af, mat)
    expect_equal(r, 0.01691729, tolerance = 0.0001)
})

test_that("shared_IS_ratio works correctly", {
    mat <- share_mat
    af <- association_file
    r <- shared_IS_ratio(af, mat)
    expect_equal(tibble::tibble(r), ratios_out, tolerance = 0.0001)
})

test_that("shared_IS_ratio_byIS works correctly", {
    mat <- share_mat
    af <- association_file
    r <- shared_IS_ratio_byIS(af, mat)
    expect_equal(tibble::tibble(r), ratios_byIS_out, tolerance = 0.0001)
})


# Errors and warnings for no sharing

test_that("shared_IS_ratio reports error and warnings for no sharing", {
    mat <- no_share_mat
    af <- association_file
    expect_error(shared_IS_ratio(af, mat))
    expect_error(shared_IS_ratio_byIS(af, mat))
})

test_that("shared_IS_ratio reports warning for no sharing from control", {
    mat <- no_share_control_mat
    af <- association_file
    expect_warning(shared_IS_ratio(af, mat))
    expect_warning(shared_IS_ratio_byIS(af, mat))
})

test_that("shared_IS_ratio reports warning for no sharing from samples", {
    mat <- no_share_others_mat
    af <- association_file
    expect_warning(shared_IS_ratio(af, mat))
    expect_warning(shared_IS_ratio_byIS(af, mat))
})


# Work with different input columns

test_that("shared_control_known_IS_ratio works with non-default columns", {
    mat <- share_mat
    af <- mod_af
    r <- shared_control_known_IS_ratio(af, mat, subject_col = "Subject")
    expect_equal(r, 138.5, tolerance = 0.0001)
})

test_that("shared_other_IS_ratio works with non-default columns", {
    mat <- share_mat
    af <- mod_af
    r <- shared_other_IS_ratio(af, mat, subject_col = "Subject")
    expect_equal(r, 0.01691729, tolerance = 0.0001)
})

test_that("shared_IS_ratio works with non-default columns", {
    mat <- share_mat
    af <- mod_af
    r <- shared_IS_ratio(af, mat, subject_col = "Subject")
    expect_equal(tibble::tibble(r), ratios_out, tolerance = 0.0001)
})

test_that("shared_IS_ratio_byIS works with non-default columns", {
    mat <- share_mat
    af <- mod_af
    r <- shared_IS_ratio_byIS(af, mat, subject_col = "Subject")
    expect_equal(tibble::tibble(r), ratios_byIS_out, tolerance = 0.0001)
})

# Work with different controls

test_that("shared_control_known_IS_ratio works with different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_control_known_IS_ratio(af, mat, ctrl = controls)
    expect_equal(r, 197.625, tolerance = 0.0001)
})

test_that("shared_other_IS_ratio works with different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_other_IS_ratio(af, mat, ctrl = controls)
    expect_equal(r, 0.01691, tolerance = 0.0001)
})

test_that("shared_IS_ratio works with different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_IS_ratio(af, mat, ctrl = controls)
    expect_equal(tibble::tibble(r), ctrl_out, tolerance = 0.0001)
})

test_that("shared_IS_ratio_byIS works with different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_IS_ratio_byIS(af, mat, ctrl = controls)
    expect_equal(tibble::tibble(r), ctrl_byIS_out, tolerance = 0.0001)
})
