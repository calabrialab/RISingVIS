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
    ~Sample, ~Ratio_CEM37, ~IS_Source,
    "A", 369.33333333, "Control",
    "B", 221.60000000, "Control", 
    "All_Samples", 138.50000000, "Control",
    "A", 0.05787781, "Samples",
    "B", 0.02390438, "Samples", 
    "All_Samples", 0.01691729, "Samples"
)

ratios_byIS_out <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~Sample, ~Ratio_CEM37, ~IS_Source,
    "11", 64537168, "-", "All_Samples", 154.333333, "Control", 
    "2", 73762398, "-", "All_Samples", 129.000000, "Control", 
    "17", 1632982, "-", "All_Samples", 0.0353697, "Samples",
    "5", 83626283, "+", "All_Samples", 0.00929614, "Samples",
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

ctrl_matrix_no_share <- tibble::tribble(
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
    "21", 9243522, "-", "B3", 600
)

ctrl_af <- tibble::tibble(ProjectID = "Proj1",  
                     SubjectID = c("ctrl1", "ctrl1", "ctrl1", 
                                   "ctrl2", "ctrl2", "ctrl2", 
                                   "A", "A", "A", "B", "B", "B"),
                     PoolID = "Pool1",
                     CompleteAmplificationID = c("ctrl1_1", "ctrl1_2", 
                                                 "ctrl1_3", "ctrl2_1", 
                                                 "ctrl2_2", "ctrl2_3", 
                                                 "A1", "A2", "A3", 
                                                 "B1", "B2", "B3"))

ctrl_out <- tibble::tribble(
    ~Sample, ~Ratio_ctrl1, ~Ratio_ctrl2, ~`Ratio_All_Controls`, ~IS_Source,
    "A", 369.33333333, 1.576667e+02, 527.00000000, "Control",
    "B", 221.60000000, NA+01, 316.20000000, "Control", 
    "All_Samples", 138.50000000, 1.576667e+02, 197.62500000, "Control",
    "A", 0.03536977, NA-02, 0.05787781, "Samples",
    "All_Samples", 0.03536977, 9.296149e-03, 0.01691729, "Samples",
    "B", NA, 9.296149e-03, 0.02390438, "Samples"
)

ctrl_mult_out <- tibble::tribble(
    ~Sample, ~`Ratio_ctrl1-Pool1`, ~`Ratio_ctrl2-Pool1`, 
    ~`Ratio_All_Controls`, ~IS_Source,
    "A-Pool1", 369.33333333, 1.576667e+02, 527.00000000, "Control",
    "B-Pool1", 221.60000000, NA+01, 316.20000000, "Control", 
    "All_Samples", 138.50000000, 1.576667e+02, 197.62500000, "Control",
    "A-Pool1", 0.03536977, NA-02, 0.05787781, "Samples",
    "All_Samples", 0.03536977, 9.296149e-03, 0.01691729, "Samples",
    "B-Pool1", NA, 9.296149e-03, 0.02390438, "Samples"
)

ctrl_byIS_out <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~Sample, ~Ratio_ctrl1, 
    ~Ratio_ctrl2, ~Ratio_All_Controls, ~IS_Source,
    "11", 64537168, "-", "All_Samples", 154.333333, 157.666666, 312, "Control",
    "2", 73762398, "-", "All_Samples", 129.000000, NA, 129.000000, "Control", 
    "17", 1632982, "-", "All_Samples", 0.03536977, NA, 0.03536977, "Samples", 
    "5", 83626283, "+", "All_Samples", NA, 0.00929615, 0.00929615, "Samples"
)

ctrl_byIS_mult_out <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~Sample, ~`Ratio_ctrl1-Pool1`, 
    ~`Ratio_ctrl2-Pool1`, ~Ratio_All_Controls, ~IS_Source,
    "11", 64537168, "-", "All_Samples", 154.333333, 157.666666, 312, "Control",
    "2", 73762398, "-", "All_Samples", 129.000000, NA, 129.000000, "Control", 
    "17", 1632982, "-", "All_Samples", 0.03536977, NA, 0.03536977, "Samples", 
    "5", 83626283, "+", "All_Samples", NA, 0.00929615, 0.00929615, "Samples"
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

controls_mult <- list("ctrl1-Pool1" = ctrl_1_known_is,
                      "ctrl2-Pool1" = ctrl_2_known_is)

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
    r <- tibble::tibble(r) %>% 
        dplyr::select("Sample", "Ratio_CEM37", "IS_Source")
    expect_equal(r, ratios_out, tolerance = 0.0001)
})

test_that("shared_IS_ratio_byIS works correctly", {
    mat <- share_mat
    af <- association_file
    r <- shared_IS_ratio_byIS(af, mat)
    r <- tibble::tibble(r) %>%
        dplyr::select("chr", "integration_locus", "strand", "Sample", 
                      "Ratio_CEM37", "IS_Source") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(tibble::tibble(r), ratios_byIS_out, tolerance = 0.0001)
})


# Errors and warnings for no sharing

test_that("shared_IS_ratio reports warning for no sharing", {
    mat <- no_share_mat
    af <- association_file
    expect_warning(shared_IS_ratio(af, mat))
    expect_warning(shared_IS_ratio_byIS(af, mat))
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
    r <- tibble::tibble(r) %>% 
        dplyr::select("Sample", "Ratio_CEM37", "IS_Source")
    expect_equal(r, ratios_out, tolerance = 0.0001)
})

test_that("shared_IS_ratio_byIS works with non-default columns", {
    mat <- share_mat
    af <- mod_af
    r <- shared_IS_ratio_byIS(af, mat, subject_col = "Subject")
    r <- tibble::tibble(r) %>%
        dplyr::select("chr", "integration_locus", "strand", "Sample", 
                      "Ratio_CEM37", "IS_Source") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(r, ratios_byIS_out, tolerance = 0.0001)
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
    expect_equal(r, 0.01691729, tolerance = 0.0001)
})

test_that("shared_IS_ratio works with different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_IS_ratio(af, mat, ctrl = controls)
    r <- tibble::tibble(r) %>% 
        dplyr::select("Sample", "Ratio_ctrl1", 
                      "Ratio_ctrl2", "Ratio_All_Controls", "IS_Source")
    expect_equal(tibble::tibble(r), ctrl_out, tolerance = 0.0001)
})

test_that("shared_IS_ratio_byIS works with different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_IS_ratio_byIS(af, mat, ctrl = controls)
    r <- tibble::tibble(r) %>%
        dplyr::select("chr", "integration_locus", "strand", "Sample", 
                      "Ratio_ctrl1", "Ratio_ctrl2", 
                      "Ratio_All_Controls", "IS_Source") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(r, ctrl_byIS_out, tolerance = 0.0001)
})

# Work with sample defined by multiple columns

test_that("shared_control_known_IS_ratio works with multiple columns sample", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_control_known_IS_ratio(af, mat, ctrl = controls_mult,
                                       subject_col = c("SubjectID", "PoolID"),
                                       field_sep = "-")
    expect_equal(r, 197.625, tolerance = 0.0001)
})

test_that("shared_other_IS_ratio works with multiple columns sample", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_other_IS_ratio(af, mat, ctrl = controls_mult,
                               subject_col = c("SubjectID", "PoolID"),
                               field_sep = "-")
    expect_equal(r, 0.01691729, tolerance = 0.0001)
})

test_that("shared_IS_ratio works with multiple columns sample", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_IS_ratio(af, mat, ctrl = controls_mult,
                         subject_col = c("SubjectID", "PoolID"),
                         field_sep = "-")
    r <- tibble::tibble(r) %>% 
        dplyr::select("Sample", "Ratio_ctrl1-Pool1", "Ratio_ctrl2-Pool1",
                      "Ratio_All_Controls", "IS_Source")
    expect_equal(r, ctrl_mult_out, tolerance = 0.0001)
})

test_that("shared_IS_ratio_byIS works with multiple columns sample", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- shared_IS_ratio_byIS(af, mat, ctrl = controls_mult,
                              subject_col = c("SubjectID", "PoolID"),
                              field_sep = "-")
    r <- tibble::tibble(r) %>% 
        dplyr::select("chr", "integration_locus", "strand", "Sample", 
                      "Ratio_ctrl1-Pool1", "Ratio_ctrl2-Pool1", 
                      "Ratio_All_Controls", "IS_Source") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(tibble::tibble(r), ctrl_byIS_mult_out, tolerance = 0.0001)
})
