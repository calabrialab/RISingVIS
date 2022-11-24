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
    "A", 2, "Controls",
    "B", 2, "Controls", 
    "All_Samples", 2, "Controls",
    "A", 2, "Samples",
    "B", 2, "Samples", 
    "All_Samples", 2, "Samples"
)

ratios_byIS_out <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~ Sample, ~Ratio_CEM37, ~IS_Source,
    "11", 64537168, "-", "All_Samples", 2, "Controls",
    "2", 73762398, "-", "All_Samples", 2, "Controls", 
    "17", 1632982, "-", "All_Samples", 2, "Samples", 
    "5", 83626283, "+", "All_Samples", 2, "Samples", 
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
    ~Sample, ~Ratio_ctrl1, ~Ratio_ctrl2, ~Ratio_All_Controls, ~IS_Source,
    "A", 2, 1, 1.5, "Controls",
    "B", 2, NA, 1.5, "Controls", 
    "All_Samples", 2, 2, 1.5, "Controls",
    "A", 1, NA, 1, "Samples",
    "B", NA, 1, 1, "Samples",
    "All_Samples", 2, 2, 1, "Samples"
)

ctrl_byIS_out <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~Sample, ~Ratio_ctrl1, 
    ~Ratio_ctrl2, ~Ratio_All_Controls, ~IS_Source,
    "11", 64537168, "-", "All_Samples", 2, 2, 2, "Controls",
    "2", 73762398, "-", "All_Samples", 2, NA, 1, "Controls", 
    "17", 1632982, "-", "All_Samples", 2, NA, 1, "Samples", 
    "5", 83626283, "+", "All_Samples", NA, 2, 1, "Samples", 
)

ctrl_mult_out <- tibble::tribble(
    ~Sample, ~`Ratio_ctrl1-Pool1`, ~`Ratio_ctrl2-Pool1`, 
    ~Ratio_All_Controls, ~IS_Source,
    "A-Pool1", 2, 1, 1.5, "Controls",
    "B-Pool1", 2, NA, 1.5, "Controls", 
    "All_Samples", 2, 2, 1.5, "Controls",
    "A-Pool1", 1, NA, 1, "Samples",
    "B-Pool1", NA, 1, 1, "Samples", 
    "All_Samples", 2, 2, 1, "Samples"
)

ctrl_byIS_mult_out <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~Sample, ~`Ratio_ctrl1-Pool1`, 
    ~`Ratio_ctrl2-Pool1`, ~Ratio_All_Controls, ~IS_Source,
    "11", 64537168, "-", "All_Samples", 2, 2, 2, "Controls",
    "2", 73762398, "-", "All_Samples", 2, NA, 1, "Controls", 
    "17", 1632982, "-", "All_Samples", 2, NA, 1, "Samples", 
    "5", 83626283, "+", "All_Samples", NA, 2, 1, "Samples"
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

test_that("replicates_IS_count works well", {
    mat <- share_mat
    af <- association_file
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
                    is.na(row$A) == TRUE &
                    row$B == 1)
    expect_true(is.na(row2$CEM37) == TRUE &
                    row2$A == 1 & 
                    is.na(row2$B) == TRUE)
})

test_that("replicates_IS_ratio works correctly", {
    mat <- share_mat
    af <- association_file
    r <- replicates_IS_ratio(af, mat)
    r <- tibble::tibble(r) %>%
        dplyr::select("Sample", "Ratio_CEM37", "IS_Source")
    expect_equal(r, ratios_out, tolerance = 0.0001)
})

test_that("replicates_IS_ratio_byIS works correctly", {
    mat <- share_mat
    af <- association_file
    r <- replicates_IS_ratio_byIS(af, mat)
    r <- tibble::tibble(r) %>% 
        dplyr::select("chr", "integration_locus", "strand", 
                      "Sample", "Ratio_CEM37", "IS_Source") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(tibble::tibble(r), ratios_byIS_out, tolerance = 0.0001)
})


# Errors and warnings for no sharing

test_that("replicates_IS_ratio warning for no sharing", {
    mat <- no_share_mat
    af <- association_file
    expect_warning(replicates_IS_ratio(af, mat))
    expect_warning(replicates_IS_ratio_byIS(af, mat))
})

test_that("replicates_IS_ratio reports warning for no sharing from CEMs", {
    mat <- no_share_control_mat
    af <- association_file
    expect_warning(replicates_IS_ratio(af, mat))
    expect_warning(replicates_IS_ratio_byIS(af, mat))
})

test_that("replicates_IS_ratio reports warning for no sharing from samples", {
    mat <- no_share_others_mat
    af <- association_file
    expect_warning(replicates_IS_ratio(af, mat))
    expect_warning(replicates_IS_ratio_byIS(af, mat))
})


# Work with different input columns

test_that("replicates_IS_count works with non-default columns", {
    mat <- share_mat
    af <- mod_af
    counts <- replicates_IS_count(af, mat, subject_col = "Subject")
    row <- tibble::tibble_row(counts %>% 
                                  dplyr::filter(chr == "2" & 
                                                    integration_locus == 
                                                    73762398 & 
                                                    strand == "-"))
    row2 <- tibble::tibble_row(counts %>% 
                                   dplyr::filter(chr == "19" & 
                                                     integration_locus == 
                                                     8621028 & 
                                                     strand == "-"))
    expect_true(row$CEM37 == 1 &
                    is.na(row$A) == TRUE &
                    row$B == 1)
    expect_true(is.na(row2$CEM37) == TRUE &
                    row2$A == 1 & 
                    is.na(row2$B) == TRUE)
})

test_that("replicates_IS_ratio works with non-default columns", {
    mat <- share_mat
    af <- mod_af
    r <- replicates_IS_ratio(af, mat, subject_col = "Subject")
    r <- tibble::tibble(r) %>%
        dplyr::select("Sample", "Ratio_CEM37", "IS_Source")
    expect_equal(r, ratios_out, tolerance = 0.0001)
})

test_that("replicates_IS_ratio_byIS works with non-default columns", {
    mat <- share_mat
    af <- mod_af
    r <- replicates_IS_ratio_byIS(af, mat, subject_col = "Subject")
    r <- tibble::tibble(r) %>%
        dplyr::select("chr", "integration_locus", "strand", "Sample", 
                      "Ratio_CEM37", "IS_Source") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(r, ratios_byIS_out, tolerance = 0.0001)
})


# Work with different controls

test_that("replicates_IS_ratio works with different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- replicates_IS_ratio(af, mat, ctrl = controls)
    r <- tibble::tibble(r) %>%
        dplyr::select("Sample", "Ratio_ctrl1", 
                      "Ratio_ctrl2", "Ratio_All_Controls", "IS_Source")
    expect_equal(r, ctrl_out, tolerance = 0.0001)
})

test_that("replicates_IS_ratio_byIS works with different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- replicates_IS_ratio_byIS(af, mat, ctrl = controls)
    r <- tibble::tibble(r) %>%
        dplyr::select("chr", "integration_locus", "strand", "Sample", 
                      "Ratio_ctrl1", "Ratio_ctrl2", 
                      "Ratio_All_Controls", "IS_Source") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(r, ctrl_byIS_out, tolerance = 0.0001)
})


# Work with sample defined by multiple columns

test_that("replicates_IS_ratio works with multiple columns sample", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- replicates_IS_ratio(af, mat, ctrl = controls_mult,
                         subject_col = c("SubjectID", "PoolID"),
                         field_sep = "-")
    r <- tibble::tibble(r) %>% 
        dplyr::select("Sample", "Ratio_ctrl1-Pool1", "Ratio_ctrl2-Pool1",
                      "Ratio_All_Controls", "IS_Source")
    expect_equal(r, ctrl_mult_out, tolerance = 0.0001)
})

test_that("replicates_IS_ratio_byIS works with multiple columns sample", {
    mat <- ctrl_matrix
    af <- ctrl_af
    r <- replicates_IS_ratio_byIS(af, mat, ctrl = controls_mult,
                              subject_col = c("SubjectID", "PoolID"),
                              field_sep = "-")
    r <- tibble::tibble(r) %>% 
        dplyr::select("chr", "integration_locus", "strand", "Sample", 
                      "Ratio_ctrl1-Pool1", "Ratio_ctrl2-Pool1", 
                      "Ratio_All_Controls", "IS_Source") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(r, ctrl_byIS_mult_out, tolerance = 0.0001)
})
