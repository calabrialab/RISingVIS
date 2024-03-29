library(RISingVIS)

# Test tables

data(association_file)

ratios_out <- tibble::tribble(
    ~Sample, ~Ratio_CEM37,
    "A", 369.33333333,
    "B", 221.60000000, 
    "All_Samples", 138.50000000
)

ratios_out_byIS <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~Sample, ~Ratio_CEM37,
    "11", 64537168, "-", "All_Samples", 154.333333,
    "2", 73762398, "-", "All_Samples", 129
)

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


# Test correct output

test_that("find_shared_IS works correctly for control IS", {
    mat <- share_mat
    af <- association_file
    agg <- ISAnalytics::aggregate_values_by_key(
        x = mat,
        association_file = af,
        value_cols = "Value",
        key = "SubjectID",
        group = ISAnalytics::mandatory_IS_vars()
    )
    colnames(agg)[which(names(agg) == "Value_sum")] <- 
        "value"
    is_vars <- get_is_vars()
    shared <- find_shared_IS(agg, af, is_vars, "SubjectID", 
                             "CompleteAmplificationID", "Value", 
                             ctrl = "CEM37", type = "control")
    shared <- shared %>% 
        dplyr::select(.data$chr, .data$integration_locus, .data$strand) %>% 
        unique()
    rows <- tibble::tribble(
        ~chr,	~integration_locus,	~strand,
        "11",	64537168,	"-",
        "2",	73762398,	"-"
    )
    expect_equal(shared, rows)
})

test_that("find_shared_IS works correctly for different control", {
    mat <- ctrl_matrix
    af <- ctrl_af
    agg <- ISAnalytics::aggregate_values_by_key(
        x = mat,
        association_file = af,
        value_cols = "Value",
        key = "SubjectID",
        group = ISAnalytics::mandatory_IS_vars()
    )
    colnames(agg)[which(names(agg) == "Value_sum")] <- 
        "Value"
    is_vars <- get_is_vars()
    shared <- find_shared_IS(agg, af, is_vars, "SubjectID", 
                             "CompleteAmplificationID", "Value", 
                             controls, "control", "_")
    shared <- shared[["ctrl1"]] %>% 
        dplyr::select(.data$chr, .data$integration_locus, .data$strand) %>% 
        unique()
    rows <- tibble::tribble(
        ~chr,	~integration_locus,	~strand,
        "11",	64537168,	"-",
        "2",	73762398,	"-"
    )
    expect_equal(shared, rows)
})

test_that("find_shared_IS works correctly for samples IS", {
    mat <- share_mat
    af <- association_file
    agg <- ISAnalytics::aggregate_values_by_key(
        x = mat,
        association_file = af,
        value_cols = "Value",
        key = "SubjectID",
        group = ISAnalytics::mandatory_IS_vars()
    )
    colnames(agg)[which(names(agg) == "Value_sum")] <- 
        "Value"
    is_vars <- get_is_vars()
    shared <- find_shared_IS(agg, af, is_vars, "SubjectID", 
                             "CompleteAmplificationID", "Value",
                             ctrl = "CEM37", type = "other", field_sep = "_")
    shared <- shared %>% 
        dplyr::select(.data$chr, .data$integration_locus, .data$strand) %>% 
        unique()
    rows <- tibble::tribble(
        ~chr,	~integration_locus,	~strand,
        "17",	"1632982",	"-",
        "5",	"83626283",	"+"
    )
    expect_equal(shared, rows)
})

test_that("compute_ratio works correctly", {
    shared <- tibble::tribble(
        ~chr, ~integration_locus, ~strand, ~SubjectID, ~value,
        "11", 64537168, "-", "A", 3, 
        "11", 64537168, "-", "CEM37", 463, 
        "2", 73762398, "-", "B", 5,
        "2", 73762398, "-", "CEM37", 645
    )
    is_vars <- get_is_vars()
    r <- compute_ratio(shared, is_vars, "SubjectID", "value", 
                       "CEM37", "_", "by sample")
    rownames(r) <- NULL
    r <- tibble::tibble(r) %>%
        dplyr::select("Sample", "Ratio_CEM37")
    expect_equal(tibble::tibble(r), ratios_out, tolerance = 0.0001)
})

test_that("compute_ratio_byIS works correctly", {
    shared <- tibble::tribble(
        ~chr, ~integration_locus, ~strand, ~SubjectID, ~value,
        "11", 64537168, "-", "A", 3, 
        "11", 64537168, "-", "CEM37", 463, 
        "2", 73762398, "-", "B", 5,
        "2", 73762398, "-", "CEM37", 645
    )
    is_vars <- get_is_vars()
    r <- compute_ratio(shared, is_vars, "SubjectID", 
                            "value", "CEM37", "_", type = "by IS")
    rownames(r) <- NULL
    r <- tibble::tibble(r) %>%
        dplyr::select("chr", "integration_locus", "strand", "Sample", 
                      "Ratio_CEM37") %>%
        dplyr::filter(Sample == "All_Samples")
    expect_equal(tibble::tibble(r), ratios_out_byIS, tolerance = 0.0001)
})


# Test files_check

test_that("files_check works correctly", {
    mat <- share_mat
    af <- association_file
    expect_error(files_check(af, matrix, subject_col = "Subject", 
                             amp_col = "CompleteAmplificationID", 
                             value_col = "Value"))
})


# Test ctrl_check

test_that("ctrl_check works correctly", {
    af <- association_file
    expect_error(ctrl_check(af, "SubjectID", "Ctrl1", "_"))
    ctrl <- list("Ctrl1", "Ctrl2", "Ctrl3")
    expect_error(ctrl_check(af, "SubjectID", ctrl, "_"))
    ctrl <- controls
    expect_error(ctrl_check(af, "SubjectID", ctrl, "_"))
    af <- ctrl_af
    expect_true(ctrl_check(af, "SubjectID", ctrl, "_"))
})


# Test get_is_vars

test_that("get_is_vars returns three character values", {
    is_vars <- get_is_vars()
    expect_true(length(is_vars) == 3)
    expect_true(is.character(is_vars))
})

# Test no_IS_shared

test_that("no_IS_shared returns all ratios equal to 0", {
    r <- no_IS_shared("CEM37", association_file, "SubjectID", "_")
    r <- tibble::tibble(r) %>%
        dplyr::select("Ratio_CEM37") %>%
        unique()
    expect_true(is.na(r))
})
