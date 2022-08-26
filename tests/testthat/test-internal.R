library(RISingVIS)

# Test tables

data(association_file)

ratios_out <- tibble::tribble(
    ~Sample, ~Ratio,
    "A", 369.33333333,
    "B", 221.60000000,
    "All samples", 138.50000000
)

ratios_out_byIS <- tibble::tribble(
    ~chr, ~integration_locus, ~strand, ~A, ~B, ~`All samples`,
    "11", 64537168, "-", 154.333333, NA, 154.333333,
    "2", 73762398, "-", NA, 129, 129
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


# Test correct output

test_that("find_shared_CEM_IS works correctly", {
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
    shared <- find_shared_CEM_IS(agg, is_vars, "SubjectID")
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

test_that("find_shared_other_IS works correctly", {
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
    shared <- find_shared_other_IS(agg, is_vars, "SubjectID", "value")
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
    mat <- share_mat
    af <- association_file
    shared <- tibble::tribble(
        ~chr, ~integration_locus, ~strand, ~SubjectID, ~value,
        "11", 64537168, "-", "A", 3, 
        "11", 64537168, "-", "CEM37", 463, 
        "2", 73762398, "-", "B", 5,
        "2", 73762398, "-", "CEM37", 645
    )
    is_vars <- get_is_vars()
    r <- compute_ratio(shared, is_ratio, "SubjectID", "value")
    rownames(r) <- NULL
    output <- ratios_out %>% 
        dplyr::filter(IS_Source == "CEM") %>% 
        dplyr::select(Sample, Ratio)
    expect_equal(tibble::tibble(r), output, tolerance = 0.0001)
})

test_that("compute_ratio_byIS works correctly", {
    mat <- share_mat
    af <- association_file
    shared <- tibble::tribble(
        ~chr, ~integration_locus, ~strand, ~SubjectID, ~value,
        "11", 64537168, "-", "A", 3, 
        "11", 64537168, "-", "CEM37", 463, 
        "2", 73762398, "-", "B", 5,
        "2", 73762398, "-", "CEM37", 645
    )
    is_vars <- get_is_vars()
    r <- compute_ratio_byIS(shared, is_vars, "SubjectID", "value")
    rownames(r) <- NULL
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


# Test get_is_vars

test_that("get_is_vars returns three character values", {
    is_vars <- get_is_vars()
    expect_true(length(is_vars) == 3)
    expect_true(is.character(is_vars))
})
