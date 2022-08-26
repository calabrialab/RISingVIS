#-----------------------------------------------------------------------------#
# Internal functions
#-----------------------------------------------------------------------------#
# All functions in this file are not exported. 

# get_is_vars
# Returns a list of the names of the columns retrieved by ISAnalytics function
# mandatory_IS_vars() that contains only chr, locus and strand
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

get_is_vars <- function() {
    is_vars <- mandatory_IS_vars(include_types = TRUE)
    chr <- as.character(is_vars %>% 
                            dplyr::filter(.data$tag == "chromosome") %>% 
                            dplyr::select(.data$names))
    int_locus <- as.character(is_vars %>% 
                                  dplyr::filter(.data$tag == "locus") %>% 
                                  dplyr::select(.data$names))
    strand <- as.character(is_vars %>% 
                               dplyr::filter(.data$tag == "is_strand") %>% 
                               dplyr::select(.data$names))
    res <- c(chr, int_locus, strand)
    return(res)
}

# files_check 
# Controls that input files have the necessary columns

files_check <- function(af, matrix, subject_col, amp_col, value_col) {
    is_vars <- get_is_vars()
    `%notin%` <- Negate(`%in%`)
    colnames_ok <- all(c(is_vars, amp_col, value_col)
                       %in% colnames(matrix))
    if (!colnames_ok) {
        stop("Missing mandatory columns in input matrix")
    }
    af_ok <- subject_col %in% colnames(af) 
    if (!af_ok) {
        stop("Missing mandatory column in input association file")
    }
    return(TRUE)
}

# find_shared_CEM_IS
# Returns a dataframe containing the shared known CEM IS.
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

find_shared_CEM_IS <- function(matrix, is_vars, subject_col) {
    # Retrieve known CEM integration sites
    known_cem_is <- known_CEM_IS()
    known_cem_is$integration_locus <-
        as.character(known_cem_is$integration_locus)
    # Filter shared known CEM IS
    filter_shared_cem_is <- 
        dplyr::bind_rows(apply(known_cem_is, 1, function(x) {
            matrix_rows <- matrix %>%
                dplyr::filter(.data[[is_vars[1]]] == x["chr"] &
                                  .data[[is_vars[2]]] == 
                                  x["integration_locus"] &
                                  .data[[is_vars[3]]] == x["strand"])
            subs <- matrix_rows[[subject_col]]
            if ("CEM37" %in% subs) {
                if (length(subs) >= 2) {
                    return(matrix_rows)
                }
            }
        }))
    return(filter_shared_cem_is)
}


# find_shared_other_IS
# Returns a dataframe containing the shared IS from samples.
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

find_shared_other_IS <- function(matrix, is_vars, subject_col, value_col) {
    # Retrieve known CEM integration sites
    known_cem_is <- known_CEM_IS()
    known_cem_is$integration_locus <- 
        as.integer(known_cem_is$integration_locus)
    colnames(known_cem_is) <- c(is_vars, "GeneName", "GeneStrand")
    # Filter shared IS from samples
    filter_other_is_full <- matrix %>%
        dplyr::anti_join(known_cem_is,
                         by = is_vars)
    filter_other_is_full[[is_vars[2]]] <-
        as.character(filter_other_is_full[[is_vars[2]]])
    if ("CEM37" %in% filter_other_is_full[[subject_col]]) {
        filter_other_is_full_wide <-
            tidyr::pivot_wider(filter_other_is_full,
                               names_from = all_of(subject_col),
                               values_from = all_of(value_col), 
                               values_fill = 0)
        filter_other_is_wide <- filter_other_is_full_wide %>%
            dplyr::filter(.data$CEM37 > 0) %>%
            dplyr::ungroup() %>%
            dplyr::filter(dplyr::if_any(c(-.data[[is_vars[1]]], 
                                          -.data[[is_vars[2]]], 
                                          -.data[[is_vars[3]]], 
                                          -.data$CEM37),
                                        ~ . > 0))
        filter_other_is_long <-
            tidyr::pivot_longer(filter_other_is_wide,
                                cols = c(-is_vars[1], -is_vars[2], -is_vars[3]),
                                names_to = subject_col, values_to = value_col)
        filter_other_is <- filter_other_is_long %>%
            dplyr::filter(.data[[value_col]] != 0)
        return(filter_other_is)
    } else {
        res <- tibble::tibble()
        return(res)
    }
}


# compute_ratio
# Returns a df with the ratio computed against each sample
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

compute_ratio <- function(filter_shared_is, is_vars, subject_col, value_col) {
    counts <- filter_shared_is %>%
        dplyr::group_by(.data[[subject_col]]) %>%
        dplyr::summarise(Sum = sum(.data[[value_col]]))
    row <- data.frame(Sub = "All samples", Sum = 
                          sum(counts %>%
                                  dplyr::filter(.data[[subject_col]] != 
                                                    "CEM37") %>%
                                  dplyr::select(.data$Sum)))
    names(row)[names(row) == "Sub"] <- subject_col
    counts <- counts %>% dplyr::bind_rows(row)
    cem_count <- as.integer(counts %>%
                                dplyr::filter(.data[[subject_col]] == 
                                                  "CEM37") %>%
                                dplyr::select(.data$Sum))
    other_count <- counts %>% dplyr::filter(.data[[subject_col]] != "CEM37")
    # Compute ratio for shared known CEM IS
    ratios <- 
        dplyr::bind_rows(apply(other_count, 1, function(x) {
            tot <- as.integer(x["Sum"])
            R <- ifelse(tot == 0, NA, cem_count / tot)
            sample <- x[subject_col]
            data <- data.frame(sample, R)
            colnames(data) <- c("Sample", "Ratio")
            return(data)
        }))
    return(ratios)
}


# compute_ratio_byIS
# Returns a df with the ratio computed for each IS against each sample
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

compute_ratio_byIS <- function(filter_shared, is_vars, 
                               subject_col, value_col) {
    counts <- filter_shared %>% 
        dplyr::filter(.data[[subject_col]] != "CEM37") %>%
        dplyr::group_by(.data[[is_vars[1]]], .data[[is_vars[2]]], 
                        .data[[is_vars[[3]]]]) %>%
        dplyr::summarise(value = sum(.data[[value_col]])) %>%
        dplyr::bind_cols(Sub = "All samples")
    names(counts)[names(counts) == "Sub"] <- subject_col
    names(counts)[names(counts) == "value"] <- value_col
    filter_shared <- filter_shared %>% 
        dplyr::bind_rows(counts)
    shared_counts <- filter_shared %>%
        tidyr::pivot_wider(names_from = all_of(subject_col),
                           values_from = all_of(value_col), values_fill = 0)
    subjects <- filter_shared %>%
        dplyr::filter(.data[[subject_col]] != "CEM37") %>%
        dplyr::pull(.data[[subject_col]]) %>%
        unique()
    ratios <- dplyr::bind_rows(apply(shared_counts, 1, function(x) {
        row <- as.list(x)
        row[[is_vars[2]]] <- as.integer(row[[is_vars[2]]])
        rats <- dplyr::bind_rows(lapply(subjects, function(y) {
            tot <- row[y]
            cem <- row["CEM37"]
            R <- ifelse(tot == 0, NA, 
                        as.integer(cem[[1]]) / as.integer(tot[[1]]))
            res <- data.frame(y, R)
            return(res)
        }))
        data <- data.frame(row[[is_vars[1]]], row[[is_vars[2]]], 
                           row[[is_vars[3]]], rats)
        colnames(data) <- c("chr", "integration_locus",
                            "strand", subject_col, "Ratio")
        return(data)
    }))
    ratios <- ratios %>%
        tidyr::pivot_wider(names_from = all_of(subject_col), 
                           values_from = Ratio)
    return(ratios)
}
