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
# Checks that input files have the necessary columns

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

# ctrl_check
# Checks that control is either CEM37 or a named list
# and is found as Subject in the association file

ctrl_check <- function(af, subject_col, ctrl) {
    `%notin%` <- Negate(`%in%`)
    if (!is.list(ctrl)) {
        if (ctrl != "CEM37") {
            stop("Control is not a list")
        }
        if (ctrl %notin% af[[subject_col]]) {
            stop("Control not found in association file")
        }
    }
    if (is.list(ctrl)) {
        if (length(names(ctrl)) != length(ctrl)) {
            stop("List of controls is not named or doesn't contain known IS")
        }
        if (any(names(ctrl) %notin% af[[subject_col]])) {
            stop("Control not found in association file")
        }
    }
    return(TRUE)
}

# find_shared_IS
# Returns a dataframe of shared IS
# Type must be "control" or "other" depending on the interested IS source
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

find_shared_IS <- function(matrix, is_vars, subject_col,
                           value_col, ctrl, type) {
    `%notin%` <- Negate(`%in%`)
    if (type %notin% c("control", "other")) {
        stop()
    }
    if (!is.list(ctrl)) {
        if (ctrl != "CEM37") {
            stop("Control is incorrect")
        }
        known_is <- known_CEM_IS()
        ctrl_line <- ctrl
        if (type == "control") {
            shared_known_is <- filter_shared_known_is(matrix, is_vars,
                                                      subject_col, ctrl_line,
                                                      known_is)
            return(shared_known_is)
        } else if (type == "other") {
            shared_other_is <- filter_shared_other_is(matrix, is_vars,
                                                      subject_col, value_col,
                                                      ctrl_line, known_is)
            return(shared_other_is)
        }
    } else {
        ctrl_names <- names(ctrl)
        res <- purrr::map(ctrl_names, function(x) {
            known_is <- ctrl[[x]]
            ctrl_line <- x
            if (length(ctrl_names) > 1) {
                other_ctrl <- ctrl_names[ctrl_names != ctrl_line]
                filtered_matrix <- matrix %>%
                    dplyr::filter(.data[[subject_col]] %notin% other_ctrl)
            } else {
                filtered_matrix <- matrix
            }
            if (type == "control") {
                shared_known_is <- filter_shared_known_is(filtered_matrix,
                                                          is_vars, subject_col,
                                                          ctrl_line, known_is)
                return(shared_known_is)
            } else if (type == "other") {
                shared_other_is <- filter_shared_other_is(filtered_matrix,
                                                          is_vars, subject_col,
                                                          value_col,
                                                          ctrl_line, known_is)
                return(shared_other_is)
            }
        })
        names(res) <- ctrl_names
        return(res)
    }
}


# find_shared_CEM_IS
# Returns a dataframe containing the shared known CEM IS
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

filter_shared_known_is <- function(matrix, is_vars, subject_col,
                                   ctrl_line, known_is) {
    dplyr::bind_rows(apply(known_is, 1, function(x) {
        matrix_rows <- matrix %>%
            dplyr::filter(.data[[is_vars[1]]] == x["chr"] &
                              .data[[is_vars[2]]] ==
                              x["integration_locus"] &
                              .data[[is_vars[3]]] == x["strand"])
        subs <- matrix_rows[[subject_col]]
        if (ctrl_line %in% subs) {
            if (length(subs) >= 2) {
                return(matrix_rows)
            }
        }
    }))
}



# find_shared_other_IS
# Returns a dataframe containing the shared IS from samples
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

filter_shared_other_is <- function(matrix, is_vars, subject_col,
                                   value_col, ctrl_line, known_is) {
    known_is$integration_locus <-
        as.integer(known_is$integration_locus)
    colnames(known_is) <- c(is_vars, "GeneName", "GeneStrand")
    filter_other_is_full <- matrix %>%
        dplyr::anti_join(known_is, by = is_vars)
    filter_other_is_full[[is_vars[2]]] <-
        as.character(filter_other_is_full[[is_vars[2]]])
    if (ctrl_line %in% filter_other_is_full[[subject_col]]) {
        filter_other_is_full_wide <-
            tidyr::pivot_wider(filter_other_is_full,
                               names_from = all_of(subject_col),
                               values_from = all_of(value_col),
                               values_fill = 0)
        filter_other_is_wide <- filter_other_is_full_wide %>%
            dplyr::filter(.data[[ctrl_line]] > 0) %>%
            dplyr::ungroup() %>%
            dplyr::filter(dplyr::if_any(c(-.data[[is_vars[1]]],
                                          -.data[[is_vars[2]]],
                                          -.data[[is_vars[3]]],
                                          -.data[[ctrl_line]]),
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
# Returns a df with the ratio computed against each sample,
# it can be done by sample or by IS
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

compute_ratio <- function(filter_shared_is, is_vars, subject_col,
                          value_col, ctrl, type) {
    `%notin%` <- Negate(`%in%`)
    if (type %notin% c("by sample", "by IS")) {
        stop()
    }
    if (!is.list(ctrl)) {
        ctrl_names <- ctrl
        ctrl_line <- ctrl
        if (type == "by sample") {
            counts <- compute_counts(filter_shared_is, subject_col,
                                     value_col, ctrl_names)
            res <- internal_compute_ratio(counts, subject_col, ctrl_line)
        }
        if (type == "by IS") {
            counts <- compute_counts_byIS(filter_shared_is, is_vars,
                                          subject_col, value_col, ctrl_names)
            res <- internal_compute_ratio_byIS(counts, is_vars, subject_col,
                                               value_col, ctrl_line)
        }
        return(res)
    } else {
        ctrl_names <- names(ctrl)
        res <- purrr::map(ctrl_names, function(x) {
            ctrl_line <- x
            shared_is <- filter_shared_is[[ctrl_line]]
            if (dim(shared_is)[1] == 0) {
                ret <- tibble::tibble()
                return(ret)
            } else {
                if (type == "by sample") {
                    counts <- compute_counts(shared_is, subject_col,
                                             value_col, ctrl_names)
                    ret <- internal_compute_ratio(counts,
                                                  subject_col, ctrl_line)
                    return(ret)
                }
                if (type == "by IS") {
                    counts <- compute_counts_byIS(shared_is, is_vars,
                                                  subject_col, value_col,
                                                  ctrl_names)
                    ret <- internal_compute_ratio_byIS(counts, is_vars,
                                                       subject_col, value_col,
                                                       ctrl_line)
                    return(ret)
                }
            }
        })
        if (type == "by sample") {
            res <- Filter(function(x) dim(x)[1] > 0, res)
            res <- res %>%
                purrr::reduce(dplyr::full_join, by = "Sample")
        }
        if (type == "by IS") {
            res <- Filter(Negate(is.null), res)
            res <- res %>%
                purrr::reduce(dplyr::bind_rows)
        }
        if (length(ctrl_names) > 1) {
            shared_is <- do.call(rbind, filter_shared_is)
            shared_is <- dplyr::distinct(shared_is)
            if (type == "by sample") {
                counts <- compute_counts(shared_is, subject_col,
                                         value_col, ctrl_names)
                row <- data.frame(Sub = "All controls", Sum = sum(
                    counts %>%
                        dplyr::filter(.data[[subject_col]] %in%
                                          ctrl_names) %>%
                        dplyr::select(.data$Sum)))
                names(row)[names(row) == "Sub"] <- subject_col
                counts <- counts %>% dplyr::bind_rows(row)
                counts <- counts %>% dplyr::filter(.data[[subject_col]]
                                                   %notin% ctrl_names)
                ratio <- internal_compute_ratio(counts, subject_col,
                                                "All controls")
                res <- res %>% dplyr::full_join(ratio, by = "Sample")
            }
            if (type == "by IS") {
                counts <- compute_counts_byIS(shared_is, is_vars, subject_col,
                                              value_col, ctrl_names)
                row <- counts %>%
                    dplyr::filter(.data[[subject_col]] %in% ctrl_names) %>%
                    dplyr::group_by(.data[[is_vars[1]]], .data[[is_vars[2]]],
                                    .data[[is_vars[[3]]]]) %>%
                    dplyr::summarise(value = sum(.data[[value_col]])) %>%
                    dplyr::bind_cols(Sub = "All controls")
                names(row)[names(row) == "Sub"] <- subject_col
                names(row)[names(row) == "value"] <- value_col
                counts <- counts %>%
                    dplyr::bind_rows(row)
                counts <- counts %>%
                    dplyr::filter(.data[[subject_col]] %notin% ctrl_names)
                ratio <- internal_compute_ratio_byIS(counts, is_vars,
                                                     subject_col, value_col,
                                                     "All controls")
                res <- res %>% dplyr::bind_rows(ratio)
            }
        }
    }
    return(res)
}

# compute_counts
# Returns a df with the replicates count computed for each sample
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

compute_counts <- function(filter_shared_is, subject_col,
                           value_col, ctrl_names) {
    counts <- filter_shared_is %>%
        dplyr::group_by(.data[[subject_col]]) %>%
        dplyr::summarise(Sum = sum(.data[[value_col]]))
    `%notin%` <- Negate(`%in%`)
    row <- data.frame(Sub = "All samples", Sum =
                          sum(counts %>%
                                  dplyr::filter(.data[[subject_col]] %notin%
                                                    ctrl_names) %>%
                                  dplyr::select(.data$Sum)))
    names(row)[names(row) == "Sub"] <- subject_col
    counts <- counts %>% dplyr::bind_rows(row)
    return(counts)
}

# internal_compute_ratio
# Returns a df with the computed ratio for each sample
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

internal_compute_ratio <- function(counts, subject_col, ctrl_line) {
    ctrl_count <- as.integer(counts %>%
                                 dplyr::filter(.data[[subject_col]] ==
                                                   ctrl_line) %>%
                                 dplyr::select(.data$Sum))
    other_count <- counts %>%
        dplyr::filter(.data[[subject_col]] != ctrl_line)
    # Compute ratio
    ratios <-
        dplyr::bind_rows(apply(other_count, 1, function(x) {
            tot <- as.integer(x["Sum"])
            R <- ifelse(tot == 0, NA, ctrl_count / tot)
            sample <- x[subject_col]
            data <- data.frame(sample, ctrl_line, R)
            colnames(data) <- c("Sample", "Control", "Ratio")
            return(data)
        }))
    ratios <- ratios %>% tidyr::pivot_wider(names_from = "Control",
                                            values_from = "Ratio",
                                            values_fill = NA)

    return(ratios)

}

# compute_counts_byIS
# Returns a df with the replicates count computed for each IS
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

compute_counts_byIS <- function(filter_shared, is_vars, subject_col,
                                value_col, ctrl_names) {
    `%notin%` <- Negate(`%in%`)
    counts <- filter_shared %>%
        dplyr::filter(.data[[subject_col]] %notin% ctrl_names) %>%
        dplyr::group_by(.data[[is_vars[1]]], .data[[is_vars[2]]],
                        .data[[is_vars[[3]]]]) %>%
        dplyr::summarise(value = sum(.data[[value_col]])) %>%
        dplyr::bind_cols(Sub = "All samples")
    names(counts)[names(counts) == "Sub"] <- subject_col
    names(counts)[names(counts) == "value"] <- value_col
    filtered_counts <- filter_shared %>%
        dplyr::bind_rows(counts)
    return(filtered_counts)
}

# internal_compute_ratio_byIS
# Returns a df with the computed ratio for each IS
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

internal_compute_ratio_byIS <- function(counts, is_vars,
                                        subject_col, value_col, ctrl_line) {
    shared_counts <- counts %>%
        tidyr::pivot_wider(names_from = all_of(subject_col),
                           values_from = all_of(value_col), values_fill = 0)
    subjects <- counts %>%
        dplyr::filter(.data[[subject_col]] != ctrl_line) %>%
        dplyr::pull(.data[[subject_col]]) %>%
        unique()
    ratios <- dplyr::bind_rows(apply(shared_counts, 1, function(x) {
        row <- as.list(x)
        row[[is_vars[2]]] <- as.integer(row[[is_vars[2]]])
        rats <- dplyr::bind_rows(lapply(subjects, function(y) {
            tot <- row[y]
            ctrl <- row[ctrl_line]
            R <- ifelse(tot == 0, NA,
                        as.integer(ctrl[[1]]) / as.integer(tot[[1]]))
            res <- data.frame(y, ctrl_line, R)
            return(res)
        }))
        data <- data.frame(row[[is_vars[1]]], row[[is_vars[2]]],
                           row[[is_vars[3]]], rats)
        colnames(data) <- c("chr", "integration_locus",
                            "strand", subject_col, "Control", "Ratio")
        return(data)
    }))
    ratios <- ratios %>%
        tidyr::pivot_wider(names_from = all_of(subject_col),
                           values_from = Ratio)
    return(ratios)
}
