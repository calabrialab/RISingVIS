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
    is_vars <- ISAnalytics::mandatory_IS_vars(include_types = TRUE)
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
    if (sum(af_ok) != length(subject_col)) {
        stop("Missing mandatory column in input association file")
    }
    return(TRUE)
}

# ctrl_unfold
# Retrieves the values of the column(s) that define each control

ctrl_unfold <- function(subject_col, ctrl, field_sep) {
    ctrls_list <- names(ctrl)
    res <- lapply(ctrls_list, function(x) {
        ctrl_line <- x
        ctrl_split <- stringr::str_split(ctrl_line, field_sep) %>%
            unlist()
        fields <- list()
        fields[subject_col] <- ctrl_split
        return(fields)
    })
    names(res) <- ctrls_list
    return(res)
}

# ctrl_check
# Checks that control is either CEM37 or a named list
# and is found as subject in the association file

ctrl_check <- function(af, subject_col, ctrl, field_sep) {
    `%notin%` <- Negate(`%in%`)
    if (!is.list(ctrl)) {
        if (ctrl != "CEM37") {
            stop("Control is not a list")
        } else if (ctrl == "CEM37") {
            if (ctrl %notin% af[[subject_col]]) { 
                stop("Control not found in association file")
            }
        }
    } 
    if (is.list(ctrl)) {
        if (length(names(ctrl)) != length(ctrl)) {
            stop("List of controls is not named or doesn't contain known IS")
        }
        if (length(subject_col) > 1) {
                fields <- ctrl_unfold(subject_col, ctrl, field_sep)
                fields_ok <- lapply(names(fields), function(x) {
                    ctrl_line <- x
                    single_ok <- lapply(
                        names(fields[[ctrl_line]]), function(y) {
                            col_ctrl <- y
                            ifelse(fields[[ctrl_line]][[col_ctrl]] %in% 
                                       af[[col_ctrl]],
                                   TRUE, FALSE)
                            })
                    ifelse(sum(unlist(single_ok)) == 
                               length(names(fields[[ctrl_line]])),
                           TRUE, FALSE)
                    })
                if (sum(unlist(fields_ok)) != length(names(fields))) {
                    stop("Control not found in association file")
                }
        } else { 
            if (any(names(ctrl) %notin% af[[subject_col]])) { 
                stop("Control not found in association file")
            }   
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
                           value_col, ctrl, type, field_sep) {
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
                                                      ctrl_line, ctrl_line, 
                                                      field_sep, known_is)
            return(shared_other_is)
        }
    } else {
        fields <- ctrl_unfold(subject_col, ctrl, field_sep)
        ctrl_names <- names(fields)
        res <- purrr::map(ctrl_names, function(x) {
            known_is <- ctrl[[x]]
            ctrl_line <- fields[[x]]
            if (length(ctrl_names) > 1) {
                other_ctrl <- ctrl_names[ctrl_names != x]
                other_ctrl <- fields[[other_ctrl]]
                filtered_matrix <- matrix %>% 
                    dplyr::anti_join(as.data.frame(other_ctrl), 
                                     by = subject_col)
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
                                                          value_col, x, 
                                                          ctrl_line, field_sep, 
                                                          known_is)
                return(shared_other_is)
            }
        })
        names(res) <- ctrl_names
        return(res)
    }
}


# find_shared_known_IS
# Returns a dataframe containing the shared known CEM IS
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

filter_shared_known_is <- function(matrix, is_vars, subject_col,
                                   ctrl_line, known_is) {
    known_is$integration_locus <- as.character(known_is$integration_locus)
    dplyr::bind_rows(apply(known_is, 1, function(x) {
        matrix_rows <- matrix %>%
            dplyr::filter(.data[[is_vars[1]]] == x["chr"] &
                              .data[[is_vars[2]]] ==
                              x["integration_locus"] &
                              .data[[is_vars[3]]] == x["strand"])
        if (any(ctrl_line == "CEM37")) {
            subs <- matrix_rows[[subject_col]]
            if (ctrl_line %in% subs) {
                if (length(subs) >= 2) {
                    return(matrix_rows)
                }
            }
        } else {
            subs <- matrix_rows %>% dplyr::select(subject_col)
            if (!is.null(dplyr::inner_join(as.data.frame(ctrl_line),
                                           subs, by = subject_col))) {
                if (dim(subs)[1] >= 2) {
                    return(matrix_rows)
                }
            }
        }
    }))
}



# find_shared_other_IS
# Returns a dataframe containing the shared IS from samples
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

filter_shared_other_is <- function(matrix, is_vars, subject_col,
                                   value_col, ctrl_name, ctrl_line, 
                                   field_sep, known_is) {
    known_is$integration_locus <-
        as.integer(known_is$integration_locus)
    colnames(known_is) <- c(is_vars, "GeneName", "GeneStrand")
    filter_other_is_full <- matrix %>%
        dplyr::anti_join(known_is, by = is_vars)
    filter_other_is_full[[is_vars[2]]] <-
        as.character(filter_other_is_full[[is_vars[2]]])
    join_ok <- FALSE
    if (ctrl_name == "CEM37") {
        subs <- filter_other_is_full[[subject_col]]
        if (ctrl_line %in% subs) {
            join_ok <- TRUE
        }
    } else {
        subs <- filter_other_is_full %>% dplyr::select(subject_col)
        if (!is.null(dplyr::inner_join(as.data.frame(ctrl_line),
                                       subs, by = subject_col))) {
            join_ok <- TRUE
        }
    }
    if (join_ok) {
        filter_other_is_full_wide <-
            tidyr::pivot_wider(filter_other_is_full,
                               names_from = dplyr::all_of(subject_col),
                               names_sep = field_sep,
                               values_from = dplyr::all_of(value_col),
                               values_fill = 0)
        filter_other_is_wide <- filter_other_is_full_wide %>%
            dplyr::filter(.data[[ctrl_name]] > 0) %>%
            dplyr::ungroup() %>%
            dplyr::filter(dplyr::if_any(c(-.data[[is_vars[1]]],
                                          -.data[[is_vars[2]]],
                                          -.data[[is_vars[3]]],
                                          -.data[[ctrl_name]]),
                                        ~ . > 0))
        if (length(subject_col) > 1) {
            filter_other_is_long <-
                tidyr::pivot_longer(filter_other_is_wide,
                                    cols = c(-is_vars[1], -is_vars[2], 
                                             -is_vars[3]),
                                    names_to = subject_col, 
                                    names_sep = field_sep, 
                                    values_to = value_col)
        } else {
            filter_other_is_long <-
                tidyr::pivot_longer(filter_other_is_wide,
                                    cols = c(-is_vars[1], -is_vars[2], 
                                             -is_vars[3]),
                                    names_to = subject_col, 
                                    values_to = value_col)
        }
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
                          value_col, ctrl, field_sep, type) {
    `%notin%` <- Negate(`%in%`)
    if (type %notin% c("by sample", "by IS")) {
        stop()
    }
    if (!is.list(ctrl)) {
        ctrl_names <- ctrl
        ctrl_line <- ctrl
        if (type == "by sample") {
            counts <- compute_counts(filter_shared_is, subject_col,
                                     value_col, ctrl_names, field_sep)
            res <- internal_compute_ratio(counts, subject_col, ctrl_line)
        } else if (type == "by IS") {
            counts <- compute_counts_byIS(filter_shared_is, is_vars,
                                          subject_col, value_col, 
                                          ctrl_names, field_sep)
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
                                             value_col, ctrl_names, field_sep)
                    ret <- internal_compute_ratio(counts,
                                                  subject_col, ctrl_line)
                    return(ret)
                }
                if (type == "by IS") {
                    counts <- compute_counts_byIS(shared_is, is_vars,
                                                  subject_col, value_col,
                                                  ctrl_names, field_sep)
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
                                         value_col, ctrl_names, field_sep)
                row <- data.frame(Sub = "All_Controls", Sum = sum(
                    counts %>%
                        dplyr::filter(.data[["Sample"]] %in%
                                          ctrl_names) %>%
                        dplyr::pull(.data$Sum)))
                names(row)[names(row) == "Sub"] <- "Sample"
                counts <- counts %>% dplyr::bind_rows(row)
                counts <- counts %>% dplyr::filter(.data[["Sample"]]
                                                   %notin% ctrl_names)
                ratio <- internal_compute_ratio(counts, subject_col,
                                                "All_Controls")
                res <- res %>% dplyr::full_join(ratio, by = "Sample")
            }
            if (type == "by IS") {
                counts <- compute_counts_byIS(shared_is, is_vars, subject_col,
                                              value_col, ctrl_names, field_sep)
                row <- counts %>%
                    dplyr::filter(.data[["Sample"]] %in% ctrl_names) %>%
                    dplyr::group_by(.data[[is_vars[1]]], .data[[is_vars[2]]],
                                    .data[[is_vars[[3]]]]) %>%
                    dplyr::summarise(value = sum(.data[[value_col]])) %>%
                    dplyr::bind_cols("Sample" = "All_Controls")
                names(row)[names(row) == "value"] <- value_col
                counts <- counts %>%
                    dplyr::bind_rows(row)
                counts <- counts %>%
                    dplyr::filter(.data[["Sample"]] %notin% ctrl_names)
                ratio <- internal_compute_ratio_byIS(counts, is_vars,
                                                     subject_col, value_col,
                                                     "All_Controls")
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
                           value_col, ctrl_names, field_sep) {
    counts <- filter_shared_is %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(subject_col))) %>%
        dplyr::summarise(Sum = sum(.data[[value_col]]))
    `%notin%` <- Negate(`%in%`)
    counts <- counts %>% tidyr::unite("Sample", subject_col, 
                                      sep = field_sep, remove = FALSE)
    row <- data.frame(Sub = "All_Samples", Sum =
                          sum(counts %>%
                                  dplyr::filter(.data[["Sample"]] %notin% 
                                                    ctrl_names) %>%
                                  dplyr::pull(.data$Sum)))
    names(row)[names(row) == "Sub"] <- "Sample"
    counts <- counts %>% dplyr::bind_rows(row)
    return(counts)
}

# internal_compute_ratio
# Returns a df with the computed ratio for each sample
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

internal_compute_ratio <- function(counts, subject_col, ctrl_line) {
    ctrl_count <- as.integer(counts %>%
                                 dplyr::filter(.data[["Sample"]] ==
                                                   ctrl_line) %>%
                                 dplyr::pull(.data$Sum))
    other_count <- counts %>%
        dplyr::filter(.data[["Sample"]] != ctrl_line)
    # Compute ratio
    ratios <-
        dplyr::bind_rows(apply(other_count, 1, function(x) {
            tot <- as.integer(x["Sum"])
            R <- ifelse(tot == 0, NA, ctrl_count / tot)
            sample <- x["Sample"]
            data <- data.frame(sample, ctrl_line, R, ctrl_count, tot)
            colnames(data) <- c("Sample", "Control", "Ratio", 
                                paste0("Count(", ctrl_line, ")"), 
                                paste0("Count(Sample-vs-", ctrl_line, ")"))
            return(data)
        }))
    ratios <- ratios %>% tidyr::pivot_wider(names_from = "Control",
                                            values_from = "Ratio",
                                            values_fill = NA)
    ratios <- ratios %>% dplyr::rename(
        !!paste0("Ratio_", ctrl_line) := ctrl_line)
    return(ratios)
}

# compute_counts_byIS
# Returns a df with the replicates count computed for each IS
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

compute_counts_byIS <- function(filter_shared, is_vars, subject_col,
                                value_col, ctrl_names, field_sep) {
    `%notin%` <- Negate(`%in%`)
    filter_shared <- filter_shared %>% tidyr::unite("Sample", 
                                                    subject_col, 
                                                    sep = field_sep)
    counts <- filter_shared %>%
        dplyr::filter(.data[["Sample"]] %notin% ctrl_names) %>%
        dplyr::group_by(.data[[is_vars[1]]], .data[[is_vars[2]]],
                        .data[[is_vars[[3]]]]) %>%
        dplyr::summarise(value = sum(.data[[value_col]])) %>%
        dplyr::bind_cols("Sample" = "All_Samples")
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
        tidyr::pivot_wider(names_from = dplyr::all_of("Sample"),
                           values_from = dplyr::all_of(value_col), 
                           values_fill = 0)
    subjects <- counts %>%
        dplyr::filter(.data[["Sample"]] != ctrl_line) %>%
        dplyr::pull(.data[["Sample"]]) %>%
        unique()
    ratios <- dplyr::bind_rows(apply(shared_counts, 1, function(x) {
        row <- as.list(x)
        row[[is_vars[2]]] <- as.integer(row[[is_vars[2]]])
        rats <- dplyr::bind_rows(lapply(subjects, function(y) {
            tot <- row[y]
            ctrl <- row[ctrl_line]
            R <- ifelse(tot == 0, NA,
                        as.integer(ctrl[[1]]) / as.integer(tot[[1]]))
            res <- data.frame(y, ctrl_line, as.integer(ctrl[[1]]), 
                              as.integer(tot[[1]]), R)
            return(res)
        }))
        data <- data.frame(row[[is_vars[1]]], row[[is_vars[2]]],
                           row[[is_vars[3]]], rats)
        colnames(data) <- c("chr", "integration_locus",
                            "strand", "Sample", "Control", 
                            paste0("Count(", ctrl_line, ")"), 
                            paste0("Count(Sample-vs-", ctrl_line, ")"),
                            "Ratio")
        return(data)
    }))
    rownames(ratios) <- NULL
    return(ratios)
}


# no_IS_shared()
# Returns the output in the case of no integration sites shared among samples
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data

no_IS_shared <- function(ctrl, af, subject_col, field_sep) {
    `%notin%` <- Negate(`%in%`)
    af <- af %>% tidyr::unite("Sample", 
                              subject_col, sep = field_sep)
    subjects <- af %>% 
        dplyr::filter(.data[["Sample"]] %notin% 
                          c(names(ctrl), "CEM37")) %>%
        dplyr::pull(.data[["Sample"]]) %>% 
        unique()
    subjects <- append(subjects, "All_Samples")
    if (!is.list(ctrl)) {
        Ratio <- dplyr::bind_rows(lapply(subjects, function(x) {
            sub_name <- x
            r <- tibble::tibble_row(
                "Sample" = sub_name, 
                "Ratio_CEM37" = NA, 
                "IS_Source" = NA, 
                "Count(CEM37)" = 0, 
                "Count(Sample)" = 0
            )
            return(r)
        }))
    } else {
        ctrl_names <- names(ctrl)
        ctrl_names <- append(ctrl_names, "All_Controls")
        Ratio <- dplyr::bind_rows(lapply(ctrl_names, function(x) {
            ctrl_line <- x
            res <- dplyr::bind_rows(lapply(subjects, function(y) {
                sub_name <- y
                r <- tibble::tibble_row(
                    "Sample" = sub_name, 
                    "Ctrl" = NA, 
                    "IS_Source" = NA, 
                    !!paste0("Count(", ctrl_line, ")") := 0, 
                    "Count(Sample)" = 0
                )
                r <- r %>% dplyr::rename(
                    !!paste0("Ratio_", x) := "Ctrl")
                return(r)
            }))
            return(res)
        }))
        Ratio <- Ratio %>% unique()
    }
    return(Ratio)
}
