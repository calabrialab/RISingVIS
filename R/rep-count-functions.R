#-----------------------------------------------------------------------------#
# Replicates count functions
#-----------------------------------------------------------------------------#

# replicates_IS_count
#' Returns a dataframe containing the replicates count for
#' each integration and for each sample.
#'
#' This function counts the number of replicates in which each IS
#' is found and sums them to obtain an overall count for each sample.
#' A dataframe is returned: one row for each IS and one column for
#' each sample, the cell containing the overall replicate count
#' for that IS in that sample.
#'
#' @param af The association file
#' @param matrix The integration matrix
#' @param subject_col The name of the subject column in af, 
#' default to "SubjectID"; if multiple columns identify the subject then
#' this must be a vector containing those columns names
#' @param field_sep The character that in control names separates the different 
#' columns values, default to "_"
#' @param amp_col The name of the amplificate column in af and matrix, 
#' default to "CompleteAmplificationID"
#' @param value_col The name of the SeqCount column in matrix, 
#' default to "Value"
#' @return Dataframe of values corresponding to the replicate counts
#'
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' counts <- replicates_IS_count(association_file, integration_matrix)
#' head(counts)

replicates_IS_count <- function(af, matrix,
                                subject_col = "SubjectID",
                                field_sep = "_",
                                amp_col = "CompleteAmplificationID",
                                value_col = "Value") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
        stop()
    }
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Retrieve subjects
    subjects <- af %>%
        dplyr::select(dplyr::all_of(subject_col)) %>%
        unique()
    # Count replicates per subject per IS
    table <- apply(subjects, 1, function(x) {
        row <- as.data.frame(as.list(x))
        replicates <- as.data.frame(af) %>%
            dplyr::inner_join(row, by = subject_col) %>%
            dplyr::pull(.data[[amp_col]])
        rows <- matrix %>%
            dplyr::filter(.data[[amp_col]] %in% replicates)
        counts <- rows %>%
            dplyr::group_by(.data[[is_vars[1]]], .data[[is_vars[2]]],
                            .data[[is_vars[3]]]) %>%
            dplyr::summarise(Count = dplyr::n())
        if (length(row) > 1) {
            row <- row %>% 
                tidyr::unite("Sample", subject_col, 
                             sep = field_sep, remove = FALSE)
            names(counts)[names(counts) == "Count"] <- row[["Sample"]]
        } else {
            names(counts)[names(counts) == "Count"] <- row[[subject_col]]
        }
        return(counts)
    })
    IS_replicate_count <- table %>%
        purrr::reduce(dplyr::full_join,
                      by = is_vars)
    return(IS_replicate_count)
}


# replicates_IS_ratio
#' Returns a dataframe containing the ratios between the number
#' of replicates for the considered type of IS in the control samples
#' and the number of replicates for the considered type of IS
#' in the other samples.
#'
#' This function counts the number of replicates in which each shared IS
#' is found and sums them to obtain an overall count for each sample.
#' This function focuses on two categories of shared IS: the ones that are
#' known to belong to controls and the one that come from the other samples.
#' Also it considers the count for single subjects and for the overall
#' sum of these subjects, meaning that the ratio is computed against
#' each subjects replicate count and against the sum of the replicate
#' count of all subjects.
#' After computation of replicate count gives as output a dataframe
#' containing all the ratios, which includes information about the
#' sample considered, the ratio itself and the type of IS considered.
#'
#' @param af The association file
#' @param matrix The integration matrix
#' @param subject_col The name of the subject column in af, 
#' default to "SubjectID"; if multiple columns identify the subject then
#' this must be a vector containing those columns names
#' @param field_sep The character that in control names separates the different 
#' columns values, default to "_"
#' @param amp_col The name of the amplificate column in af and matrix, 
#' default to "CompleteAmplificationID"
#' @param value_col The name of the SeqCount column in matrix, 
#' default to "Value"
#' @param ctrl The named list of control with known integration sites,
#' default is "CEM37" with known IS as in known_CEM_IS()
#' @return Dataframe of values corresponding to the ratios
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- replicates_IS_ratio(association_file, integration_matrix)
#' head(R)

replicates_IS_ratio <- function(af, matrix,
                                subject_col = "SubjectID",
                                field_sep = "_",
                                amp_col = "CompleteAmplificationID",
                                value_col = "Value",
                                ctrl = "CEM37") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
        stop()
    }
    # Check control
    if (ctrl_check(af, subject_col, ctrl, field_sep) != TRUE) {
        stop()
    }
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Count replicates per IS
    IS_replicate_count <- replicates_IS_count(af, matrix, subject_col,
                                              field_sep, amp_col, value_col)
    if (length(subject_col) > 1) {
        IS_replicate_count  <- IS_replicate_count %>%
            tidyr::pivot_longer(cols = c(-is_vars[1], -is_vars[2], -is_vars[3]),
                                names_to = subject_col, names_sep = field_sep, 
                                values_to = value_col, values_drop_na = TRUE)
    } else {
        IS_replicate_count  <- IS_replicate_count %>%
            tidyr::pivot_longer(cols = c(-is_vars[1], -is_vars[2], -is_vars[3]),
                                names_to = subject_col,  
                                values_to = value_col, values_drop_na = TRUE)
    }
    # Filter shared integrations belonging to controls
    filter_control <- find_shared_IS(IS_replicate_count,
                                               is_vars, subject_col,
                                               value_col, ctrl,
                                               type = "control", field_sep)
    # Filter shared integrations belonging to samples
    filter_other <- find_shared_IS(IS_replicate_count,
                                             is_vars, subject_col,
                                             value_col, ctrl,
                                             type = "other", field_sep)
    # Error if no IS is shared
    if (!is.list(ctrl)) {
        lengths_ctrl <- length(filter_control)
        lengths_other <- length(filter_other)
    } else {
        lengths_ctrl <- purrr::map(filter_control[], function(x) {
            dim(x)[1]
        })
        lengths_other <- purrr::map(filter_other[], function(x) {
            dim(x)[1]
        })
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) ==
        length(lengths_ctrl) &
        sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) ==
        length(lengths_other)) {
        warning("There are no IS shared")
        Ratio <- no_IS_shared(ctrl, af)
        Ratio <- Ratio %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "RepCount"))
        return(Ratio)
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) > 0) {
        warning("One of the control lines is not sharing IS with samples")
    }
    if (sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) > 0) {
        warning("One of the control lines has no shared IS from samples")
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_ctrl)) {
        Ratios_known_control_replicate_count <-
            compute_ratio(filter_control, is_vars,
                          subject_col, value_col, ctrl,
                          type = "by sample")
        Ratios_known_control_replicate_count$IS_Source <- "Control"
        Ratios_known_control_replicate_count <- 
            Ratios_known_control_replicate_count %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "RepCount"))
    } else {
        warning("There are no IS shared from controls to other samples")
    }
    if (sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) !=
            length(lengths_other)) {
        Ratios_other_replicate_count <-
            compute_ratio(filter_other, is_vars,
                          subject_col, value_col, ctrl,
                          type = "by sample")
        Ratios_other_replicate_count$IS_Source <- "Samples"
        Ratios_other_replicate_count <- 
            Ratios_other_replicate_count %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "RepCount"))
    } else {
        warning("There are no IS shared from samples to controls")
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_ctrl) &
        sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_other)) {
        Ratios_replicate_count <- Ratios_known_control_replicate_count %>%
            dplyr::bind_rows(Ratios_other_replicate_count)
        return(Ratios_replicate_count)
    } else if (sum(sapply(lengths_ctrl,
                          function(x) all(x == 0, na.rm = TRUE))) !=
               length(lengths_ctrl) &
               sum(sapply(lengths_other,
                          function(x) all(x == 0, na.rm = TRUE))) ==
               length(lengths_other)) {
        return(Ratios_known_control_replicate_count)
    } else if (sum(sapply(lengths_ctrl,
                          function(x) all(x == 0, na.rm = TRUE))) ==
               length(lengths_ctrl) &
               sum(sapply(lengths_other,
                          function(x) all(x == 0, na.rm = TRUE))) !=
               length(lengths_other)) {
        return(Ratios_other_replicate_count)
    }
}


# replicates_IS_ratio_byIS
#' Returns a dataframe containing the ratios for each IS between
#' the number of replicates for that IS in the control samples and
#' the number of replicates for that same IS in the other samples.
#'
#' This function counts the number of replicates in which each shared IS
#' is found and computes the ratio between controls and other samples.
#' This function focuses on two categories of shared IS: the ones that are
#' known to belong to controls and the one that come from the other samples.
#' Also it considers the count for single subjects and for the overall
#' sum of these subjects, meaning that the ratio is computed against
#' each subjects replicate count and against the sum of the replicate
#' count of all subjects.
#' After computation of replicate count gives as output a dataframe
#' containing all the ratios, which includes information about the IS,
#' the sample, the ratio itself and the type of IS considered.
#'
#' @param af The association file
#' @param matrix The integration matrix
#' @param subject_col The name of the subject column in af, 
#' default to "SubjectID"; if multiple columns identify the subject then
#' this must be a vector containing those columns names
#' @param field_sep The character that in control names separates the different 
#' columns values, default to "_"
#' @param amp_col The name of the amplificate column in af and matrix, 
#' default to "CompleteAmplificationID"
#' @param value_col The name of the SeqCount column in matrix, 
#' default to "Value"
#' @param ctrl The named list of control with known integration sites,
#' default is "CEM37" with known IS as in known_CEM_IS()
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- replicates_IS_ratio_byIS(association_file, integration_matrix)
#' head(R)

replicates_IS_ratio_byIS <- function(af, matrix,
                                     subject_col = "SubjectID", 
                                     field_sep = "_",
                                     amp_col = "CompleteAmplificationID",
                                     value_col = "Value",
                                     ctrl = "CEM37") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
        stop()
    }
    # Check control
    if (ctrl_check(af, subject_col, ctrl, field_sep) != TRUE) {
        stop()
    }
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Count replicates per IS
    IS_replicate_count <- replicates_IS_count(af, matrix, subject_col,
                                              field_sep, amp_col, value_col)
    if (length(subject_col) > 1) {
        IS_replicate_count  <- IS_replicate_count %>%
            tidyr::pivot_longer(cols = c(-is_vars[1], -is_vars[2], -is_vars[3]),
                                names_to = subject_col, names_sep = field_sep, 
                                values_to = value_col, values_drop_na = TRUE)
    }
    else {
        IS_replicate_count  <- IS_replicate_count %>%
            tidyr::pivot_longer(cols = c(-is_vars[1], -is_vars[2], -is_vars[3]),
                                names_to = subject_col,  
                                values_to = value_col, values_drop_na = TRUE)
    }
    # Filter shared integrations belonging to controls
    filter_control <- find_shared_IS(IS_replicate_count, is_vars,
                                     subject_col, value_col, 
                                     ctrl, "control", field_sep)
    # Filter shared integrations belonging to samples
    filter_other <- find_shared_IS(IS_replicate_count, is_vars,
                                   subject_col, value_col, 
                                   ctrl, "other", field_sep)
    # Error if no IS is shared
    if (!is.list(ctrl)) {
        lengths_ctrl <- length(filter_control)
        lengths_other <- length(filter_other)
    } else {
        lengths_ctrl <- purrr::map(filter_control[], function(x) {
            dim(x)[1]
        })
        lengths_other <- purrr::map(filter_other[], function(x) {
            dim(x)[1]
        })
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) ==
        length(lengths_ctrl) &
        sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) ==
        length(lengths_other)) {
        warning("There are no IS shared")
        Ratio <- no_IS_shared(ctrl, af)
        Ratio <- Ratio %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "RepCount"))
        return(Ratio)
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) > 0) {
        warning("One of the control lines is not sharing IS with samples")
    }
    if (sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) > 0) {
        warning("One of the control lines has no shared IS from samples")
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_ctrl)) {
        Ratios_known_control_replicate_count <-
            compute_ratio(filter_control, is_vars,
                          subject_col, value_col, ctrl, type = "by IS")
        Ratios_known_control_replicate_count$IS_Source <- "Control"
        Ratios_known_control_replicate_count <- 
            Ratios_known_control_replicate_count %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "RepCount"))
    } else {
        warning("There are no IS shared from controls to other samples")
    }
    if (sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_other)) {
        Ratios_other_replicate_count <-
            compute_ratio(filter_other, is_vars,
                          subject_col, value_col, ctrl, type = "by IS")
        Ratios_other_replicate_count$IS_Source <- "Samples"
        Ratios_other_replicate_count <- 
            Ratios_other_replicate_count %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "RepCount"))
    } else {
        warning("There are no IS shared from samples to controls")
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_ctrl) &
        sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_other)) {
        Ratios_replicate_count <- Ratios_known_control_replicate_count %>%
            dplyr::bind_rows(Ratios_other_replicate_count)
        return(Ratios_replicate_count)
    } else if (sum(sapply(lengths_ctrl,
                          function(x) all(x == 0, na.rm = TRUE))) !=
               length(lengths_ctrl) &
               sum(sapply(lengths_other,
                          function(x) all(x == 0, na.rm = TRUE))) ==
               length(lengths_other)) {
        return(Ratios_known_control_replicate_count)
    } else if (sum(sapply(lengths_ctrl,
                          function(x) all(x == 0, na.rm = TRUE))) ==
               length(lengths_ctrl) &
               sum(sapply(lengths_other,
                          function(x) all(x == 0, na.rm = TRUE))) !=
               length(lengths_other)) {
        return(Ratios_other_replicate_count)
    }
}
