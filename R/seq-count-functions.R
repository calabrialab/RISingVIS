#-----------------------------------------------------------------------------#
# Sequence count functions
#-----------------------------------------------------------------------------#

# shared_control_known_IS_ratio
#' Returns the ratio bewteen the SeqCount for shared known control IS
#' in all controls and the SeqCount for those shared IS
#' in the other samples
#'
#' This function focuses on the shared IS that are known
#' to belong to controls.
#' After aggregation of the data gives as output
#' the ratio between the SeqCount for those IS in all controls and
#' the SeqCount for those IS in the other samples.
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
#' @return Single value of the ratio between SeqCounts
#'
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- shared_control_known_IS_ratio(association_file, integration_matrix)
#' R

shared_control_known_IS_ratio <- function(af, matrix,
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
    # Aggregate matrix and af
    aggreg_matrix <- ISAnalytics::aggregate_values_by_key(
        x = matrix,
        association_file = af,
        value_cols = value_col,
        key = subject_col,
        group = ISAnalytics::mandatory_IS_vars()
    )
    colnames(aggreg_matrix)[which(names(aggreg_matrix) == "Value_sum")] <-
        value_col
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Retrieve shared integration sites
    filter_control <- find_shared_IS(aggreg_matrix, af, is_vars,
                                     subject_col, amp_col, value_col,
                                     ctrl, "control", field_sep)
    if (!is.list(ctrl)) {
        lengths_ctrl <- length(filter_control)
    } else {
        lengths_ctrl <- purrr::map(filter_control[], function(x) {
            dim(x)[1]
        })
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) ==
        length(lengths_ctrl)) {
        warning("There are no IS shared from controls to other samples")
        Ratio <- NA
        return(Ratio)
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) > 0) {
        warning("One of the control lines is not sharing IS with samples")
    }
    # Compute ratio
    R <- compute_ratio(filter_control, is_vars, subject_col, 
                       value_col, ctrl, field_sep, type = "by sample")
    if (length(names(ctrl)) > 1) {
        Ratio <- R %>%
            dplyr::filter(.data[["Sample"]] == "All_Samples") %>%
            dplyr::pull(.data$`Ratio_All_Controls`)
        return(Ratio)
    } else {
        if (!is.list(ctrl)) {
            ctrl_name <- ctrl
        } else {
            ctrl_name <- names(ctrl)
        }
        Ratio <- R %>%
            dplyr::filter(.data[["Sample"]] == "All_Samples") %>%
            dplyr::pull(paste0("Ratio_", ctrl_name))
        return(Ratio)
    }
}


# shared_other_IS_ratio
#' Returns the ratio bewteen the SeqCount for shared IS coming from
#' other samples in the controls and the SeqCount for those shared IS
#' in the other samples
#'
#' This function focuses on the shared IS that are not known to
#' belong to controls but come from the other samples.
#' After aggregation of the data gives as output the ratio between
#' the SeqCount for those IS in all controls and the SeqCount for those IS
#' in the other samples.
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
#' @return Single value of the ratio between SeqCounts
#'
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- shared_other_IS_ratio(association_file, integration_matrix)
#' R

shared_other_IS_ratio <- function(af, matrix,
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
    # Aggregate matrix and af
    aggreg_matrix <- ISAnalytics::aggregate_values_by_key(
        x = matrix,
        association_file = af,
        value_cols = value_col,
        key = subject_col,
        group = ISAnalytics::mandatory_IS_vars()
    )
    colnames(aggreg_matrix)[which(names(aggreg_matrix) == "Value_sum")] <-
        value_col
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Retrieve shared integration sites
    filter_other <- find_shared_IS(aggreg_matrix, af, is_vars,
                                   subject_col, amp_col, value_col,
                                   ctrl, type = "other", field_sep)
    if (!is.list(ctrl)) {
        lengths_other <- length(filter_other)
    } else {
        lengths_other <- purrr::map(filter_other[], function(x) {
            dim(x)[1]
        })
    }
    if (sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) ==
        length(lengths_other)) {
        warning("There are no IS shared from samples to controls")
        Ratio <- NA
        return(Ratio)
    }
    if (sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) > 0) {
        warning("One of the control lines has no shared IS from samples")
    }
    # Compute ratio
    R <- compute_ratio(filter_other, is_vars, subject_col,
                       value_col, ctrl, field_sep, type = "by sample")
    if (length(names(ctrl)) > 1) {
        Ratio <- R %>%
            dplyr::filter(.data[["Sample"]] == "All_Samples") %>%
            dplyr::pull(.data$`Ratio_All_Controls`)
        return(Ratio)
    } else {
        if (!is.list(ctrl)) {
            ctrl_name <- ctrl
        } else {
            ctrl_name <- names(ctrl)
        }
        Ratio <- R %>%
            dplyr::filter(.data[["Sample"]] == "All_Samples") %>%
            dplyr::pull(paste0("Ratio_", ctrl_name))
        return(Ratio)
    }
}


# shared_IS_ratio
#' Returns a dataframe containing the ratios between the
#' SeqCount for the considered IS in the control samples and the
#' SeqCount for the considered IS in the other samples.
#'
#' This function focuses on two categories of shared IS: the ones
#' that are known to belong to controls and the one that come from
#' the other samples. Also it considers the count for single
#' subjects and for the overall sum of these subjects, meaning
#' that the ratio is computed against each subject's
#' SeqCount and against the sum of the SeqCount of all subjects.
#' After aggregation of the data gives as output a dataframe
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
#' @return Dataframe of values corresponding to the ratios between SeqCounts
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- shared_IS_ratio(association_file, integration_matrix)
#' head(R)

shared_IS_ratio <- function(af, matrix,
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
    # Aggregate matrix and af
    aggreg_matrix <- ISAnalytics::aggregate_values_by_key(
        x = matrix,
        association_file = af,
        value_cols = value_col,
        key = subject_col,
        group = ISAnalytics::mandatory_IS_vars()
    )
    aggreg_matrix <- aggreg_matrix %>%
      dplyr::rename(
        !!value_col := paste0(value_col, "_sum")
      )
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Find shared integration sites belonging to controls
    filter_control <- find_shared_IS(aggreg_matrix, af, is_vars,
                                     subject_col, amp_col, value_col, ctrl, 
                                     type = "control", field_sep)
    # Find shared integration sites belonging to samples
    filter_other <- find_shared_IS(aggreg_matrix, af, is_vars,
                                   subject_col, amp_col, value_col, ctrl, 
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
        Ratio <- no_IS_shared(ctrl, af, subject_col, field_sep)
        Ratio <- Ratio %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "SeqCount"))
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
    # Compute ratios
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_ctrl)) {
        Ratios_known_control_IS <- compute_ratio(filter_control,
                                                 is_vars, subject_col,
                                                 value_col, ctrl,
                                                 field_sep, type = "by sample")
        Ratios_known_control_IS$IS_Source <- "Control"
        Ratios_known_control_IS <- Ratios_known_control_IS %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "SeqCount"))
    } else {
        warning("There are no IS shared from controls to other samples")
    }
    if (sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) !=
            length(lengths_other)) {
        Ratios_other_IS <- compute_ratio(filter_other, is_vars,
                                         subject_col, value_col, ctrl,
                                         field_sep, type = "by sample")
        Ratios_other_IS$IS_Source <- "Samples"
        Ratios_other_IS <- Ratios_other_IS %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "SeqCount"))
    } else {
        warning("There are no IS shared from the samples to controls")
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_ctrl) &
        sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_other)) {
        Ratios_shared_IS <- Ratios_known_control_IS %>%
            dplyr::bind_rows(Ratios_other_IS)
        return(Ratios_shared_IS)
    } else if (sum(sapply(lengths_ctrl,
                          function(x) all(x == 0, na.rm = TRUE))) !=
               length(lengths_ctrl) &
               sum(sapply(lengths_other,
                          function(x) all(x == 0, na.rm = TRUE))) ==
               length(lengths_other)) {
        return(Ratios_known_control_IS)
    } else if (sum(sapply(lengths_ctrl,
                          function(x) all(x == 0, na.rm = TRUE))) ==
               length(lengths_ctrl) &
               sum(sapply(lengths_other,
                          function(x) all(x == 0, na.rm = TRUE)))!=
               length(lengths_other)) {
        return(Ratios_other_IS)
    }
}


# shared_IS_ratio_byIS
#' Returns a dataframe containing the ratios between the SeqCount
#' for each IS in the control samples and the SeqCount for the same IS
#' in the other samples.
#'
#' This function returns the above mentioned ratio for each shared IS.
#' It focuses on two categories of shared IS: the ones that are
#' known to belong to controls and the one that come from the other samples.
#' Also it considers the count for single subjects and for the
#' overall sum of these subjects, meaning that the ratio is computed
#' against each subjects SeqCount and against the sum of the SeqCount
#' of all subjects.
#' After aggregation of the data gives as output a
#' dataframe containing all the ratios, which includes information about
#' the IS, the sample, the ratio itself and the type of IS considered.
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
#' @return Dataframe of IS and values corresponding to the ratios of SeqCounts
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- shared_IS_ratio_byIS(association_file, integration_matrix)
#' head(R)

shared_IS_ratio_byIS <- function(af, matrix,
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
    # Aggregate matrix and af
    aggreg_matrix <- ISAnalytics::aggregate_values_by_key(
        x = matrix,
        association_file = af,
        value_cols = value_col,
        key = subject_col,
        group = ISAnalytics::mandatory_IS_vars()
    )
    aggreg_matrix <- aggreg_matrix %>%
      dplyr::rename(
        !!value_col := paste0(value_col, "_sum")
      )
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Find shared integration sites
    # Belonging to controls
    filter_control <- find_shared_IS(aggreg_matrix, af, is_vars,
                                     subject_col, amp_col, value_col,
                                     ctrl, "control", field_sep)
    # Belonging to samples
    filter_other <- find_shared_IS(aggreg_matrix, af, is_vars,
                                   subject_col, amp_col, value_col,
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
        Ratio <- no_IS_shared(ctrl, af, subject_col, field_sep)
        Ratio <- Ratio %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "SeqCount"))
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
    # Compute ratios
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_ctrl)) {
        # Compute ratio for known control IS
        known_control_is_ratios <-
            compute_ratio(filter_control, is_vars, subject_col, 
                          value_col, ctrl, field_sep, type = "by IS")
        known_control_is_ratios$IS_Source <- "Control"
        known_control_is_ratios <- known_control_is_ratios %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "SeqCount"))
    } else {
        warning("There are no IS shared from controls to other samples")
    }
    if (sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_other)) {
        # Compute ratio for shared IS from samples
        other_is_ratios <-
            compute_ratio(filter_other, is_vars, subject_col, 
                          value_col, ctrl, field_sep, type = "by IS")
        other_is_ratios$IS_Source <- "Samples"
        other_is_ratios <- other_is_ratios %>% 
            dplyr::rename_all(~stringr::str_replace_all(.x, 
                                                        "Count", "SeqCount"))
    } else {
        warning("There are no IS shared from samples to controls")
    }
    if (sum(sapply(lengths_ctrl,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_ctrl) &
        sum(sapply(lengths_other,
                   function(x) all(x == 0, na.rm = TRUE))) !=
        length(lengths_other)) {
        shared_IS_ratio <- known_control_is_ratios %>%
            dplyr::bind_rows(other_is_ratios)
        return(shared_IS_ratio)
    } else if (sum(sapply(lengths_ctrl,
                          function(x) all(x == 0, na.rm = TRUE))) !=
               length(lengths_ctrl) &
               sum(sapply(lengths_other,
                          function(x) all(x == 0, na.rm = TRUE))) ==
               length(lengths_other)) {
        
        return(known_control_is_ratios)
    } else if (sum(sapply(lengths_ctrl,
                          function(x) all(x == 0, na.rm = TRUE))) ==
               length(lengths_ctrl) &
               sum(sapply(lengths_other,
                          function(x) all(x == 0, na.rm = TRUE))) !=
               length(lengths_other))  {
        return(other_is_ratios)
    }
}
