#-----------------------------------------------------------------------------#
# Sequence count functions
#-----------------------------------------------------------------------------#

# shared_CEM_known_IS_ratio
#' Returns the ratio bewteen the SeqCount for shared known CEM IS 
#' in the CEMs and the SeqCount for those shared IS 
#' in the other samples
#'
#' This function focuses on the shared IS that are known 
#' to belong to CEMs. 
#' After aggregation of the data gives as output 
#' the ratio between the SeqCount for those IS in the CEMs and 
#' the SeqCount for those IS in the other samples.
#'
#' @usage shared_CEM_known_IS_ratio(af, matrix)
#' @param af The association file
#' @param matrix The integration matrix
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
#' R <- shared_CEM_known_IS_ratio(association_file, integration_matrix)
#' R

shared_CEM_known_IS_ratio <- function(af, matrix, 
                                      subject_col = "SubjectID", 
                                      amp_col = "CompleteAmplificationID", 
                                      value_col = "Value") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
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
    filter_shared_cem_is <- find_shared_CEM_IS(aggreg_matrix, is_vars, 
                                               subject_col)
    if (nrow(filter_shared_cem_is) == 0) {
        stop("There are no IS shared from CEMs to other samples")
    }
    # Compute ratio
    R <- compute_ratio(filter_shared_cem_is, is_vars, subject_col, value_col)
    Ratio <- R %>% 
        dplyr::filter(.data$Sample == "All samples") %>% 
        dplyr::pull(.data$Ratio)
    return(Ratio)
}


# shared_other_IS_ratio
#' Returns the ratio bewteen the SeqCount for shared IS coming from 
#' other samples in the CEMs and the SeqCount for those shared IS
#' in the other samples
#'
#' This function focuses on the shared IS that are not known to 
#' belong to CEMs but come from the other samples.
#' After aggregation of the data gives as output the ratio between 
#' the SeqCount for those IS in the CEMs and the SeqCount for those IS 
#' in the other samples.
#'
#' @usage shared_other_IS_ratio(af, matrix)
#' @param af The association file
#' @param matrix The integration matrix
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
                                  amp_col = "CompleteAmplificationID", 
                                  value_col = "Value") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
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
    filter_other_is <- find_shared_other_IS(aggreg_matrix, is_vars, 
                                            subject_col, value_col)
    if (nrow(filter_other_is) == 0) {
        stop("There are no IS shared from the samples to CEMs")
    }
    # Compute ratio
    R <- compute_ratio(filter_other_is, is_vars, subject_col, value_col)
    Ratio <- R %>% 
        dplyr::filter(.data$Sample == "All samples") %>% 
        dplyr::pull(.data$Ratio)
    return(Ratio)
}


# shared_IS_ratio
#' Returns a dataframe containing the ratios between the 
#' SeqCount for the considered IS in the CEM samples and the 
#' SeqCount for the considered IS in the other samples.
#'
#' This function focuses on two categories of shared IS: the ones 
#' that are known to belong to CEMs and the one that come from 
#' the other samples. Also it considers the count for single 
#' subjects and for the overall sum of these subjects, meaning 
#' that the ratio is computed against each subjects
#' SeqCount and against the sum of the SeqCount of all subjects.
#' After aggregation of the data gives as output a dataframe 
#' containing all the ratios, which includes information about the 
#' sample considered, the ratio itself and the type of IS considered.
#'
#' @usage shared_IS_ratio(af, matrix)
#' @param af The association file
#' @param matrix The integration matrix
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
                            amp_col = "CompleteAmplificationID", 
                            value_col = "Value") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
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
    # Find shared integration sites belonging to controls
    filter_shared_cem_is <- find_shared_CEM_IS(aggreg_matrix, is_vars,
                                               subject_col)
    # Find shared integration sites belonging to samples
    filter_shared_other_is <- find_shared_other_IS(aggreg_matrix, is_vars,
                                                   subject_col, value_col)
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 & nrow(filter_shared_other_is) == 0) {
        stop("There are no IS shared")
    }
    # Compute ratios
    if (nrow(filter_shared_cem_is) > 0) {
        Ratios_known_CEM_IS <- compute_ratio(filter_shared_cem_is, is_vars,
                                             subject_col, value_col)
        Ratios_known_CEM_IS$IS_Source <- "CEM"
        rownames(Ratios_known_CEM_IS) <- NULL
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    if (nrow(filter_shared_other_is) > 0) {
        Ratios_other_IS <- compute_ratio(filter_shared_other_is, is_vars,
                                         subject_col, value_col)
        Ratios_other_IS$IS_Source <- "Samples"
        rownames(Ratios_other_IS) <- NULL
    } else {
        warning("There are no IS shared from the samples to CEMs")
    }
    if (nrow(filter_shared_cem_is) > 0 & nrow(filter_shared_other_is) > 0) {
        Ratios_shared_IS <- Ratios_known_CEM_IS %>%
            dplyr::bind_rows(Ratios_other_IS)
        return(Ratios_shared_IS)
    } else if (nrow(filter_shared_cem_is) > 0 & 
               nrow(filter_shared_other_is) == 0) {
        return(Ratios_known_CEM_IS)
    } else if (nrow(filter_shared_cem_is) == 0 & 
               nrow(filter_shared_other_is) > 0) {
        return(Ratios_other_IS)
    }
}


# shared_IS_ratio_byIS
#' Returns a dataframe containing the ratios between the SeqCount 
#' for each IS in the CEM samples and the SeqCount for the same IS 
#' in the other samples.
#'
#' This function returns the above mentioned ratio for each shared IS.
#' It focuses on two categories of shared IS: the ones that are
#' known to belong to CEMs and the one that come from the other samples.
#' Also it considers the count for single subjects and for the 
#' overall sum of these subjects, meaning that the ratio is computed 
#' against each subjects SeqCount and against the sum of the SeqCount 
#' of all subjects. 
#' After aggregation of the data gives as output a 
#' dataframe containing all the ratios, which includes information about 
#' the IS, the sample, the ratio itself and the type of IS considered.
#'
#' @usage shared_IS_ratio_byIS(af, matrix)
#' @param af The association file
#' @param matrix The integration matrix
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
                                 amp_col = "CompleteAmplificationID", 
                                 value_col = "Value") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
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
    # Find shared integration sites
    # Belonging to controls
    filter_shared_cem_is <- find_shared_CEM_IS(aggreg_matrix, is_vars,
                                               subject_col)
    # Belonging to samples
    filter_shared_other_is <- find_shared_other_IS(aggreg_matrix, is_vars,
                                                   subject_col, value_col)
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 & nrow(filter_shared_other_is) == 0) {
        stop("There are no IS shared")
    }
    # Compute ratios
    if (nrow(filter_shared_cem_is) > 0) {
        # Compute ratio for known CEM IS
        known_cem_is_ratios <- 
            compute_ratio_byIS(filter_shared_cem_is, is_vars, 
                               subject_col, value_col)
        known_cem_is_ratios$IS_Source <- "CEM"
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    if (nrow(filter_shared_other_is) > 0) {
        # Compute ratio for shared IS from samples
        other_is_ratios <-
            compute_ratio_byIS(filter_shared_other_is, is_vars,
                               subject_col, value_col)
        other_is_ratios$IS_Source <- "Samples"
    } else {
        warning("There are no IS shared from the samples to CEMs")
    }
    if (nrow(filter_shared_cem_is) > 0 & nrow(filter_shared_other_is) > 0) {
        shared_IS_ratio <- known_cem_is_ratios %>%
            dplyr::bind_rows(other_is_ratios)
        return(shared_IS_ratio)
    } else if (nrow(filter_shared_cem_is) > 0 & 
               nrow(filter_shared_other_is) == 0) {
        return(known_cem_is_ratios)
    } else if (nrow(filter_shared_cem_is) == 0 & 
               nrow(filter_shared_other_is) > 0) {
        return(other_is_ratios)
    }
}
