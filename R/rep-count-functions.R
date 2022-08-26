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
#' @usage replicates_IS_count(af, matrix)
#' @param af The association file
#' @param matrix The integration matrix
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
        dplyr::select(.data[[subject_col]]) %>%
        unique()
    # Count replicates per subject per IS
    table <- apply(subjects, 1, function(x) {
        replicates <- af %>% dplyr::filter(.data[[subject_col]] == x) %>%
            dplyr::pull(.data[[amp_col]])
        rows <- matrix %>%
            dplyr::filter(.data[[amp_col]] %in% replicates)
        counts <- rows %>%
            dplyr::group_by(.data[[is_vars[1]]], .data[[is_vars[2]]], 
                            .data[[is_vars[3]]]) %>%
            dplyr::summarise(Count = dplyr::n())
        names(counts)[names(counts) == "Count"] <- x
        return(counts)
    })
    IS_replicate_count <- table %>%
        purrr::reduce(dplyr::full_join,
                      by = is_vars)
    return(IS_replicate_count)
}


# replicates_IS_ratio
#' Returns a dataframe containing the ratios between the number 
#' of replicates for the considered type of IS in the CEM samples 
#' and the number of replicates for the considered type of IS 
#' in the other samples.
#'
#' This function counts the number of replicates in which each shared IS
#' is found and sums them to obtain an overall count for each sample.
#' This function focuses on two categories of shared IS: the ones that are
#' known to belong to CEMs and the one that come from the other samples.
#' Also it considers the count for single subjects and for the overall 
#' sum of these subjects, meaning that the ratio is computed against 
#' each subjects replicate count and against the sum of the replicate 
#' count of all subjects.
#' After computation of replicate count gives as output a dataframe 
#' containing all the ratios, which includes information about the 
#' sample considered, the ratio itself and the type of IS considered.
#'
#' @usage replicates_IS_ratio(af, matrix)
#' @param af The association file
#' @param matrix The integration matrix
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
                                amp_col = "CompleteAmplificationID",
                                value_col = "Value") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
        stop()
    }
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Count replicates per IS
    IS_replicate_count <- replicates_IS_count(af, matrix, subject_col, 
                                              amp_col, value_col)
    IS_replicate_count  <- IS_replicate_count %>% 
        tidyr::pivot_longer(cols = c(-is_vars[1], -is_vars[2], -is_vars[3]), 
                            names_to = subject_col, values_to = value_col, 
                            values_drop_na = TRUE)
    known_cem_is <- known_CEM_IS()
    # Filter shared integrations belonging to controls
    filter_shared_cem_is <- find_shared_CEM_IS(IS_replicate_count, 
                                               is_vars, subject_col)
    # Filter shared integrations belonging to samples
    filter_shared_other_is <- find_shared_other_IS(IS_replicate_count,
                                                   is_vars, subject_col,
                                                   value_col)
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 & nrow(filter_shared_other_is) == 0) {
        stop("There are no IS shared")
    }
    if (nrow(filter_shared_cem_is) > 0) {
        Ratios_known_CEM_replicate_count <- 
            compute_ratio(filter_shared_cem_is, is_vars, 
                          subject_col, value_col)
        Ratios_known_CEM_replicate_count$IS_Source <- "CEM"
        rownames(Ratios_known_CEM_replicate_count) <- NULL
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    if (nrow(filter_shared_other_is) > 0) {
        Ratios_other_replicate_count <- 
            compute_ratio(filter_shared_other_is, is_vars, 
                          subject_col, value_col)
        Ratios_other_replicate_count$IS_Source <- "Samples"
        rownames(Ratios_other_replicate_count) <- NULL
    } else {
        warning("There are no IS shared from the samples to CEMs")
    }
    if (nrow(filter_shared_cem_is) > 0 &
        nrow(filter_shared_other_is) > 0) {
        Ratios_replicate_count <- Ratios_known_CEM_replicate_count %>%
            dplyr::bind_rows(Ratios_other_replicate_count)
        return(Ratios_replicate_count)
    } else if (nrow(filter_shared_cem_is) > 0 &
               nrow(filter_shared_other_is) == 0) {
        return(Ratios_known_CEM_replicate_count)
    } else if (nrow(filter_shared_cem_is) == 0 &
               nrow(filter_shared_other_is) > 0) {
        return(Ratios_other_replicate_count)
    }
}


# replicates_IS_ratio_byIS
#' Returns a dataframe containing the ratios for each IS between 
#' the number of replicates for that IS in the CEM samples and 
#' the number of replicates for that same IS in the other samples.
#'
#' This function counts the number of replicates in which each shared IS
#' is found and computes the ratio between controls and other samples.
#' This function focuses on two categories of shared IS: the ones that are
#' known to belong to CEMs and the one that come from the other samples.
#' Also it considers the count for single subjects and for the overall 
#' sum of these subjects, meaning that the ratio is computed against 
#' each subjects replicate count and against the sum of the replicate 
#' count of all subjects.
#' After computation of replicate count gives as output a dataframe 
#' containing all the ratios, which includes information about the IS, 
#' the sample, the ratio itself and the type of IS considered.
#'
#' @usage replicates_IS_ratio_byIS(af, matrix)
#' @param af The association file
#' @param matrix The integration matrix
#' @return Dataframe of values corresponding to the ratios
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
                                     amp_col = "CompleteAmplificationID",
                                     value_col = "Value") {
    # Check input files
    if (files_check(af, matrix, subject_col, amp_col, value_col) != TRUE) {
        stop()
    }
    # Retrieve IS variables
    is_vars <- get_is_vars()
    # Count replicates per IS
    IS_replicate_count <- replicates_IS_count(af, matrix, subject_col,
                                              amp_col, value_col)
    IS_replicate_count  <- IS_replicate_count %>% 
        tidyr::pivot_longer(cols = c(-is_vars[1], -is_vars[2], -is_vars[3]), 
                            names_to = subject_col, values_to = value_col, 
                            values_drop_na = TRUE)
    known_cem_is <- known_CEM_IS()
    # Filter shared integrations belonging to controls
    filter_shared_cem_is <- find_shared_CEM_IS(IS_replicate_count, is_vars, 
                                               subject_col)
    # Filter shared integrations belonging to samples
    filter_shared_other_is <- find_shared_other_IS(IS_replicate_count, is_vars,
                                                   subject_col, value_col)
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 & nrow(filter_shared_other_is) == 0) {
        stop("There are no IS shared")
    }
    if (nrow(filter_shared_cem_is) > 0) {
        Ratios_known_CEM_replicate_count <- 
            compute_ratio_byIS(filter_shared_cem_is, is_vars,
                               subject_col, value_col)
        Ratios_known_CEM_replicate_count$IS_Source <- "CEM"
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    if (nrow(filter_shared_other_is) > 0) {
        Ratios_other_replicate_count <- 
            compute_ratio_byIS(filter_shared_other_is, is_vars,
                               subject_col, value_col)
        Ratios_other_replicate_count$IS_Source <- "Samples"
    } else {
        warning("There are no IS shared from the samples to CEMs")
    }
    if (nrow(filter_shared_cem_is) > 0 &
        nrow(filter_shared_other_is) > 0) {
        Ratios_replicate_count <- Ratios_known_CEM_replicate_count %>%
            dplyr::bind_rows(Ratios_other_replicate_count)
        return(Ratios_replicate_count)
    } else if (nrow(filter_shared_cem_is) > 0 &
               nrow(filter_shared_other_is) == 0) {
        return(Ratios_known_CEM_replicate_count)
    } else if (nrow(filter_shared_cem_is) == 0 &
               nrow(filter_shared_other_is) > 0) {
        return(Ratios_other_replicate_count)
    }
}
