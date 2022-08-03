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
#' @importFrom utils data
#'
#' @export
#' 
#' @examples 
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' counts <- replicates_IS_count(association_file, integration_matrix)
#' head(counts)

replicates_IS_count <- function(af, matrix) {
    # Retrieve subjects
    subjects <- af %>%
        dplyr::select(.data$SubjectID) %>%
        unique()
    # Count replicates per subject per IS
    table <- apply(subjects, 1, function(x) {
        replicates <- af %>% dplyr::filter(.data$SubjectID == x) %>%
            dplyr::pull(.data$CompleteAmplificationID)
        rows <- matrix %>%
            dplyr::filter(.data$CompleteAmplificationID %in% replicates)
        counts <- rows %>%
            dplyr::group_by(.data$chr, .data$integration_locus, 
                            .data$strand) %>%
            dplyr::summarise(Count = dplyr::n())
        names(counts)[names(counts) == "Count"] <- x
        return(counts)
    })
    IS_replicate_count <- table %>%
        purrr::reduce(dplyr::full_join,
                      by = c("chr", "integration_locus", "strand"))
    #IS_replicate_count[is.na(IS_replicate_count)] <- 0
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
#' @importFrom utils data
#'
#' @export
#' 
#' @examples 
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- replicates_IS_ratio(association_file, integration_matrix)
#' head(R)

replicates_IS_ratio <- function(af, matrix) {
    # Count replicates per IS
    IS_replicate_count <- replicates_IS_count(af, matrix)
    IS_replicate_count  <- IS_replicate_count %>% 
        tidyr::pivot_longer(cols = c(-chr, -integration_locus, -strand), 
                            names_to = "SubjectID", values_drop_na = TRUE)
    known_cem_is <- known_CEM_IS()
    # Filter shared integrations belonging to controls
    filter_shared_cem_is <- find_shared_CEM_IS(IS_replicate_count)
    # Filter shared integrations belonging to samples
    filter_shared_other_is <- find_shared_other_IS(IS_replicate_count)
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 & nrow(filter_shared_other_is) == 0) {
        stop("There are no IS shared")
    }
    if (nrow(filter_shared_cem_is) > 0) {
        filter_shared_cem_is <- filter_shared_cem_is %>% 
            tidyr::pivot_wider(names_from = SubjectID, 
                               values_from = value, values_fill = 0)
        Ratios_known_CEM_replicate_count <- 
            compute_rep_count_ratio(filter_shared_cem_is)
        Ratios_known_CEM_replicate_count$IS_Source <- "CEM"
        rownames(Ratios_known_CEM_replicate_count) <- NULL
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    if (nrow(filter_shared_other_is) > 0) {
        filter_shared_other_is <- filter_shared_other_is %>% 
            tidyr::pivot_wider(names_from = SubjectID, 
                               values_from = value, values_fill = 0)
        Ratios_other_replicate_count <- 
            compute_rep_count_ratio(filter_shared_other_is)
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
#' @importFrom utils data
#'
#' @export
#' 
#' @examples 
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- replicates_IS_ratio_byIS(association_file, integration_matrix)
#' head(R)

replicates_IS_ratio_byIS <- function(af, matrix) {
    # Count replicates per IS
    IS_replicate_count <- replicates_IS_count(af, matrix)
    IS_replicate_count  <- IS_replicate_count %>% 
        tidyr::pivot_longer(cols = c(-chr, -integration_locus, -strand), 
                            names_to = "SubjectID", values_drop_na = TRUE)
    known_cem_is <- known_CEM_IS()
    # Filter shared integrations belonging to controls
    filter_shared_cem_is <- find_shared_CEM_IS(IS_replicate_count)
    # Filter shared integrations belonging to samples
    filter_shared_other_is <- find_shared_other_IS(IS_replicate_count)
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 & nrow(filter_shared_other_is) == 0) {
        stop("There are no IS shared")
    }
    if (nrow(filter_shared_cem_is) > 0) {
        filter_shared_cem_is <- filter_shared_cem_is %>% 
            tidyr::pivot_wider(names_from = SubjectID, 
                               values_from = value, values_fill = 0)
        Ratios_known_CEM_replicate_count <- 
            compute_rep_count_ratio_byIS(filter_shared_cem_is)
        Ratios_known_CEM_replicate_count$IS_Source <- "CEM"
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    if (nrow(filter_shared_other_is) > 0) {
        filter_shared_other_is <- filter_shared_other_is %>% 
            tidyr::pivot_wider(names_from = SubjectID, 
                               values_from = value, values_fill = 0)
        Ratios_other_replicate_count <- 
            compute_rep_count_ratio_byIS(filter_shared_other_is)
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
