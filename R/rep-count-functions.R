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
        dplyr::select(SubjectID) %>%
        unique()
    
    # Count replicates per subject per IS
    table <- apply(subjects, 1, function(x) {
        replicates <- af %>% dplyr::filter(SubjectID == x) %>%
            dplyr::pull(CompleteAmplificationID)
        rows <- matrix %>%
            dplyr::filter(CompleteAmplificationID %in% replicates)
        counts <- rows %>%
            dplyr::group_by(chr, integration_locus, strand) %>%
            dplyr::summarise(Count = dplyr::n())
        names(counts)[names(counts) == "Count"] <- x
        return(counts)
    })
    
    IS_replicate_count <- table %>%
        purrr::reduce(dplyr::full_join,
                      by = c("chr", "integration_locus", "strand"))
    
    IS_replicate_count[is.na(IS_replicate_count)] <- 0
    
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
    
    data("known_cem_is")
    
    # Filter shared known CEM IS
    filter_shared_cem_is <- IS_replicate_count %>%
        dplyr::inner_join(known_cem_is %>% 
                              dplyr::select(-GeneName, -GeneStrand),
                          by = c("chr", "integration_locus", "strand")) %>%
        dplyr::ungroup() %>%
        dplyr::filter(CEM37 > 0) %>%
        dplyr::filter(dplyr::if_any(c(-chr, -integration_locus, 
                                      -strand, -CEM37),
                                    ~ . > 0))
    
    # Filter shared IS from samples
    filter_other_is_full <- IS_replicate_count %>%
        dplyr::anti_join(known_cem_is,
                         by = c("chr", "integration_locus", "strand"))
    
    filter_shared_other_is <- filter_other_is_full %>% dplyr::ungroup() %>%
        dplyr::filter(CEM37 > 0) %>%
        dplyr::filter(dplyr::if_any(c(-chr, -integration_locus, 
                                      -strand, -CEM37),
                                    ~ . > 0))
    
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 &
        nrow(filter_shared_other_is) == 0) {
        
        stop("There are no IS shared")
        
    }
    
    if (nrow(filter_shared_cem_is) > 0) {
        
        replicates_count <-
            as.data.frame(colSums(filter_shared_cem_is %>%
                                      dplyr::select(-chr,
                                                    -integration_locus, 
                                                    -strand)))
        
        colnames(replicates_count) <- "Count"
        
        replicates_count <- replicates_count %>%
            dplyr::bind_cols(SubjectID = rownames(replicates_count))
        
        cem_total <- replicates_count["CEM37",] %>%
            dplyr::select(Count)
        
        samples_replicate_count <- replicates_count %>%
            dplyr::filter(SubjectID != "CEM37")
        
        samples_total <- sum(samples_replicate_count %>%
                                 dplyr::select(Count))
        
        row <- data.frame(Count = samples_total, SubjectID = "All samples")
        
        samples_replicate_count <- samples_replicate_count %>%
            dplyr::bind_rows(row)
        
        # Compute ratio for known CEM IS
        Ratios_known_CEM_replicate_count <-
            dplyr::bind_rows(apply(samples_replicate_count, 1, function(x) {
                row <- as.list(x)
                row$Count <- as.integer(row$Count)
                tot <- row$Count
                ifelse(tot == 0, R <- NA, R <- cem_total / tot)
                sample <- row$SubjectID
                data <- data.frame(sample, R)
                colnames(data) <- c("Sample", "Ratio")
                return(data)
            }))
        
        Ratios_known_CEM_replicate_count$IS_Source <- "CEM"
        rownames(Ratios_known_CEM_replicate_count) <- NULL
        
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    
    if (nrow(filter_shared_other_is) > 0) {
        
        replicates_count <-
            as.data.frame(colSums(filter_shared_other_is %>%
                                      dplyr::select(-chr,
                                                    -integration_locus, 
                                                    -strand)))
        
        colnames(replicates_count) <- "Count"
        
        replicates_count <- replicates_count %>%
            dplyr::bind_cols(SubjectID = rownames(replicates_count))
        
        cem_total <- replicates_count["CEM37",] %>%
            dplyr::select(Count)
        
        samples_replicate_count <- replicates_count %>%
            dplyr::filter(SubjectID != "CEM37")
        
        samples_total <- sum(samples_replicate_count %>%
                                 dplyr::select(Count))
        
        row <- data.frame(Count = samples_total, SubjectID = "All samples")
        
        samples_replicate_count <- samples_replicate_count %>%
            dplyr::bind_rows(row)
        
        # Compute ratio for shared IS from samples
        Ratios_other_replicate_count <-
            dplyr::bind_rows(apply(samples_replicate_count, 1, function(x) {
                row <- as.list(x)
                row$Count <- as.integer(row$Count)
                tot <- row$Count
                ifelse(tot == 0, R <- NA, R <- cem_total / tot)
                sample <- row$SubjectID
                data <- data.frame(sample, R)
                colnames(data) <- c("Sample", "Ratio")
                return(data)
            }))
        
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
    
    data("known_cem_is")
    
    # Filter shared known CEM IS
    filter_shared_cem_is <- IS_replicate_count %>%
        dplyr::inner_join(known_cem_is %>%
                              dplyr::select(-GeneName, -GeneStrand),
                          by = c("chr", "integration_locus", "strand")) %>%
        dplyr::ungroup() %>%
        dplyr::filter(CEM37 > 0) %>%
        dplyr::filter(dplyr::if_any(c(-chr, -integration_locus, 
                                      -strand, -CEM37),
                                    ~ . > 0))
    
    # Filter shared IS from samples
    filter_other_is_full <- IS_replicate_count %>%
        dplyr::anti_join(known_cem_is,
                         by = c("chr", "integration_locus", "strand"))
    
    filter_shared_other_is <- filter_other_is_full %>% dplyr::ungroup() %>%
        dplyr::filter(CEM37 > 0) %>%
        dplyr::filter(dplyr::if_any(c(-chr, -integration_locus, 
                                      -strand, -CEM37),
                                    ~ . > 0))
    
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 &
        nrow(filter_shared_other_is) == 0) {
        
        stop("There are no IS shared")
        
    }
    
    if (nrow(filter_shared_cem_is) > 0) {
        
        filter_shared_cem_is <- filter_shared_cem_is %>%
            tidyr::pivot_longer(cols = c(-chr, -integration_locus, -strand),
                                names_to = "SubjectID")
        
        all_counts <- filter_shared_cem_is %>%
            dplyr::filter(SubjectID != "CEM37") %>%
            dplyr::group_by(chr, integration_locus, strand) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::bind_cols(SubjectID = "All samples")
        
        filter_shared_cem_is <- filter_shared_cem_is %>%
            dplyr::bind_rows(all_counts)
        
        shared_known_cem_IS_replicate_counts <- filter_shared_cem_is %>%
            tidyr::pivot_wider(names_from = SubjectID,
                               values_from = value, values_fill = 0)
        
        subs <- filter_shared_cem_is %>%
            dplyr::filter(SubjectID != "CEM37") %>%
            dplyr::pull(SubjectID) %>%
            unique()
        
        # Compute ratio for shared known CEM IS
        known_cem_is_replicate_ratios <-
            dplyr::bind_rows(apply(
                shared_known_cem_IS_replicate_counts,
                1, function(x) {
                    row <- as.list(x)
                    row$integration_locus <-
                        as.integer(row$integration_locus)
                    rats <- dplyr::bind_rows(lapply(subs, function(y) {
                        tot <- row[y]
                        cem <- row["CEM37"]
                        ifelse(tot == 0, r <- NA,
                               r <- as.integer(cem[[1]]) / 
                                   as.integer(tot[[1]]))
                        res <- data.frame(y, r)
                        return(res)
                    }))
                    data <- data.frame(row$chr,
                                       row$integration_locus,
                                       row$strand, rats)
                    colnames(data) <- c("chr", "integration_locus",
                                        "strand", "SubjectID",
                                        "Ratio")
                    return(data)
                }))
        
        known_cem_is_replicate_ratios <- known_cem_is_replicate_ratios %>%
            tidyr::pivot_wider(names_from = SubjectID, values_from = Ratio)
        
        known_cem_is_replicate_ratios$IS_Source <- "CEM"
        
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    
    if (nrow(filter_shared_other_is) > 0) {
        
        filter_shared_other_is <- filter_shared_other_is %>%
            tidyr::pivot_longer(cols = c(-chr, -integration_locus, -strand),
                                names_to = "SubjectID")
        
        all_other_counts <- filter_shared_other_is %>%
            dplyr::filter(SubjectID != "CEM37") %>%
            dplyr::group_by(chr, integration_locus, strand) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::bind_cols(SubjectID = "All samples")
        
        filter_shared_other_is <- filter_shared_other_is %>%
            dplyr::bind_rows(all_other_counts)
        
        shared_other_IS_replicate_counts <- filter_shared_other_is %>%
            tidyr::pivot_wider(names_from = SubjectID,
                               values_from = value, values_fill = 0)
        
        subs <- filter_shared_other_is %>%
            dplyr::filter(SubjectID != "CEM37") %>%
            dplyr::pull(SubjectID) %>%
            unique()
        
        # Compute ratio for shared IS from samples
        other_is_replicate_ratios <-
            dplyr::bind_rows(apply(
                shared_other_IS_replicate_counts,
                1, function(x) {
                    row <- as.list(x)
                    row$integration_locus <-
                        as.integer(row$integration_locus)
                    rats <-
                        dplyr::bind_rows(lapply(subs, function(y) {
                            tot <- row[y]
                            cem <- row["CEM37"]
                            ifelse(tot == 0, r <- NA,
                                   r <- as.integer(cem[[1]]) /
                                       as.integer(tot[[1]]))
                            res <- data.frame(y, r)
                            return(res)
                        }))
                    data <- data.frame(row$chr,
                                       row$integration_locus,
                                       row$strand, rats)
                    colnames(data) <- c("chr", "integration_locus",
                                        "strand", "SubjectID",
                                        "Ratio")
                    return(data)
                }))
        
        other_is_replicate_ratios <- other_is_replicate_ratios %>%
            tidyr::pivot_wider(names_from = SubjectID, values_from = Ratio)
        
        other_is_replicate_ratios$IS_Source <- "Samples"
        
    } else {
        warning("There are no IS shared from the samples to CEMs")
    }
    
    if (nrow(filter_shared_cem_is) > 0 &
        nrow(filter_shared_other_is) > 0) {
        
        shared_IS_ratio <- known_cem_is_replicate_ratios %>%
            dplyr::bind_rows(other_is_replicate_ratios)
        
        return(shared_IS_ratio)
        
    } else if (nrow(filter_shared_cem_is) > 0 &
               nrow(filter_shared_other_is) == 0) {
        
        return(known_cem_is_replicate_ratios)
        
    } else if (nrow(filter_shared_cem_is) == 0 &
               nrow(filter_shared_other_is) > 0) {
        
        return(other_is_replicate_ratios)
        
    }
    
}
