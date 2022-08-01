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
#' @importFrom utils data
#'
#' @export
#' 
#' @examples 
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- shared_CEM_known_IS_ratio(association_file, integration_matrix)
#' R

shared_CEM_known_IS_ratio <- function(af, matrix) {
    
    # Aggregate matrix and af
    aggreg_matrix <- ISAnalytics::aggregate_values_by_key(
        x = matrix,
        association_file = af,
        value_cols = "Value",
        key = "SubjectID",
        group = ISAnalytics::mandatory_IS_vars()
    )
    
    data("known_cem_is")
    
    known_cem_is$integration_locus <-
        as.character(known_cem_is$integration_locus)
    
    # Filter shared known CEM IS
    filter_shared_cem_is <-
        dplyr::bind_rows(apply(known_cem_is, 1, function(x) {
            matrix_rows <- aggreg_matrix %>%
                dplyr::filter(chr == x["chr"] &
                                  integration_locus == x["integration_locus"] &
                                  strand == x["strand"])
            subs <- matrix_rows$SubjectID
            if ("CEM37" %in% subs) {
                if (length(subs) > 1) {
                    return(matrix_rows)
                }
            }
        }))
    
    if (nrow(filter_shared_cem_is) == 0) {
        stop("There are no IS shared from CEMs to other samples")
    }
    
    counts <- filter_shared_cem_is %>%
        dplyr::group_by(SubjectID) %>%
        dplyr::summarise(Sum = sum(Value_sum))
    
    tot_cem <- as.integer(counts %>%
                              dplyr::filter(SubjectID == "CEM37") %>%
                              dplyr::select(Sum))
    
    tot_others <- sum(counts %>%
                          dplyr::filter(SubjectID != "CEM37") %>%
                          dplyr::select(Sum))
    
    # Compute ratio
    ifelse(tot_others == 0, R <- NA, R <- tot_cem / tot_others)
    
    return(R)
    
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
#' @importFrom utils data
#'
#' @export
#' 
#' @examples 
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- shared_other_IS_ratio(association_file, integration_matrix)
#' R

shared_other_IS_ratio <- function(af, matrix) {
    
    # Aggregate matrix and af
    aggreg_matrix <- ISAnalytics::aggregate_values_by_key(
        x = matrix,
        association_file = af,
        value_cols = "Value",
        key = "SubjectID",
        group = ISAnalytics::mandatory_IS_vars()
    )
    
    data("known_cem_is")
    
    # Filter shared IS from the samples
    filter_other_is_full <- aggreg_matrix %>%
        dplyr::anti_join(known_cem_is,
                         by = c("chr", "integration_locus", "strand"))
    
    filter_other_is_full$integration_locus <-
        as.character(filter_other_is_full$integration_locus)
    
    filter_other_is_full_wide <-
        tidyr::pivot_wider(filter_other_is_full, names_from = SubjectID,
                           values_from = Value_sum, values_fill = 0)
    
    filter_other_is_wide <- filter_other_is_full_wide %>%
        dplyr::filter(CEM37 > 0) %>%
        dplyr::filter(dplyr::if_any(c(-chr, -integration_locus, 
                                      -strand, -CEM37),
                                    ~ . > 0))
    
    filter_other_is_long <-
        tidyr::pivot_longer(filter_other_is_wide,
                            cols = c(-chr, -integration_locus, -strand),
                            names_to = "SubjectID")
    
    filter_other_is <- filter_other_is_long %>%
        dplyr::filter(value != 0)
    
    `%notin%` <- Negate(`%in%`)
    if ("CEM37" %notin% filter_other_is$SubjectID) {
        stop("There are no IS shared from the samples to CEMs")
    }
    
    counts <- filter_other_is %>%
        dplyr::group_by(SubjectID) %>%
        dplyr::summarise(Sum = sum(value))
    
    tot_cem <- as.integer(counts %>%
                              dplyr::filter(SubjectID == "CEM37") %>%
                              dplyr::select(Sum))
    tot_others <- sum(counts %>%
                          dplyr::filter(SubjectID != "CEM37") %>%
                          dplyr::select(Sum))
    
    # Compute ratio
    ifelse(tot_others == 0, R <- NA, R <- tot_cem / tot_others)
    
    return(R)
    
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
#' @importFrom utils data
#'
#' @export
#' 
#' @examples 
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- shared_IS_ratio(association_file, integration_matrix)
#' head(R)

shared_IS_ratio <- function(af, matrix) {
    
    `%notin%` <- Negate(`%in%`)
    
    # Aggregate matrix and af
    aggreg_matrix <- ISAnalytics::aggregate_values_by_key(
        x = matrix,
        association_file = af,
        value_cols = "Value",
        key = "SubjectID",
        group = ISAnalytics::mandatory_IS_vars()
    )
    
    data("known_cem_is")
    
    known_cem_is$integration_locus <-
        as.character(known_cem_is$integration_locus)
    
    # Filter shared known CEM IS
    filter_shared_cem_is <- 
        dplyr::bind_rows(apply(known_cem_is, 1, function(x) {
            matrix_rows <- aggreg_matrix %>%
                dplyr::filter(chr == x["chr"] &
                                  integration_locus == x["integration_locus"] &
                                  strand == x["strand"])
            subs <- matrix_rows$SubjectID
            if ("CEM37" %in% subs) {
                if (length(subs) > 1) {
                    return(matrix_rows)
                }
            }
        }))
    
    known_cem_is$integration_locus <- 
        as.integer(known_cem_is$integration_locus)
    
    # Filter shared IS from samples
    filter_other_is_full <- aggreg_matrix %>%
        dplyr::anti_join(known_cem_is,
                         by = c("chr", "integration_locus", "strand"))
    
    filter_other_is_full$integration_locus <-
        as.character(filter_other_is_full$integration_locus)
    
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 &
        "CEM37" %notin% filter_other_is_full$SubjectID) {
        
        stop("There are no IS shared")
        
    }
    
    if (nrow(filter_shared_cem_is) > 0) {
        
        counts <- filter_shared_cem_is %>%
            dplyr::group_by(SubjectID) %>%
            dplyr::summarise(Sum = sum(Value_sum))
        
        row <- data.frame(SubjectID = "All samples",
                          Sum = sum(counts %>%
                                        dplyr::filter(SubjectID != "CEM37") %>%
                                        dplyr::select(Sum)))
        
        counts <- counts %>% dplyr::bind_rows(row)
        
        cem_count <- as.integer(counts %>%
                                    dplyr::filter(SubjectID == "CEM37") %>%
                                    dplyr::select(Sum))
        
        other_count <- counts %>% dplyr::filter(SubjectID != "CEM37")
        
        # Compute ratio for shared known CEM IS
        Ratios_known_CEM_IS <- 
            dplyr::bind_rows(apply(other_count, 1, function(x) {
                tot <- as.integer(x["Sum"])
                ifelse(tot == 0, R <- NA, R <- cem_count / tot)
                sample <- x["SubjectID"]
                data <- data.frame(sample, R)
                colnames(data) <- c("Sample", "Ratio")
                return(data)
            }))
        
        Ratios_known_CEM_IS$IS_Source <- "CEM"
        rownames(Ratios_known_CEM_IS) <- NULL
        
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    
    if ("CEM37" %in% filter_other_is_full$SubjectID) {
        
        filter_other_is_full_wide <-
            tidyr::pivot_wider(filter_other_is_full,
                               names_from = SubjectID,
                               values_from = Value_sum, 
                               values_fill = 0)
        
        filter_other_is_wide <- filter_other_is_full_wide %>%
            dplyr::filter(CEM37 > 0) %>%
            dplyr::filter(dplyr::if_any(c(-chr, -integration_locus, 
                                          -strand, -CEM37),
                                        ~ . > 0))
        
        filter_other_is_long <-
            tidyr::pivot_longer(filter_other_is_wide,
                                cols = c(-chr, -integration_locus, -strand),
                                names_to = "SubjectID")
        
        filter_other_is <- filter_other_is_long %>%
            dplyr::filter(value != 0)
        
        counts <- filter_other_is %>%
            dplyr::group_by(SubjectID) %>%
            dplyr::summarise(Sum = sum(value))
        
        row <- data.frame(SubjectID = "All samples",
                          Sum = sum(counts %>%
                                        dplyr::filter(SubjectID != "CEM37") %>%
                                        dplyr::select(Sum)))
        
        counts <- counts %>% dplyr::bind_rows(row)
        
        cem_count <- counts %>%
            dplyr::filter(SubjectID == "CEM37") %>%
            dplyr::select(Sum)
        
        other_count <- counts %>%
            dplyr::filter(SubjectID != "CEM37")
        
        # Compute ratio for shared IS from samples
        Ratios_other_IS <- dplyr::bind_rows(apply(other_count, 1, function(x) {
            tot <- as.integer(x["Sum"])
            ifelse(tot == 0, R <- NA, R <- cem_count / tot)
            sample <- x["SubjectID"]
            data <- data.frame(sample, R)
            colnames(data) <- c("Sample", "Ratio")
            return(data)
        }))
        
        Ratios_other_IS$IS_Source <- "Samples"
        rownames(Ratios_other_IS) <- NULL
        
    } else {
        warning("There are no IS shared from the samples to CEMs")
    }
    
    if (nrow(filter_shared_cem_is) > 0 &
        "CEM37" %in% filter_other_is_full$SubjectID) {
        
        Ratios_shared_IS <- Ratios_known_CEM_IS %>%
            dplyr::bind_rows(Ratios_other_IS)
        
        return(Ratios_shared_IS)
        
    } else if (nrow(filter_shared_cem_is) > 0 &
               "CEM37" %notin% filter_other_is_full$SubjectID) {
        
        return(Ratios_known_CEM_IS)
        
    } else if (nrow(filter_shared_cem_is) == 0 &
               "CEM37" %in% filter_other_is_full$SubjectID) {
        
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
#' @importFrom utils data
#'
#' @export
#' 
#' @examples 
#' data("association_file", package = "RISingVIS")
#' data("integration_matrix", package = "RISingVIS")
#' R <- shared_IS_ratio_byIS(association_file, integration_matrix)
#' head(R)

shared_IS_ratio_byIS <- function(af, matrix) {
    
    `%notin%` <- Negate(`%in%`)
    
    # Aggregate matrix and af
    aggreg_matrix <- ISAnalytics::aggregate_values_by_key(
        x = matrix,
        association_file = af,
        value_cols = "Value",
        key = "SubjectID",
        group = ISAnalytics::mandatory_IS_vars()
    )
    
    data("known_cem_is")
    
    known_cem_is$integration_locus <-
        as.character(known_cem_is$integration_locus)
    
    # Filter shared known CEM IS
    filter_shared_cem_is <- 
        dplyr::bind_rows(apply(known_cem_is, 1, function(x) {
            matrix_rows <- aggreg_matrix %>%
                dplyr::filter(chr == x["chr"] &
                                  integration_locus == x["integration_locus"] &
                                  strand == x["strand"])
            subs <- matrix_rows$SubjectID
            if ("CEM37" %in% subs) {
                if (length(subs) > 1) {
                    return(matrix_rows)
                }
            }
        }))
    
    known_cem_is$integration_locus <- 
        as.integer(known_cem_is$integration_locus)
    
    # Filter shared IS from samples
    filter_other_is_full <- aggreg_matrix %>%
        dplyr::anti_join(known_cem_is,
                         by = c("chr", "integration_locus", "strand"))
    
    filter_other_is_full$integration_locus <-
        as.character(filter_other_is_full$integration_locus)
    
    # Error if no IS is shared
    if (nrow(filter_shared_cem_is) == 0 &
        "CEM37" %notin% filter_other_is_full$SubjectID) {
        
        stop("There are no IS shared")
        
    }
    
    if (nrow(filter_shared_cem_is) > 0) {
        
        counts <- filter_shared_cem_is %>% 
            dplyr::filter(SubjectID != "CEM37") %>%
            dplyr::group_by(chr, integration_locus, strand) %>%
            dplyr::summarise(Value_sum = sum(Value_sum)) %>%
            dplyr::bind_cols(SubjectID = "All samples")
        
        filter_shared_cem_is <- filter_shared_cem_is %>% 
            dplyr::bind_rows(counts)
        
        shared_known_cem_IS_counts <- filter_shared_cem_is %>%
            tidyr::pivot_wider(names_from = SubjectID,
                               values_from = Value_sum, values_fill = 0)
        
        subs <- filter_shared_cem_is %>%
            dplyr::filter(SubjectID != "CEM37") %>%
            dplyr::pull(SubjectID) %>%
            unique()
        
        # Compute ratio for known CEM IS
        known_cem_is_ratios <-
            dplyr::bind_rows(apply(shared_known_cem_IS_counts, 1, function(x) {
                row <- as.list(x)
                row$integration_locus <- as.integer(row$integration_locus)
                rats <- dplyr::bind_rows(lapply(subs, function(y) {
                    tot <- row[y]
                    cem <- row["CEM37"]
                    ifelse(tot == 0, r <- NA,
                           r <- as.integer(cem[[1]]) / as.integer(tot[[1]]))
                    res <- data.frame(y, r)
                    return(res)
                }))
                data <- data.frame(row$chr, row$integration_locus, 
                                   row$strand, rats)
                colnames(data) <- c("chr", "integration_locus",
                                    "strand", "SubjectID", "Ratio")
                return(data)
            }))
        
        known_cem_is_ratios <- known_cem_is_ratios %>%
            tidyr::pivot_wider(names_from = SubjectID, values_from = Ratio)
        
        known_cem_is_ratios$IS_Source <- "CEM"
        
    } else {
        warning("There are no IS shared from CEMs to other samples")
    }
    
    if ("CEM37" %in% filter_other_is_full$SubjectID) {
        
        filter_other_is_full_wide <-
            tidyr::pivot_wider(filter_other_is_full,
                               names_from = SubjectID,
                               values_from = Value_sum, 
                               values_fill = 0)
        
        filter_other_is_wide <- filter_other_is_full_wide %>%
            dplyr::filter(CEM37 > 0) %>%
            dplyr::filter(dplyr::if_any(c(-chr, -integration_locus, 
                                          -strand, -CEM37),
                                        ~ . > 0))
        
        filter_other_is_long <-
            tidyr::pivot_longer(filter_other_is_wide,
                                cols = c(-chr, -integration_locus, -strand),
                                names_to = "SubjectID")
        
        filter_other_is <- filter_other_is_long %>%
            dplyr::filter(value != 0)
        
        names(filter_other_is)[names(filter_other_is) == 'value'] <- "Value_sum"
        
        counts <- filter_other_is %>% dplyr::filter(SubjectID != "CEM37") %>%
            dplyr::group_by(chr, integration_locus, strand) %>%
            dplyr::summarise(Value_sum = sum(Value_sum)) %>%
            dplyr::bind_cols(SubjectID = "All samples")
        
        filter_other_is <- filter_other_is %>% dplyr::bind_rows(counts)
        
        shared_other_IS_counts <- filter_other_is %>%
            tidyr::pivot_wider(names_from = SubjectID,
                               values_from = Value_sum, values_fill = 0)
        
        subs <- filter_other_is %>%
            dplyr::filter(SubjectID != "CEM37") %>%
            dplyr::pull(SubjectID) %>%
            unique()
        
        # Compute ratio for shared IS from samples
        other_is_ratios <-
            dplyr::bind_rows(apply(shared_other_IS_counts, 1, function(x) {
                row <- as.list(x)
                row$integration_locus <- as.integer(row$integration_locus)
                rats <- dplyr::bind_rows(lapply(subs, function(y) {
                    tot <- row[y]
                    cem <- row["CEM37"]
                    ifelse(tot == 0, r <- NA,
                           r <- as.integer(cem[[1]]) / as.integer(tot[[1]]))
                    res <- data.frame(y, r)
                    return(res)
                }))
                data <- data.frame(row$chr, row$integration_locus, 
                                   row$strand, rats)
                colnames(data) <- c("chr", "integration_locus",
                                    "strand", "SubjectID", "Ratio")
                return(data)
            }))
        
        other_is_ratios <- other_is_ratios %>%
            tidyr::pivot_wider(names_from = SubjectID, values_from = Ratio)
        
        other_is_ratios$IS_Source <- "Samples"
        
    } else {
        warning("There are no IS shared from the samples to CEMs")
    }
    
    if (nrow(filter_shared_cem_is) > 0 &
        "CEM37" %in% filter_other_is_full$SubjectID) {
        
        shared_IS_ratio <- known_cem_is_ratios %>%
            dplyr::bind_rows(other_is_ratios)
        
        return(shared_IS_ratio)
        
    } else if (nrow(filter_shared_cem_is) > 0 &
               "CEM37" %notin% filter_other_is_full$SubjectID) {
        
        return(known_cem_is_ratios)
        
    } else if (nrow(filter_shared_cem_is) == 0 &
               "CEM37" %in% filter_other_is_full$SubjectID) {
        
        return(other_is_ratios)
        
    }
    
}
