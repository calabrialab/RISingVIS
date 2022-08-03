#-----------------------------------------------------------------------------#
# Internal functions
#-----------------------------------------------------------------------------#
# All functions in this file are not exported. 

# find_shared_CEM_IS
# Returns a dataframe containing the shared known CEM IS.
#' @importFrom magrittr `%>%`

find_shared_CEM_IS <- function(matrix) {
    # Retrieve known CEM integration sites
    known_cem_is <- known_CEM_IS()
    known_cem_is$integration_locus <-
        as.character(known_cem_is$integration_locus)
    # Filter shared known CEM IS
    filter_shared_cem_is <- 
        dplyr::bind_rows(apply(known_cem_is, 1, function(x) {
            matrix_rows <- IS_replicate_count %>%
                dplyr::filter(.data$chr == x["chr"] &
                                  .data$integration_locus == 
                                  x["integration_locus"] &
                                  .data$strand == x["strand"])
            subs <- matrix_rows$SubjectID
            print(subs)
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

find_shared_other_IS <- function(matrix) {
    # Retrieve known CEM integration sites
    known_cem_is <- known_CEM_IS()
    known_cem_is$integration_locus <- 
        as.integer(known_cem_is$integration_locus)
    # Filter shared IS from samples
    filter_other_is_full <- matrix %>%
        dplyr::anti_join(known_cem_is,
                         by = c("chr", "integration_locus", "strand"))
    filter_other_is_full$integration_locus <-
        as.character(filter_other_is_full$integration_locus)
    if ("CEM37" %in% filter_other_is_full$SubjectID) {
        filter_other_is_full_wide <-
            tidyr::pivot_wider(filter_other_is_full,
                               names_from = SubjectID,
                               values_from = value, 
                               values_fill = 0)
        filter_other_is_wide <- filter_other_is_full_wide %>%
            dplyr::filter(.data$CEM37 > 0) %>%
            dplyr::ungroup() %>%
            dplyr::filter(dplyr::if_any(c(-.data$chr, -.data$integration_locus, 
                                          -.data$strand, -.data$CEM37),
                                        ~ . > 0))
        filter_other_is_long <-
            tidyr::pivot_longer(filter_other_is_wide,
                                cols = c(-chr, -integration_locus, -strand),
                                names_to = "SubjectID")
        filter_other_is <- filter_other_is_long %>%
            dplyr::filter(.data$value != 0)
        return(filter_other_is)
    } else {
        res <- tibble::tibble()
        return(res)
    }
}


# compute_seq_count_ratio_merged
# Returns the value of the ratio for all IS computed against all samples
#' @importFrom magrittr `%>%`

compute_seq_count_ratio_merged <- function(filter_shared) {
    counts <- filter_shared %>%
        dplyr::group_by(.data$SubjectID) %>%
        dplyr::summarise(Sum = sum(.data$value))
    tot_cem <- as.integer(counts %>%
                              dplyr::filter(.data$SubjectID == "CEM37") %>%
                              dplyr::select(.data$Sum))
    tot_others <- sum(counts %>%
                          dplyr::filter(.data$SubjectID != "CEM37") %>%
                          dplyr::select(.data$Sum))
    # Compute ratio
    R <- ifelse(tot_others == 0, NA, tot_cem / tot_others)
    return(R)
}

# compute_seq_count_ratio
# Returns a df with the ratio computed against each sample
#' @importFrom magrittr `%>%`

compute_seq_count_ratio <- function(filter_shared_is) {
    counts <- filter_shared_is %>%
        dplyr::group_by(.data$SubjectID) %>%
        dplyr::summarise(Sum = sum(.data$value))
    row <- data.frame(SubjectID = "All samples", Sum = 
                          sum(counts %>%
                                  dplyr::filter(.data$SubjectID != "CEM37") %>%
                                  dplyr::select(.data$Sum)))
    counts <- counts %>% dplyr::bind_rows(row)
    cem_count <- as.integer(counts %>%
                                dplyr::filter(.data$SubjectID == "CEM37") %>%
                                dplyr::select(.data$Sum))
    other_count <- counts %>% dplyr::filter(.data$SubjectID != "CEM37")
    # Compute ratio for shared known CEM IS
    ratios <- 
        dplyr::bind_rows(apply(other_count, 1, function(x) {
            tot <- as.integer(x["Sum"])
            R <- ifelse(tot == 0, NA, cem_count / tot)
            sample <- x["SubjectID"]
            data <- data.frame(sample, R)
            colnames(data) <- c("Sample", "Ratio")
            return(data)
        }))
    return(ratios)
}


# compute_seq_count_ratio_byIS
# Returns a df with the ratio computed for each IS against each sample
#' @importFrom magrittr `%>%`

compute_seq_count_ratio_byIS <- function(filter_shared) {
    counts <- filter_shared %>% 
        dplyr::filter(.data$SubjectID != "CEM37") %>%
        dplyr::group_by(.data$chr, .data$integration_locus, 
                        .data$strand) %>%
        dplyr::summarise(value = sum(.data$value)) %>%
        dplyr::bind_cols(SubjectID = "All samples")
    filter_shared <- filter_shared %>% 
        dplyr::bind_rows(counts)
    shared_counts <- filter_shared %>%
        tidyr::pivot_wider(names_from = SubjectID,
                           values_from = value, values_fill = 0)
    subjects <- filter_shared %>%
        dplyr::filter(.data$SubjectID != "CEM37") %>%
        dplyr::pull(.data$SubjectID) %>%
        unique()
    ratios <- dplyr::bind_rows(apply(shared_counts, 1, function(x) {
        row <- as.list(x)
        row$integration_locus <- as.integer(row$integration_locus)
        rats <- dplyr::bind_rows(lapply(subjects, function(y) {
            tot <- row[y]
            cem <- row["CEM37"]
            R <- ifelse(tot == 0, NA, 
                        as.integer(cem[[1]]) / as.integer(tot[[1]]))
            res <- data.frame(y, R)
            return(res)
        }))
        data <- data.frame(row$chr, row$integration_locus, 
                           row$strand, rats)
        colnames(data) <- c("chr", "integration_locus",
                            "strand", "SubjectID", "Ratio")
        return(data)
    }))
    ratios <- ratios %>%
        tidyr::pivot_wider(names_from = SubjectID, values_from = Ratio)
    return(ratios)
}


# compute_rep_count_ratio
# Returns a df with the ratio for all IS computed against each sample
#' @importFrom magrittr `%>%`

compute_rep_count_ratio <- function(filter_shared) {
    replicates_count <-
        as.data.frame(colSums(filter_shared %>%
                                  dplyr::ungroup() %>%
                                  dplyr::select(-.data$chr,
                                                -.data$integration_locus, 
                                                -.data$strand)))
    colnames(replicates_count) <- "Count"
    replicates_count <- replicates_count %>%
        dplyr::bind_cols(SubjectID = rownames(replicates_count))
    cem_total <- replicates_count["CEM37",] %>%
        dplyr::select(.data$Count)
    samples_replicate_count <- replicates_count %>%
        dplyr::filter(.data$SubjectID != "CEM37")
    samples_total <- sum(samples_replicate_count %>%
                             dplyr::select(.data$Count))
    row <- data.frame(Count = samples_total, SubjectID = "All samples")
    samples_replicate_count <- samples_replicate_count %>%
        dplyr::bind_rows(row)
    # Compute ratio for known CEM IS
    ratios <- dplyr::bind_rows(apply(samples_replicate_count, 1, function(x) {
        row <- as.list(x)
        row$Count <- as.integer(row$Count)
        tot <- row$Count
        R <- ifelse(tot == 0, NA, cem_total / tot)
        sample <- row$SubjectID
        data <- data.frame(sample, R)
        colnames(data) <- c("Sample", "Ratio")
        return(data)
    }))
    return(ratios)
}


# compute_rep_count_ratio_byIS
# Returns a df with the ratio for each IS computed against each sample
#' @importFrom magrittr `%>%`

compute_rep_count_ratio_byIS <- function(filter_shared) {
    filter_shared <- filter_shared %>%
        tidyr::pivot_longer(cols = c(-chr, -integration_locus, -strand),
                            names_to = "SubjectID")
    all_counts <- filter_shared %>%
        dplyr::filter(.data$SubjectID != "CEM37") %>%
        dplyr::group_by(.data$chr, .data$integration_locus, 
                        .data$strand) %>%
        dplyr::summarise(value = sum(.data$value)) %>%
        dplyr::bind_cols(SubjectID = "All samples")
    filter_shared <- filter_shared %>%
        dplyr::bind_rows(all_counts)
    counts <- filter_shared %>%
        tidyr::pivot_wider(names_from = SubjectID,
                           values_from = value, values_fill = 0)
    subjects <- filter_shared %>%
        dplyr::filter(.data$SubjectID != "CEM37") %>%
        dplyr::pull(.data$SubjectID) %>%
        unique()
    # Compute ratio for shared known CEM IS
    ratios <- dplyr::bind_rows(apply(counts, 1, function(x) {
        row <- as.list(x)
        row$integration_locus <- as.integer(row$integration_locus)
        rats <- dplyr::bind_rows(lapply(subjects, function(y) {
            tot <- row[y]
            cem <- row["CEM37"]
            R <- ifelse(tot == 0, NA, 
                        as.integer(cem[[1]]) / as.integer(tot[[1]]))
            res <- data.frame(y, R)
            return(res)
        }))
        data <- data.frame(row$chr, row$integration_locus, 
                           row$strand, rats)
        colnames(data) <- c("chr", "integration_locus",
                            "strand", "SubjectID", "Ratio")
        return(data)
    }))
    ratios <- ratios %>%
        tidyr::pivot_wider(names_from = SubjectID, values_from = Ratio)
    return(ratios)
}

