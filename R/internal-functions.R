#------------------------------------------------------------------------------#
# Internal functions
#------------------------------------------------------------------------------#
# All functions in this file are not exported.

# Utilities --------------------------------------------------------------------

# Checks if all the supplied keys are in the column names of df
# if blocking is set to FALSE does not produce an error (useful for shiny)
# but simply returns FALSE
.key_in_table <- function(df, key, blocking) {
    all_keys_present <- all(key %in% colnames(df))
    if (all_keys_present) {
        return(TRUE)
    }
    if (blocking) {
        error_msg <- c(
            "Keys not found in data frame columns",
            x = paste(
                "The following supplied keys where not found:",
                paste0(key[!key %in% colnames(df)], collapse = ", ")
            )
        )
        rlang::abort(error_msg)
    } else {
        return(FALSE)
    }
}

# Assembles a unique list of control lines based on input
# control_lines can contain both strings - for default control lines, and
# objects of type ControlLine
.get_control_lines <- function(control_lines, blocking = TRUE) {
    def_names <- control_lines[purrr::map_lgl(
        control_lines,
        ~ is.character(.x)
    )]
    def_obj <- control_lines[purrr::map_lgl(
        control_lines,
        ~ is(.x, "ControlLine")
    )]
    def_obj_names <- purrr::map_chr(def_obj, ~ .x$name)
    names(def_obj) <- def_obj_names

    selected_defaults <- list()
    if (!purrr::is_empty(def_names)) {
        selected_defaults <- default_cell_lines(def_names)
    }
    all_cl <- append(selected_defaults, def_obj)
    if (purrr::is_empty(all_cl)) {
        if (blocking) {
            error_msg <- c("No control cell line supplied",
                x = paste(
                    "Control cell lines list was either empty",
                    "or defaults were not found"
                )
            )
            rlang::abort(error_msg, class = "control_lines_absent")
        } else {
            return(list(result = NULL, error = TRUE))
        }
    }
    # Removing duplicates
    cl_pruned <- list()
    for (nm in names(all_cl)) {
        if (!nm %in% names(cl_pruned)) {
            cl_pruned[[nm]] <- all_cl[[nm]]
        }
    }
    return(list(result = cl_pruned, error = FALSE))
}

# Error message displayed for suggestion packages that are not installed
# but required by the called function
.missing_pkg_error <- function(pkg, lib = "CRAN") {
    if (!is.null(names(pkg))) {
        pkgs_str <- paste0('"', names(pkg), '"', collapse = ", ")
        from_cran <- pkg[pkg == "CRAN"]
        from_cran <- if (length(from_cran) > 0) {
            paste0(
                "`install.packages(",
                paste0('"', names(from_cran), '"', collapse = ","),
                ")`"
            )
        } else {
            NULL
        }

        from_bioc <- pkg[pkg == "BIOC"]
        from_bioc <- if (length(from_bioc) > 0) {
            paste0(
                "`BiocManager::install(",
                paste0('"', names(from_bioc), '"', collapse = ","),
                ")`"
            )
        } else {
            NULL
        }
        inst_sugg <- paste0("To install: ", from_cran, ", ", from_bioc)
    } else {
        pkgs_str <- paste0('"', pkg, '"', collapse = ", ")
        inst_sugg <- if (lib == "CRAN") {
            paste0("To install: `install.packages(", pkgs_str, ")`")
        } else if (lib == "BIOC") {
            paste0("To install: `BiocManager::install(", pkgs_str, ")`")
        }
    }

    c("Missing package(s)",
        x = paste0(
            "Package(s) ", pkgs_str,
            " are required for this functionality."
        ),
        i = inst_sugg
    )
}


# Checks if packages for parallel processing are installed
.check_parallel_packages <- function() {
    if (getOption("RISingVIS.parallel_processing", default = TRUE) == TRUE) {
        required_parallel_pkgs <- list(
            BiocParallel = "BIOC",
            doFuture = "CRAN",
            future = "CRAN",
            foreach = "CRAN"
        )
        pkgs_present <- purrr::map_lgl(
            names(required_parallel_pkgs),
            ~ requireNamespace(.x, quietly = TRUE)
        )
        if (any(pkgs_present == FALSE)) {
            missing_pkgs <- required_parallel_pkgs[!pkgs_present]
            options(RISingVIS.parallel_processing = FALSE)
            info_msg <- c(.missing_pkg_error(missing_pkgs),
                i = paste(
                    "Packages for parallel computation are not available,",
                    "switching to default sequential computation",
                    "(certain operations might be slower).",
                    "To re-activate the functionality, install the",
                    "packages and set",
                    "`options(RISingVIS.parallel_processing = TRUE)`"
                )
            )
            rlang::inform(info_msg)
        }
    }
}


# Establishes whether a map job can be executed in parallel (multi-threading)
# or needs to be executed sequentially and calls appropriate functions.
#
# Functions in "fun_to_apply" should always have at least 2 args:
# - the data
# - a `progress` arg
# The list of args must contain everything needed by the function except for
# the first argument (passed in data_list) and progress (internally managed)
#' @importFrom purrr map map_lgl
.execute_map_job <- function(data_list,
                             fun_to_apply,
                             fun_args,
                             stop_on_error,
                             max_workers,
                             progrs = NULL) {
    # Set up progressor if package available
    prog <- if (rlang::is_installed("progressr") & is.null(progrs)) {
        progressr::progressor(steps = length(data_list))
    } else {
        progrs
    }
    .check_parallel_packages()
    if (length(data_list) == 1) {
        max_workers <- 1
    } else {
        if (getOption("RISingVIS.parallel_processing", default = TRUE) == TRUE) {
            # Manage workers
            bioc_workers <- if (.Platform$OS.type == "windows") {
                BiocParallel::snowWorkers()
            } else {
                BiocParallel::multicoreWorkers()
            }
            if (is.null(max_workers)) {
                max_workers <- min(length(data_list), bioc_workers)
            } else {
                max_workers <- min(length(data_list), max_workers)
            }
        }
    }

    if (getOption("RISingVIS.parallel_processing", default = TRUE) == TRUE &
        (!is.null(max_workers) && max_workers > 1)) {
        # Set up parallel workers
        old_be <- doFuture::registerDoFuture()
        old_plan <- future::plan(future::multisession, workers = max_workers)
        on.exit(
            {
                future::plan(old_plan)
                foreach::setDoPar(
                    fun = old_be$fun,
                    data = old_be$data, info = old_be$info
                )
            },
            add = TRUE
        )
        p <- BiocParallel::DoparParam()
        if (!stop_on_error) {
            fun_to_apply <- purrr::safely(fun_to_apply)
        }

        # Execute
        arg_list <- append(fun_args, list(
            X = data_list,
            FUN = fun_to_apply,
            BPPARAM = p,
            progress = prog
        ))
        results <- rlang::exec(
            BiocParallel::bplapply,
            !!!arg_list
        )
        if (!stop_on_error) {
            return(list(
                res = purrr::map(results, ~ .x$result),
                mode = "par", err = purrr::map(results, ~ .x$error)
            ))
        }
        return(list(res = results, mode = "par"))
    }

    # Sequential
    arg_list <- append(
        list(
            .x = data_list,
            .f = fun_to_apply,
            progress = prog
        ),
        fun_args
    )
    if (stop_on_error == TRUE) {
        results <- rlang::exec(purrr::map, !!!arg_list)
        return(list(res = results, mode = "seq"))
    } else {
        results <- rlang::exec(purrr::safely(purrr::map), !!!arg_list)
        errs <- results$error
        results <- results$result
        return(list(res = results, mode = "seq", err = errs))
    }
}


# Internals for sharing --------------------------------------------------------

# Checks that needed arguments are not empty
.check_sharing_param_values <- function(
    key,
    pool_col,
    seqCount_col,
    replicate_col,
    control_lines,
    check_pool) {
    err_msg <- function(arg) {
        c("Argument is empty",
            x = paste(
                "Argument", paste0("'", arg, "'"),
                "is empty but is required"
            )
        )
    }
    if (purrr::is_empty(key) || key == "") {
        rlang::abort(err_msg("key"))
    }
    if (purrr::is_empty(seqCount_col) || seqCount_col == "") {
        rlang::abort(err_msg("seqCount_col"))
    }
    if (purrr::is_empty(replicate_col) || replicate_col == "") {
        rlang::abort(err_msg("replicate_col"))
    }
    if (purrr::is_empty(control_lines) || control_lines == "") {
        rlang::abort(err_msg("control_lines"))
    }
    if (check_pool && (purrr::is_empty(pool_col) || pool_col == "")) {
        rlang::abort(err_msg("pool_col"))
    }
}

# Checks correspondence between input data frames and set of keys
.verify_df_keys <- function(dfs, key) {
    # Key can be either a character vector or a list of character vectors
    if (!(is.character(key) ||
        is.list(key) && all(purrr::map_lgl(key, ~ is.character(.x))))) {
        err_msg <- c("Key format incorrect",
            x = paste(
                "Provided key must be either a character vector",
                "of column names or a list of character vectors"
            )
        )
        rlang::abort(err_msg, class = "key_format_error")
    }
    # dfs must be a list of data frames
    if (!all(purrr::map_lgl(dfs, ~ is.data.frame(.x)))) {
        err_msg <- c("Inputs must be in data frame format",
            x = paste("All inputs must be provided as data frames")
        )
        rlang::abort(err_msg, class = "inputs_not_df_error")
    }
    # Lengths must correspond
    if (is.list(key) & !(length(key) == 1 || length(key) == length(dfs))) {
        err_msg <- c("Incorrect number of keys",
            x = paste(
                "Provided keys can be either one or a number",
                "corresponding to the number of data frames",
                "in input"
            ),
            i = paste(
                "If a single key is provided with multiple",
                "data frames, the key will be applied to",
                "all inputs"
            )
        )
        rlang::abort(err_msg, class = "key_length_error")
    }
    # Check keys are present in the corresponding inputs
    key_checks <- if (length(key) == 1) {
        purrr::map_lgl(dfs, ~ all(key %in% colnames(.x)))
    } else {
        purrr::map2_lgl(dfs, key, ~ all(.y %in% colnames(.x)))
    }
    if (!all(key_checks)) {
        err_msg <- c("Key not found in inputs",
            x = paste(
                "Key wasn't found in inputs",
                paste0(which(!key_checks), collapse = ", ")
            )
        )
        rlang::abort(err_msg, class = "key_not_found_error")
    }
}

# Returns a lookup table for a given dataframe of ISs and a given key
# NOTE: replicate_col is meant to be CompleteAmplification id or the equivalent
# pcr_id_col tag (not replicate number)
.obtain_lookup_tbl <- function(
    df, key,
    seqCount_col,
    replicate_col,
    barcode_mux_col) {
    replicate_totals <- df |>
        tidyr::unite(col = "group_id", dplyr::all_of(key), remove = FALSE) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(
            c("group_id")
        ))) |>
        dplyr::summarise(
            tot_rep = dplyr::n_distinct(.data[[replicate_col]]),
            .groups = "drop"
        )
    lookup <- df |>
        tidyr::unite(col = "group_id", dplyr::all_of(key), remove = FALSE) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(
            c("group_id", key, ISAnalytics::mandatory_IS_vars())
        )))

    lookup <- if (!is.null(barcode_mux_col)) {
        lookup |>
            dplyr::summarise(
                seq_count = sum(.data[[seqCount_col]], na.rm = TRUE),
                abs_rep_count = length(unique(.data[[replicate_col]])),
                raw_reads = sum(.data[[barcode_mux_col]], na.rm = TRUE),
                .groups = "drop"
            )
    } else {
        lookup |>
            dplyr::summarise(
                seq_count = sum(.data[[seqCount_col]], na.rm = TRUE),
                abs_rep_count = length(unique(.data[[replicate_col]])),
                .groups = "drop"
            )
    }
    lookup <- lookup |>
        dplyr::left_join(replicate_totals, by = "group_id") |>
        dplyr::mutate(
            rep_count = .data$abs_rep_count / .data$tot_rep
        )
    return(lookup)
}

# Internal for computing group labels for sharing - returns only minimal
# combinations (no duplicates)
# lookups - a list of lookup tables
# self_comb - if TRUE keeps the combinations of g1 g2 ... with same label
.get_label_combinations <- function(lookups, self_comb) {
    group_ids <- purrr::map(lookups, ~ .x$group_id) |>
        purrr::set_names(paste("g", seq_along(lookups), sep = ""))
    full_combos <- if (length(group_ids) > 1) {
        tidyr::crossing(!!!group_ids)
    } else {
        group_ids[["g2"]] <- group_ids[["g1"]]
        tidyr::crossing(!!!group_ids)
    }
    # Count the distinct lables on a single row
    final_combos <- full_combos |>
        dplyr::mutate(un = purrr::pmap_int(
            full_combos,
            function(...) {
                row <- list(...)
                length(unique(row))
            }
        ))
    # Discard all labels that have a count different than 1 or the number
    # of groups
    final_combos <- final_combos |>
        dplyr::filter(.data$un == 1 | .data$un == length(group_ids))
    if (!self_comb) {
        # If self combination is FALSE, keep only combinations with un equal to
        # number of groups
        final_combos <- final_combos |>
            dplyr::filter(.data$un > 1)
    }
    final_combos <- final_combos |>
        dplyr::mutate(
            labs = purrr::pmap_chr(
                final_combos |>
                    dplyr::select(-.data$un),
                function(...) {
                    row <- as.character(list(...))
                    paste0(sort(row), collapse = "_")
                }
            )
        ) |>
        dplyr::group_by(.data$labs) |>
        dplyr::group_modify(~ .x[1, ]) |>
        dplyr::ungroup()
    return(final_combos)
}

# Needs to be called in a mapping function on each row of the label combinations
# table
# - lookups is a LIST of lookup tables (one for each df in input)
.find_in_common <- function(..., lookups) {
    row <- rlang::list2(...)
    process_row <- function(content, name) {
        if (!stringr::str_starts(name, "g")) {
            return(NULL)
        }
        g_number <- as.numeric(stringr::str_remove(name, "g"))
        tot_rep <- (lookups[[g_number]] |>
            dplyr::filter(.data$group_id == content))$tot_rep[1]
        selection <- lookups[[g_number]] |>
            dplyr::filter(.data$group_id == content) |>
            dplyr::select(-.data$group_id, -.data$tot_rep) |>
            dplyr::rename_with(
                .cols = !ISAnalytics::mandatory_IS_vars(),
                .fn = ~ paste0(.x, "_", name)
            )
        return(list(selection, tot_rep))
    }
    data_selection <- purrr::map2(row, names(row), process_row)
    data_selection <- purrr::discard(data_selection, is.null)
    in_common <- purrr::reduce(
        data_selection,
        ~ dplyr::inner_join(.x[[1]], .y[[1]],
            by = ISAnalytics::mandatory_IS_vars()
        )
    )
    new_row <- row[stringr::str_detect(names(row), "^g[0-9]+")]
    new_row[["shared"]] <- nrow(in_common)
    counts_g <- purrr::map_int(data_selection, ~ nrow(.x[[1]]))
    names(counts_g) <- paste0("count_", names(counts_g))
    new_row <- append(new_row, counts_g)
    union_selections <- purrr::reduce(
        data_selection,
        ~ .x[[1]] |>
            dplyr::select(dplyr::all_of(ISAnalytics::mandatory_IS_vars())) |>
            dplyr::union(
                .y[[1]] |>
                    dplyr::select(dplyr::all_of(ISAnalytics::mandatory_IS_vars()))
            )
    )
    new_row[["count_union"]] <- nrow(union_selections)
    tot_reps_g <- purrr::map_int(data_selection, ~ .x[[2]])
    names(tot_reps_g) <- paste0("tot_rep_", names(tot_reps_g))
    new_row <- append(new_row, tot_reps_g)
    new_row[["is_details"]] <- list(in_common)
    new_row <- tibble::tibble_row(
        !!!new_row
    )
    return(new_row)
}

## Internal, to use on each row of the combinations df.
## Expands the row with all its permutations keeping same absolute shared is
## and counts -> this is for having data to plot in heatmap
## - ... : row passed as a list
## - g_names: names of the groups (g1, g2...)
.sh_row_permut <- function(..., g_names) {
    og_row <- list(...)
    ids <- unlist(og_row[g_names])
    # If row of all equal elements no need for permutations
    if (length(unique(ids)) == 1) {
        og_row[["is_details"]] <- list(og_row[["is_details"]])
        return(tibble::as_tibble(og_row))
    }
    # If elements are different
    shared_is <- og_row$shared
    count_union <- og_row$count_union
    is_details <- og_row$is_details

    perm <- gtools::permutations(
        n = length(g_names),
        r = length(g_names),
        v = g_names,
        set = TRUE,
        repeats.allowed = FALSE
    )
    colnames(perm) <- g_names
    perm <- tibble::as_tibble(perm)
    cols_to_substitute <- names(og_row)[!names(og_row) %in% c(
        "shared",
        "count_union",
        "is_details",
        g_names
    )]
    rearrange <- function(...) {
        row <- list(...)
        obtain_mapping <- function(col) {
            ref_group <- stringr::str_extract(col, "g[1-9]+")
            general_col <- stringr::str_remove(col, "g[0-9]+")
            return(paste0(general_col, row[[ref_group]]))
        }
        cols_mappings <- purrr::map(cols_to_substitute, obtain_mapping) |>
            purrr::set_names(cols_to_substitute)
        tibble::as_tibble_row(cols_mappings)
    }
    perm <- perm |>
        dplyr::mutate(
            !!!purrr::pmap_df(perm, rearrange)
        )

    perm[["shared"]] <- shared_is
    perm[["count_union"]] <- count_union
    # Remap is details
    details_cols <- colnames(is_details)
    rearrange_df <- function(...) {
        row <- list(...)
        obtain_mapping <- function(col) {
            ref_group <- stringr::str_extract(col, "g[1-9]+")
            if (is.na(ref_group)) {
                return(rep(col, nrow(is_details)))
            }
            general_col <- stringr::str_remove(col, "g[0-9]+")
            return(rep(paste0(general_col, row[[ref_group]]), nrow(is_details)))
        }
        cols_mappings <- purrr::map(details_cols, obtain_mapping) |>
            purrr::set_names(details_cols)
        tibble::as_tibble(cols_mappings)
    }
    new_is_details <- purrr::pmap(perm, rearrange_df)
    # Substitute placeholders
    cols_to_substitute <- c(cols_to_substitute, g_names)
    perm <- perm |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::all_of(cols_to_substitute),
                .fns = ~ unlist(purrr::map(.x, ~ og_row[[.x]])),
                .names = "{.col}"
            )
        )

    df_mapper <- function(col, index) {
        is_details[[col]][index]
    }
    new_is_details <- purrr::map(
        new_is_details,
        ~ .x |>
            dplyr::mutate(
                dplyr::across(
                    .cols = dplyr::everything(),
                    .fns = ~ unlist(purrr::imap(.x, df_mapper)),
                    .names = "{.col}"
                )
            )
    )
    perm[["is_details"]] <- new_is_details
    return(perm |> dplyr::select(
        dplyr::all_of(names(og_row))
    ))
}

# For each IS in the sharing table determines if it comes from controls or
# not where applicable
.find_is_source <- function(df, control_lines, lookups) {
    # Get all known control ISs
    knowns <- control_lines |>
        purrr::map(~ .x$known_iss)
    knowns <- purrr::map2(
        knowns, names(knowns),
        ~ .x |>
            dplyr::mutate(
                IS_source = paste0("control|", .y)
            )
    ) |>
        purrr::reduce(~ dplyr::bind_rows(.x, .y) |> dplyr::distinct())

    non_control_is <- purrr::reduce(lookups, dplyr::union) |>
        dplyr::anti_join(knowns, by = ISAnalytics::mandatory_IS_vars()) |>
        dplyr::mutate(IS_source = dplyr::if_else(
            condition = !.data$group_id %in% names(control_lines),
            true = "sample",
            false = NA_character_
        )) |>
        dplyr::filter(!is.na(.data$IS_source)) |>
        dplyr::distinct(
            dplyr::across(
                dplyr::all_of(c(
                    ISAnalytics::mandatory_IS_vars(),
                    "IS_source"
                ))
            )
        )

    cols_to_keep <- colnames(df)[colnames(df) != "is_details"]
    processed_df <- df |>
        tidyr::unnest(cols = "is_details") |>
        dplyr::left_join(
            knowns |>
                dplyr::bind_rows(non_control_is),
            by = ISAnalytics::mandatory_IS_vars()
        ) |>
        tidyr::replace_na(list(IS_source = "unknown")) |>
        tidyr::nest(.by = cols_to_keep, .key = "is_details")

    return(processed_df)
}

# The function computes user defined stats on sc, raw reads and rep
# - df: input data frame with sharing df structure (output of .find_in_common
# or .sh_row_permut mapping)
# *_fns: named lists of purrr style lambdas to apply respectively to
# sc, raw reads and replicates
.compute_shared_stats <- function(df, sc_fns, raw_reads_fns, rep_fns) {
    df_with_stats <- df |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::matches("^count_(g[1-9]+|union)"),
                .fns = ~ shared / .x,
                .names = "on_{.col}"
            ),
            .before = "is_details"
        ) |>
        dplyr::rename_with(
            .cols = dplyr::starts_with("on_count"),
            .fn = ~ stringr::str_remove(.x, "_count")
        )
    if (!is.null(sc_fns)) {
        sc_fns <- purrr::map(
            sc_fns, ~ rlang::as_function(.x, env = rlang::current_env())
        )
        df_with_stats <- df_with_stats |>
            dplyr::mutate(
                stats = purrr::map(
                    .data$is_details,
                    ~ dplyr::summarise(
                        .x,
                        dplyr::across(
                            .cols = dplyr::all_of(dplyr::starts_with("seq_count")),
                            .fns = sc_fns,
                            .names = "{.col}_{.fn}"
                        )
                    )
                )
            ) |>
            tidyr::unnest(cols = "stats")
    }
    if (!is.null(raw_reads_fns)) {
        raw_reads_fns <- purrr::map(
            raw_reads_fns, ~ rlang::as_function(.x, env = rlang::current_env())
        )
        df_with_stats <- df_with_stats |>
            dplyr::mutate(
                stats = purrr::map(
                    .data$is_details,
                    ~ dplyr::summarise(
                        .x,
                        dplyr::across(
                            .cols = dplyr::all_of(dplyr::starts_with("raw_reads")),
                            .fns = raw_reads_fns,
                            .names = "{.col}_{.fn}"
                        )
                    )
                )
            ) |>
            tidyr::unnest(cols = "stats")
    }
    if (!is.null(rep_fns)) {
        rep_fns <- purrr::map(
            rep_fns, ~ rlang::as_function(.x, env = rlang::current_env())
        )
        df_with_stats <- df_with_stats |>
            dplyr::mutate(
                stats = purrr::map(
                    .data$is_details,
                    ~ dplyr::summarise(
                        .x,
                        dplyr::across(
                            .cols = dplyr::all_of(dplyr::starts_with("rep_count")),
                            .fns = rep_fns,
                            .names = "{.col}_{.fn}"
                        )
                    )
                )
            ) |>
            tidyr::unnest(cols = "stats")
    }
    return(df_with_stats)
}


.chuck_shared_by_source <- function(df,
                                    control_lines) {
    filter_adjust <- function(..., control = NULL) {
        row <- rlang::list2(...)
        source <- if (!is.null(control)) {
            paste0("control|", control)
        } else {
            "sample"
        }
        filtered_is <- row$is_details |>
            dplyr::filter(.data$IS_source == source)
        new_row <- tibble::tibble_row(
            !!!row[names(row) != "is_details"],
            is_details = list(filtered_is)
        )
        new_row$shared <- nrow(filtered_is)
        return(new_row)
    }

    # ISs coming from controls
    controls_sharing <- purrr::map(names(control_lines), ~ {
        cl_name <- .x
        purrr::pmap_df(df, filter_adjust, control = cl_name)
    }) |>
        purrr::set_names(names(control_lines))

    # ISs coming from samples
    samples_sharing <- purrr::pmap_df(df, filter_adjust)

    # Unknown ISs (only in controls)
    unknown_is <- df |>
        tidyr::unnest(cols = "is_details") |>
        dplyr::filter(.data$IS_source == "unknown") |>
        tidyr::nest(
            .by = colnames(df)[colnames(df) != "is_details"],
            .key = "is_details"
        ) |>
        dplyr::mutate(shared = purrr::map_int(.data$is_details, nrow))

    return(list(
        controls = controls_sharing, samples = samples_sharing,
        unknowns = unknown_is
    ))
}

# Calculates sharing relative to a single pool or df
# - dfs is a list that either contains multiple dfs relative to a single pool
# (or overall), or contains a single df relative to the single pool (or
# overall)
.calculate_sharing <- function(dfs, key,
                               seqCount_col,
                               replicate_col,
                               barcode_mux_col,
                               self_comb,
                               minimal,
                               control_lines,
                               progress) {
    # 1 - Get lookup tables
    lookups <- purrr::map(
        dfs,
        ~ .obtain_lookup_tbl(
            .x,
            key = key, seqCount_col = seqCount_col,
            replicate_col = replicate_col,
            barcode_mux_col = barcode_mux_col
        )
    )
    if (!is.null(progress)) {
        progress()
    }
    # 2 - Get label combinations
    label_combos <- .get_label_combinations(
        lookups = lookups,
        self_comb = self_comb
    )
    if (!is.null(progress)) {
        progress()
    }
    # 3 - Get common iss
    if (length(lookups) == 1) {
        lookups[[2]] <- lookups[[1]]
    }
    common_tbl <- purrr::pmap_df(label_combos, .find_in_common,
        lookups = lookups
    )
    if (!is.null(progress)) {
        progress()
    }
    # 4 - Assign source
    gn <- colnames(common_tbl)[stringr::str_detect(
        colnames(common_tbl),
        pattern = "^g[1-9]+$"
    )]
    common_tbl <- .find_is_source(common_tbl,
        control_lines = control_lines,
        lookups = lookups
    )
    if (!is.null(progress)) {
        progress()
    }

    # 5 - eventually permute
    if (!minimal) {
        common_tbl <- purrr::pmap_df(common_tbl,
            .f = .sh_row_permut,
            g_names = gn
        )
    }
    if (!is.null(progress)) {
        progress()
    }
    # 6 - divide
    shared_by_source <- .chuck_shared_by_source(common_tbl,
        control_lines = control_lines
    )
    if (!is.null(progress)) {
        progress()
    }
    return(list(
        overall = common_tbl,
        by_source = shared_by_source
    ))
}









# get_is_vars
# Returns a list of the names of the columns retrieved by ISAnalytics function
# mandatory_IS_vars() that contains only chr, locus and strand
#' @importFrom rlang .data

get_is_vars <- function() {
    is_vars <- ISAnalytics::mandatory_IS_vars(include_types = TRUE)
    chr <- as.character(is_vars |>
        dplyr::filter(.data$tag == "chromosome") |>
        dplyr::select(.data$names))
    int_locus <- as.character(is_vars |>
        dplyr::filter(.data$tag == "locus") |>
        dplyr::select(.data$names))
    strand <- as.character(is_vars |>
        dplyr::filter(.data$tag == "is_strand") |>
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
        ctrl_split <- stringr::str_split(ctrl_line, field_sep) |>
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
                        TRUE, FALSE
                        )
                    }
                )
                ifelse(sum(unlist(single_ok)) ==
                    length(names(fields[[ctrl_line]])),
                TRUE, FALSE
                )
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
#' @importFrom rlang .data
find_shared_IS <- function(
    matrix, af, is_vars, subject_col, amp_col,
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
            shared_known_is <- filter_shared_known_is(
                matrix, af, is_vars,
                subject_col, amp_col,
                ctrl_line, known_is
            )
            return(shared_known_is)
        } else if (type == "other") {
            shared_other_is <- filter_shared_other_is(
                matrix, af, is_vars,
                subject_col, amp_col,
                value_col, ctrl_line,
                ctrl_line, field_sep,
                known_is
            )
            return(shared_other_is)
        }
    } else {
        if (amp_col %in% colnames(matrix)) {
            matrix <- matrix |>
                dplyr::left_join(
                    af |>
                        dplyr::select(amp_col, subject_col),
                    by = amp_col
                )
        }
        fields <- ctrl_unfold(subject_col, ctrl, field_sep)
        ctrl_names <- names(fields)
        res <- purrr::map(ctrl_names, function(x) {
            known_is <- ctrl[[x]]
            ctrl_line <- fields[[x]]
            if (length(ctrl_names) > 1) {
                other_ctrl <- ctrl_names[ctrl_names != x]
                other_ctrl <- fields[[other_ctrl]]
                filtered_matrix <- matrix |>
                    dplyr::anti_join(as.data.frame(other_ctrl),
                        by = subject_col
                    )
            } else {
                filtered_matrix <- matrix
            }
            if (type == "control") {
                shared_known_is <- filter_shared_known_is(
                    filtered_matrix, af,
                    is_vars, subject_col,
                    amp_col, ctrl_line,
                    known_is
                )
                return(shared_known_is)
            } else if (type == "other") {
                shared_other_is <- filter_shared_other_is(
                    filtered_matrix, af,
                    is_vars, subject_col,
                    amp_col, value_col, x,
                    ctrl_line, field_sep,
                    known_is
                )
                return(shared_other_is)
            }
        })
        names(res) <- ctrl_names
        return(res)
    }
}


# find_shared_known_IS
# Returns a dataframe containing the shared known CEM IS
#' @importFrom rlang .data
filter_shared_known_is <- function(
    matrix, af, is_vars, subject_col,
    amp_col, ctrl_line, known_is) {
    known_is$integration_locus <- as.character(known_is$integration_locus)
    dplyr::bind_rows(apply(known_is, 1, function(x) {
        matrix_rows <- matrix |>
            dplyr::filter(.data[[is_vars[1]]] == x["chr"] &
                .data[[is_vars[2]]] ==
                    x["integration_locus"] &
                .data[[is_vars[3]]] == x["strand"])
        if (any(ctrl_line == "CEM37")) {
            if (amp_col %in% colnames(matrix)) {
                matrix_rows <- matrix_rows |>
                    dplyr::left_join(
                        af |>
                            dplyr::select(amp_col, subject_col),
                        by = amp_col
                    )
            }
            subs <- matrix_rows[[subject_col]] |> unique()
            if (ctrl_line %in% subs) {
                if (length(subs) >= 2) {
                    return(matrix_rows)
                }
            }
        } else {
            subs <- matrix_rows |> dplyr::select(subject_col)
            if (!is.null(dplyr::inner_join(as.data.frame(ctrl_line),
                subs,
                by = subject_col
            ))) {
                if (dim(subs)[1] >= 2) {
                    return(matrix_rows)
                }
            }
        }
    }))
}



# find_shared_other_IS
# Returns a dataframe containing the shared IS from samples
#' @importFrom rlang .data
filter_shared_other_is <- function(
    matrix, af, is_vars, subject_col,
    amp_col, value_col, ctrl_name, ctrl_line,
    field_sep, known_is) {
    known_is$integration_locus <-
        as.integer(known_is$integration_locus)
    matrix$integration_locus <-
        as.integer(matrix$integration_locus)
    colnames(known_is) <- c(is_vars, "GeneName", "GeneStrand")
    filter_other_is_full <- matrix |>
        dplyr::anti_join(known_is, by = is_vars)
    filter_other_is_full[[is_vars[2]]] <-
        as.character(filter_other_is_full[[is_vars[2]]])
    join_ok <- FALSE
    if (ctrl_name == "CEM37") {
        if (amp_col %in% colnames(matrix)) {
            filter_other_is_full <- filter_other_is_full |>
                dplyr::left_join(
                    af |>
                        dplyr::select(
                            dplyr::all_of(c(amp_col, subject_col))
                        ),
                    by = amp_col
                )
        }
        subs <- filter_other_is_full[[subject_col]] |> unique()
        if (ctrl_line %in% subs) {
            join_ok <- TRUE
        }
    } else {
        subs <- filter_other_is_full |> dplyr::select(subject_col)
        if (!is.null(dplyr::inner_join(as.data.frame(ctrl_line),
            subs,
            by = subject_col
        ))) {
            join_ok <- TRUE
        }
    }
    if (join_ok) {
        if (amp_col %in% colnames(matrix)) {
            filter_other_is_full <- filter_other_is_full |>
                dplyr::select(-dplyr::all_of(subject_col))
            filter_other_is_full_wide <-
                tidyr::pivot_wider(filter_other_is_full,
                    names_from = dplyr::all_of(amp_col),
                    names_sep = field_sep,
                    values_from = dplyr::all_of(value_col),
                    values_fill = 0
                )
            if (length(subject_col) > 1) {
                ctrl_amp <- af |>
                    tidyr::unite("Sample", subject_col, sep = field_sep) |>
                    dplyr::filter(.data[["Sample"]] == ctrl_name) |>
                    dplyr::filter(.data[[amp_col]] %in%
                        colnames(filter_other_is_full_wide)) |>
                    dplyr::pull(amp_col)
            } else {
                ctrl_amp <- af |>
                    dplyr::filter(.data[[subject_col]] == ctrl_name) |>
                    dplyr::filter(.data[[amp_col]] %in%
                        colnames(filter_other_is_full_wide)) |>
                    dplyr::pull(amp_col)
            }
            filter_other_is_wide <- filter_other_is_full_wide |>
                dplyr::filter(dplyr::if_any(
                    dplyr::any_of(ctrl_amp),
                    ~ . > 0
                )) |>
                dplyr::filter(dplyr::if_any(
                    c(
                        -dplyr::all_of(is_vars),
                        -dplyr::all_of(ctrl_amp)
                    ),
                    ~ . > 0
                ))
        } else {
            filter_other_is_full_wide <-
                tidyr::pivot_wider(filter_other_is_full,
                    names_from = dplyr::all_of(subject_col),
                    names_sep = field_sep,
                    values_from = dplyr::all_of(value_col),
                    values_fill = 0
                )
            filter_other_is_wide <- filter_other_is_full_wide |>
                dplyr::filter(.data[[ctrl_name]] > 0) |>
                dplyr::ungroup() |>
                dplyr::filter(dplyr::if_any(
                    c(
                        -.data[[is_vars[1]]],
                        -.data[[is_vars[2]]],
                        -.data[[is_vars[3]]],
                        -.data[[ctrl_name]]
                    ),
                    ~ . > 0
                ))
        }
        if (length(subject_col) > 1) {
            if (amp_col %in% colnames(matrix)) {
                filter_other_is_long <-
                    tidyr::pivot_longer(filter_other_is_wide,
                        cols = c(
                            -is_vars[1], -is_vars[2],
                            -is_vars[3]
                        ),
                        names_to = amp_col,
                        values_to = value_col
                    )
            } else {
                filter_other_is_long <-
                    tidyr::pivot_longer(filter_other_is_wide,
                        cols = c(
                            -is_vars[1], -is_vars[2],
                            -is_vars[3]
                        ),
                        names_to = subject_col,
                        names_sep = field_sep,
                        values_to = value_col
                    )
            }
        } else {
            if (amp_col %in% colnames(matrix)) {
                filter_other_is_long <-
                    tidyr::pivot_longer(filter_other_is_wide,
                        cols = c(
                            -is_vars[1], -is_vars[2],
                            -is_vars[3]
                        ),
                        names_to = amp_col,
                        values_to = value_col
                    )
                filter_other_is_long <- filter_other_is_long |>
                    dplyr::left_join(
                        af |>
                            dplyr::select(amp_col, subject_col),
                        by = amp_col
                    )
            } else {
                filter_other_is_long <-
                    tidyr::pivot_longer(filter_other_is_wide,
                        cols = c(
                            -is_vars[1], -is_vars[2],
                            -is_vars[3]
                        ),
                        names_to = subject_col,
                        values_to = value_col
                    )
            }
        }
        filter_other_is <- filter_other_is_long |>
            dplyr::filter(.data[[value_col]] != 0)
        return(filter_other_is)
    } else {
        res <- tibble::tibble()
        return(res)
    }
}

# compute_n_rep
# Computes the total number of replicates for each sample from
# the association file
compute_n_rep <- function(af, subject_col, field_sep, ctrl) {
    `%notin%` <- Negate(`%in%`)
    n_rep <- af |>
        dplyr::group_by(dplyr::across(dplyr::all_of(subject_col))) |>
        dplyr::summarise(n = dplyr::n())
    if (length(subject_col) > 1) {
        n_rep <- n_rep |>
            tidyr::unite("Sample", subject_col,
                sep = field_sep, remove = FALSE
            )
        s_col <- "Sample"
    } else {
        s_col <- subject_col
    }
    if (is.list(ctrl)) {
        tot_c <- n_rep |>
            dplyr::filter(.data[[s_col]] %in% names(ctrl)) |>
            dplyr::pull(n) |>
            sum()
        tot_s <- n_rep |>
            dplyr::filter(.data[[s_col]] %notin% names(ctrl)) |>
            dplyr::pull(n) |>
            sum()
    } else {
        tot_c <- n_rep |>
            dplyr::filter(.data[[s_col]] == ctrl) |>
            dplyr::pull(n)
        tot_s <- n_rep |>
            dplyr::filter(.data[[s_col]] != ctrl) |>
            dplyr::pull(n) |>
            sum()
    }
    row_c <- tibble::tibble_row("All_Controls", tot_c)
    names(row_c) <- c(s_col, "n")
    row_s <- tibble::tibble_row("All_Samples", tot_s)
    names(row_s) <- c(s_col, "n")
    n_rep <- n_rep |>
        dplyr::bind_rows(row_c, row_s)
    return(n_rep)
}

# compute_rep_count
# Computes the replicate count by sample for the given table of shared IS
#' @importFrom rlang .data
compute_rep_count <- function(
    current_table, af, subject_col,
    amp_col, value_col, x) {
    if (dim(current_table)[1] == 0) {
        return(tibble::tibble())
    }
    if (length(subject_col) > 1) {
        subs <- af |>
            dplyr::select(subject_col) |>
            unique()
        rep_count <- dplyr::bind_rows(apply(subs, 1, function(y) {
            row <- as.list(y)
            mod_af <- dplyr::inner_join(af, as.data.frame(row), by = subject_col)
            reps <- mod_af |>
                dplyr::pull(amp_col)
            rep_vals <- purrr::map(reps, function(z) {
                if (z %in% current_table[[amp_col]]) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            })
            sub_count <- data.frame(row, sum(unlist(rep_vals)))
            colnames(sub_count) <- c(names(row), value_col)
            return(sub_count)
        }))
    } else {
        subs <- af |>
            dplyr::pull(dplyr::all_of(subject_col)) |>
            unique()
        rep_count <- purrr::map(subs, function(y) {
            reps <- af |>
                dplyr::filter(.data[[subject_col]] == y) |>
                dplyr::pull(amp_col)
            rep_vals <- purrr::map(reps, function(z) {
                if (z %in% current_table[[amp_col]]) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            })
            sub_count <- tibble::tibble("Sub" = y, "Value" = sum(unlist(rep_vals)))
            sub_count <- sub_count |>
                dplyr::rename(!!subject_col := "Sub")
            sub_count <- sub_count |>
                dplyr::rename(!!value_col := "Value")
            return(sub_count)
        })
        rep_count <- rep_count |>
            purrr::reduce(dplyr::bind_rows)
    }
    return(rep_count)
}


# compute_ratio
# Returns a df with the ratio computed against each sample,
# it can be done by sample or by IS
#' @importFrom rlang .data
compute_ratio <- function(
    filter_shared_is, is_vars, subject_col,
    value_col, ctrl, field_sep, type, n_rep = NULL) {
    `%notin%` <- Negate(`%in%`)
    if (type %notin% c("by sample", "by IS")) {
        stop()
    }
    if (!is.list(ctrl)) {
        ctrl_names <- ctrl
        ctrl_line <- ctrl
        if (type == "by sample") {
            counts <- compute_counts(
                filter_shared_is, subject_col,
                value_col, ctrl_names, field_sep
            )
            res <- internal_compute_ratio(
                counts, subject_col,
                ctrl_line, n_rep
            )
        } else if (type == "by IS") {
            counts <- compute_counts_byIS(
                filter_shared_is, is_vars,
                subject_col, value_col,
                ctrl_names, field_sep
            )
            res <- internal_compute_ratio_byIS(
                counts, is_vars, subject_col,
                value_col, ctrl_line, n_rep
            )
        }
        return(res)
    } else {
        ctrl_names <- names(ctrl)
        res <- purrr::map(ctrl_names, function(x) {
            ctrl_line <- x
            shared_is <- filter_shared_is[[ctrl_line]]
            if (type == "by sample") {
                other_ctrl <- ctrl_names[ctrl_names != x]
                shared_is <- shared_is |>
                    tidyr::unite("Sample", subject_col,
                        sep = field_sep, remove = FALSE
                    ) |>
                    dplyr::filter(.data[["Sample"]] != dplyr::all_of(other_ctrl)) |>
                    dplyr::select(-.data[["Sample"]])
            }
            if (dim(shared_is)[1] == 0) {
                ret <- tibble::tibble()
                return(ret)
            } else {
                if (type == "by sample") {
                    counts <- compute_counts(
                        shared_is, subject_col,
                        value_col, ctrl_names, field_sep
                    )
                    ret <- internal_compute_ratio(
                        counts, subject_col,
                        ctrl_line, n_rep
                    )
                    return(ret)
                }
                if (type == "by IS") {
                    counts <- compute_counts_byIS(
                        shared_is, is_vars,
                        subject_col, value_col,
                        ctrl_names, field_sep
                    )
                    ret <- internal_compute_ratio_byIS(
                        counts, is_vars,
                        subject_col, value_col,
                        ctrl_line, n_rep
                    )
                    return(ret)
                }
            }
        })
        names(res) <- ctrl_names
        if (type == "by sample") {
            res <- Filter(function(x) dim(x)[1] > 0, res)
            res <- res |>
                purrr::reduce(dplyr::full_join, by = "Sample")
        }
        if (type == "by IS") {
            res <- Filter(Negate(is.null), res)
            res <- res |>
                purrr::reduce(dplyr::full_join,
                    by = c(
                        is_vars[1], is_vars[2],
                        is_vars[3], "Sample"
                    )
                )
        }
        n_ctrl <- length(ctrl_names)
        if (n_ctrl > 1) {
            shared_is <- do.call(rbind, filter_shared_is)
            shared_is <- dplyr::distinct(shared_is)
            if (type == "by sample") {
                counts <- compute_counts(
                    shared_is, subject_col,
                    value_col, ctrl_names, field_sep
                )
                row <- data.frame(Sub = "All_Controls", Sum = sum(
                    counts |>
                        dplyr::filter(.data[["Sample"]] %in%
                            ctrl_names) |>
                        dplyr::pull(.data$Sum)
                ))
                names(row)[names(row) == "Sub"] <- "Sample"
                counts <- counts |> dplyr::bind_rows(row)
                counts <- counts |> dplyr::filter(.data[["Sample"]]
                %notin% ctrl_names)
                ratio <- internal_compute_ratio(
                    counts, subject_col,
                    "All_Controls", n_rep
                )
                res <- res |> dplyr::full_join(ratio, by = "Sample")
            }
            if (type == "by IS") {
                counts <- compute_counts_byIS(
                    shared_is, is_vars, subject_col,
                    value_col, ctrl_names, field_sep
                )
                row <- counts |>
                    dplyr::filter(.data[["Sample"]] %in% ctrl_names) |>
                    dplyr::group_by(
                        .data[[is_vars[1]]], .data[[is_vars[2]]],
                        .data[[is_vars[[3]]]]
                    ) |>
                    dplyr::summarise(value = sum(.data[[value_col]])) |>
                    dplyr::bind_cols("Sample" = "All_Controls")
                names(row)[names(row) == "value"] <- value_col
                counts <- counts |>
                    dplyr::bind_rows(row)
                counts <- counts |>
                    dplyr::filter(.data[["Sample"]] %notin% ctrl_names)
                ratio <- internal_compute_ratio_byIS(
                    counts, is_vars,
                    subject_col, value_col,
                    "All_Controls", n_rep
                )
                res <- res |> dplyr::full_join(ratio,
                    by = c(
                        is_vars[1], is_vars[2],
                        is_vars[3], "Sample"
                    )
                )
            }
        }
    }
    return(res)
}

# compute_counts
# Returns a df with the replicates count computed for each sample
#' @importFrom rlang .data
compute_counts <- function(
    filter_shared_is, subject_col,
    value_col, ctrl_names, field_sep) {
    counts <- filter_shared_is |>
        dplyr::group_by(dplyr::across(dplyr::all_of(subject_col))) |>
        dplyr::summarise(Sum = sum(.data[[value_col]]))
    `%notin%` <- Negate(`%in%`)
    counts <- counts |> tidyr::unite("Sample", subject_col,
        sep = field_sep, remove = FALSE
    )
    row <- data.frame(
        Sub = "All_Samples", Sum =
            sum(counts |>
                dplyr::filter(.data[["Sample"]] %notin%
                    ctrl_names) |>
                dplyr::pull(.data$Sum))
    )
    names(row)[names(row) == "Sub"] <- "Sample"
    counts <- counts |> dplyr::bind_rows(row)
    return(counts)
}

# internal_compute_ratio
# Returns a df with the computed ratio for each sample
#' @importFrom rlang .data
internal_compute_ratio <- function(counts, subject_col, ctrl_line, n_rep) {
    ctrl_count <- as.integer(counts |>
        dplyr::filter(.data[["Sample"]] ==
            ctrl_line) |>
        dplyr::pull(.data$Sum))
    other_count <- counts |>
        dplyr::filter(.data[["Sample"]] != ctrl_line)
    # Compute ratio
    ratios <-
        dplyr::bind_rows(apply(other_count, 1, function(x) {
            tot <- as.integer(x["Sum"])
            sample <- as.character(x["Sample"])
            if (is.null(n_rep)) {
                R <- ifelse(tot == 0, NA, ctrl_count / tot)
                data <- data.frame(sample, ctrl_line, R, ctrl_count, tot)
                colnames(data) <- c(
                    "Sample", "Control", "Ratio",
                    paste0("Count(", ctrl_line, ")"),
                    paste0("Count(Sample-vs-", ctrl_line, ")")
                )
                return(data)
            } else {
                s_col <- ifelse(length(subject_col) > 1,
                    "Sample", subject_col
                )
                rep_ctrl <- n_rep |>
                    dplyr::filter(.data[[s_col]] == ctrl_line) |>
                    dplyr::pull("n")
                rep_sample <- n_rep |>
                    dplyr::filter(.data[[s_col]] == sample) |>
                    dplyr::pull("n")
                R <- ifelse(tot == 0, NA,
                    (ctrl_count / rep_ctrl) / (tot / rep_sample)
                )
                data <- data.frame(
                    sample, ctrl_line, R, ctrl_count,
                    rep_ctrl, tot, rep_sample
                )
                colnames(data) <- c(
                    "Sample", "Control", "Ratio",
                    paste0("Count(", ctrl_line, ")"),
                    paste0("TotRep(", ctrl_line, ")"),
                    paste0("Count(Sample-vs-", ctrl_line, ")"),
                    paste0(
                        "TotRep(Sample-vs-",
                        ctrl_line, ")"
                    )
                )
                return(data)
            }
        }))
    ratios <- ratios |> tidyr::pivot_wider(
        names_from = "Control",
        values_from = "Ratio",
        values_fill = NA
    )
    ratios <- ratios |> dplyr::rename(
        !!paste0("Ratio_", ctrl_line) := ctrl_line
    )
    return(ratios)
}

# compute_counts_byIS
# Returns a df with the replicates count computed for each IS
#' @importFrom rlang .data
compute_counts_byIS <- function(
    filter_shared, is_vars, subject_col,
    value_col, ctrl_names, field_sep) {
    `%notin%` <- Negate(`%in%`)
    filter_shared <- filter_shared |> tidyr::unite("Sample",
        subject_col,
        sep = field_sep
    )
    counts <- filter_shared |>
        dplyr::filter(.data[["Sample"]] %notin% ctrl_names) |>
        dplyr::group_by(
            .data[[is_vars[1]]], .data[[is_vars[2]]],
            .data[[is_vars[[3]]]]
        ) |>
        dplyr::summarise(value = sum(.data[[value_col]])) |>
        dplyr::bind_cols("Sample" = "All_Samples")
    names(counts)[names(counts) == "value"] <- value_col
    filtered_counts <- filter_shared |>
        dplyr::bind_rows(counts)
    return(filtered_counts)
}

# internal_compute_ratio_byIS
# Returns a df with the computed ratio for each IS
#' @importFrom rlang .data
internal_compute_ratio_byIS <- function(
    counts, is_vars,
    subject_col, value_col,
    ctrl_line, n_rep) {
    shared_counts <- counts |>
        tidyr::pivot_wider(
            names_from = dplyr::all_of("Sample"),
            values_from = dplyr::all_of(value_col),
            values_fill = 0
        )
    subjects <- counts |>
        dplyr::filter(.data[["Sample"]] != ctrl_line) |>
        dplyr::pull(.data[["Sample"]]) |>
        unique()
    ratios <- dplyr::bind_rows(apply(shared_counts, 1, function(x) {
        row <- as.list(x)
        row[[is_vars[2]]] <- as.integer(row[[is_vars[2]]])
        rats <- dplyr::bind_rows(lapply(subjects, function(y) {
            tot <- as.integer(row[[y]])
            ctrl_count <- as.integer(row[[ctrl_line]])
            if (is.null(n_rep)) {
                R <- ifelse(tot == 0, NA,
                    as.integer(ctrl_count[[1]]) / as.integer(tot[[1]])
                )
                res <- data.frame(
                    y, as.integer(ctrl_count[[1]]),
                    as.integer(tot[[1]]), R
                )
                return(res)
            } else {
                s_col <- ifelse(length(subject_col) > 1,
                    "Sample", subject_col
                )
                rep_ctrl <- n_rep |>
                    dplyr::filter(.data[[s_col]] == ctrl_line) |>
                    dplyr::pull("n")
                rep_sample <- n_rep |>
                    dplyr::filter(.data[[s_col]] == y) |>
                    dplyr::pull("n")
                R <- ifelse(tot == 0, NA,
                    (ctrl_count / rep_ctrl) / (tot / rep_sample)
                )
                res <- data.frame(
                    y, ctrl_line, R, ctrl_count,
                    rep_ctrl, tot, rep_sample
                )
                return(res)
            }
        }))
        data <- data.frame(
            row[[is_vars[1]]], row[[is_vars[2]]],
            row[[is_vars[3]]], rats
        )
        if (is.null(n_rep)) {
            colnames(data) <- c(
                "chr", "integration_locus",
                "strand", "Sample",
                paste0("Count(", ctrl_line, ")"),
                paste0("Count(Sample-vs-", ctrl_line, ")"),
                "Ratio"
            )
        } else {
            colnames(data) <- c(
                "chr", "integration_locus",
                "strand", "Sample", "Control", "Ratio",
                paste0("Count(", ctrl_line, ")"),
                paste0("TotRep(", ctrl_line, ")"),
                paste0("Count(Sample-vs-", ctrl_line, ")"),
                paste0(
                    "TotRep(Sample-vs-",
                    ctrl_line, ")"
                )
            )
        }
        data <- data |> dplyr::rename(
            !!paste0("Ratio_", ctrl_line) := "Ratio"
        )
        return(data)
    }))
    rownames(ratios) <- NULL
    return(ratios)
}


# no_IS_shared()
# Returns the output in the case of no integration sites shared among samples
#' @importFrom rlang .data
no_IS_shared <- function(ctrl, af, subject_col, field_sep) {
    `%notin%` <- Negate(`%in%`)
    af <- af |> tidyr::unite("Sample",
        subject_col,
        sep = field_sep
    )
    subjects <- af |>
        dplyr::filter(.data[["Sample"]] %notin%
            c(names(ctrl), "CEM37")) |>
        dplyr::pull(.data[["Sample"]]) |>
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
                r <- r |> dplyr::rename(
                    !!paste0("Ratio_", x) := "Ctrl"
                )
                return(r)
            }))
            return(res)
        }))
        Ratio <- Ratio |> unique()
    }
    return(Ratio)
}
