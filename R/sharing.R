# Functions for sharing of IS --------------------------------------------------

default_shared_stats <- function(
    type = c("seq_count", "raw_reads", "rep_count")) {
  type <- rlang::arg_match(type)
  sc_fns <- list(
    sum = ~ ifelse(length(.x) > 0, sum(.x, na.rm = TRUE), 0),
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ stats::sd(.x, na.rm = TRUE),
    max = ~ ifelse(length(.x) > 0, max(.x, na.rm = TRUE), 0),
    min = ~ ifelse(length(.x) > 0, min(.x, na.rm = TRUE), 0)
  )
  raw_reads_fns <- sc_fns
  rep_fns <- list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ stats::sd(.x, na.rm = TRUE),
    max = ~ ifelse(length(.x) > 0, max(.x, na.rm = TRUE), 0)
  )
  if (type == "seq_count") {
    return(sc_fns)
  }
  if (type == "raw_reads") {
    return(raw_reads_fns)
  }
  return(rep_fns)
}

# also multipool
get_sharing <- function(...,
                        key = get_isa_tag("subject"),
                        pool_col = get_isa_tag("pool_id"),
                        seqCount_col = "seqCount",
                        replicate_col = get_isa_tag("pcr_repl_id"),
                        control_lines = list("CEM37"),
                        by_pool = TRUE,
                        self_comparison = FALSE,
                        minimal = TRUE,
                        seq_count_stats = default_shared_stats("seq_count"),
                        raw_reads_stats = default_shared_stats("raw_reads"),
                        rep_count_stats = default_shared_stats("rep_count"),
                        max_workers = NULL
                        ) {
  # Checks ----
  .check_sharing_param_values(key,
                              pool_col,
                              seqCount_col,
                              replicate_col,
                              control_lines,
                              check_pool = by_pool)
  dfs <- rlang::list2(...)
  .verify_df_keys(dfs, key)
  required_cols <- if (by_pool) {
    c(pool_col, seqCount_col, replicate_col)
  } else {
    c(seqCount_col, replicate_col)
  }
  purrr::walk(dfs, ~ .key_in_table(.x, required_cols, TRUE))
  barcode_mux_col <- if (all(purrr::map_lgl(dfs, ~ "BARCODE_MUX" %in%
                                            colnames(.x)))) {
    "BARCODE_MUX"
  } else {
    NULL
  }
  # TODO - implement non blocking behavior for shiny
  control_lines <- .get_control_lines(control_lines, blocking = TRUE)
  control_lines <- control_lines$result

  # Separate if needed
  sep_and_name <- function(df) {
    sep <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(pool_col))) |>
      dplyr::group_split()
    sep_names <- purrr::map_chr(sep, ~ .x[[pool_col]][1])
    sep <- sep |>
      purrr::set_names(sep_names)
    return(sep)
  }
  dfs_to_process <- if (by_pool) {
    tmp_dfs <- purrr::map(
      dfs,
      sep_and_name
    )
    distinct_pools <- purrr::map(tmp_dfs, ~ names(.x)) |>
      purrr::reduce(union)
    extract_pool_dfs <- function(pool_name) {
      purrr::map(tmp_dfs, ~ .x[[pool_name]])
    }
    purrr::map(distinct_pools, extract_pool_dfs) |>
      purrr::set_names(distinct_pools)
  } else {
    dfs
  }

  dfs_names <- names(dfs_to_process)
  # Distribute work if needed
  internal_steps <- 6
  progrs <- if (rlang::is_installed("progressr")) {
    progressr::progressor(steps = length(dfs_to_process)*internal_steps)
  } else {
    NULL
  }
  sharing_dfs <- .execute_map_job(
    data_list = dfs_to_process,
    fun_to_apply = .calculate_sharing,
    fun_args = list( key = key,
                     seqCount_col = seqCount_col,
                     replicate_col = replicate_col,
                     barcode_mux_col = barcode_mux_col,
                     self_comb = self_comparison,
                     minimal = minimal,
                     control_lines = control_lines),
    stop_on_error = FALSE,
    max_workers = max_workers,
    progrs = progrs
  )

  # TODO: error handling
  # if (!all(is.null(sharing_dfs$err))) {
  #   errors_msg <- purrr::flatten_chr(sharing_dfs$err)
  #   names(errors_msg) <- "x"
  #   errors_msg <- c(
  #     "Errors occurred during computation",
  #     errors_msg
  #   )
  #   rlang::inform(errors_msg)
  # }
  sharing <- sharing_dfs$res
  sharing_stats <- purrr::map(sharing, ~ {
    overall <- .compute_shared_stats(
      .x$overall, sc_fns = seq_count_stats, raw_reads_fns = raw_reads_stats,
      rep_fns = rep_count_stats
    )
    controls <- purrr::map(.x$by_source$controls, .compute_shared_stats,
                           sc_fns = seq_count_stats,
                           raw_reads_fns = raw_reads_stats,
                           rep_fns = rep_count_stats)
    samples <- .compute_shared_stats(
      .x$by_source$samples, sc_fns = seq_count_stats,
      raw_reads_fns = raw_reads_stats,
      rep_fns = rep_count_stats
    )
    unknowns <- .compute_shared_stats(
      .x$by_source$unknowns, sc_fns = seq_count_stats,
      raw_reads_fns = raw_reads_stats,
      rep_fns = rep_count_stats
    )
    return(list(overall = overall, controls = controls, samples = samples,
                unknowns = unknowns))
  })
  return(sharing_stats)
}
