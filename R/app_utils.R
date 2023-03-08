# Utilities and internal functions ---------------------------------------------

## Utils and vars --------------------------------------------------------------
id_list <- function() {
  list(
    side_graph = "svg-graph-mini",
    data_import = list(
      section_id = "data-import-section",
      inputs = list(
        next_btn = "next-btn"
      ),
      isa_opt_section = list(
        section_id = "isa-opt-subsection",
        inputs = list(
          toggle1 = "toggle-default-import",
          json_file_input = "js-filepath",
          upload_config_btn = "upload-config-btn"
        ),
        outputs = list(
          import_status = "import-status",
          confirm_choice = "confirm-choice",
          status_container = "status-container"
        )
      ),
      metadata_section = list(
        section_id = "metadata-subsection",
        inputs = list(
          file_input = "metadata-file",
          switch = "switch-fs-align",
          root_container = "root-container",
          root_dir = "root-dir-btn",
          project = "proj-text-in",
          separator = "sep-select",
          dates_format = "dates-format-select",
          sample_id = "sample-id-select",
          control_line = "control-cell-line-select",
          import_btn = "import-btn",
          import_spinner = "import-btn-spinner",
          details_btn = "details-btn",
          details_collapse = "details-collapse"
        ),
        outputs = list(
          root_display = "root-display",
          import_status = "import-status",
          status_container = "status-container",
          details_content = "details-content",
          checks_tbl_1 = "fs-align-tbl",
          checks_tbl_2 = "iss-summary-tbl",
          checks_tbl_3 = "iss-missing-tbl",
          checks_tbl_4 = "control-cl-tbl"
        )
      ),
      data_section = list(
        section_id = "data-subsection",
        inputs = list(
          import_mode = "import-mode-switch",
          annotation = "matrix-annot-check",
          separator = "sep-select",
          workers = "max-workers",
          adv_op_btn = "adv-op-btn",
          adv_op_collapse = "adv-op-collapse",
          file_patterns = "patterns-select",
          matching_op = "match-op-select",
          tidy_switch = "tidy-switch",
          files = "data-files",
          import_btn = "import-btn",
          spinner_btn = "spinner-btn",
          details_btn = "details-btn",
          details_collapse = "details-collapse"
        ),
        outputs = list(
          import_status_cont = "import-status-container",
          import_status = "import-status",
          details_content = "details-content",
          checks_tbl_1 = "files-found-tbl",
          checks_tbl_2 = "files-imp-tbl",
          checks_tbl_3 = "missing-data-tbl"
        )
      )
    ),
    recalibration = list(
      section_id = "recalibr-section",
      inputs = list(
        threshold = "threshold",
        is_tags = "is-tags",
        criteria = "criteria",
        workers = "workers",
        rec_map_switch = "rec-map-switch",
        rec_map_container = "rec-map-container",
        rec_map_path = "rec-map-path",
        rec_btn = "recalibrate-btn",
        skip_btn = "skip-btn",
        next_btn = "next-btn"
      ),
      outputs = list(
        rec_map_display = "rec-map-display",
        info_panel = "info-panel"
      )
    ),
    plot_section = list(
      section_id = "plot-section",
      inputs = list(
        data_type = "data-type",
        pool_select = "pool-select",
        seq_filter = "seq-count-threshold",
        seq_filter_in = "seq-count-threshold-direct",
        plots_panels = "plots-panels",
        plot_panel_1 = "plot-panel-1",
        plot_panel_2 = "plot-panel-2",
        plot_panel_3 = "plot-panel-3",
        plot_panel_4 = "plot-panel-4",
        rbtn_1 = "rbtns-1",
        rbtn_2 = "rbtns-2",
        rbtn_3 = "rbtns-3",
        rbtn_4 = "rbtns-4",
        rbtn_5 = "rbtns-5",
        recompute_btn = "recompute-btn",
        next_btn = "next-btn"
      ),
      outputs = list(
        counts_plot_sc = "plot-1-sc",
        counts_plot_sc_down = "plot-1-sc-download",
        counts_plot_bm = "plot-1-bm",
        counts_plot_bm_down = "plot-1-bm-download",
        sc_ratio_control_all = "sc-ratio-control-all",
        rc_ratio_control_all = "rc-ratio-control-all",
        sc_ratio_sample_all = "sc-ratio-sample-all",
        rc_ratio_sample_all = "rc-ratio-sample-all",
        overall_is_heatmap = "overall-is-heatmap",
        control_is_heatmap = "control-is-heatmap",
        samples_is_heatmap = "samples-is-heatmap",
        hm_desc_1 = "hm-desc-1",
        hm_desc_2 = "hm-desc-2",
        hm_desc_3 = "hm-desc-3",
        ratio_tbl_sc_control = "ratio-tbl-sc-control",
        ratio_barplot_sc_control = "ratio-bp-sc-control",
        ratio_tbl_sc_samples = "ratio-tbl-sc-samples",
        ratio_barplot_sc_samples = "ratio-bp-sc-samples",
        ratio_tbl_rep_control = "ratio-tbl-rep-control",
        ratio_barplot_rep_control = "ratio-bp-rep-control",
        ratio_tbl_rep_samples = "ratio-tbl-rep-samples",
        ratio_barplot_rep_samples = "ratio-bp-rep-samples",
        ratio_contour_controls = "ratio-contour-controls",
        ratio_contour_samples = "ratio-contour-samples"
      )
    ),
    saving_section = list(
        section_id = "saving-section",
        inputs = list(
            dir_container = "dir_container",
            dir_path = "dir_path",
            data_form = "data_form",
            by_pool = "by_pool",
            file_form = "file_form",
            save_btn = "save_btn"
        ),
        outputs = list(
            dir_display = "dir_display"
        )
    )
  )
}

## For generating components ---------------------------------------------------
.generate_status_banner <- function(type = c(
                                      "success", "warning",
                                      "danger", "info",
                                      "primary", "secondary",
                                      "light", "dark"
                                    ),
                                    content = NULL,
                                    ...) {
  type <- rlang::arg_match(type)
  class <- switch(type,
    "success" = "alert alert-success",
    "warning" = "alert alert-warning",
    "danger" = "alert alert-danger",
    "info" = "alert alert-info",
    "primary" = "alert alert-primary",
    "secondary" = "alert alert-secondary",
    "light" = "alert alert-light",
    "dark" = "alert alert-dark"
  )
  banner <- div(
    class = class,
    !!!content,
    ...
  )
  return(banner)
}

.generate_hm_card_content <- function(ns,
                                      type = c("overall", "control", "other")) {
  type <- rlang::arg_match(type)
  btn_id <- if (type == "overall") {
    ns(id_list()$plot_section$inputs$rbtn_1)
  } else if (type == "control") {
    ns(id_list()$plot_section$inputs$rbtn_2)
  } else {
    ns(id_list()$plot_section$inputs$rbtn_3)
  }
  hm_id <- if (type == "overall") {
    ns(id_list()$plot_section$outputs$overall_is_heatmap)
  } else if (type == "control") {
    ns(id_list()$plot_section$outputs$control_is_heatmap)
  } else {
    ns(id_list()$plot_section$outputs$samples_is_heatmap)
  }
  hm_desc_id <- if (type == "overall") {
    ns(id_list()$plot_section$outputs$hm_desc_1)
  } else if (type == "control") {
    ns(id_list()$plot_section$outputs$hm_desc_2)
  } else {
    ns(id_list()$plot_section$outputs$hm_desc_3)
  }
  div(
    shinyWidgets::radioGroupButtons(
      inputId = btn_id,
      choices = c("Shared IS", "Shared IS seq count",
                  "Shared IS replicates"),
      size = "sm",
      status = "secondary"
    ),
    div(
      class = "card-subtitle mb-2 text-muted",
      textOutput(hm_desc_id)
    ),
    shinycustomloader::withLoader(plotly::plotlyOutput(
      hm_id
    ), type = "html",
    loader = "dnaspin")
  )
}

.generate_ratios_segment <- function(ns, panel_number) {
  if (panel_number == 2) {
    ratio_toggles_id <- ns(id_list()$plot_section$inputs$rbtn_4)
    sc_ratio_tbl_id <- ns(id_list()$plot_section$outputs$ratio_tbl_sc_control)
    rep_ratio_tbl_id <- ns(id_list()$plot_section$outputs$ratio_tbl_rep_control)
    sc_ratio_bp_id <- ns(
      id_list()$plot_section$outputs$ratio_barplot_sc_control)
    rep_ratio_bp_id <- ns(
      id_list()$plot_section$outputs$ratio_barplot_rep_control)
    contour_id <- ns(
      id_list()$plot_section$outputs$ratio_contour_controls
    )
  } else if (panel_number == 3) {
    ratio_toggles_id <- ns(id_list()$plot_section$inputs$rbtn_5)
    sc_ratio_tbl_id <- ns(id_list()$plot_section$outputs$ratio_tbl_sc_samples)
    rep_ratio_tbl_id <- ns(id_list()$plot_section$outputs$ratio_tbl_rep_samples)
    sc_ratio_bp_id <- ns(
      id_list()$plot_section$outputs$ratio_barplot_sc_samples)
    rep_ratio_bp_id <- ns(
      id_list()$plot_section$outputs$ratio_barplot_rep_samples)
    contour_id <- ns(
      id_list()$plot_section$outputs$ratio_contour_samples
    )
  } else {
    return(NULL)
  }

  div(
    h4("Ratios details - per sample"),
    shinyWidgets::radioGroupButtons(
      inputId = ratio_toggles_id,
      choices = c("Table view", "Plot view"),
      size = "sm",
      status = "secondary"
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Table view'", ratio_toggles_id),
      div(
        style = "padding: 10px;",
        h5("Sequence count ratios per sample"),
        shinycustomloader::withLoader(
        reactable::reactableOutput(sc_ratio_tbl_id),
        type = "html",
        loader = "dnaspin")
      ),
      div(
        style = "padding: 10px;",
        h5("Replicates count ratios per sample"),
        shinycustomloader::withLoader(
          reactable::reactableOutput(rep_ratio_tbl_id),
          type = "html",
          loader = "dnaspin")
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Plot view'", ratio_toggles_id),
      div(
        style = "padding: 10px;",
        h5("Sequence count ratios per sample"),
        shinycustomloader::withLoader(
          plotly::plotlyOutput(sc_ratio_bp_id),
          type = "html",
          loader = "dnaspin")
      ),
      div(
        style = "padding: 10px;",
        h5("Replicates count ratios per sample"),
        shinycustomloader::withLoader(
          plotly::plotlyOutput(rep_ratio_bp_id),
          type = "html",
          loader = "dnaspin")
      )
    ),
    h4("Ratios details - per integration"),
    div(
      h5("Sequence count ratio vs. Replicate count ratio"),
      p("The plot shows a 2D histogram contour plot with marginals for ",
        "each IS"),
      shinycustomloader::withLoader(
        plotly::plotlyOutput(contour_id),
        type = "html",
        loader = "dnaspin")
    )
  )
}

.generate_plots_card <- function(ns, panel_number) {
  if (panel_number == 2) {
    card_id <- ns(id_list()$plot_section$inputs$plot_panel_2)
    title <- "Control to sample contamination"
    overall_sc <- ns(id_list()$plot_section$outputs$sc_ratio_control_all)
    overall_rc <- ns(id_list()$plot_section$outputs$rc_ratio_control_all)
    is_heatmap <- ns(id_list()$plot_section$outputs$control_is_heatmap)
    type_hm <- "control"
  } else if (panel_number == 3) {
    card_id <- ns(id_list()$plot_section$inputs$plot_panel_3)
    title <- "Samples to control contamination"
    overall_sc <- ns(id_list()$plot_section$outputs$sc_ratio_sample_all)
    overall_rc <- ns(id_list()$plot_section$outputs$rc_ratio_sample_all)
    is_heatmap <- ns(id_list()$plot_section$outputs$samples_is_heatmap)
    type_hm <- "other"
  } else {
    return(NULL)
  }

  card <- div(
    id = card_id,
    style = "width:80%; margin-bottom: 15px;",
    class = "card",
    div(
      class = "card-body",
      h2(
        class = "card-title",
        title
      ),
      div(
        style = "display: flex;",
        div(
          class = "card text-bg-secondary mb-3",
          style = paste("width: 0; flex: 1 1 0; margin-top: 10px;",
                        "margin-bottom: 10px; margin-right:15px;"),
          div(
            class = "card-header",
            "Overall sequence count ratio"
          ),
          div(
            class = "card-body",
            h4(
              class = "card-title",
              textOutput(
                overall_sc
              )
            ),
            div(
              class = "card-text",
              "Sequence count overall ratio is calculated as",
              "ratio between total sequence count of the control",
              "line samples and sum of the total sequence count of",
              "all other samples"
            )
          )
        ),
        div(
          class = "card text-bg-secondary mb-3",
          style = paste("width: 0; flex: 1 1 0; margin-top: 10px;",
                        "margin-bottom: 10px;"),
          div(
            class = "card-header",
            "Overall replicate count ratio"
          ),
          div(
            class = "card-body",
            h4(
              class = "card-title",
              textOutput(
                overall_rc
              )
            ),
            div(
              class = "card-text",
              "Replicate count overall ratio is calculated as",
              "ratio between total number of replicates having shared IS",
              "of the control line samples and sum of",
              "the total number of replicates having said IS in",
              "all other samples"
            )
          )
        )
      ),
      .generate_hm_card_content(ns, type_hm),
      hr(),
      .generate_ratios_segment(ns, panel_number)
    )
  )
}

## For server processing -------------------------------------------------------

#### ISAnalytics options
.global_required_af_tags <- function() {
  c("project_id", "pool_id", "subject", "pcr_repl_id", "vispa_concatenate")
}

.load_isa_opts <- function(file_path) {
  withr::local_options(list("ISAnalytics.verbose" = FALSE))
  to_return <- tryCatch(
    {
      catched <- purrr::quietly(
        ISAnalytics::import_ISA_settings
      )(file_path)$warnings
      if (!is.null(catched) && !purrr::is_empty(catched)) {
        if (!all(.global_required_af_tags() %in%
          ISAnalytics::association_file_columns(TRUE)$tag)) {
          err_miss_tags <- paste0(
            .global_required_af_tags()[
              !.global_required_af_tags() %in%
                ISAnalytics::association_file_columns(TRUE)$tag
            ],
            collapse = ", "
          )
          ISAnalytics::reset_dyn_vars_config()
          rlang::abort(err_miss_tags,
            class = "missing_req_tags_opts"
          )
        } else {
          status_banner <- .generate_status_banner(
            type = "warning",
            content = list(
              strong("Warning"),
              div(catched)
            )
          )
        }
      } else {
        status_banner <- .generate_status_banner(
          type = "success",
          content = list(
            strong("Success"),
            p("ISAnalytics configuration imported successfully")
          )
        )
      }
      list(status_banner = status_banner, flag = TRUE)
    },
    error = function(e) {
      details <- if ("missing_req_tags_opts" %in% attributes(e)$class) {
        div(
          strong("Missing required tags in ISAnalytics options"),
          p(
            "The following tags are required but",
            "missing from the association file specs:",
            br(),
            tags$code(e$message)
          ),
          p(
            icon("xmark"),
            "Can't proceed, please choose a different config file"
          ),
          p(
            icon("circle-info"),
            "ISAnalytics settings will be reverted to default"
          )
        )
      } else {
        div(e$message)
      }
      status_banner <- .generate_status_banner(
        type = "danger",
        content = list(
          strong("Something went wrong"),
          p(
            "Details of the error are specified below. ",
            "If you proceed without importing options,",
            "defaults will be used"
          ),
          hr(),
          details
        )
      )
      list(status_banner = status_banner, flag = FALSE)
    }
  )
  return(to_return)
}

## Performs additional checks on metadata
.meta_additional_checks <- function(imported_meta,
                                    control_cell_line,
                                    indep_sample_id) {
  add_checks <- list()
  add_checks$meta_empty <- nrow(imported_meta) == 0
  col_specs <- ISAnalytics::association_file_columns(TRUE) %>%
    dplyr::filter(.data$tag %in% .global_required_af_tags())
  add_checks$req_cols_present <- col_specs$names[!col_specs$names %in%
    colnames(imported_meta)]
  add_checks$ind_smpl_id <- all(indep_sample_id %in% colnames(imported_meta))
  if (add_checks$meta_empty == FALSE &
    purrr::is_empty(add_checks$req_cols_present)) {
    pool_col <- col_specs %>%
      dplyr::filter(.data$tag == "vispa_concatenate") %>%
      dplyr::pull(.data$names)
    subj_col <- col_specs %>%
      dplyr::filter(.data$tag == "subject") %>%
      dplyr::pull(.data$names)
    control_cl <- imported_meta %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(pool_col))) %>%
      dplyr::summarise(
        control_line_present = control_cell_line %in% .data[[subj_col]]
      )
    add_checks$control_cl <- control_cl
  }
  return(add_checks)
}

# Function responsible of the import of metadata, performing additional
# checks and producing the correct classification of statuses/reports
.meta_import_and_check <- function(path,
                                   root,
                                   control_cell_line,
                                   sep,
                                   dates_format,
                                   filter,
                                   indep_sample_id,
                                   nsp) {
  result <- list()
  withr::local_options(list(
    ISAnalytics.reports = FALSE,
    ISAnalytics.verbose = FALSE
  ))
  ## Import VISPA2 iss if root is not NULL (alignment requested)
  iss_import <- if (!is.null(root)) TRUE else FALSE
  separator <- switch(sep,
    "tab" = "\t",
    "," = ",",
    ";" = ";",
    "space" = " "
  )
  checks_res <- rlang::new_environment() # Extract internal checks results
  iss_failed <- FALSE
  import_ok <- tryCatch(
    {
      withCallingHandlers(
        {
          af <- ISAnalytics::import_association_file(
            path = path, root = root,
            dates_format = dates_format,
            separator = separator,
            filter_for = filter,
            import_iss = iss_import,
            checks_env = checks_res
          )
        },
        fail_stats_msg = function(m) {
          ## Signals the iss import step was skipped entirely
          rlang::env_bind(rlang::env_parent(), iss_failed = TRUE)
        }
      )
      correctly_aligned <- if (any(is.na(af$Path))) {
        FALSE
      } else if (any(is.na(af$Path_quant))) {
        FALSE
      } else {
        TRUE
      }
      list(status = TRUE) # Signal import is done
    },
    error = function(e) {
      list(status = FALSE, error = e$message) # Signal import failed
    }
  )

  ## STATUS LEGEND:
  ## - 0: AF imported, all checks ok
  ## - 1: AF not imported due to error
  ## - 2: AF imported but with warnings to display
  ##
  ## SUB-STATUS:
  ## - 1.0: generic error
  ## - 1.1: fail with simple error - no additional info
  ## - 1.2: fail because no pools found
  if (import_ok$status == FALSE) {
    result$status <- 1
    result$substatus <- 0
    result$errors <- div(import_ok$error)
    return(result)
  }
  ## Perform additional checks
  add_checks <- .meta_additional_checks(af, control_cell_line, indep_sample_id)

  ## ---- Catch and propagate errors
  ### Error if metadata is empty - no data to import
  if (add_checks$meta_empty == TRUE) {
    result$status <- 1
    result$substatus <- 1
    result$error <- div(
      strong("Metadata is empty"), br(),
      "Did you set filters correctly?"
    )
    return(result)
  }

  ### Error if any of the required columns is missing
  if (!purrr::is_empty(add_checks$req_cols_present)) {
    result$status <- 1
    result$substatus <- 1
    result$error <- div(
      strong("Metadata is missing the following required columns"),
      br(),
      HTML(
        paste0(
          "<ul>",
          paste0(
            paste0("<li>", add_checks$req_cols_present, "</li>"),
            collapse = "\n"
          ),
          "</ul>"
        )
      )
    )
    return(result)
  }

  ### Error if independent sample key is not present in the columns
  if (add_checks$ind_smpl_id == FALSE) {
    result$status <- 1
    result$substatus <- 1
    result$error <- div(
      strong("Missing independent sample id"),
      br(),
      p(
        "Independent sample id ",
        tags$code(paste0(indep_sample_id, collapse = ", ")),
        " was not found in metadata columns"
      )
    )
    return(result)
  }

  ### Error if no pool contains the control cell line
  if (all(add_checks$control_cl$control_line_present == FALSE)) {
    result$status <- 1
    result$substatus <- 1
    result$error <- div(
      strong("Missing control cell line"),
      br(),
      p("Control cell line selected is missing in all found pools")
    )
    return(result)
  }

  ### Error if no pool was found
  if (!is.null(root) & !correctly_aligned &
    all(is.na(checks_res$fs_align$Path_quant))) {
    result$status <- 1
    result$substatus <- 2
    alignment_tbl <- .render_fs_align_tbl(
      checks_res$fs_align,
      nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_1))
    result$error <- div(
      strong("No pools found on disk"),
      br(),
      p("No quantification folders were found for any of the pools")
    )
    result$info <- list(alignment_tbl = alignment_tbl)
    return(result)
  }

  ### ---- Manage success with warnings
  any_missing_controls <- any(
    add_checks$control_cl$control_line_present == FALSE
  )
  any_missing_stats <- if (!is.null(root)) {
    any(is.na(checks_res$fs_align$Path_iss))
  } else {
    FALSE
  }

  result$status <- if (iss_failed || any_missing_stats || !correctly_aligned ||
    any_missing_controls) {
    2
  } else {
    0
  }
  result$info <- list()
  if (!is.null(root)) {
    result$info$alignment_tbl <- .render_fs_align_tbl(
      checks_res$fs_align,
      nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_1)
    )
    if (iss_failed) {
      result$info$iss_failed <- TRUE
    } else {
      result$info$iss_failed <- FALSE
      result$info$iss_summary <- .render_iss_checks_tbl(
        checks_res$iss_stats,
        nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_2)
      )
      result$info$iss_missing <- .render_iss_missing_tbl(
        checks_res$iss_stats_miss,
        nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_3)
      )
    }
  }
  result$info$control_cl <- .render_control_line_tbl(
    add_checks$control_cl,
    nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_4)
  )
  proj_col <- ISAnalytics::association_file_columns(TRUE) %>%
    dplyr::filter(.data$tag == "project_id") %>%
    dplyr::pull(.data$names)
  pool_col <- ISAnalytics::association_file_columns(TRUE) %>%
    dplyr::filter(.data$tag == "vispa_concatenate") %>%
    dplyr::pull(.data$names)

  result$af <- af
  if (!is.null(root)) {
    result$af <- result$af %>%
      dplyr::semi_join(checks_res$fs_align %>%
        dplyr::filter(!is.na(.data$Path_quant)),
      by = c(proj_col, pool_col)
      )
  }
  result$af <- result$af %>%
    dplyr::semi_join(add_checks$control_cl %>%
      dplyr::filter(.data$control_line_present == TRUE),
    by = pool_col
    )

  result$info$pool_in <- length(unique(af[[pool_col]]))
  result$info$pool_out <- length(unique(result$af[[pool_col]]))
  return(result)
}

.data_import_and_check_auto <- function(metadata,
                                        matrix_annotated,
                                        workers,
                                        file_patterns,
                                        match_opt,
                                        separator,
                                        nsp) {
  result <- list()
  separator <- switch(separator,
                      "tab" = "\t",
                      "," = ",",
                      ";" = ";",
                      "space" = " "
  )
  withr::local_options(list(
    ISAnalytics.reports = FALSE,
    ISAnalytics.verbose = FALSE
  ))
  checks <- rlang::new_environment()
  import_ok <- tryCatch({
    progressr::withProgressShiny({
      matrices <- ISAnalytics::import_parallel_Vispa2Matrices(
        metadata, quantification_type = "seqCount",
        matrix_type = ifelse(matrix_annotated == TRUE, "annotated",
                             "not_annotated"),
        workers = workers, report_path = NULL, mode = "AUTO",
        patterns = file_patterns, matching_opt = match_opt,
        checks_env = checks, separator = separator
      )
    }, message = "Importing data...")
    list(status = TRUE)
  },
  error = function(e) {
    list(status = FALSE, error = e$message)
  })

  if (import_ok$status == FALSE) {
    result$status <- 1
    result$error <- div(import_ok$error)
    return(result)
  }

  ## Errors if all files have anomalies or were not imported
  all_anomalies <- all(checks$files_found$Anomalies == TRUE)
  all_not_imported <- all(checks$files_imp$Imported == FALSE)

  if (all_anomalies || all_not_imported) {
    result$status <- 1
    result$error <- div(
      strong("Error: no files imported"),
      p("Issues identified when trying to import files, see below")
    )
  } else if (all(checks$files_found$Anomalies == FALSE) &&
        all(checks$files_imp$Imported == TRUE)) {
    result$status <- 0
  } else {
    result$status <- 2
  }

  result$info$files_found_tbl <- .render_files_found_auto(
    checks$files_found,
    nsp(id_list()$data_import$data_section$outputs$checks_tbl_1))

  result$info$files_imported <- .render_files_imp_auto(
    checks$files_imp,
    nsp(id_list()$data_import$data_section$outputs$checks_tbl_2))
  result$matrices <- matrices
  return(result)
}

.data_import_and_check_manual <- function(paths,
                                          tidy_format,
                                          matrix_annotated,
                                          separator,
                                          metadata,
                                          nsp) {
  result <- list()
  separator <- switch(separator,
                      "tab" = "\t",
                      "," = ",",
                      ";" = ";",
                      "space" = " "
  )
  withr::local_options(list(
    ISAnalytics.reports = FALSE,
    ISAnalytics.verbose = FALSE
  ))
  prog <- progressr::progressor(along = paths)
  if (!tidy_format) {
    import_with_isa <- function(path) {
      matrix <- ISAnalytics::import_single_Vispa2Matrix(
        path = path, separator = separator
      )
      prog()
      return(matrix)
    }
    import_ok <- tryCatch({
      progressr::withProgressShiny({
        matrices <- purrr::map(paths, import_with_isa)
      }, message = "Importing data...")
      list(status = TRUE)
    },
    error = function(e) {
      list(status = FALSE, error = e$message)
    })
  } else {
    import_tidy <- function(path) {
      col_types <- ISAnalytics:::.mandatory_IS_types("classic")
      if (matrix_annotated) {
        col_types <- append(
          col_types,
          ISAnalytics:::.annotation_IS_types("classic")
        )
      }
      col_types[[ISAnalytics::pcr_id_column()]] <- "c"
      col_types[[".default"]] <- "n"
      df <- readr::read_delim(
        file = path,
        delim = separator,
        col_types = do.call(readr::cols, col_types),
        na = c("NONE", "NA", "NULL", "NaN", ""),
        trim_ws = TRUE,
        progress = FALSE
      )
      prog()
      return(df)
    }
    import_ok <- tryCatch({
      progressr::withProgressShiny({
        matrices <- purrr::map(paths, import_tidy)
      }, message = "Importing data...")
      list(status = TRUE)
    },
    error = function(e) {
      list(status = FALSE, error = e$message)
    })
  }

  if (import_ok$status == FALSE) {
    result$status <- 1
    result$error <- div(import_ok$error)
    return(result)
  }
  final_matrices <- purrr::reduce(matrices, dplyr::bind_rows)
  final_matrices_filt <- final_matrices %>%
    dplyr::semi_join(metadata, by = ISAnalytics::pcr_id_column())

  ## --- Selecting only data in metadata file
  if (nrow(final_matrices_filt) == 0) {
    result$status <- 1
    result$error <- div(
      strong("Error: no correspondance between data and metadata"),
      p("Please check data and metadata files and ensure correspondance",
        "between the two")
    )
    return(result)
  }
  if (nrow(final_matrices_filt) < nrow(final_matrices)) {
    result$status <- 2
    result$info$missing_data <- .render_missing_data_tbl(
      final_matrices %>%
        dplyr::anti_join(
          metadata, by = ISAnalytics::pcr_id_column()
        ),
      nsp(id_list()$data_import$data_section$outputs$checks_tbl_3)
    )
  }
  if (nrow(final_matrices_filt) == nrow(final_matrices)) {
    result$status <- 0
  }
  result$info$n_imported <- length(matrices)
  result$info$distinct_replic <- length(unique(
    final_matrices_filt[[ISAnalytics::pcr_id_column()]]))
  result$matrices <- final_matrices_filt
  return(result)
}

.recalibrate <- function(data, map, map_path, workers, is_tags,
                         criteria, session) {
  silent_rec <- purrr::quietly(ISAnalytics::compute_near_integrations)
  rec_data <- NULL
  rec_ok <- tryCatch({
    progressr::withProgressShiny(
      {
        rec_data <- silent_rec(
          x = data,
          value_columns = "seqCount",
          map_as_file = map,
          file_path = map_path,
          max_workers = workers,
          is_identity_tags = is_tags,
          keep_criteria = criteria
        )
      },
      message = "Recalibrating",
      session = session
    )
    list(status = TRUE)
  }, error = function(e) {
    list(status = FALSE, error = e$message)
  })
  return(list(res = rec_data, info = rec_ok))
}

## Converts data from tidy format to sparse format
.tidy_to_sparse <- function(data, session) {

    conv_data <- ISAnalytics::as_sparse_matrix(data)

    return(conv_data)
}

## Splits data by pool into different data frames
.split_by_pool <- function(data, session) {

    pool_col <- ISAnalytics::association_file_columns(TRUE) %>%
        dplyr::filter(.data$tag == "vispa_concatenate") %>%
        dplyr::pull(.data$names)

    # Test
    # pool_col <- "PoolID"

    split_data <- split(x = data, f = data[[pool_col]])

    return(split_data)
}

## Utils for tables styling ---

.filter_input <- function(id) {
  function(values, name) {
    tags$select(
      # Set to undefined to clear the filter
      onchange = sprintf(
        paste0(
          "Reactable.setFilter('", id, "', ",
          "'%s', event.target.value || undefined)"
        ),
        name
      ),
      # "All" has an empty value to clear the filter, and is the default option
      tags$option(value = "", "All"),
      lapply(unique(values), tags$option),
      "aria-label" = sprintf("Filter %s", name),
      style = "width: 100%;"
    )
  }
}

.na_as_cross <- function() {
  success_color <- bslib::bs_get_variables(
    app_theme(),
    "success"
  )
  danger_color <- bslib::bs_get_variables(
    app_theme(),
    "danger"
  )
  render_miss <- function(value) {
    if (is.na(value)) {
      icon("times", style = paste0("color:", danger_color, ";"))
    } else {
      value
    }
  }
  return(render_miss)
}

.style_true_false <- function(invert = FALSE, colname) {
  success_color <- bslib::bs_get_variables(
    app_theme(),
    "success"
  )
  danger_color <- bslib::bs_get_variables(
    app_theme(),
    "danger"
  )
  if (invert) {
    return(htmlwidgets::JS(sprintf("
         function(rowInfo) {
         if (rowInfo.values['%s'] == true) {
          color = '%s';
         } else {
          color = '%s';
         }
          return {color: color, fontWeight: 'bold', textTransform: 'uppercase'}
         }
         ", colname, danger_color, success_color)))
  }
  return(htmlwidgets::JS(sprintf("
         function(rowInfo) {
         if (rowInfo.values['%s'] == true) {
          color = '%s';
         } else {
          color = '%s';
         }
          return {color: color, fontWeight: 'bold', textTransform: 'uppercase'}
         }
         ", colname, success_color, danger_color)))
}

## Creates table for the metadata fs alignment summary
.render_fs_align_tbl <- function(data, tbl_id) {
  reactable::reactable(
    data,
    defaultPageSize = 10,
    pageSizeOptions = c(5, 10, 25, 30, 40, 50),
    showPagination = TRUE,
    defaultSorted = list(
      Found = "asc", Path = "desc", Path_quant = "desc",
      Path_iss = "desc"
    ),
    defaultColDef = reactable::colDef(
      sortNALast = FALSE,
      filterInput = .filter_input(tbl_id),
      align = "center"
    ),
    showSortable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    columns = list(
      Found = reactable::colDef(
        style = .style_true_false(colname = "Found")
      ),
      Path = reactable::colDef(
        cell = .na_as_cross()
      ),
      Path_quant = reactable::colDef(
        cell = .na_as_cross()
      ),
      Path_iss = reactable::colDef(
        cell = .na_as_cross()
      )
    )
  )
}

## Creates table for iss summary
.render_iss_checks_tbl <- function(data, tbl_id) {
  reactable::reactable(
    data,
    defaultPageSize = 10,
    pageSizeOptions = c(5, 10, 25, 30, 40, 50),
    showPagination = TRUE,
    defaultSorted = list(Imported = "asc"),
    defaultColDef = reactable::colDef(
      sortNALast = FALSE,
      filterInput = .filter_input(tbl_id),
      align = "center"
    ),
    showSortable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    columns = list(
      Imported = reactable::colDef(
        style = .style_true_false(colname = "Imported")
      ),
      stats_files = reactable::colDef(
        cell = .na_as_cross()
      ),
      Path_iss = reactable::colDef(
        cell = .na_as_cross()
      )
    )
  )
}

## Creates table for missing iss in samples
.render_iss_missing_tbl <- function(data, tbl_id) {
  reactable::reactable(
    data,
    defaultPageSize = 10,
    pageSizeOptions = c(5, 10, 25, 30, 40, 50),
    showPagination = TRUE,
    defaultColDef = reactable::colDef(
      align = "center",
      filterInput = .filter_input(tbl_id)
    ),
    showSortable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    resizable = TRUE
  )
}

## Creates table for control line checks
.render_control_line_tbl <- function(data, tbl_id) {
  reactable::reactable(
    data,
    defaultPageSize = 10,
    pageSizeOptions = c(5, 10, 25, 30, 40, 50),
    showPagination = TRUE,
    defaultSorted = list(control_line_present = "asc"),
    defaultColDef = reactable::colDef(
      align = "center",
      filterInput = .filter_input(tbl_id)
    ),
    showSortable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    columns = list(
      control_line_present = reactable::colDef(
        style = .style_true_false(colname = "control_line_present")
      )
    )
  )
}

## Creates table for data files found (auto mode)
.render_files_found_auto <- function(data, tbl_id) {
  reactable::reactable(
    data %>%
      dplyr::select(!c("Files", "Files_count")),
    defaultPageSize = 10,
    pageSizeOptions = c(5, 10, 25, 30, 40, 50),
    showPagination = TRUE,
    defaultSorted = list(Anomalies = "desc"),
    defaultColDef = reactable::colDef(
      align = "center",
      filterInput = .filter_input(tbl_id)
    ),
    showSortable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    columns = list(
      Anomalies = reactable::colDef(
        style = .style_true_false(colname = "Anomalies", invert = TRUE)
      )
    ),
    details = function(index) {
      div(
        style = "padding: 10px;",
        reactable::reactable(
          data$Files_count[[index]],
          compact = TRUE
        )
      )
    }
  )
}

## Creates table for data files imported (auto mode)
.render_files_imp_auto <- function(data, tbl_id) {
  reactable::reactable(
    data,
    defaultPageSize = 10,
    pageSizeOptions = c(5, 10, 25, 30, 40, 50),
    showPagination = TRUE,
    defaultSorted = list(Imported = "asc"),
    defaultColDef = reactable::colDef(
      align = "center",
      filterInput = .filter_input(tbl_id)
    ),
    showSortable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    columns = list(
      Imported = reactable::colDef(
        style = .style_true_false(colname = "control_line_present",
                                  invert = TRUE)
      ),
      Number_of_samples = reactable::colDef(filterable = FALSE),
      Distinct_is = reactable::colDef(filterable = FALSE)
    )
  )
}

## Creates table for missing data (manual mode)
.render_missing_data_tbl <- function(data, tbl_id) {
  reactable::reactable(
    data,
    defaultPageSize = 10,
    pageSizeOptions = c(5, 10, 25, 30, 40, 50),
    showPagination = TRUE,
    defaultColDef = reactable::colDef(
      align = "center",
      filterInput = .filter_input(tbl_id)
    ),
    showSortable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    resizable = TRUE
  )
}

## ---- For generating UI ---- ##
.meta_checks_panel <- function(status,
                               substatus = NULL,
                               align_requested = FALSE,
                               error = NULL,
                               info = NULL,
                               nsp) {
  if (align_requested == TRUE & status %in% c(0, 2)) {
    fs_align_card <- div(
      class = "card",
      style = "margin-bottom: 5px;",
      div(
        class = "card-body",
        h5(
          class = "card-title",
          "File system alignment summary"
        ),
        reactable::reactableOutput(
          nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_1))
      )
    )
    iss_card <- if (info$iss_failed) {
      div(
        class = "card",
        style = "margin-bottom: 5px;",
        div(
          class = "card-body",
          h5(
            class = "card-title",
            "VISPA2 stats import"
          ),
          h6(
            class = "card-subtitle text-muted",
            "SKIPPED"
          ),
          p(
            class = "card-text",
            "VISPA2 stats import was skipped due to an error"
          )
        )
      )
    } else {
      div(
        class = "card",
        style = "margin-bottom: 5px;",
        div(
          class = "card-body",
          h5(
            class = "card-title",
            "VISPA2 stats import"
          ),
          h6(
            class = "card-subtitle text-muted",
            "IMPORTED"
          ),
          strong(
            class = "card-text",
            "Summary of files found per pool"
          ),
          reactable::reactableOutput(
            nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_2)
          ),
          strong(
            class = "card-text",
            "Missing stats for specific samples"
          ),
          reactable::reactableOutput(
            nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_3)
          )
        )
      )
    }
  } else {
    fs_align_card <- NULL
    iss_card <- NULL
  }

  card_content <- if (status == 1 &&
    substatus %in% c(0, 1)) {
    tagAppendAttributes(
      error,
      class = "card-body text-danger"
    )
  } else if (status == 1 &&
    substatus == 2) {
    div(
      class = "card-body",
      error,
      reactable::reactableOutput(
        nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_1)
      )
    )
  } else {
    div(
      class = "card-body",
      h3(
        class = "card-title",
        "Metadata checks summary"
      ),
      p(
        "Metadata was correctly imported, below a summary of checks",
        "performed - review carefully."
      ),
      ## --- Using card group
      div(
        class = "card-group",
        style = "margin-bottom: 5px;",
        ### --- Alignment performed?
        div(
          class = "card",
          div(
            class = "card-body",
            h5(
              class = "card-title",
              "File system alignment",
            ),
            h6(
              class = "card-subtitle text-muted",
              ifelse(align_requested == TRUE, "PERFORMED", "NOT PERFORMED")
            ),
            p(
              class = "card-text",
              ifelse(align_requested == TRUE,
                "File system alignment was requested and performed",
                paste(
                  "File system alignment was not requested - please",
                  "note that data automated import will be disabled",
                  "and matrices will have to be imported manually"
                )
              )
            )
          )
        ),
        ### --- Pools counts
        div(
          class = "card",
          div(
            class = "card-body",
            h5(
              class = "card-title",
              "Pool counts"
            ),
            p(
              class = "card-text",
              "Number of pools in input: ",
              strong(info$pool_in),
              br(),
              "Number of pools after cleaning: ",
              strong(info$pool_out),
              "(", round((info$pool_out / info$pool_in) * 100, 2), "% on ",
              "input)"
            )
          )
        )
      ),
      ### --- Optional: fs alignment results + stats
      fs_align_card,
      iss_card,
      ### --- Control cell line summary
      div(
        class = "card",
        div(
          class = "card-body",
          h5(
            class = "card-title",
            "Control cell line checks"
          ),
          p(
            class = "card-text",
            "Pools that do not contain the control cell line will",
            "NOT be analysed"
          ),
          reactable::reactableOutput(
            nsp(id_list()$data_import$metadata_section$outputs$checks_tbl_4)
          )
        )
      )
    )
  }

  ## Border according to status
  card_classes <- if (status == 1) {
    "card border-danger"
  } else if (status == 2) {
    "card border-warning"
  } else {
    "card border-success"
  }
  checks_card <- div(
    class = card_classes,
    card_content
  )
  return(checks_card)
}

.data_checks_panel <- function(status,
                               mode,
                               error = NULL,
                               info = NULL,
                               nsp) {
  if (mode == "AUTO") {
    files_found_card <- if (!is.null(info) &&
                            "files_found_tbl" %in% names(info)) {
      div(
        class = "card",
        style = "margin-bottom: 5px;",
        div(
          class = "card-body",
          h5(
            class = "card-title",
            "Files found summary"
          ),
          reactable::reactableOutput(
            nsp(id_list()$data_import$data_section$outputs$checks_tbl_1)
          )
        )
      )
    } else {
      NULL
    }
    files_imp_card <- if (!is.null(info) && "files_imported" %in% names(info)) {
      div(
        class = "card",
        style = "margin-bottom: 5px;",
        div(
          class = "card-body",
          h5(
            class = "card-title",
            "Files imported summary"
          ),
          reactable::reactableOutput(
            nsp(id_list()$data_import$data_section$outputs$checks_tbl_2)
          )
        )
      )
    } else {
      NULL
    }
    card_content <- if (status == 1) {
      div(
        class = "card-body text-danger",
        error,
        files_found_card,
        files_imp_card
      )
    } else {
      div(
        class = "card-body",
        h5(
          class = "card-title",
          ifelse(status == 0, "Files imported successfully",
                 "Anomalies detected")
        ),
        files_found_card,
        files_imp_card
      )
    }
  } else {
    n_imported_card <- div(
      class = "card",
      div(
        class = "card-body",
        h5(
          class = "card-title",
          "Number of imported matrices"
        ),
        strong(info$n_imported)
      )
    )
    distinct_repl_card <- div(
      class = "card",
      div(
        class = "card-body",
        h5(
          class = "card-title",
          "Number of distinct PCR replicates"
        ),
        strong(info$distinct_replic)
      )
    )
    card_content <- if (status == 1) {
      div(
        class = "card-body text-danger",
        error
      )
    } else if (status == 2) {
      div(
        class = "card-body",
        h5(
          class = "card-title",
          "Warning: some data missing in final matrices"
        ),
        p("This may be due to replicate information being present in",
          " matrices but not in the metadata file. Review carefully the ",
          "missing replicates"),
        div(
          class = "card-group",
          style = "margin-bottom: 5px;",
          n_imported_card,
          distinct_repl_card,
        ),
        reactable::reactableOutput(
          nsp(id_list()$data_import$data_section$outputs$checks_tbl_3)
        )
      )
    } else {
      div(
        class = "card-body",
        div(
          class = "card-group",
          style = "margin-bottom: 5px;",
          n_imported_card,
          distinct_repl_card,
        )
      )
    }
  }

  ## Border according to status
  card_classes <- if (status == 1) {
    "card border-danger"
  } else if (status == 2) {
    "card border-warning"
  } else {
    "card border-success"
  }
  checks_card <- div(
    class = card_classes,
    card_content
  )
  return(checks_card)
}

.recalibr_info_panel <- function(info_list) {
  status_banner <- if (info_list$info$status == FALSE) {
    .generate_status_banner(
      type = "danger",
      content = list(
        div(
          strong("Something went wrong:"),
          info_list$info$status$error
        )
      )
    )
  } else {
    warnings <- if (!purrr::is_empty(info_list$res$warnings)) {
      div(
        "Warnings:",
        br(),
        info_list$res$warnings
      )
    } else {
      NULL
    }
    messages <- if (!purrr::is_empty(info_list$res$messages)) {
      div(
        "Messages:",
        br(),
        info_list$res$messages
      )
    } else {
      NULL
    }
    sep <- if (is.null(warnings) & is.null(messages)) {
      NULL
    } else {
      hr()
    }
    .generate_status_banner(
      type = "success",
      content = list(
        strong("Data recalibrated successfully"),
        sep,
        warnings,
        messages
      )
    )
  }

  return(status_banner)
}

.get_counts <- function(dataset, ind_sample_id, pool_col) {
  pcr_id_col <- ISAnalytics::pcr_id_column()

  counts_cols <- c("seqCount")
  if ("BARCODE_MUX" %in% colnames(dataset)) {
    counts_cols <- c(counts_cols, "BARCODE_MUX")
  }

  counts_single_pcr <- dataset %>%
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c(ind_sample_id, pool_col, pcr_id_col))
    )) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = counts_cols,
        .fns = ~ sum(.x, na.rm = TRUE),
        .names = "{.col}"
      ), .groups = "drop"
    ) %>%
    tidyr::unite(col = "id", !!!ind_sample_id)

  counts_sample <- dataset %>%
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c(ind_sample_id, pool_col))
    )) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = counts_cols,
        .fns = ~ sum(.x, na.rm = TRUE),
        .names = "{.col}"
      ), .groups = "drop"
    ) %>%
    tidyr::unite(col = "id", !!!ind_sample_id)

  return(list(single = counts_single_pcr, sample = counts_sample))
}

.get_counts_plots <- function(counts_single, counts_sample,
                              ind_sample_id, pool_col, chosen_pool,
                              proj_name, threshold) {
  if (is.null(counts_single) || is.null(counts_sample)) {
    return(list(sc_plot = NULL, bm_plot = NULL))
  }

  trace_plots <- function() {
    annotations <- list()
    for (i in seq(1, nrow(counts_sample))) {
      ## Seq count annot
      annotations$seqCount[[i]] <- list(
        x = counts_sample$id[[i]],
        y = counts_sample$seqCount[[i]] + 5,
        text = format(counts_sample$seqCount[[i]],
                      big.mark = ","),
        yanchor = "bottom",
        showarrow = FALSE
      )
      ## Raw reads
      if ("BARCODE_MUX" %in% colnames(counts_sample)) {
        annotations$barcodemux[[i]] <- list(
          x = counts_sample$id[[i]],
          y = counts_sample$BARCODE_MUX[[i]] + 5,
          text = format(counts_sample$BARCODE_MUX[[i]],
                        big.mark = ","),
          yanchor = "bottom",
          showarrow = FALSE
        )
      }
    }
    sc_filname <- .get_plot_filname(proj_name = proj_name,
                                    pool_name = chosen_pool,
                                    fixed_threshold = threshold,
                                    plot_type = "totals-seqCount")
    sc_plot <- plotly::plot_ly(
      data = counts_single,
      x = ~id,
      y = ~seqCount,
      color = as.formula(paste0("~", ISAnalytics::pcr_id_column())),
      colors = viridisLite::inferno(
        length(unique(counts_single$id)), begin = 0.3)
    ) %>%
      plotly::add_bars(color = ~id,
                       showlegend = FALSE,
                       marker = list(
                         line = list(
                           color = "rgb(0,0,0)",
                           width = 1
                         )
                       ),
                       customdata = counts_single %>%
                         dplyr::pull(.data[[ISAnalytics::pcr_id_column()]]),
                       hovertemplate = paste(
                         "PCR replicate: %{customdata}",
                         "<br>Seq count: %{y}"
                       )) %>%
      plotly::layout(barmode = "stack",
                     annotations = annotations$seqCount,
                     title = list(
                       text = paste("Pool", chosen_pool, "total seq count")
                     ),
                     xaxis = list(
                       title = "Independent sample"
                     ),
                     yaxis = list(
                       title = "Total sequence count"
                     )) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = "png",
          filename = sc_filname,
          scale = 3, height = NULL, width = NULL
        )
      )

    bm_filname <- .get_plot_filname(proj_name = proj_name,
                                    pool_name = chosen_pool,
                                    fixed_threshold = threshold,
                                    plot_type = "totals-rawReads")
    bm_plot <- NULL
    if ("BARCODE_MUX" %in% colnames(counts_single)) {
      bm_plot <- plotly::plot_ly(
        data = counts_single,
        x = ~id,
        y = ~BARCODE_MUX,
        color = as.formula(paste0("~", ISAnalytics::pcr_id_column())),
        colors = viridisLite::inferno(
          length(unique(counts_single$id)), begin = 0.3)
      ) %>%
        plotly::add_bars(color = ~id,
                         showlegend = FALSE,
                         marker = list(
                           line = list(
                             color = "rgb(0,0,0)",
                             width = 1
                           )
                         ),
                         customdata = counts_single %>%
                           dplyr::pull(.data[[ISAnalytics::pcr_id_column()]]),
                         hovertemplate = paste(
                           "PCR replicate: %{customdata}",
                           "<br>Raw reads: %{y}"
                         )) %>%
        plotly::layout(barmode = "stack",
                       annotations = annotations$barcodemux,
                       title = list(
                         text = paste("Pool", chosen_pool, "total raw reads")
                       ),
                       xaxis = list(
                         title = "Independent sample"
                       ),
                       yaxis = list(
                         title = "Total raw reads"
                       )) %>%
        plotly::config(
          toImageButtonOptions = list(
            format = "png",
            filename = bm_filname,
            scale = 3, height = NULL, width = NULL
          )
        )
    }
    return(list(sc_plot = sc_plot, bm_plot = bm_plot))
  }

  plot_list <- trace_plots()
  return(plot_list)
}

.get_plot_filname <- function(proj_name, pool_name,
                              fixed_threshold, plot_type) {
  file_name <- paste(Sys.Date(), proj_name, pool_name,
                     paste0("threshold-", fixed_threshold),
                     plot_type,
                     sep = "_")
  return(file_name)
}

.get_sharing <- function(df, ind_sample_id, distinct_ind_samples) {
  sharing_overall <- ISAnalytics::is_sharing(
      df,
      group_key = ind_sample_id, minimal = FALSE,
      include_self_comp = TRUE, n_comp = 2,
      keep_genomic_coord = TRUE
    )
  counts_overall <- sharing_overall %>%
    dplyr::distinct(.data$g1, .data$count_g1) %>%
    dplyr::mutate(count_g2 = .data$count_g1)
  sharing_overall <- sharing_overall %>%
    dplyr::mutate(g1 = factor(.data$g1, levels = distinct_ind_samples),
                  g2 = factor(.data$g2, levels = distinct_ind_samples)) %>%
    tidyr::complete(.data$g1, .data$g2, fill = list(
      shared = 0
    )) %>%
    dplyr::select(-dplyr::all_of(c("count_g1", "count_g2"))) %>%
    dplyr::left_join(counts_overall %>%
                       dplyr::select(-dplyr::all_of("count_g2")),
                     by = "g1") %>%
    dplyr::left_join(counts_overall %>%
                       dplyr::select(-dplyr::all_of("count_g1")),
                     by = c("g2" = "g1"))

  sharing_control <- ISAnalytics::is_sharing(
      df %>%
        dplyr::semi_join(known_CEM_IS(),
          by = ISAnalytics::mandatory_IS_vars()
        ),
      group_key = ind_sample_id, minimal = FALSE,
      include_self_comp = TRUE, n_comp = 2,
      keep_genomic_coord = TRUE
    )
  counts_control <- sharing_control %>%
    dplyr::distinct(.data$g1, .data$count_g1) %>%
    dplyr::mutate(count_g2 = .data$count_g1)
  sharing_control <- sharing_control %>%
    dplyr::mutate(g1 = factor(.data$g1, levels = distinct_ind_samples),
                  g2 = factor(.data$g2, levels = distinct_ind_samples)) %>%
    tidyr::complete(.data$g1, .data$g2, fill = list(
      shared = 0
    )) %>%
    dplyr::select(-dplyr::all_of(c("count_g1", "count_g2"))) %>%
    dplyr::left_join(counts_control %>%
                       dplyr::select(-dplyr::all_of("count_g2")),
                     by = "g1") %>%
    dplyr::left_join(counts_control %>%
                       dplyr::select(-dplyr::all_of("count_g1")),
                     by = c("g2" = "g1"))

  sharing_other <- ISAnalytics::is_sharing(
      df %>%
        dplyr::anti_join(known_CEM_IS(),
          by = ISAnalytics::mandatory_IS_vars()
        ),
      group_key = ind_sample_id, minimal = FALSE,
      include_self_comp = TRUE, n_comp = 2,
      keep_genomic_coord = TRUE
    )
  counts_other <- sharing_other %>%
    dplyr::distinct(.data$g1, .data$count_g1) %>%
    dplyr::mutate(count_g2 = .data$count_g1)
  sharing_other <- sharing_other %>%
    dplyr::mutate(g1 = factor(.data$g1, levels = distinct_ind_samples),
                  g2 = factor(.data$g2, levels = distinct_ind_samples)) %>%
    tidyr::complete(.data$g1, .data$g2, fill = list(
      shared = 0
    )) %>%
    dplyr::select(-dplyr::all_of(c("count_g1", "count_g2"))) %>%
    dplyr::left_join(counts_other %>%
                       dplyr::select(-dplyr::all_of("count_g2")),
                     by = "g1") %>%
    dplyr::left_join(counts_other %>%
                       dplyr::select(-dplyr::all_of("count_g1")),
                     by = c("g2" = "g1"))
  return(list(
    overall = sharing_overall, control = sharing_control,
    other = sharing_other
  ))
}

.get_heatmap <- function(df, on_x, on_y, value,
                         colorscale,
                         type = c("sharing", "sc", "rep")) {
  h_type <- rlang::arg_match(type)
  on_x_sym <- rlang::sym(on_x)
  on_y_sym <- rlang::sym(on_y)
  value_sym <- rlang::sym(value)
  x_data <- rlang::expr(~ !!on_x_sym)
  y_data <- rlang::expr(~ !!on_y_sym)
  z_data <- rlang::expr(~ !!value_sym)

  hm <- if (h_type == "sharing") {
    plotly::plot_ly(
      data = df,
      type = "heatmap",
      x = rlang::eval_tidy(x_data),
      y = rlang::eval_tidy(y_data),
      z = rlang::eval_tidy(z_data),
      colors = colorscale,
      hoverinfo = "text",
      text = ~ paste(
        "</br>X:", rlang::eval_tidy(on_x_sym),
        "</br>Y:", rlang::eval_tidy(on_y_sym),
        "</br>Absolute shared IS:", rlang::eval_tidy(value_sym),
        "</br>% on X:", ifelse(is.na(on_g1), "NA", round(on_g1, 2)),
        "</br>% on union:", ifelse(is.na(on_g1), "NA", round(on_union, 2)),
        "</br>Tot IS count X:", count_g1,
        "</br>Tot IS count Y:", count_g2
      )
    )
  } else if (h_type == "sc") {
    plotly::plot_ly(
      data = df,
      type = "heatmap",
      x = rlang::eval_tidy(x_data),
      y = rlang::eval_tidy(y_data),
      z = rlang::eval_tidy(z_data),
      colors = colorscale,
      hoverinfo = "text",
      text = ~ paste(
        "</br>X:", rlang::eval_tidy(on_x_sym),
        "</br>Y:", rlang::eval_tidy(on_y_sym),
        "</br>Shared is seq count of X (showed):", scales::label_number(
          big.mark = ","
        )(sc_g1),
        "</br>Shared is seq count of Y:", scales::label_number(
          big.mark = ","
        )(sc_g2),
        "</br>Shared is seq count union:", scales::label_number(
          big.mark = ","
        )(rlang::eval_tidy(sc_union))
      )
    )
  } else {
    plotly::plot_ly(
      data = df,
      type = "heatmap",
      x = rlang::eval_tidy(x_data),
      y = rlang::eval_tidy(y_data),
      z = rlang::eval_tidy(z_data),
      colors = colorscale,
      hoverinfo = "text",
      text = ~ paste(
        "</br>X:", rlang::eval_tidy(on_x_sym),
        "</br>Y:", rlang::eval_tidy(on_y_sym),
        "</br>Number of replicates tracked on X (showing):", rep_g1,
        paste0("(", scales::label_number(
          accuracy = 0.01, scale = 100, suffix = "%"
        )(rep_g1_perc), " on ", max_rep_g1, " total replicates)"),
        "</br>Number of replicates tracked on Y:", rep_g2,
        paste0("(", scales::label_number(
          accuracy = 0.01, scale = 100, suffix = "%"
        )(rep_g2_perc), " on ", max_rep_g2, " total replicates)"),
        "</br>Perc of overall replicates tracked:", scales::label_number(
          accuracy = 0.01, scale = 100, suffix = "%"
        )(rlang::eval_tidy(value_sym)),
        "</br>Number of overall replicates tracked:", rep_union,
        paste0("(", scales::label_number(
          accuracy = 0.01, scale = 100, suffix = "%"
        )(rep_union_perc), " on ", max_rep_g1 + max_rep_g2,
        " total replicates)")
      )
    )
  }

  if (h_type == "sc") {
    hm <- hm %>%
      plotly::add_annotations(
        text = ~ ifelse(is.na(rlang::eval_tidy(value_sym)),
                        "",
                        scales::label_number(accuracy = 0.1,
                                             scale_cut = scales::cut_short_scale())(
                                               rlang::eval_tidy(value_sym)
                                             )),
        showarrow = FALSE
      ) %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
    return(hm)
  }

  if (h_type == "rep") {
    hm <- hm %>%
      plotly::add_annotations(
        text = ~ ifelse(is.na(rlang::eval_tidy(value_sym)),
                        "",
                        scales::label_number(accuracy = 0.1,
                                             scale = 100, suffix = "%")(
                                               rlang::eval_tidy(value_sym)
                                             )),
        showarrow = FALSE
      ) %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
    return(hm)
  }
  hm <- hm %>%
    plotly::add_annotations(
      text = ~ifelse(is.na(rlang::eval_tidy(value_sym)), "",
                     rlang::eval_tidy(value_sym)),
      showarrow = FALSE
    ) %>%
    plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )

  return(hm)
}

.get_shared_is_counts <- function(df, ref, sample_id, pool_col, pool) {
  ref_with_id <- ref %>%
    dplyr::filter(.data[[pool_col]] == pool) %>%
    tidyr::unite(dplyr::all_of(sample_id), col = "id", remove = FALSE)
  cache_df <- tibble::tibble(
    g1 = character(0),
    g2 = character(0),
    sc_g1 = numeric(0),
    sc_g2 = numeric(0),
    sc_union = numeric(0),
    rep_g1 = numeric(0),
    rep_g2 = numeric(0),
    rep_union = numeric(0),
    max_rep_g1 = numeric(0),
    max_rep_g2 = numeric(0),
    rep_g1_perc = numeric(0),
    rep_g2_perc = numeric(0),
    rep_union_perc = numeric(0)
  )
  p <- progressr::progressor(steps = nrow(df))
  process_row <- function(...) {
    row <- list(...)
    is_coord <- row$is_coord
    cache <- rlang::env_get(env = rlang::env_parent(),
                            nm = "cache_df",
                            default = NULL)
    if (nrow(cache) > 0) {
      # Check 1
      comb_1_present <- cache %>%
        dplyr::filter(.data$g1 == row$g1 & .data$g2 == row$g2)
      if (nrow(comb_1_present) > 0) {
        p()
        return()
      }
      # Check 2
      comb_2_present <- cache %>%
        dplyr::filter(.data$g1 == row$g2 & .data$g2 == row$g1)
      if (nrow(comb_2_present) > 0) {
        rlang::env_poke(env = rlang::env_parent(),
                        nm = "cache_df",
                        value = cache %>%
                          tibble::add_row(
                            g1 = row$g1,
                            g2 = row$g2,
                            sc_g1 = comb_2_present$sc_g2[1],
                            sc_g2 = comb_2_present$sc_g1[1],
                            sc_union = comb_2_present$sc_union[1],
                            rep_g1 = comb_2_present$rep_g2[1],
                            rep_g2 = comb_2_present$rep_g1[1],
                            rep_union = comb_2_present$rep_union[1],
                            max_rep_g1 = comb_2_present$max_rep_g2[1],
                            max_rep_g2 = comb_2_present$max_rep_g1[1],
                            rep_g1_perc = comb_2_present$rep_g2_perc[1],
                            rep_g2_perc = comb_2_present$rep_g1_perc[1],
                            rep_union_perc = comb_2_present$rep_union_perc[1]
                          ))
        p()
        return()
      }
    }
    # If not in cache or cache null calculate
    if (is.null(is_coord)) {
      rlang::env_poke(env = rlang::env_parent(),
                      nm = "cache_df",
                      value = cache %>%
                        tibble::add_row(
                          g1 = row$g1,
                          g2 = row$g2,
                          sc_g1 = NA_real_,
                          sc_g2 = NA_real_,
                          sc_union = NA_real_,
                          rep_g1 = NA_real_,
                          rep_g2 = NA_real_,
                          rep_union = NA_real_,
                          max_rep_g1 = NA_real_,
                          max_rep_g2 = NA_real_,
                          rep_g1_perc = NA_real_,
                          rep_g2_perc = NA_real_,
                          rep_union_perc = NA_real_
                        ))
      p()
      return()
    }
    filter_exp <- if (row$g1 == row$g2) {
      rlang::expr(.data$id == row$g1)
    } else {
      rlang::expr(.data$id %in% c(row$g1, row$g2))
    }
    is_from_ref <- ref_with_id %>%
      dplyr::filter(!!filter_exp)
    max_rep_g1 <- max((is_from_ref %>%
                         dplyr::filter(.data$id == row$g1))$ReplicateNumber)
    max_rep_g2 <- max((is_from_ref %>%
                         dplyr::filter(.data$id == row$g2))$ReplicateNumber)
    is_from_ref <- is_from_ref %>%
      dplyr::semi_join(is_coord, by = ISAnalytics::mandatory_IS_vars())
    if (row$g1 == row$g2) {
      seq_count_sum <- sum(is_from_ref$seqCount, na.rm = TRUE)
      sc_sums <- list(sc_g1 = seq_count_sum, sc_g2 = seq_count_sum,
                      sc_union = seq_count_sum)
      rep_count_1 <- length(unique(is_from_ref$ReplicateNumber))
      rep_counts <- list(rep_g1 = rep_count_1,
                         rep_g2 = rep_count_1,
                         rep_union = rep_count_1,
                         rep_g1_perc = rep_count_1 / max_rep_g1,
                         rep_g2_perc = rep_count_1 / max_rep_g1,
                         rep_union_perc = rep_count_1 / max_rep_g1)
    } else {
      seq_count_sum_1 <- sum(is_from_ref %>%
                               dplyr::filter(.data$id == row$g1) %>%
                               dplyr::pull(.data$seqCount),
                             na.rm = TRUE)
      seq_count_sum_2 <- sum(is_from_ref %>%
                               dplyr::filter(.data$id == row$g2) %>%
                               dplyr::pull(.data$seqCount),
                             na.rm = TRUE)
      sc_sums <- list(sc_g1 = seq_count_sum_1, sc_g2 = seq_count_sum_2,
                      sc_union = seq_count_sum_1 + seq_count_sum_2)

      rep_count_1 <- length(unique(
        is_from_ref %>%
          dplyr::filter(.data$id == row$g1) %>%
          dplyr::pull(.data$ReplicateNumber)
      ))
      rep_count_2 <- length(unique(
        is_from_ref %>%
          dplyr::filter(.data$id == row$g2) %>%
          dplyr::pull(.data$ReplicateNumber)
      ))
      rep_counts <- list(
        rep_g1 = rep_count_1,
        rep_g2 = rep_count_2,
        rep_union = rep_count_1 + rep_count_2,
        rep_g1_perc = rep_count_1 / max_rep_g1,
        rep_g2_perc = rep_count_2 / max_rep_g2,
        rep_union_perc = (rep_count_1 + rep_count_2) /
          (max_rep_g1 + max_rep_g2)
      )
    }
    rlang::env_poke(env = rlang::env_parent(),
                    nm = "cache_df",
                    value = cache %>%
                      tibble::add_row(
                        g1 = row$g1,
                        g2 = row$g2,
                        sc_g1 = sc_sums$sc_g1,
                        sc_g2 = sc_sums$sc_g2,
                        sc_union = sc_sums$sc_union,
                        rep_g1 = rep_counts$rep_g1,
                        rep_g2 = rep_counts$rep_g2,
                        rep_union = rep_counts$rep_union,
                        max_rep_g1 = max_rep_g1,
                        max_rep_g2 = max_rep_g2,
                        rep_g1_perc = rep_counts$rep_g1_perc,
                        rep_g2_perc = rep_counts$rep_g2_perc,
                        rep_union_perc = rep_counts$rep_union_perc
                      ))
    p()
    return()
  }
  purrr::pwalk(df, process_row)
  return(cache_df)
}

.get_is_counts_with_plots <- function(type,
                                      filtered_dataset,
                                      is_shared,
                                      indep_sample_id,
                                      chosen_pool,
                                      pool_col,
                                      input,
                                      uuids) {

  if (type == "overall") {
    sel_data <- is_shared$overall
    prog_msg <- "Calculating shared IS stats - overall"
  } else if (type == "control") {
    sel_data <- is_shared$control
    prog_msg <- "Calculating shared IS stats - control"
  } else {
    sel_data <- is_shared$other
    prog_msg <- "Calculating shared IS stats - samples"
  }

  counts <- progressr::withProgressShiny(
      .get_shared_is_counts(
        sel_data,
        ref = filtered_dataset,
        sample_id = indep_sample_id,
        pool_col = pool_col,
        pool = chosen_pool
      ),
      message = prog_msg
    )

  seq_hm <- .get_heatmap(
    counts,
    on_x = "g1",
    on_y = "g2",
    value = "sc_g1",
    colorscale = viridisLite::inferno(256),
    type = "sc"
  )
  rep_hm <- .get_heatmap(
    counts,
    on_x = "g1",
    on_y = "g2",
    value = "rep_g1_perc",
    colorscale = viridisLite::inferno(256),
    type = "rep"
  )

  seq_rep_heatmap <- list(sc = seq_hm, rep = rep_hm)

  return(list(counts = counts,
              sc_r_hmps = seq_rep_heatmap))
}

.get_ratio_barplot <- function(ratio_df,
                               type = c("sc", "rep"),
                               source = c("Controls", "Samples")) {
  type <- rlang::arg_match(type)
  source <- rlang::arg_match(source)
  selection_df <- ratio_df %>%
      dplyr::filter(.data$IS_Source == source) %>%
      dplyr::mutate(Sample = forcats::fct_relevel(
        factor(.data$Sample),
        "All_Samples",
        after = Inf
      ))
  if (nrow(selection_df) == 0) {
    return(NULL)
  }
  plt_title <- if (type == "sc") {
    "Sequence count ratio per sample"
  } else {
    "Replicate count ratio per sample"
  }
  ratio_plot <- plotly::plot_ly(
    data = selection_df,
    x = ~Sample,
    y = ~Ratio_CEM37,
    color = ~Sample,
    colors = viridisLite::inferno(
      length(unique(selection_df$Sample)), begin = 0.3)
  )

  if (type == "sc") {
    ratio_plot <- ratio_plot %>%
      plotly::add_bars(
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~ paste(
          "</br>Sample:", Sample,
          "</br>Ratio:",  scales::label_number(
            big.mark = ",", accuracy = 0.01
          )(Ratio_CEM37),
          "</br>Sample seqCount:", `SeqCount(Sample-vs-CEM37)`,
          "</br>Control seqCount:", `SeqCount(CEM37)`
        )
      )
  } else {
    ratio_plot <- ratio_plot %>%
      plotly::add_bars(
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~ paste(
          "</br>Sample:", Sample,
          "</br>Ratio:", scales::label_number(
            big.mark = ",", accuracy = 0.01
          )(Ratio_CEM37),
          "</br>Sample replicate count:", `RepCount(Sample-vs-CEM37)`,
          "</br>Control replicate count:", `RepCount(CEM37)`
        )
      )
  }

  ratio_plot <- ratio_plot %>%
    plotly::layout(
      title = list(
        text = plt_title
      ),
      xaxis = list(
        title = "Independent sample"
      ),
      yaxis = list(
        title = "Ratio"
      )
    )
  return(ratio_plot)
}

## Creates table for the sequence count ratios
.render_ratio_tbl <- function(data, tbl_id) {
  data <- data %>%
    dplyr::mutate(Sample = as.factor(.data$Sample),
                  IS_Source = as.factor(.data$IS_Source))
  reactable::reactable(
    data,
    defaultPageSize = 5,
    pageSizeOptions = c(5, 10, 25),
    showPagination = TRUE,
    defaultColDef = reactable::colDef(
      sortNALast = FALSE,
      filterInput = .filter_input(tbl_id),
      align = "center"
    ),
    showSortable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    resizable = TRUE
  )
}

.get_ratio_contour <- function(ratio_df,
                               source = c("Controls", "Samples")) {
  source <- rlang::arg_match(source)
  selection_df <- ratio_df %>%
    dplyr::filter(.data$IS_Source == source,
                  .data$Sample != "All_Samples",
                  !is.na(.data$sc_rep_ratio))
  if (nrow(selection_df) == 0) {
    return(NULL)
  }
  is_details <- selection_df %>%
    tidyr::nest(data = !dplyr::all_of(c(ISAnalytics::mandatory_IS_vars(),
                                       "Ratio_CEM37_sc", "Ratio_CEM37_rep"))
                ) %>%
    dplyr::mutate(Samples = purrr::map_chr(.data$data,
                                           ~ paste0(unique(.x$Sample),
                                                    collapse = ", "))) %>%
    dplyr::select(-.data$data)

  plot_cont <- plotly::subplot(
    # x-axis histogram
    plotly::plot_ly(selection_df,
            x = ~ Ratio_CEM37_sc,
            type = 'histogram',
            color = I("grey40"),
            name = "SC ratios histogram",
            showlegend = FALSE),
    plotly::plotly_empty(type = "scatter", mode = "markers"),
    # 2d contour + scatter
    plotly::plot_ly(
      selection_df,
      x = ~ Ratio_CEM37_sc,
      y = ~ Ratio_CEM37_rep,
      type = 'histogram2dcontour',
      colors = viridisLite::inferno(256, begin = 0.15)) %>%
      plotly::layout(xaxis = list(title = "Sequence count ratio"),
             yaxis = list(title = "Replicate count ratio")) %>%
      plotly::add_trace(
        inherit = FALSE,
        data = is_details,
        x = ~ Ratio_CEM37_sc,
        y = ~ Ratio_CEM37_rep,
        type = "scatter",
        mode = "markers",
        opacity = 0.7,
        marker = list(
          size = 7.5,
          color = "rgb(0,0,0)"
        ),
        hoverinfo = "text",
        text = ~ paste(
          "</br>Sequence count ratio:", Ratio_CEM37_sc,
          "</br>Rep count ratio:", Ratio_CEM37_rep,
          "</br>IS coordinates:", paste0("(", chr, ", ",
                                         integration_locus, ", ",
                                         strand, ")"),
          "</br>Samples:", Samples
        ),
        showlegend = FALSE
      ),
    # y-axis histogram
    plotly::plot_ly(selection_df,
            y = ~ Ratio_CEM37_rep,
            type = 'histogram',
            color = I("grey40"),
            name = "REP ratios histogram",
            showlegend = FALSE
    ) %>%
      plotly::layout(
        yaxis = list(title = "",
                     showline = TRUE,
                     showticklabels = FALSE,
                     linewidth = 1.2),
        xaxis = list(showgrid = TRUE,
                     showticklabels = TRUE)
      ),
    nrows = 2,
    heights = c(0.2, 0.8),
    widths = c(0.8, 0.2),
    shareX = TRUE,
    titleY = TRUE,
    titleX = TRUE
  )
  return(plot_cont)
}
