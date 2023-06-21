#' Internal class used to store all relevant data about a workflow.
#' This class is not exported and it is used internally only by the
#' Shiny application.
#' @noRd
WorkFlow <- R6::R6Class(
  classname = "WorkFlow",
  # Private fields and functions --------------------------------------------
  private = list(
    # A string with the name of the workflow
    .name = NULL,
    # The date the workflow was created (automatic)
    .date_created = NULL,
    # A list with 2 components, `json_source` and `options`.
    # `json_source` is either NULL or a string with the path to the JSON file
    # `options` is a named list with the actual lookup tables
    .isa_options = NULL,
    # Path were ISAnalytics reports are optionally saved
    .isa_report_path = NULL,
    # A list with the following components:
    # - `content`: the imported metadata, as a data frame
    # - `was_aligned`: if import was done with alignment or not
    # - `file_path`: path to the tabular file used
    # - `project`: name of the project
    # - `separator`: character used to separate fields
    # - `date_format`: format of the date fields (general)
    # - `ind_sample_id`: character vector of fields that identify
    #    an independent sample
    # - `lines_choices`: a list with the following
    #    - `cl_summary_tbl`: data frame containing the summary of pools
    #       and cell lines
    #    - `cl_selection_tbl`: data frame containing the choices after
    #       selection
    #    - `exclude_data`: one between "after_rec" and "before_rec"
    #       or NULL
    #    - `exclude_cl`: one between "as_sample", "remove_after",
    #       "remove_before" or NULL
    # - `internal_checks`: env containing internal checks performed by
    #    ISAnalytics
    .metadata = NULL,
    # It is the metadata that has to be considered depending on the
    # workflow step - aka before or after the recalibration step
    .active_metadata = NULL,
    .data = NULL,
    .recalibrated = NULL,
    .filter_threshold = NULL,
    .seq_ratio = NULL,
    .repl_ratio = NULL,
    .plots = NULL,
    ## Private funs ----------------------------------------------------------
    #' Renders the components to assemble the report for the metadata checks
    #' @param mode One between "interactive" and "report" - interactive is
    #' used if the checks have to be rendered in the Shiny app,
    #' report is used when the report in rmarkdown is rendered
    #' @param output Only relevant in case mode is interactive. The output
    #' object on which to render server-side tables
    #' @noRd
    .render_meta_checks = function(mode = c("interactive", "report"),
                                   output = NULL) {
      mode <- rlang::arg_match(mode)
      if (is.null(private$.metadata)) {
        return(NULL)
      }
      # Outputs -------------------------------------------------------------
      samples_per_pool_tbl <- reactable::reactable(
        self$metadata_summary$samples_per_pool
      )
      fs_align_tbl <- if (not_null(
        private$.metadata$internal_checks$fs_align
      )) {
        reactable::reactable(
          private$.metadata$internal_checks$fs_align,
          sortable = TRUE,
          searchable = TRUE,
          columns = list(
            Found = reactable::colDef(
              html = TRUE,
              cell = .render_boolean_col()
            ),
            Path_quant = reactable::colDef(
              html = TRUE,
              cell = .render_na_as_cross()
            ),
            Path_iss = reactable::colDef(
              html = TRUE,
              cell = .render_na_as_cross()
            )
          )
        )
      } else {
        NULL
      }
      vispa_iss_stats <- if (not_null(
        private$.metadata$internal_checks$iss_stats
      )) {
        reactable::reactable(
          private$.metadata$internal_checks$iss_stats,
          sortable = TRUE,
          searchable = TRUE,
          columns = list(
            Imported = reactable::colDef(
              html = TRUE,
              cell = .render_boolean_col()
            )
          )
        )
      } else {
        NULL
      }
      vispa_iss_missing <- if (not_null(
        private$.metadata$internal_checks$iss_stats_miss
      )) {
        reactable::renderReactable(
          reactable::reactable(
            private$.metadata$internal_checks$iss_stats_miss,
            sortable = TRUE,
            searchable = TRUE
          )
        )
      } else {
        NULL
      }
      cl_checks_tbl <- .cl_checks_table(
        isolate(private$.metadata$lines_choices$cl_summary_tbl)
      )
      cl_select_tbl <- .cl_post_sel_tbl(
        isolate(private$.metadata$lines_choices$cl_selection_tbl)
      )
      exclude_data <- isolate(private$.metadata$lines_choices$exclude_data)
      exclude_cl <- isolate(private$.metadata$lines_choices$exclude_cl)
      if (mode == "interactive") {
        ns <- getDefaultReactiveDomain()$ns
        output[[
          "meta-summary-table"
        ]] <- reactable::renderReactable(
          samples_per_pool_tbl
        )
        output[[
          "align-checks-table"
        ]] <- if (not_null(fs_align_tbl)) {
          reactable::renderReactable(
            fs_align_tbl
          )
        } else {
          NULL
        }
        output[[
          "meta-iss-stats-table"
        ]] <- if (not_null(vispa_iss_stats)) {
          reactable::renderReactable(
            vispa_iss_stats
          )
        } else {
          NULL
        }
        output[[
          "meta-iss-stats-miss-table"
        ]] <- if (not_null(vispa_iss_missing)) {
          vispa_iss_missing
        } else {
          NULL
        }
        output[[
          "meta-cl-summary-table"
        ]] <- reactable::renderReactable(
          cl_checks_tbl
        )
        output[[
          "meta-cl-selection-table"
        ]] <- reactable::renderReactable(
          cl_select_tbl
        )
      }
      # UI ------------------------------------------------------------------
      ui_to_render <- if (mode == "interactive") {
        ## To display in Shiny app --------------------------------------------
        ### Alignment status --------------------------------------------------
        aligned_status_classes <- if (private$.metadata$was_aligned) {
          "alert alert-success"
        } else {
          "alert alert-danger"
        }
        aligned_status <- div(
          class = paste(aligned_status_classes, "mb-3"),
          "Alignment performed: ", strong(private$.metadata$was_aligned)
        )
        ### Metadata stats ----------------------------------------------------
        stats_section <- div(
          class = "container",
          div(
            class = "row",
            div(
              class = "col",
              div(
                class = "card text-bg-info",
                div(
                  class = "card-body",
                  div(
                    class = "display-5",
                    self$metadata_summary$distinct_pools
                  ),
                  div(
                    "Distinct pools"
                  )
                )
              )
            ),
            div(
              class = "col",
              div(
                class = "card text-bg-info",
                div(
                  class = "card-body",
                  div(
                    class = "display-5",
                    self$metadata_summary$distinct_samples
                  ),
                  div(
                    "Distinct independent samples"
                  )
                )
              )
            )
          ),
          div(
            class = "row mt-3",
            h4("Independent samples per pool")
          ),
          div(
            class = "row",
            reactable::reactableOutput(
              outputId = ns("meta-summary-table"),
              width = "100%"
            )
          )
        )
        ### Alignment checks (if aligned) --------------------------------------
        align_checks_warn <- if (
          private$.metadata$was_aligned &&
            any(private$.metadata$internal_checks$fs_align$Found == FALSE) ||
            any(is.na(private$.metadata$internal_checks$fs_align$Path_quant))
        ) {
          div(
            class = "alert alert-warning",
            "Alignment for some pools failed. Matrices for these pools",
            "need to be imported manually in the next step."
          )
        } else {
          NULL
        }
        iss_stats_panel <- if (
          is.null(private$.metadata$internal_checks$iss_stats) &
            is.null(private$.metadata$internal_checks$iss_stats_miss)
        ) {
          div(
            class = "alert alert-warning",
            "VISPA2 stats not available."
          )
        } else {
          list(
            div(
              class = "text-muted",
              "Summary of imported VISPA2 stats files"
            ),
            reactable::reactableOutput(
              outputId = ns("meta-iss-stats-table"),
              width = "100%"
            ),
            div(
              class = "text-muted mt-5",
              "Overview of missing VISPA2 stats for individual",
              "PCR replicates"
            ),
            reactable::reactableOutput(
              outputId = ns("meta-iss-stats-miss-table"),
              width = "100%"
            )
          )
        }
        align_checks <- if (private$.metadata$was_aligned) {
          div(
            h4("Alignment checks"),
            align_checks_warn,
            reactable::reactableOutput(
              outputId = ns("align-checks-table"),
              width = "100%"
            ),
            h4("VISPA2 stats"),
            iss_stats_panel,
            hr()
          )
        } else {
          NULL
        }
        controls_panel <- div(
          h4("Control cell lines"),
          reactable::reactableOutput(
            outputId = ns("meta-cl-summary-table"),
            width = "100%"
          ),
          h4("Control cell lines selected"),
          reactable::reactableOutput(
            outputId = ns("meta-cl-selection-table"),
            width = "100%"
          )
        )
        ### Overall ui --------------------------------------------------------
        div(
          aligned_status,
          stats_section,
          hr(),
          align_checks,
          controls_panel
        )
      } else {
        ## To display in Rmd report --------------------------------------------
        details_panel <- tagList(
          div(
            class = "card me-2 mb-2",
            div(
              class = "card-header",
              "Details"
            ),
            div(
              class = "card-body",
              strong("File path: "),
              private$.metadata$file_path,
              br(),
              strong("Was aligned? "),
              span(
                class = ifelse(
                  private$.metadata$was_aligned,
                  "badge rounded-pill text-bg-success",
                  "badge rounded-pill text-bg-danger"
                ),
                private$.metadata$was_aligned
              ),
              br(),
              strong("Project name: "),
              private$.metadata$project,
              br(),
              strong("Date format: "),
              private$.metadata$date_format,
              br(),
              strong("Independent sample ID: "),
              purrr::map(
                private$.metadata$ind_sample_id,
                ~ span(
                  class = "badge rounded-pill text-bg-info",
                  .x
                )
              )
            )
          ),
          div(
            class = "card me-2",
            div(
              class = "card-header",
              "Control line choices"
            ),
            div(
              class = "card-body",
              strong("Excluded pool data policy:"),
              if (is.null(exclude_data)) {
                "Unnecessary"
              } else if (
                exclude_data == "after_rec") {
                p(
                  "If imported, excluded pool data will be removed after",
                  "recalibration"
                )
              } else {
                "Excluded pool data will be removed before recalibration"
              },
              br(),
              strong("Excluded control lines policy:"),
              if (is.null(exclude_cl)) {
                "Unnecessary"
              } else if (
                exclude_cl == "as_sample") {
                p(
                  "Excluded control lines that were found in the dataset",
                  "will be treated as normal samples"
                )
              } else if (
                exclude_cl == "remove_after") {
                p(
                  "Excluded control lines that were found in the dataset",
                  "will be removed after recalibration"
                )
              } else {
                p(
                  "Excluded control lines that were found in the dataset",
                  "will be removed before recalibration"
                )
              }
            )
          )
        )

        tables <- list(
          stats = list(
            link = tags$li(
              role = "presentation",
              class = "nav-item active show",
              a(
                role = "tab",
                href = "#meta-stats",
                class = "nav-link",
                `data-toggle` = "tab",
                `aria-controls` = "meta-stats",
                `aria-selected` = "true",
                "Samples per pool"
              )
            ),
            content = div(
              id = "meta-stats",
              role = "tabpanel",
              class = "tab-pane w-100 fade show active",
              div(
                class = "w-100",
                h4("Number of distinct independent samples per pool"),
                samples_per_pool_tbl
              )
            )
          ),
          align = list(
            link = if (not_null(fs_align_tbl)) {
              tags$li(
                role = "presentation",
                class = "nav-item",
                a(
                  role = "tab",
                  href = "#meta-align",
                  class = "nav-link",
                  `data-toggle` = "tab",
                  `aria-controls` = "meta-align",
                  `aria-selected` = "false",
                  "Alignment checks"
                )
              )
            } else {
              NULL
            },
            content = if (
              not_null(fs_align_tbl)) {
              div(
                id = "meta-align",
                role = "tabpanel",
                class = "tab-pane w-100 fade",
                div(
                  class = "w-100",
                  fs_align_tbl
                )
              )
            } else {
              NULL
            }
          ),
          iss_stats = list(
            link = if (not_null(vispa_iss_stats)) {
              tags$li(
                role = "presentation",
                class = "nav-item",
                a(
                  role = "tab",
                  href = "#meta-iss-stats",
                  class = "nav-link",
                  `data-toggle` = "tab",
                  `aria-controls` = "meta-iss-stats",
                  `aria-selected` = "false",
                  "VISPA2 stats summary"
                )
              )
            } else {
              NULL
            },
            content = if (not_null(vispa_iss_stats)) {
              div(
                id = "meta-iss-stats",
                role = "tabpanel",
                class = "tab-pane w-100 fade",
                div(
                  class = "w-100",
                  vispa_iss_stats
                )
              )
            } else {
              NULL
            }
          ),
          iss_stats_miss = list(
            link = if (not_null(vispa_iss_missing)) {
              tags$li(
                role = "presentation",
                class = "nav-item",
                a(
                  role = "tab",
                  href = "#meta-iss-stats-miss",
                  class = "nav-link",
                  `data-toggle` = "tab",
                  `aria-controls` = "meta-iss-stats-miss",
                  `aria-selected` = "false",
                  "VISPA2 stats missing"
                )
              )
            } else {
              NULL
            },
            content = if (not_null(vispa_iss_missing)) {
              div(
                id = "meta-iss-stats-miss",
                role = "tabpanel",
                class = "tab-pane w-100 fade",
                div(
                  class = "w-100",
                  vispa_iss_missing
                )
              )
            } else {
              NULL
            }
          ),
          lines_summary = list(
            link = tags$li(
              role = "presentation",
              class = "nav-item",
              a(
                role = "tab",
                href = "#meta-cl-summary",
                class = "nav-link",
                `data-toggle` = "tab",
                `aria-controls` = "meta-cl-summary",
                `aria-selected` = "false",
                "Control cell lines summary"
              )
            ),
            content = div(
              id = "meta-cl-summary",
              role = "tabpanel",
              class = "tab-pane w-100 fade",
              div(
                class = "w-100",
                cl_checks_tbl
              )
            )
          ),
          lines_selection = list(
            link = tags$li(
              role = "presentation",
              class = "nav-item",
              a(
                role = "tab",
                href = "#meta-cl-selection",
                class = "nav-link",
                `data-toggle` = "tab",
                `aria-controls` = "meta-cl-selection",
                `aria-selected` = "false",
                "Control cell lines selection"
              )
            ),
            content = div(
              id = "meta-cl-selection",
              role = "tabpanel",
              class = "tab-pane w-100 fade",
              div(
                class = "w-100",
                cl_select_tbl
              )
            )
          )
        )

        ui_to_render <- list(
          side_panel = details_panel,
          value_boxes = list(
            distinct_pools = self$metadata_summary$distinct_pools,
            distinct_samples = self$metadata_summary$distinct_samples
          ),
          tables = tables
        )
      }
      return(ui_to_render)
    }
  ),
  # Active fields --------------------------------------------------------------
  active = list(
    #' @field pool_col The name of the column in the metadata table that
    #' contains the pool ID.
    pool_col = function(value) {
      if (is.null(private$.isa_options)) {
        return(NULL)
      }
      private$.isa_options$options$af_specs |>
        dplyr::filter(.data$tag == "vispa_concatenate") |>
        dplyr::pull(.data$names)
    },
    #' @field proj_col The name of the column in the metadata table that
    #' contains the project ID.
    proj_col = function(value) {
      if (is.null(private$.isa_options)) {
        return(NULL)
      }
      private$.isa_options$options$af_specs |>
        dplyr::filter(.data$tag == "project_id") |>
        dplyr::pull(.data$names)
    },
    #' @field independent_sample_id The list of fields in the metadata table
    #' that identifies an independent sample.
    independent_sample_id = function(value) {
      if (is.null(private$.metadata)) {
        return(NULL)
      }
      return(private$.metadata$ind_sample_id)
    },
    #' @field metadata The active metadata table (data frame)
    metadata = function(value) {
      if (is.null(private$.active_metadata)) {
        return(NULL)
      }
      return(private$.active_metadata)
    },
    #' @field metadata_summary A list with summary information about the
    #' metadata table.
    metadata_summary = function(value) {
      if (is.null(private$.metadata)) {
        return(NULL)
      }
      # Number of distinct pools
      distinct_pools <- private$.metadata$content |>
        dplyr::pull(.data[[self$pool_col]]) |>
        unique() |>
        length()
      # Number of independent samples per project
      ind_samples <- private$.metadata$content |>
        dplyr::pull(.data[[self$independent_sample_id]]) |>
        unique() |>
        length()
      # Independent samples per pool
      ind_samples_per_pool <- private$.metadata$content |>
        dplyr::group_by(.data[[self$pool_col]]) |>
        dplyr::summarise(
          n = dplyr::n_distinct(.data[[self$independent_sample_id]]),
          .groups = "drop"
        )
      return(
        list(
          distinct_pools = distinct_pools,
          distinct_samples = ind_samples,
          samples_per_pool = ind_samples_per_pool
        )
      )
    }
  ),
  # Public fields and functions ------------------------------------------------
  public = list(
    #' Initialises a new workflow object
    initialize = function(name = "Workflow-1") {
      private$.name <- name
      private$.date_created <- Sys.time()
      private$.isa_options <- list(
        json_source = NULL,
        options = .get_isa_is_vars()
      )
      private$.isa_report_path <- ISAnalytics::default_report_path()
    },
    #' Returns the name of the workflow
    get_name = function() {
      return(private$.name)
    },
    #' Allows to change the name of the workflow, if provided triggers
    #' a gargoyle flag to notify the change.
    change_name = function(new_name, flag = NULL) {
      old <- private$.name
      private$.name <- new_name
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
      return(invisible(old))
    },
    #' Returns the date of creation of the workflow
    get_date = function() {
      return(private$.date_created)
    },
    #' Allows to set the chosen ISAnalytics options, if provided triggers
    #' a gargoyle flag to notify the change.
    #' @param opt Has to be in the format specified in the private field
    #' `.isa_options`
    set_ISA_options = function(opt, flag = NULL) {
      private$.isa_options <- opt
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
    },
    #' Returns the chosen ISAnalytics options
    get_ISA_options = function() {
      return(private$.isa_options)
    },
    #' Allows to set the path where the ISA report will be saved, if provided
    #' triggers a gargoyle flag to notify the change.
    #' @param path has to be a valid path on disk
    set_ISA_report_path = function(path, flag = NULL) {
      private$.isa_report_path <- path
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
    },
    #' Returns the path where the ISA report will be saved
    get_ISA_report_path = function() {
      return(private$.isa_report_path)
    },
    #' Allows to set the metadata and relative checks, if provided triggers
    #' a gargoyle flag to notify the change.
    #' @param metadata is a list with the structure specified in the private
    #' field `.metadata`
    set_metadata = function(metadata, flag = NULL) {
      private$.metadata <- metadata
      self$set_active_metadata("before")
      if (not_null(flag)) {
        gargoyle::trigger(flag)
      }
    },
    set_active_metadata = function(stage = c("before", "after")) {
      stage <- rlang::arg_match(stage)
      lines_choices <- private$.metadata$lines_choices

      if (is.null(lines_choices$exclude_data) ||
        all(lines_choices$cl_selection_tbl$data)) {
        private$.active_metadata <- private$.metadata$content
        return()
      }
      present_data <- lines_choices$cl_selection_tbl |>
        dplyr::filter(.data$data == TRUE)
      filtered_meta <- private$.metadata$content |>
        dplyr::semi_join(
          present_data,
          by = self$pool_col
        )
      excluded_cl <- if (is.null(lines_choices$exclude_cl) ||
        lines_choices$exclude_cl == "as_sample") {
        NULL
      } else {
        tmp <- lines_choices$cl_summary_tbl |>
          dplyr::filter(
            .data$present == TRUE
          ) |>
          dplyr::anti_join(
            present_data |>
              dplyr::rename(control_line = "control_lines") |>
              tidyr::unnest(cols = c("control_line")),
            by = c(self$pool_col, "control_line")
          ) |>
          dplyr::distinct(.data[[self$pool_col]], .data[["control_line"]])
        if (length(self$independent_sample_id) > 1) {
          tmp <- tmp |>
            tidyr::separate(
              col = "control_line",
              into = self$independent_sample_id, sep = "_"
            )
        } else {
          tmp <- tmp |>
            dplyr::rename(
              !!self$independent_sample_id := "control_line"
            )
        }
        tmp
      }
      to_return <- private$.metadata$content
      if (stage == "before") {
        if (not_null(lines_choices$exclude_data) &&
          lines_choices$exclude_data == "before_rec") {
          to_return <- filtered_meta
        }
        if (not_null(excluded_cl) &&
          lines_choices$exclude_cl == "remove_before") {
          to_return <- to_return |>
            dplyr::anti_join(
              excluded_cl,
              by = c(self$pool_col, self$independent_sample_id)
            )
        }
      } else {
        if (not_null(lines_choices$exclude_data)) {
          to_return <- filtered_meta
        }
        if (not_null(excluded_cl) &&
          lines_choices$exclude_cl != "as_sample") {
          to_return <- to_return |>
            dplyr::anti_join(
              excluded_cl,
              by = c(self$pool_col, self$independent_sample_id)
            )
        }
      }
      private$.active_metadata <- to_return
    },
    was_system_aligned = function() {
      if (not_null(private$.metadata)) {
        return(private$.metadata$was_aligned)
      }
      return(NULL)
    },
    #' Public function that can be called to render the checks performed
    #' on metadata and data
    #' @param type The type of report to render, can be either "metadata"
    #' or "data"
    #' @param mode The mode of rendering, can be either "interactive" or
    #' "report". In interactive mode the function returns the UI to render
    #' in the Shiny app, otherwise it will return the appropriate elements
    #' to render in the Rmd report.
    render_report = function(type = c("metadata", "data"),
                             mode = c("interactive", "report"),
                             output = NULL) {
      if (type == "metadata") {
        return(private$.render_meta_checks(mode = mode, output = output))
      } else {
        # TODO
      }
    }
  )
)
