################################################################################
# Utility internal functions for the whole shiny application                   #
################################################################################

#' Id list
#'
#' @description A list of ids for the shiny app components
#'
#' @return A nested list
#'
#' @noRd
ids <- function() {
  list(
    workflow_start = list(
      section_id = "workflow-start-section",
      inputs = list(
        new_wf_btn = "new-workflow-btn",
        import_wf_btn = "import-workflow-btn",
        wf_name_input = "new-workflow-name-in",
        wf_create_modal_btn = "new-workflow-confirm"
      ),
      outputs = list(
        wf_name_out = "wf-name-out"
      )
    ),
    side_bar = list(
      section_id = "side-bar",
      inputs = list(
        wf_name_txt_btn = "wf-name-display",
        name_modal_textbox = "name-modal-textbox",
        name_modal_confirm_btn = "name-modal-confirm",
        side_nav_element = "svg-graph",
        side_nav_1 = "step-1",
        side_nav_2 = "step-2",
        side_nav_3 = "step-3",
        side_nav_4 = "step-4",
        back_btn = "back-btn",
        new_btn = "new-wf-btn"
      ),
      outputs = list()
    ),
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
          upload_config_btn = "upload-config-btn",
          report_folder = "report-folder-btn"
        ),
        outputs = list(
          import_status = "import-status",
          confirm_choice = "confirm-choice",
          status_container = "status-container",
          report_folder_display = "report-folder-display"
        )
      ),
      metadata_section = list(
        section_id = "metadata-subsection",
        inputs = list(
          file_input = "metadata-file",
          root_container = "root-container",
          root_dir = "root-dir-btn",
          project = "proj-text-in",
          separator = "sep-select",
          dates_format = "dates-format-select",
          sample_id = "sample-id-select",
          control_line = "control-cell-line-select",
          add_cell_line = "add-cell-line-btn",
          cl_name = "add-cl-name",
          cl_file = "add-cl-file",
          confirm_cl_modal_btn = "confirm-cl-modal-btn",
          dismiss_cl_modal_btn = "dismiss-cl-modal-btn",
          empty_root_alert = "empty-root-alert",
          import_btn = "import-btn",
          import_spinner = "import-btn-spinner",
          details_btn = "details-btn",
          details_collapse = "details-collapse",
          save_report = "save-report-check"
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
    ),
    control_lines_db = list(
      section_id = "cell-lines-db-tab",
      inputs = list(
        add_cell_line_btn = "add-cell-line-btn",
        show_edit_modal = "show-edit-modal",
        edit_modal_name = "edit-modal-name",
        show_delete_modal = "show-delete-modal",
        delete_modal_name = "delete-modal-name",
        modal_name_input_edit = "modal-name-input-edit",
        modal_name_input_add = "modal-name-input-add",
        modal_desc_input_edit = "modal-desc-input-edit",
        modal_desc_input_add = "modal-desc-input-add",
        modal_btn_cancel_edit = "modal-btn-cancel-edit",
        modal_btn_confirm_edit = "modal-btn-confirm-edit",
        modal_source_selectize_edit = "modal-source-selectize-edit",
        modal_source_selectize_add = "modal-source-selectize-add",
        modal_file_input_edit = "modal-file-input-edit",
        modal_file_input_add = "modal-file-input-add",
        modal_file_input_btn_edit = "modal-file-input-btn-edit",
        modal_file_input_btn_add = "modal-file-input-btn-add",
        modal_cl_input_selectize_edit = "modal-cl-input-selectize-edit",
        modal_cl_input_selectize_add = "modal-cl-input-selectize-add",
        modal_cl_input_btn_edit = "modal-cl-input-btn-edit",
        modal_cl_input_btn_add = "modal-cl-input-btn-add",
        modal_confirm_copy_edit = "modal-confirm-copy-edit",
        modal_confirm_copy_add = "modal-confirm-copy-add",
        modal_add_row_btn_edit = "modal-add-row-btn-edit",
        modal_add_row_btn_add = "modal-add-row-btn-add",
        modal_confirm_row_btn_edit = "modal-confirm-row-btn-edit",
        modal_confirm_row_btn_add = "modal-confirm-row-btn-add",
        modal_delete_row_btn_edit = "modal-delete-row-btn-edit",
        modal_delete_row_btn_add = "modal-delete-row-btn-add",
        modal_confirm_delete_edit = "modal-confirm-delete-edit",
        modal_confirm_delete_add = "modal-confirm-delete-add",
        modal_edit_headers_btn_edit = "modal-edit-headers-btn-edit",
        modal_edit_headers_btn_add = "modal-edit-headers-btn-add",
        modal_btn_cancel_add = "modal-btn-cancel-add",
        modal_btn_confirm_add = "modal-btn-confirm-add",
        delete_cl_confirm_btn = "delete-cl-confirm-btn"
      ),
      outputs = list(
        table = "cell-lines-db-table",
        modal_known_iss_tbl_edit = "modal-known-iss-tbl-edit",
        modal_known_iss_tbl_add = "modal-known-iss-tbl-add",
        modal_source_ui_edit = "modal-source-ui-edit",
        modal_source_ui_add = "modal-source-ui-add",
        modal_add_row_ui_edit = "modal-add-row-ui-edit",
        modal_add_row_ui_add = "modal-add-row-ui-add",
        modal_edit_row_ui_edit = "modal-edit-row-ui-edit",
        modal_edit_row_ui_add = "modal-edit-row-ui-add",
        modal_edit_headers_ui_edit = "modal-edit-headers-ui-edit",
        modal_edit_headers_ui_add = "modal-edit-headers-ui-add"
      )
    )
  )
}

#' Set of globally required ISAnalytics tags for association file
#' @noRd
.global_required_af_tags <- function() {
  c("project_id", "pool_id", "subject", "pcr_repl_id", "vispa_concatenate")
}

#' Scroll page next
#'
#' @description Convenience method to scroll to next page
#'
#' @return Nothing
#' @importFrom shinyjs runjs show
#'
#' @noRd
.page_scroll_next <- function(current_page_id, next_page_id, asis = TRUE) {
  shinyjs::runjs(sprintf(
    "$('#%s').slideUp('fast')",
    current_page_id
  ))
  shinyjs::show(id = next_page_id, asis = asis)
}

#' Utility for creating sections and subsections with popovers
#' @param section_name The actual name of the section
#' @param type type of html tag - h1, h2, ...
#' @param class an additional class to append (for example display)
#' @param tooltip_content_builder a function that returns the content
#' of the tooltip
#' @param ... additional arguments to pass to the tooltip builder function
#' @return a div with the section name and a popover
#' @noRd
.section_name_with_tooltip <- function(
    section_name,
    type,
    class = NULL,
    tooltip_content_builder,
    ...) {
  div(
    class = "title_with_popover",
    rlang::exec(type, section_name, class = class),
    icon("circle-info",
      `data-bs-toggle` = "popover",
      `data-bs-placement` = "right",
      `data-bs-content` = rlang::exec(tooltip_content_builder, ...),
      `data-bs-html` = "true",
      `data-bs-sanitize` = "false"
    )
  )
}

#' Utility for retrieving all ISAnalytics lookup tables as a list
#' @noRd
.get_isa_is_vars <- function(default = TRUE) {
  is_vars <- list()
  if (default) {
    withr::local_options(
      list(
        ISAnalytics.mandatory_is_vars = "default",
        ISAnalytics.genomic_annotation_vars = "default",
        ISAnalytics.af_specs = "default",
        ISAnalytics.iss_stats_specs = "default",
        ISAnalytics.matrix_file_suffix = "default"
      )
    )
  }
  is_vars[[
    "mandatory_is_vars"
  ]] <- ISAnalytics::mandatory_IS_vars(TRUE)
  is_vars[[
    "genomic_annotation_vars"
  ]] <- ISAnalytics::annotation_IS_vars(TRUE)
  is_vars[[
    "af_specs"
  ]] <- ISAnalytics::association_file_columns(TRUE)
  is_vars[[
    "iss_stats_specs"
  ]] <- ISAnalytics::iss_stats_specs(TRUE)
  is_vars[[
    "matrix_file_suffix"
  ]] <- ISAnalytics::matrix_file_suffixes()

  return(is_vars)
}

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

#' Read a file with readr or openxlsx depending on the file extension
#' @return A list with `result`, `error`, `messages`, `warnings`, `output`
#' @noRd
.read_file <- function(file_path, send_confirm_alerts = TRUE) {
  file_ext <- tools::file_ext(file_path)
  if (file_ext %in% c("xls", "xlsx")) {
    if (!rlang::is_installed("openxlsx")) {
      err_display <- .missing_pkg_error("openxlsx")
      shinyWidgets::sendSweetAlert(
        title = "Error",
        text = HTML(stringr::str_replace_all(err_display, "\n", " <br/> ")),
        type = "error",
        html = TRUE,
        btn_labels = "OK",
        btn_colors = "#dee2e6"
      )
      return(NULL)
    }
    quiet_import <- purrr::safely(purrr::quietly(openxlsx::read.xlsx))
  } else {
    quiet_import <- purrr::safely(purrr::quietly(readr::read_delim))
  }
  import_result <- quiet_import(file_path)
  if (not_null(import_result$error)) {
    err_display <- paste0(
      "Error while reading file: ",
      import_result$error
    )
    shinyWidgets::sendSweetAlert(
      title = "Error",
      text = HTML(stringr::str_replace_all(err_display, "\n", " <br/> ")),
      type = "error",
      html = TRUE,
      btn_labels = "OK",
      btn_colors = "#dee2e6"
    )
    return(NULL)
  }
  import_result <- purrr::list_flatten(import_result, name_spec = "{inner}")
  if (send_confirm_alerts) {
    if (not_null(import_result$warnings)) {
      shinyWidgets::sendSweetAlert(
        title = "File imported with warnings",
        text = HTML(stringr::str_replace_all(
          import_result$warnings, "\n", " <br/> "
        )),
        type = "warning",
        html = TRUE,
        btn_labels = "OK",
        btn_colors = "#dee2e6"
      )
    } else {
      shinyWidgets::sendSweetAlert(
        title = "File imported successfully",
        text = "",
        type = "success",
        btn_labels = "OK",
        btn_colors = "#dee2e6"
      )
    }
  }
  return(import_result$result)
}
