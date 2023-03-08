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
      outputs = list(

      )
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

#' Scroll page next
#'
#' @description Convenience method to scroll to next page
#'
#' @return Nothing
#' @importFrom shinyjs runjs show
#'
#' @noRd
.page_scroll_next <- function(current_page_id, next_page_id, asis = TRUE) {
  shinyjs::runjs(sprintf("$('#%s').slideUp('fast')",
                         current_page_id))
  shinyjs::show(id = next_page_id, asis = asis)
}


