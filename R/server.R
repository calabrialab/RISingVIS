options(shiny.maxRequestSize = 50 * 1024^2)

svg_graph_full <- function() {
  paste0(
    '<div class="svg-container">',
    '<svg xmlns="http://www.w3.org/2000/svg" ',
    'viewBox="0 0 605.404 650.197" preserveAspectRatio="xMinYMin meet" ',
    'class="svg-content">',
    '<g id="Layer_2" data-name="Layer 2" transform="translate(0 0.197)">',
    '<g id="step4" transform="translate(22.735 533.25)">',
    '<g class="node" id="node-4" transform="translate(213.923)">',
    '<circle class="node-main" id="node-main-4" data-name="node-main-4" ',
    'cx="44.679" cy="44.679" r="44.679" transform="translate(0 34.195) ',
    'rotate(-22.5)"/>',
    '<path class="node-arc-l" id="node-arc-4" data-name="node-arc-4" ',
    'd="M-6.320999999999998,44.679a51,51 0 1,0 102,0a51,51 0 1,0 -102,0" ',
    'transform="translate(0 34.195) rotate(-22.5)"/>',
    '</g><text id="Save_results" data-name="Save results" ',
    'transform="translate(0 39.987)" font-size="37" font-family="Helvetica">',
    '<tspan x="0" y="0">S</tspan><tspan y="0" ',
    'letter-spacing="-0.01em">av</tspan><tspan y="0">e </tspan>',
    '<tspan y="0" letter-spacing="-0.01em">r</tspan><tspan y="0">esults</tspan>',
    '</text><line class="text-line-l" id="text-line-4" data-name="text-line-4" ',
    'x2="141.548" stroke-miterlimit="10" stroke-width="1"/>',
    '</g><g id="path3" transform="translate(295.78 474.969)">',
    '<line id="Line_42" data-name="Line 42" y2="53.772" fill="none" ',
    'stroke="#000" stroke-miterlimit="10" stroke-width="1"/>',
    '</g><g id="step3" transform="translate(242.767 359.097)">',
    '<g class="node" id="node-3" data-name="node"><circle class="node-main" ',
    'id="node-main-3" data-name="node-main-3" cx="44.679" ',
    'cy="44.679" r="44.679" transform="translate(8.335 8.303)"/>',
    '<path class="node-arc-r" id="Path_669" data-name="Path 669" ',
    'd="M-6.320999999999998,44.679a51,51 0 1,0 102,0a51,51 0 1,0 -102,0" ',
    'transform="translate(8.335 8.303)"/>',
    '</g><text id="Per_pool_stats" data-name="Per pool stats" ',
    'transform="translate(136.637 33.931)" font-size="37" ',
    'font-family="Helvetica"><tspan x="0" y="0" letter-spacing="-0.03em">',
    'P</tspan><tspan y="0">er pool stats</tspan></text>',
    '<line class="text-line-r" id="text-line-3" data-name="text-line-3" ',
    'x2="141.548" stroke-miterlimit="10" stroke-width="1"/>',
    '</g><g id="path2" transform="translate(295.78 295.416)">',
    '<line id="Line_44" data-name="Line 44" y2="53.772" fill="none" ',
    'stroke="#000" stroke-miterlimit="10" stroke-width="1"/>',
    '</g><g id="step2" transform="translate(0 174.15)">',
    '<g class="node" id="node-2" data-name="node-2" ',
    'transform="translate(236.659)"><circle class="node-main" ',
    'id="node-main-2" data-name="node-main-2" cx="44.679" cy="44.679" ',
    'r="44.679" transform="translate(0 34.195) rotate(-22.5)"/>',
    '<path class="node-arc-l" id="node-arc-2" data-name="node-arc-2" ',
    'd="M-6.320999999999998,44.679a51,51 0 1,0 102,0a51,51 0 1,0 -102,0" ',
    'transform="translate(0 34.195) rotate(-22.5)"/>',
    '</g><text id="Recalibration" transform="translate(0 39.989)" ',
    'font-size="37" font-family="Helvetica"><tspan x="0" ',
    'y="0">Recalibration</tspan></text><line class="text-line-l" ',
    'id="text-line-2" data-name="text-line-2" x2="141.548" ',
    'stroke-miterlimit="10" stroke-width="1"/></g>',
    '<g id="path1" transform="translate(295.78 115.872)">',
    '<line id="Line_46" data-name="Line 46" y2="53.772" fill="none" ',
    'stroke="#000" stroke-miterlimit="10" stroke-width="1"/>',
    '</g><g id="step1" transform="translate(242.767)">',
    '<g class="node clickable" id="node-1" data-name="node">',
    '<circle class="node-main" ',
    'id="node-main-1" data-name="node-main-1" cx="44.679" cy="44.679" ',
    'r="44.679" transform="translate(8.335 8.303)"/>',
    '<path class="node-arc-r" id="node-arc-1" data-name="node-arc-1" ',
    'd="M-6.320999999999998,44.679a51,51 0 1,0 102,0a51,51 0 1,0 -102,0" ',
    'transform="translate(8.335 8.303)"/>',
    '</g><text id="Data_Import" data-name="Data Import" ',
    'transform="translate(146.925 33.931)" font-size="37" ',
    'font-family="Helvetica"><tspan x="0" y="0">Data </tspan>',
    '<tspan y="0" letter-spacing="0.01em">I</tspan><tspan y="0">mpo</tspan>',
    '<tspan y="0" letter-spacing="0.02em">r</tspan><tspan y="0">t</tspan></text>',
    '<line class="text-line-r" id="text-line-1" data-name="text-line-1" ',
    'x2="141.548" fill="none" troke-miterlimit="10" stroke-width="1"/>',
    "</g></g></svg></div>"
  )
}

svg_graph_mini <- function() {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 72.022 402.5"',
    'preserveAspectRatio="xMinYMin meet" class="svg-content-mini">',
    '<g id="Layer_2" data-name="Layer 2" transform="translate(-237.236)">',
    '<g id="step1" class="mini-node" transform="translate(246.145 2.5)">',
    '<circle id="mini-node-1-main" class="mini-node-main" ',
    'data-name="mini-node-1-main" cx="27.561" cy="27.561" r="27.561" ',
    'transform="translate(0 4.146)"/><path id="mini-node-1-arc" ',
    'data-name="mini-node-1-arc" class="mini-node-arc" d="M374,2.5a31.708,',
    '31.708,0,1,1,0,63.416" transform="translate(-346.458 -2.5)"/>',
    '</g><g id="step2" class="mini-node" transform="translate(237.236 108.956)">',
    '<circle id="mini-node-2-main" data-name="mini-node-2-main" ',
    'class="mini-node-main" cx="27.561" cy="27.561" r="27.561" ',
    'transform="translate(0 21.094) rotate(-22.5)"/>',
    '<path id="mini-node-2-arc" data-name="mini-node-2-arc" ',
    'class="mini-node-arc" d="M340.068,229.23a31.708,31.708,0,0,0,0,63.416" ',
    'transform="translate(-303.929 -225.084)"/>',
    '</g><g id="step4" class="mini-node" transform="translate(237.236 330.479)">',
    '<circle id="mini-node-4-main" data-name="mini-node-4-main" ',
    'class="mini-node-main" cx="27.561" cy="27.561" r="27.561" ',
    'transform="translate(0 21.094) rotate(-22.5)"/>',
    '<path id="mini-node-4-arc" data-name="mini-node-4-arc" ',
    'd="M341.2,683.24a31.708,31.708,0,0,0,0,63.416" ',
    'transform="translate(-304.508 -679.144)" class="mini-node-arc"/>',
    '</g><g id="path1" transform="translate(273.706 73.004)">',
    '<line id="Line_11" data-name="Line 11" y2="33.171" fill="none" ',
    'stroke="#727272" stroke-miterlimit="10" stroke-width="3"/></g>',
    '<g id="step3" class="mini-node"  transform="translate(246.145 223.938)">',
    '<circle id="mini-node-3-main" data-name="mini-node-3-main" cx="27.561" ',
    'cy="27.561" r="27.561" transform="translate(0 4.229)" ',
    'class="mini-node-main"/><path id="mini-node-3-arc" ',
    'data-name="mini-node-3-arc" d="M373.1,456.44a31.708,31.708,0,1,1,0,63.416" ',
    'transform="translate(-345.997 -456.44)" class="mini-node-arc"/>',
    '</g><g id="path3" transform="translate(273.706 294.525)">',
    '<line id="Line_13" data-name="Line 13" y2="33.171" fill="none" ',
    'stroke="#727272" stroke-width="3"/></g><g id="path2" ',
    'transform="translate(273.706 183.762)"><line id="Line_14" ',
    'data-name="Line 14" y2="33.171" fill="none" stroke="#727272" ',
    'stroke-width="3"/></g></g></svg>'
  )
}

svg_anim_script <- '$(".node.clickable").hover(
  function() {
    $(this).removeClass("out").addClass("over");
  },
  function() {
    $(this).removeClass("over").addClass("out");
  }
);'

#' @importFrom shinyjs runjs
#' @importFrom shinyFiles getVolumes shinyDirChoose parseDirPath
#' @importFrom fs path_home
server <- function(input, output, session) {
  output$svg_full <- renderUI({
    HTML(svg_graph_full())
  })
  output$svg_mini <- renderUI({
    HTML(svg_graph_mini())
  })
  session$onFlushed(function() {
    shinyjs::runjs(svg_anim_script)
    shinyjs::runjs("attachSlider()")
    shinyjs::runjs("styleFileInput('isa_config')")
    shinyjs::runjs("styleFileInput('metadata_file')")
    shinyjs::runjs("styleFileInput('data_files')")
  }, once = TRUE)

  # --- ISA options section
  flag_isa_opt_set <- reactiveVal(FALSE)
  af_file_cols <- reactiveVal(
    withr::with_options(list(ISAnalytics.af_specs = "default"),
                        ISAnalytics::association_file_columns())
    )
  observeEvent(af_file_cols(), {
    updateSelectizeInput(
      session = session, inputId = "indep_sample_id",
      choices = af_file_cols()
    )
  })
  observeEvent(input$isaoptions,
    {
      if (input$isaoptions == "Use defaults") {
        if (flag_isa_opt_set() == TRUE) {
          withr::with_options(list("ISAnalytics.verbose" = FALSE), {
            ISAnalytics::reset_dyn_vars_config()
          })
          flag_isa_opt_set(FALSE)
          output$isa_opt_import_status <- NULL
        }
      }
    },
    ignoreInit = TRUE
  )
  observeEvent(flag_isa_opt_set(), {
    if (flag_isa_opt_set() == FALSE) {
      output$confirm_isa_opt_choice <- renderUI({
        div(
          class = "alert alert-primary",
          strong(
            "Using",
            span("default",
              style = "text-decoration: underline;"
            ),
            "ISAnalytics settings"
          )
        )
      })
    } else if (flag_isa_opt_set() == TRUE) {
      output$confirm_isa_opt_choice <- renderUI({
        div(
          class = "alert alert-primary",
          strong(
            "Using",
            span("custom",
              style = "text-decoration: underline;"
            ),
            "ISAnalytics settings"
          )
        )
      })
    }
    af_file_cols(ISAnalytics::association_file_columns())
  })

  chosen_config_file_queue <- reactiveVal(list())
  observeEvent(input$isa_config, {
    if (!isTruthy(input$isa_config)) {
      shinyjs::disable("load_isa_opt_btn")
    } else {
      shinyjs::enable("load_isa_opt_btn")
      chosen_config_file_queue(append(chosen_config_file_queue(),
                                      input$isa_config$datapath))
      if (length(chosen_config_file_queue()) > 1) {
        shinyjs::hide(id = "isa_opt_import_status")
      }
    }
  })
  observeEvent(input$load_isa_opt_btn, {
    load_status <- .load_isa_opts(input$isa_config$datapath)
    flag_isa_opt_set(load_status$flag)
    if (length(chosen_config_file_queue()) > 1) {
      af_file_cols(ISAnalytics::association_file_columns())
    }
    output$isa_opt_import_status <- renderUI(
      load_status$status_banner
    )
    shinyjs::show(id = "isa_opt_import_status")
  })

  # --- Metadata section
  observeEvent(input$align_fs_option, {
    if (isTRUE(input$align_fs_option)) {
      shinyjs::runjs(
        paste0(
          "enablePseudoInput('root_dir_container', ",
          "['root_dir'], ['root_dir_display'])"
        )
      )
    } else {
      shinyjs::runjs(
        paste0(
          "disablePseudoInput('root_dir_container', ",
          "['root_dir'], ['root_dir_display'])"
        )
      )
    }
  })
  volumes <- c(
    Home = fs::path_home(), "R Installation" = R.home(),
    shinyFiles::getVolumes()()
  )
  shinyFiles::shinyDirChoose(input, "root_dir",
    roots = volumes,
    session = session
  )
  root_dir_path <- reactive({
    if (isTRUE(input$align_fs_option)) {
      if (is.integer(input$root_dir)) {
        NULL
      } else {
        shinyFiles::parseDirPath(volumes, input$root_dir)
      }
    } else {
      NULL
    }
  })
  output$root_dir_display <- renderPrint({
    if (!is.null(root_dir_path())) {
      root_dir_path()
    } else {
      cat("Choose dir...")
    }
  })
  # -------- Validation of fields
  iv_metadata <- shinyvalidate::InputValidator$new()
  iv_metadata$add_rule("metadata_file", shinyvalidate::sv_required())
  iv_metadata$add_rule("proj_name", shinyvalidate::sv_required())
  iv_metadata$add_rule("indep_sample_id", shinyvalidate::sv_required())
  iv_metadata$enable()
  observeEvent(eventExpr = {
      input$metadata_file
      input$meta_separator
      input$meta_dates_format
      input$control_cell_line
      input$proj_name
      input$indep_sample_id
    }, handlerExpr = {
      if (iv_metadata$is_valid()) {
        shinyjs::enable("meta_import_btn")
      } else {
        shinyjs::disable("meta_import_btn")
      }
    }, ignoreNULL = FALSE
  )
  # -------- File import
  metadata <- reactiveVal(NULL)
  fs_align_tbl <- reactiveVal(NULL)
  iss_stats_tbl <- reactiveVal(NULL)
  iss_miss_tbl <- reactiveVal(NULL)
  control_cl_tbl <- reactiveVal(NULL)
  fs_aligned_af <- reactiveVal(FALSE)
  observeEvent(metadata(), {
    # Data panel shows only if metadata is set
    if (is.null(metadata())) {
      shinyjs::hide("data-sub-section")
    } else {
      shinyjs::show("data-sub-section")
    }
  })
  observeEvent(fs_aligned_af(), {
    # For data import toggle
    if (fs_aligned_af() == FALSE || is.null(metadata())) {
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "auto_manual_import_switch",
        selected = "Manual import",
        disabledChoices = "Automatic import"
      )
    } else {
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "auto_manual_import_switch",
        selected = "Automatic import",
        disabledChoices = NULL
      )
    }
  })
  observeEvent(fs_align_tbl(), {
    output$fs_align_tbl <- if (is.null(fs_align_tbl())) NULL else
      reactable::renderReactable(fs_align_tbl())
  })
  observeEvent(iss_stats_tbl(), {
    output$af_iss_summary_tbl <- if (is.null(iss_stats_tbl())) NULL else
      reactable::renderReactable(iss_stats_tbl())
  })
  observeEvent(iss_miss_tbl(), {
    output$af_iss_missing_tbl <- if (is.null(iss_miss_tbl())) NULL else
      reactable::renderReactable(iss_miss_tbl())
  })
  observeEvent(control_cl_tbl(), {
    output$af_control_cl_tbl <- if (is.null(control_cl_tbl())) NULL else
      reactable::renderReactable(control_cl_tbl())
  })
  meta_status_fail <- .generate_status_banner(
    type = "danger", "Something went wrong - see check results"
  )
  meta_status_warn <- .generate_status_banner(
    type = "warning", "Warnings - see checks"
  )
  meta_status_succ <- .generate_status_banner(
    type = "success", "Metadata imported successfully"
  )
  observeEvent(input$meta_import_btn, {
    shinyjs::disable(id = "meta_import_btn")
    shinyjs::show(id = "import_meta_spinner")
    shinyjs::runjs("
                   $('#meta_detail_btn').css('visibility', 'hidden')
                   ")
    shinyjs::runjs("
                   $('#metaImportDetails').css('visibility', 'hidden')
                   ")
    proj_col <- ISAnalytics::association_file_columns(TRUE) %>%
             dplyr::filter(.data$tag == "project_id") %>%
             dplyr::pull(.data$names)
    filtering_list <- list(input$proj_name)
    names(filtering_list) <- proj_col
    meta_imp_results <- .meta_import_and_check(
      path = input$metadata_file$datapath,
      root = root_dir_path(),
      control_cell_line = input$control_cell_line,
      sep = input$meta_separator,
      dates_format = input$meta_dates_format,
      filter = filtering_list,
      indep_sample_id = input$indep_sample_id
    )
    shinyjs::hide(id = "import_meta_spinner")
    shinyjs::enable(id = "meta_import_btn")
    if (meta_imp_results$status == 1) {
      output$meta_import_status <- renderUI(meta_status_fail)
      shinyjs::addCssClass("meta_detail_btn", "btn-danger")
      shinyjs::removeCssClass("meta_detail_btn",
                              class = "btn-success btn-warning")
    } else if (meta_imp_results$status == 2) {
      output$meta_import_status <- renderUI(meta_status_warn)
      shinyjs::addCssClass("meta_detail_btn", "btn-warning")
      shinyjs::removeCssClass("meta_detail_btn",
                              class = "btn-success btn-danger")
    } else {
      output$meta_import_status <- renderUI(meta_status_succ)
      shinyjs::addCssClass("meta_detail_btn", "btn-success")
      shinyjs::removeCssClass("meta_detail_btn",
                              class = "btn-warning btn-danger")
    }
    if (meta_imp_results$status != 1) {
      metadata(meta_imp_results$af)
      fs_aligned_af(ifelse(is.null(root_dir_path()), FALSE, TRUE))
    } else {
      fs_aligned_af(FALSE)
    }
    if ("info" %in% names(meta_imp_results)) {
      fs_align_tbl(meta_imp_results$info$alignment_tbl)
      iss_stats_tbl(meta_imp_results$info$iss_summary)
      iss_miss_tbl(meta_imp_results$info$iss_missing)
      control_cl_tbl(meta_imp_results$info$control_cl)
    }
    output$metaImportDetailsContent <- renderUI(
      .meta_checks_panel(meta_imp_results$status,
                         meta_imp_results$substatus,
                         !is.null(root_dir_path()),
                         meta_imp_results$error,
                         meta_imp_results$info[!names(meta_imp_results$info) %in%
                                                 c("alignment_tbl",
                                                      "iss_summary",
                                                      "iss_missing",
                                                   "control_cl")])
    )
    shinyjs::runjs("
                   $('#meta_detail_btn').css('visibility', 'visible')
                   ")
    shinyjs::runjs("
                   $('#metaImportDetails').css('visibility', 'visible')
                   ")
  })
  # --- Data section
  # -------- Validation of fields
  iv_data_manual <- shinyvalidate::InputValidator$new()
  iv_data_manual$add_rule("data_files", shinyvalidate::sv_required())
  iv_data_manual$enable()
  iv_data_all <- shinyvalidate::InputValidator$new()
  iv_data_all$add_rule("max_par_workers", shinyvalidate::sv_required())
  iv_data_all$add_rule("max_par_workers", function(value) {
    as_num <- suppressWarnings(as.integer(value))
    if (is.na(as_num)) {
      return("An integer is required")
    }
    if (as_num <= 0) {
      return("Number should be greater than 0")
    }
  })
  iv_data_all$enable()
  observeEvent({
    input$auto_manual_import_switch
    input$data_files
    }, {
    if (input$auto_manual_import_switch != "Automatic import" &&
        (iv_data_manual$is_valid() & iv_data_all$is_valid())) {
      shinyjs::enable("data_import_btn")
    } else if (input$auto_manual_import_switch == "Automatic import"
               && iv_data_all$is_valid()) {
      shinyjs::enable("data_import_btn")
    } else {
      shinyjs::disable("data_import_btn")
    }
  })
  # -------- File import
  data_status_succ <- .generate_status_banner(
    type = "success", "Data imported successfully"
  )
  matrices <- reactiveVal(NULL)
  data_ff_tbl <- reactiveVal(NULL)
  data_fi_tbl <- reactiveVal(NULL)
  data_miss_tbl <- reactiveVal(NULL)
  observeEvent(data_ff_tbl(), {
    output$data_files_found_tbl <- if (is.null(data_ff_tbl())) NULL else
      reactable::renderReactable(data_ff_tbl())
  })
  observeEvent(data_fi_tbl(), {
    output$data_files_imp_tbl <- if (is.null(data_fi_tbl())) NULL else
      reactable::renderReactable(data_fi_tbl())
  })
  observeEvent(data_miss_tbl(), {
    output$missing_data_tbl <- if (is.null(data_miss_tbl())) NULL else
      reactable::renderReactable(data_miss_tbl())
  })
  observeEvent({
    metadata()
    matrices()
  }, {
    if (is.null(metadata()) | is.null(matrices())) {
      shinyjs::hide("to_rec_page")
    } else {
      shinyjs::show("to_rec_page")
    }
  })
  observeEvent(input$to_rec_page, {
    shinyjs::runjs("
                   $('#data-import-section').slideUp('fast')
                   ")
    shinyjs::show(id = "recalibration-section")
  })
  observeEvent(input$data_import_btn, {
    shinyjs::runjs("
                   $('#data_detail_btn').css('visibility', 'hidden')
                   ")
    shinyjs::runjs("
                   $('#dataImportDetails').css('visibility', 'hidden')
                   ")
    shinyjs::disable("data_import_btn")
    if (input$auto_manual_import_switch == "Automatic import") {
      data_imp_results <- .data_import_and_check_auto(
        metadata = metadata(), matrix_annotated = input$matrix_annotated,
        workers = as.integer(input$max_par_workers),
        file_patterns = input$data_file_patterns,
        match_opt = input$data_file_matching_opt,
        separator = input$data_separator
      )
      if (data_imp_results$status == 1) {
        output$data_import_status <- renderUI(meta_status_fail)
        shinyjs::addCssClass("data_detail_btn", "btn-danger")
        shinyjs::removeCssClass("data_detail_btn",
                                class = "btn-success btn-warning")
      } else if (data_imp_results$status == 2) {
        output$data_import_status <- renderUI(meta_status_warn)
        shinyjs::addCssClass("data_detail_btn", "btn-warning")
        shinyjs::removeCssClass("data_detail_btn",
                                class = "btn-success btn-danger")
      } else {
        output$data_import_status <- renderUI(data_status_succ)
        shinyjs::addCssClass("data_detail_btn", "btn-success")
        shinyjs::removeCssClass("data_detail_btn",
                                class = "btn-danger btn-warning")
      }
      if ("info" %in% names(data_imp_results)) {
        data_ff_tbl(data_imp_results$info$files_found_tbl)
        data_fi_tbl(data_imp_results$info$files_imported)
      }
      matrices(data_imp_results$matrices)
      output$dataImportDetailsContent <- renderUI(
        .data_checks_panel(
          status = data_imp_results$status,
          mode = "AUTO",
          error = data_imp_results$error,
          info = data_imp_results$info
        )
      )
    } else {
      data_imp_results <- .data_import_and_check_manual(
        paths = input$data_files$datapath,
        tidy_format = input$files_tidy,
        matrix_annotated = input$matrix_annotated,
        separator = input$data_separator,
        metadata = metadata()
      )
      if (data_imp_results$status == 1) {
        output$data_import_status <- renderUI(meta_status_fail)
        shinyjs::addCssClass("data_detail_btn", "btn-danger")
        shinyjs::removeCssClass("data_detail_btn",
                                class = "btn-success btn-warning")
      } else if (data_imp_results$status == 2) {
        output$data_import_status <- renderUI(meta_status_warn)
        shinyjs::addCssClass("data_detail_btn", "btn-warning")
        shinyjs::removeCssClass("data_detail_btn",
                                class = "btn-success btn-danger")
      } else {
        output$data_import_status <- renderUI(data_status_succ)
        shinyjs::addCssClass("data_detail_btn", "btn-success")
        shinyjs::removeCssClass("data_detail_btn",
                                class = "btn-danger btn-warning")
      }
      if ("info" %in% names(data_imp_results)) {
        data_miss_tbl(data_imp_results$info$missing_data)
      }
      matrices(data_imp_results$matrices)
      output$dataImportDetailsContent <- renderUI(
        .data_checks_panel(
          status = data_imp_results$status,
          mode = "MANUAL",
          error = data_imp_results$error,
          info = data_imp_results$info
        )
      )
    }
    shinyjs::runjs("
                   $('#data_detail_btn').css('visibility', 'visible')
                   ")
    shinyjs::runjs("
                   $('#dataImportDetails').css('visibility', 'visible')
                   ")
    shinyjs::enable("data_import_btn")
  })
  # --- Recalibration section
  observeEvent(input$`rec-map`, {
    if (isTRUE(input$`rec-map`)) {
      shinyjs::runjs(
        paste0(
          "enablePseudoInput('rec-map-path-container', ",
          "['rec-map-path'], ['map-rec-path-display'])"
        )
      )
    } else {
      shinyjs::runjs(
        paste0(
          "disablePseudoInput('rec-map-path-container', ",
          "['rec-map-path'], ['map-rec-path-display'])"
        )
      )
    }
  })
  observeEvent(input$`rec-btn`, {
    to_rec <- matrices()
    recalibr <- progressr::withProgressShiny({
        ISAnalytics::compute_near_integrations(x = to_rec,
                                               value_columns = "seqCount",
                                               map_as_file = FALSE)
      }, message = "Recalibrating",
      session = session)
    print(head(recalibr))
    })

}

server <- function(input, output, session) {
  # Workflow nav ----
  workflow <- WorkFlow$new()
  ## Side graph ----
  output[[
    id_list()$side_graph
  ]] <- renderUI({
    HTML(.svg_graph_mini())
  })
  ## Data import workflow section ----
  data_imp_returns <- dataImportServer(
    id_list()$data_import$section_id,
    workflow)
  rec_returns <- RecServer(
    id_list()$recalibration$section_id,
    workflow
  )
}
