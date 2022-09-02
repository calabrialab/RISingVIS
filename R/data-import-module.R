# Data import module -----------------------------------------------------------

dataImportUI <- function(id) {
    ns <- NS(id)
    div(
        id = id,
        width = "100%",
        div("Data import", class = "display-2"),
        dataImport.ISAoptUI(
          ns(id_list()$data_import$isa_opt_section$section_id)),
        dataImport.metadataUI(
            ns(id_list()$data_import$metadata_section$section_id)
        ),
        dataImport.dataUI(
            ns(id_list()$data_import$data_section$section_id)
        ),
        div(
          align = "center",
          style = "width: 80%;",
          tagAppendAttributes(
            actionButton(
              inputId = ns(id_list()$data_import$inputs$next_btn),
              label = "NEXT"
            ),
            class = "btn btn-primary btn-lg",
            style = paste("margin-top: 10px;",
                          "display: none;"
            )
          )
        )
    )
}

dataImportServer <- function(id, workflow) {
  moduleServer(id, function(input, output, session) {
    isa_opt_returns <- dataImport.ISAoptServer(
      id_list()$data_import$isa_opt_section$section_id, workflow
    )
    meta_returns <- dataImport.metadataServer(
      id_list()$data_import$metadata_section$section_id, workflow,
      isa_opt_returns$af_file_cols
    )
    observeEvent(meta_returns$metadata(), {
      # Data panel shows only if metadata is set
      if (is.null(meta_returns$metadata())) {
        shinyjs::hide(id_list()$data_import$data_section$section_id)
      } else {
        shinyjs::show(id_list()$data_import$data_section$section_id)
      }
    })
    data_returns <- dataImport.dataServer(
      id_list()$data_import$data_section$section_id,
      workflow, meta_returns$metadata, meta_returns$fs_aligned_af
    )
    observeEvent({
      meta_returns$metadata()
      data_returns$matrices()
    }, {
      if (is.null(meta_returns$metadata()) | is.null(data_returns$matrices())) {
        shinyjs::hide(id_list()$data_import$inputs$next_btn)
      } else {
        shinyjs::show(id_list()$data_import$inputs$next_btn)
      }
    })
    observeEvent(input[[
      id_list()$data_import$inputs$next_btn
    ]], {
      shinyjs::runjs(sprintf("
                   $('#%s').slideUp('fast')
                   ", id_list()$data_import$section_id))
      shinyjs::show(id = id_list()$recalibration$section_id, asis = TRUE)
    })
  })
}

## Sub-module: ISA options -----------------------------------------------------
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyjs disabled
dataImport.ISAoptUI <- function(id) {
    ns <- NS(id)
    div(
        id = id,
        h2("ISAnalytics options configuration"),
        shinyWidgets::radioGroupButtons(
            inputId = ns(id_list()$data_import$isa_opt_section$inputs$toggle1),
            choices = c("Use defaults", "Import configuration"),
            size = "sm",
            status = "secondary"
        ),
        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Import configuration'",
                ns(id_list()$data_import$isa_opt_section$inputs$toggle1)
            ),
            div(
                class = "input-row-container",
                div(
                    style = "padding-right: 10px;",
                    tagAppendAttributes(
                        fileInput(
                            inputId = ns(
                                id_list()$data_import$isa_opt_section$inputs$json_file_input
                            ),
                            label = "Choose configuration file (*.json)",
                            accept = ".json", width = "100%"
                        ),
                        style = "margin-bottom: 0px;"
                    )
                ),
                div(
                    style = "align-self: flex-end;",
                    shinyjs::disabled(actionButton(
                        inputId = ns(
                            id_list()$data_import$isa_opt_section$inputs$upload_config_btn
                        ),
                        label = "Load"
                    ))
                )
            ),
            div(
                id = ns(
                    id_list()$data_import$isa_opt_section$outputs$status_container
                ),
                style = "width: 80%; padding-top: 10px;",
                uiOutput(
                    ns(id_list()$data_import$isa_opt_section$outputs$import_status)
                )
            )
        ),
        div(
            style = "width: 80%;",
            uiOutput(ns(id_list()$data_import$isa_opt_section$outputs$confirm_choice))
        )
    )
}

#' @importFrom shinyjs runjs enable disable hide show
#' @importFrom withr with_options
#' @importFrom ISAnalytics association_file_columns reset_dyn_vars_config
dataImport.ISAoptServer <- function(id, workflow) {
    moduleServer(
        id,
        function(input, output, session) {
            session$onFlushed(function() {
                shinyjs::runjs(
                    sprintf(
                        "styleFileInput('%s')",
                        session$ns(
                            id_list()$data_import$isa_opt_section$inputs$json_file_input
                        )
                    )
                )
            })
            to_return <- list()
            flag_isa_opt_set <- reactiveVal(FALSE)
            af_file_cols <- reactiveVal(
                withr::with_options(
                    list(ISAnalytics.af_specs = "default"),
                    ISAnalytics::association_file_columns()
                )
            )
            to_return$flag_isa_opt_set <- flag_isa_opt_set
            to_return$af_file_cols <- af_file_cols
            observeEvent(input[[
            id_list()$data_import$isa_opt_section$inputs$toggle1
            ]],
            {
                if (input[[
                id_list()$data_import$isa_opt_section$inputs$toggle1
                ]] == "Use defaults") {
                    if (flag_isa_opt_set() == TRUE) {
                        withr::with_options(list("ISAnalytics.verbose" = FALSE), {
                            ISAnalytics::reset_dyn_vars_config()
                        })
                        flag_isa_opt_set(FALSE)
                        output[[
                        id_list()$data_import$isa_opt_section$outputs$import_status
                        ]] <- NULL
                    }
                }
            },
            ignoreInit = TRUE
            )
            observeEvent(flag_isa_opt_set(), {
                if (flag_isa_opt_set() == FALSE) {
                    output[[
                    id_list()$data_import$isa_opt_section$outputs$confirm_choice
                    ]] <- renderUI({
                        .generate_status_banner(
                            type = "primary",
                            content = list(div(
                                strong(
                                    "Using",
                                    span("default",
                                        style = "text-decoration: underline;"
                                    ),
                                    "ISAnalytics settings"
                                )
                            ))
                        )
                    })
                } else if (flag_isa_opt_set() == TRUE) {
                    output[[
                    id_list()$data_import$isa_opt_section$outputs$confirm_choice
                    ]] <- renderUI({
                        .generate_status_banner(
                            type = "primary",
                            content = list(
                                div(
                                    strong(
                                        "Using",
                                        span("custom",
                                            style = "text-decoration: underline;"
                                        ),
                                        "ISAnalytics settings"
                                    )
                                )
                            )
                        )
                    })
                }
                af_file_cols(ISAnalytics::association_file_columns())
            })

            chosen_config_file_queue <- reactiveVal(list())
            observeEvent(input[[
            id_list()$data_import$isa_opt_section$inputs$json_file_input]], {
                if (!isTruthy(input[[
                id_list()$data_import$isa_opt_section$inputs$json_file_input
                ]])) {
                    shinyjs::disable(
                        id_list()$data_import$isa_opt_section$inputs$upload_config_btn
                    )
                } else {
                    shinyjs::enable(
                        id_list()$data_import$isa_opt_section$inputs$upload_config_btn
                    )
                    chosen_config_file_queue(append(
                        chosen_config_file_queue(),
                        input[[
                        id_list()$data_import$isa_opt_section$inputs$json_file_input
                        ]]$datapath
                    ))
                    if (length(chosen_config_file_queue()) > 1) {
                        shinyjs::hide(
                            id = id_list()$data_import$isa_opt_section$outputs$import_status
                        )
                    }
                }
            })
            observeEvent(input[[
            id_list()$data_import$isa_opt_section$inputs$upload_config_btn
            ]], {
                load_status <- .load_isa_opts(
                    input[[
                    id_list()$data_import$isa_opt_section$inputs$json_file_input
                    ]]$datapath
                )
                flag_isa_opt_set(load_status$flag)
                if (length(chosen_config_file_queue()) > 1) {
                    af_file_cols(ISAnalytics::association_file_columns())
                }
                output[[
                id_list()$data_import$isa_opt_section$outputs$import_status
                ]] <- renderUI(
                    load_status$status_banner
                )
                shinyjs::show(
                    id = id_list()$data_import$isa_opt_section$outputs$import_status
                )
            })
            workflow$set_ISA_options(
                list(
                    mand_vars = ISAnalytics::mandatory_IS_vars(TRUE),
                    annot_vars = ISAnalytics::annotation_IS_vars(TRUE),
                    af_cols = ISAnalytics::association_file_columns(TRUE)
                )
            )
            return(to_return)
        }
    )
}

## Sub-module: Metadata --------------------------------------------------------
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinyFiles shinyDirButton
#' @importFrom ISAnalytics date_formats
#' @importFrom shinyjs disabled
dataImport.metadataUI <- function(id) {
    ns <- NS(id)
    div(
        id = id,
        h2("Metadata"),
        fileInput(
            inputId = ns(id_list()$data_import$metadata_section$inputs$file_input),
            label = "Choose metadata file (*.tsv, *.csv or *.xlsx)",
            accept = c(".csv", ".tsv", ".xls", ".xlsx"),
            width = "80%"
        ),
        div(
            align = "left",
            shinyWidgets::materialSwitch(
                inputId = ns(id_list()$data_import$metadata_section$inputs$switch),
                label = "Align file system (VISPA2)",
                value = TRUE,
                status = "primary"
            )
        ),
        div(
            align = "left",
            id = ns(id_list()$data_import$metadata_section$inputs$root_container),
            class = "input-mock-container",
            shinyFiles::shinyDirButton(
                id = ns(id_list()$data_import$metadata_section$inputs$root_dir),
                title = "Root directory",
                label = "Choose",
                icon = icon("folder"),
                buttonType = "primary",
                class = "choose-dir-btn"
            ),
            tagAppendAttributes(
                textOutput(
                    outputId = ns(
                        id_list()$data_import$metadata_section$outputs$root_display
                    )
                ),
                class = "text-out-mock-enabled"
            )
        ),
        div(
            class = "input-row-container",
            style = "margin-top: 2em;",
            div(
                style = "padding-right: 10px;",
                textInput(
                    inputId = ns(
                        id_list()$data_import$metadata_section$inputs$project
                    ),
                    label = "Project name",
                    width = "100%"
                )
            ),
            div(
                style = "padding-right: 10px;",
                selectizeInput(
                    inputId = ns(
                        id_list()$data_import$metadata_section$inputs$separator
                    ),
                    label = "File separator",
                    choices = c("tab", ",", ";", "space"),
                    width = "100%"
                )
            ),
            div(
                selectizeInput(
                    inputId = ns(
                        id_list()$data_import$metadata_section$inputs$dates_format
                    ),
                    label = "General dates format",
                    choices = ISAnalytics::date_formats(),
                    width = "100%"
                )
            )
        ),
        div(
            class = "input-row-container",
            div(
                style = "padding-right: 10px;",
                selectizeInput(
                    inputId = ns(
                        id_list()$data_import$metadata_section$inputs$sample_id
                    ),
                    label = "Independent sample identifier",
                    choices = NULL,
                    multiple = TRUE,
                    width = "100%"
                )
            ),
            div(
                style = "padding-right: 10px;",
                selectizeInput(
                    inputId = ns(
                        id_list()$data_import$metadata_section$inputs$control_line
                    ),
                    label = "Control cell line",
                    choices = c("CEM37"),
                    multiple = FALSE,
                    width = "100%"
                )
            )
        ),
        div(
            class = "input-row-container",
            shinyjs::disabled(actionButton(
                inputId = ns(
                    id_list()$data_import$metadata_section$inputs$import_btn
                ),
                label = tags$div(
                    tags$span(
                        id = ns(
                            id_list()$data_import$metadata_section$inputs$import_spinner
                        ),
                        class = "spinner-border spinner-border-sm",
                        style = "display: none;"
                    ),
                    tags$span(
                        "Import"
                    )
                )
            )),
            div(
                id = ns(id_list()$data_import$metadata_section$outputs$status_container),
                style = "margin-left: 10px;",
                uiOutput(ns(
                    id_list()$data_import$metadata_section$outputs$import_status
                ))
            )
        ),
        tags$button(
            class = "btn",
            id = ns(
                id_list()$data_import$metadata_section$inputs$details_btn
            ),
            `data-bs-toggle` = "collapse",
            `data-bs-target` = paste0(
                "#", ns(id_list()$data_import$metadata_section$inputs$details_collapse)
            ),
            `aria-expanded` = "false",
            `aria-controls` = ns(
                id_list()$data_import$metadata_section$inputs$details_collapse
            ),
            style = paste(
                "margin-top:10px; margin-bottom: 5px;",
                "visibility: hidden;"
            ),
            icon("info"),
            "Details"
        ),
        div(
            class = "collapse",
            id = ns(
                id_list()$data_import$metadata_section$inputs$details_collapse
            ),
            uiOutput(ns(
                id_list()$data_import$metadata_section$outputs$details_content
            ))
        )
    )
}

#' @importFrom shinyjs runjs enable disable show hide addCssClass removeCssClass
#' @importFrom shinyFiles shinyDirChoose getVolumes parseDirPath
#' @importFrom fs path_home
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom reactable renderReactable
#' @importFrom dplyr filter pull .data
#' @importFrom magrittr `%>%`
dataImport.metadataServer <- function(id, workflow, af_file_cols) {
    moduleServer(
        id,
        function(input, output, session) {
            session$onFlushed(function() {
                shinyjs::runjs(sprintf(
                    "styleFileInput('%s')",
                    session$ns(id_list()$data_import$metadata_section$inputs$file_input)
                ))
            }, once = TRUE)
          to_return <- list()
            observeEvent(af_file_cols(), {
                updateSelectizeInput(
                    session = session,
                    inputId = id_list()$data_import$metadata_section$inputs$sample_id,
                    choices = af_file_cols()
                )
            })
            observeEvent(input[[
            id_list()$data_import$metadata_section$inputs$switch
            ]], {
                if (isTRUE(input[[
                id_list()$data_import$metadata_section$inputs$switch
                ]])) {
                    shinyjs::runjs(
                        paste0(
                            sprintf(
                                "enablePseudoInput('%s', ['%s'], ['%s'])",
                                session$ns(
                                    id_list()$data_import$metadata_section$inputs$root_container
                                ),
                                session$ns(
                                    id_list()$data_import$metadata_section$inputs$root_dir
                                ),
                                session$ns(
                                    id_list()$data_import$metadata_section$outputs$root_display
                                )
                            )
                        )
                    )
                } else {
                    shinyjs::runjs(
                        paste0(
                            sprintf(
                                "disablePseudoInput('%s', ['%s'], ['%s'])",
                                session$ns(
                                    id_list()$data_import$metadata_section$inputs$root_container
                                ),
                                session$ns(
                                    id_list()$data_import$metadata_section$inputs$root_dir
                                ),
                                session$ns(
                                    id_list()$data_import$metadata_section$outputs$root_display
                                )
                            )
                        )
                    )
                }
            })
            volumes <- c(
                Home = fs::path_home(), "R Installation" = R.home(),
                shinyFiles::getVolumes()()
            )
            shinyFiles::shinyDirChoose(input,
                id_list()$data_import$metadata_section$inputs$root_dir,
                roots = volumes,
                session = session
            )
            root_dir_path <- reactive({
                if (isTRUE(input[[
                id_list()$data_import$metadata_section$inputs$switch
                ]])) {
                    if (is.integer(input[[
                    id_list()$data_import$metadata_section$inputs$root_dir
                    ]])) {
                        NULL
                    } else {
                        shinyFiles::parseDirPath(volumes, input[[
                        id_list()$data_import$metadata_section$inputs$root_dir
                        ]])
                    }
                } else {
                    NULL
                }
            })
            output[[
            id_list()$data_import$metadata_section$outputs$root_display
            ]] <- renderPrint({
                if (!is.null(root_dir_path())) {
                    root_dir_path()
                } else {
                    cat("Choose dir...")
                }
            })
            # -------- Validation of fields
            iv_metadata <- shinyvalidate::InputValidator$new()
            iv_metadata$add_rule(
                id_list()$data_import$metadata_section$inputs$file_input,
                shinyvalidate::sv_required()
            )
            iv_metadata$add_rule(
                id_list()$data_import$metadata_section$inputs$project,
                shinyvalidate::sv_required()
            )
            iv_metadata$add_rule(
                id_list()$data_import$metadata_section$inputs$sample_id,
                shinyvalidate::sv_required()
            )
            iv_metadata$enable()
            observeEvent(eventExpr = {
                input[[id_list()$data_import$metadata_section$inputs$file_input]]
                input[[id_list()$data_import$metadata_section$inputs$separator]]
                input[[id_list()$data_import$metadata_section$inputs$dates_format]]
                input[[id_list()$data_import$metadata_section$inputs$control_line]]
                input[[id_list()$data_import$metadata_section$inputs$project]]
                input[[id_list()$data_import$metadata_section$inputs$sample_id]]
            }, handlerExpr = {
                if (iv_metadata$is_valid()) {
                    shinyjs::enable(
                        id_list()$data_import$metadata_section$inputs$import_btn
                    )
                } else {
                    shinyjs::disable(
                        id_list()$data_import$metadata_section$inputs$import_btn
                    )
                }
            }, ignoreNULL = FALSE)
            # -------- Reactive vals & observers
            metadata <- reactiveVal(NULL)
            fs_align_tbl <- reactiveVal(NULL)
            iss_stats_tbl <- reactiveVal(NULL)
            iss_miss_tbl <- reactiveVal(NULL)
            control_cl_tbl <- reactiveVal(NULL)
            fs_aligned_af <- reactiveVal(FALSE)
            meta_checks_info <- reactiveVal(NULL)
            to_return$metadata <- metadata
            to_return$fs_aligned_af <- fs_aligned_af
            observeEvent(fs_align_tbl(), {
                output[[
                id_list()$data_import$metadata_section$outputs$checks_tbl_1
                ]] <- if (is.null(fs_align_tbl())) {
                    NULL
                } else {
                    reactable::renderReactable(fs_align_tbl())
                }
            })
            observeEvent(iss_stats_tbl(), {
                output[[
                id_list()$data_import$metadata_section$outputs$checks_tbl_2
                ]] <- if (is.null(iss_stats_tbl())) {
                    NULL
                } else {
                    reactable::renderReactable(iss_stats_tbl())
                }
            })
            observeEvent(iss_miss_tbl(), {
                output[[
                id_list()$data_import$metadata_section$outputs$checks_tbl_3
                ]] <- if (is.null(iss_miss_tbl())) {
                    NULL
                } else {
                    reactable::renderReactable(iss_miss_tbl())
                }
            })
            observeEvent(control_cl_tbl(), {
                output[[
                id_list()$data_import$metadata_section$outputs$checks_tbl_4
                ]] <- if (is.null(control_cl_tbl())) {
                    NULL
                } else {
                    reactable::renderReactable(control_cl_tbl())
                }
            })
            observeEvent(metadata(), {
                meta_to_set <- metadata()
                checks <- if (!is.null(meta_to_set)) {
                    list(
                        tbl1 = fs_align_tbl(),
                        tbl2 = iss_stats_tbl(),
                        tbl3 = iss_miss_tbl(),
                        tbl4 = control_cl_tbl(),
                        fs_aligned = fs_aligned_af(),
                        info = meta_checks_info()
                    )
                } else {
                    NULL
                }
                workflow$set_metadata(df = meta_to_set, checks = checks)
            })
            fail_banner <- .generate_status_banner(
                type = "danger", "Something went wrong - see check results"
            )
            warn_banner <- .generate_status_banner(
                type = "warning", "Warnings - see checks"
            )
            succ_banner <- .generate_status_banner(
                type = "success", "Metadata imported successfully"
            )
            observeEvent(input[[
            id_list()$data_import$metadata_section$inputs$import_btn]], {
                shinyjs::disable(
                    id = id_list()$data_import$metadata_section$inputs$import_btn
                )
                shinyjs::show(
                    id = id_list()$data_import$metadata_section$inputs$import_spinner
                )
                shinyjs::runjs(sprintf(
                    "
                   $('#%s').css('visibility', 'hidden')
                   ",
                    session$ns(
                        id_list()$data_import$metadata_section$inputs$details_btn
                    )
                ))
                shinyjs::runjs(sprintf(
                    "
                   $('#%s').css('visibility', 'hidden')
                   ",
                    session$ns(
                        id_list()$data_import$metadata_section$inputs$details_collapse
                    )
                ))
                proj_col <- ISAnalytics::association_file_columns(TRUE) %>%
                    dplyr::filter(.data$tag == "project_id") %>%
                    dplyr::pull(.data$names)
                filtering_list <- list(
                    input[[id_list()$data_import$metadata_section$inputs$project]]
                )
                names(filtering_list) <- proj_col
                meta_imp_results <- .meta_import_and_check(
                    path = input[[
                    id_list()$data_import$metadata_section$inputs$file_input
                    ]]$datapath,
                    root = root_dir_path(),
                    control_cell_line = input[[
                    id_list()$data_import$metadata_section$inputs$control_line]],
                    sep = input[[
                    id_list()$data_import$metadata_section$inputs$separator]],
                    dates_format = input[[
                    id_list()$data_import$metadata_section$inputs$dates_format]],
                    filter = filtering_list,
                    indep_sample_id = input[[
                    id_list()$data_import$metadata_section$inputs$sample_id]],
                    session$ns
                )
                shinyjs::hide(id = id_list()$data_import$metadata_section$inputs$import_spinner)
                shinyjs::enable(id = id_list()$data_import$metadata_section$inputs$import_btn)
                if (meta_imp_results$status == 1) {
                    output[[
                    id_list()$data_import$metadata_section$outputs$import_status
                    ]] <- renderUI(fail_banner)
                    shinyjs::addCssClass(
                        id_list()$data_import$metadata_section$inputs$details_btn,
                        "btn-danger"
                    )
                    shinyjs::removeCssClass(
                        id_list()$data_import$metadata_section$inputs$details_btn,
                        class = "btn-success btn-warning"
                    )
                } else if (meta_imp_results$status == 2) {
                    output[[
                    id_list()$data_import$metadata_section$outputs$import_status
                    ]] <- renderUI(warn_banner)
                    shinyjs::addCssClass(
                        id_list()$data_import$metadata_section$inputs$details_btn,
                        "btn-warning"
                    )
                    shinyjs::removeCssClass(
                        id_list()$data_import$metadata_section$inputs$details_btn,
                        class = "btn-success btn-danger"
                    )
                } else {
                    output[[
                    id_list()$data_import$metadata_section$outputs$import_status
                    ]] <- renderUI(succ_banner)
                    shinyjs::addCssClass(
                        id_list()$data_import$metadata_section$inputs$details_btn,
                        "btn-success"
                    )
                    shinyjs::removeCssClass(
                        id_list()$data_import$metadata_section$inputs$details_btn,
                        class = "btn-warning btn-danger"
                    )
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
                    meta_checks_info(meta_imp_results$info[
                        !names(meta_imp_results$info) %in% c(
                            "alignment_tbl",
                            "iss_summary",
                            "iss_missing",
                            "control_cl"
                        )
                    ])
                }
                output[[
                id_list()$data_import$metadata_section$outputs$details_content
                ]] <- renderUI(
                    .meta_checks_panel(
                        meta_imp_results$status,
                        meta_imp_results$substatus,
                        !is.null(root_dir_path()),
                        meta_imp_results$error,
                        meta_checks_info(),
                        session$ns
                    )
                )
                shinyjs::runjs(sprintf(
                    "
                   $('#%s').css('visibility', 'visible')
                   ",
                    session$ns(id_list()$data_import$metadata_section$inputs$details_btn)
                ))
                shinyjs::runjs(sprintf(
                    "
                   $('#%s').css('visibility', 'visible')
                   ",
                    session$ns(id_list()$data_import$metadata_section$inputs$details_collapse)
                ))
            })
            return(to_return)
        }
    )
}

## Sub-module: Data ------------------------------------------------------------
#' @importFrom shinyWidgets radioGroupButtons awesomeCheckbox
dataImport.dataUI <- function(id) {
    ns <- NS(id)
    div(
        id = id,
        style = "display: none;",
        h2("Data"),
        shinyWidgets::radioGroupButtons(
            inputId = ns(
                id_list()$data_import$data_section$inputs$import_mode
            ),
            choices = c("Automatic import", "Manual import"),
            size = "sm",
            status = "secondary"
        ),
        shinyWidgets::awesomeCheckbox(
            inputId = ns(
                id_list()$data_import$data_section$inputs$annotation
            ),
            label = "Matrices are annotated",
            value = TRUE,
            status = "success"
        ),
        div(
            selectizeInput(
                inputId = ns(
                    id_list()$data_import$data_section$inputs$separator
                ),
                label = "File separator",
                choices = c("tab", ",", ";", "space"),
                width = "50%"
            )
        ),
        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Automatic import'",
                ns(id_list()$data_import$data_section$inputs$import_mode)
            ),
            p("Import matrices automatically using metadata"),
            numericInput(
                inputId = ns(id_list()$data_import$data_section$inputs$workers),
                label = "Maximum parallel workers",
                value = 2, min = 1, step = 1
            ),
            tags$button(
                class = "btn btn-light",
                style = "margin-bottom: 10px;",
                id = ns(id_list()$data_import$data_section$inputs$adv_op_btn),
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", ns(id_list()$data_import$data_section$inputs$adv_op_collapse)),
                `aria-expanded` = "false",
                `aria-controls` = ns(id_list()$data_import$data_section$inputs$adv_op_collapse),
                "Advanced options"
            ),
            div(
                class = "collapse",
                style = "margin-bottom: 10px;",
                id = ns(id_list()$data_import$data_section$inputs$adv_op_collapse),
                div(
                    class = "card",
                    div(
                        class = "card-body",
                        selectizeInput(
                            inputId = ns(id_list()$data_import$data_section$inputs$file_patterns),
                            options = list(create = TRUE),
                            multiple = TRUE,
                            choices = c(),
                            label = "File patterns (regular expressions)"
                        ),
                        selectizeInput(
                            inputId = ns(id_list()$data_import$data_section$inputs$matching_op),
                            multiple = FALSE,
                            choices = c("ANY", "ALL", "OPTIONAL"),
                            label = "Matching option",
                            selected = "ANY"
                        )
                    )
                )
            )
        ),
        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'Manual import'",
                ns(id_list()$data_import$data_section$inputs$import_mode)
            ),
            div(
                align = "left",
                style = "margin-top: 5px;",
                shinyWidgets::materialSwitch(
                    inputId = ns(id_list()$data_import$data_section$inputs$tidy_switch),
                    label = "My files are in tidy format",
                    value = FALSE,
                    status = "primary"
                )
            ),
            fileInput(
                inputId = ns(id_list()$data_import$data_section$inputs$files),
                label = "Choose file(s) (*.tsv, *.csv or *.xlsx)",
                accept = c(".csv", ".tsv", ".xls", ".xlsx"),
                width = "80%", multiple = TRUE
            ),
        ),
        div(
            class = "input-row-container",
            actionButton(
                inputId = ns(id_list()$data_import$data_section$inputs$import_btn),
                label = tags$div(
                    tags$span(
                        id = ns(id_list()$data_import$data_section$inputs$spinner_btn),
                        class = "spinner-border spinner-border-sm",
                        style = "display: none;"
                    ),
                    tags$span(
                        "Import"
                    )
                )
            ),
            div(
                id = ns(id_list()$data_import$data_section$outputs$import_status_cont),
                style = "margin-left: 10px;",
                uiOutput(ns(id_list()$data_import$data_section$outputs$import_status))
            )
        ),
        tags$button(
            class = "btn",
            id = ns(id_list()$data_import$data_section$inputs$details_btn),
            `data-bs-toggle` = "collapse",
            `data-bs-target` = paste0(
                "#",
                ns(id_list()$data_import$data_section$inputs$details_collapse)
            ),
            `aria-expanded` = "false",
            `aria-controls` = ns(id_list()$data_import$data_section$inputs$details_collapse),
            style = paste(
                "margin-top:10px; margin-bottom: 5px;",
                "visibility: hidden;"
            ),
            icon("info"),
            "Details"
        ),
        div(
            class = "collapse",
            id = ns(id_list()$data_import$data_section$inputs$details_collapse),
            uiOutput(ns(id_list()$data_import$data_section$outputs$details_content))
        )
    )
}

#' @importFrom shinyWidgets updateRadioGroupButtons
#' @importFrom shinyjs runjs enable disable addCssClass removeCssClass
#' @importFrom shinyvalidate InputValidator sv_required
dataImport.dataServer <- function(id, workflow, metadata, fs_aligned_af) {
    moduleServer(
        id,
        function(input, output, session) {
            session$onFlushed(function() {
                shinyjs::runjs(
                  sprintf("styleFileInput('%s')",
                          session$ns(
                            id_list()$data_import$data_section$inputs$files))
                )
            }, once = TRUE)
          to_return <- list()
          observeEvent(fs_aligned_af(), {
            # For data import toggle
            if (fs_aligned_af() == FALSE || is.null(metadata())) {
              shinyWidgets::updateRadioGroupButtons(
                session = session,
                inputId = id_list()$data_import$data_section$inputs$import_mode,
                selected = "Manual import",
                disabledChoices = "Automatic import"
              )
            } else {
              shinyWidgets::updateRadioGroupButtons(
                session = session,
                inputId = id_list()$data_import$data_section$inputs$import_mode,
                selected = "Automatic import",
                disabledChoices = NULL
              )
            }
          })
          # -------- Validation of fields
          iv_data_manual <- shinyvalidate::InputValidator$new()
          iv_data_manual$add_rule(
            id_list()$data_import$data_section$inputs$files,
            shinyvalidate::sv_required())
          iv_data_manual$enable()
          observeEvent({
            input[[id_list()$data_import$data_section$inputs$import_mode]]
            input[[id_list()$data_import$data_section$inputs$files]]
          }, {
            if (input[[id_list()$data_import$data_section$inputs$import_mode]] ==
                "Manual import" & iv_data_manual$is_valid()) {
              shinyjs::enable(id_list()$data_import$data_section$inputs$import_btn)
            } else if (
              input[[id_list()$data_import$data_section$inputs$import_mode]] ==
              "Automatic import") {
              shinyjs::enable(id_list()$data_import$data_section$inputs$import_btn)
            } else {
              shinyjs::disable(id_list()$data_import$data_section$inputs$import_btn)
            }
          })
          # -------- File import
          fail_banner <- .generate_status_banner(
            type = "danger", "Something went wrong - see check results"
          )
          warn_banner <- .generate_status_banner(
            type = "warning", "Warnings - see checks"
          )
          succ_banner <- .generate_status_banner(
            type = "success", "Data imported successfully"
          )
          matrices <- reactiveVal(NULL)
          data_ff_tbl <- reactiveVal(NULL)
          data_fi_tbl <- reactiveVal(NULL)
          data_miss_tbl <- reactiveVal(NULL)
          observeEvent(data_ff_tbl(), {
            output[[
              id_list()$data_import$data_section$outputs$checks_tbl_1
            ]] <- if (is.null(data_ff_tbl())) NULL else
              reactable::renderReactable(data_ff_tbl())
          })
          observeEvent(data_fi_tbl(), {
            output[[
              id_list()$data_import$data_section$outputs$checks_tbl_2
            ]] <- if (is.null(data_fi_tbl())) NULL else
              reactable::renderReactable(data_fi_tbl())
          })
          observeEvent(data_miss_tbl(), {
            output[[
              id_list()$data_import$data_section$outputs$checks_tbl_3
            ]] <- if (is.null(data_miss_tbl())) NULL else
              reactable::renderReactable(data_miss_tbl())
          })
          observeEvent(matrices(), {
            check_list <- if (!is.null(matrices())) {
              list(
                tbl1 = data_ff_tbl(),
                tbl2 = data_fi_tbl(),
                tbl3 = data_miss_tbl()
              )
            } else {
              NULL
            }
            workflow$set_data(df = matrices(), checks = check_list)
          })
          observeEvent(input[[
            id_list()$data_import$data_section$inputs$import_btn
          ]], {
            shinyjs::runjs(sprintf(
              "$('#%s').css('visibility', 'hidden')",
              session$ns(
                id_list()$data_import$data_section$inputs$details_btn
              )
            ))
            shinyjs::runjs(sprintf(
              "$('#%s').css('visibility', 'hidden')",
              session$ns(
                id_list()$data_import$data_section$inputs$details_collapse
              )
            ))
            shinyjs::disable(
              id_list()$data_import$data_section$inputs$import_btn)
            if (input[[
              id_list()$data_import$data_section$inputs$import_mode
            ]] == "Automatic import") {
              data_imp_results <- .data_import_and_check_auto(
                metadata = metadata(),
                matrix_annotated = input[[
                  id_list()$data_import$data_section$inputs$annotation
                ]],
                workers = as.integer(
                  input[[id_list()$data_import$data_section$inputs$workers]]),
                file_patterns = input[[
                  id_list()$data_import$data_section$inputs$file_patterns
                ]],
                match_opt = input[[
                  id_list()$data_import$data_section$inputs$matching_op
                ]],
                separator = input[[
                  id_list()$data_import$data_section$inputs$separator
                ]],
                session$ns
              )
              if ("info" %in% names(data_imp_results)) {
                data_ff_tbl(data_imp_results$info$files_found_tbl)
                data_fi_tbl(data_imp_results$info$files_imported)
              }
              matrices(data_imp_results$matrices)
              output[[
                id_list()$data_import$data_section$outputs$details_content
              ]] <- renderUI(
                .data_checks_panel(
                  status = data_imp_results$status,
                  mode = "AUTO",
                  error = data_imp_results$error,
                  info = data_imp_results$info,
                  session$ns
                )
              )
            } else {
              data_imp_results <- .data_import_and_check_manual(
                paths = input[[
                  id_list()$data_import$data_section$inputs$files
                ]]$datapath,
                tidy_format = input[[
                  id_list()$data_import$data_section$inputs$tidy_switch
                ]],
                matrix_annotated = input[[
                  id_list()$data_import$data_section$inputs$annotation
                ]],
                separator = input[[
                  id_list()$data_import$data_section$inputs$separator
                ]],
                metadata = metadata(),
                session$ns
              )
              if ("info" %in% names(data_imp_results)) {
                data_miss_tbl(data_imp_results$info$missing_data)
              }
              matrices(data_imp_results$matrices)
              output[[
                id_list()$data_import$data_section$outputs$details_content
              ]] <- renderUI(
                .data_checks_panel(
                  status = data_imp_results$status,
                  mode = "MANUAL",
                  error = data_imp_results$error,
                  info = data_imp_results$info,
                  session$ns
                )
              )
            }
            if (data_imp_results$status == 1) {
              output[[
                id_list()$data_import$data_section$outputs$import_status
              ]] <- renderUI(fail_banner)
              shinyjs::addCssClass(
                id_list()$data_import$data_section$inputs$details_btn,
                "btn-danger"
              )
              shinyjs::removeCssClass(
                id_list()$data_import$data_section$inputs$details_btn,
                class = "btn-success btn-warning"
              )
            } else if (data_imp_results$status == 2) {
              output[[
                id_list()$data_import$data_section$outputs$import_status
              ]] <- renderUI(warn_banner)
              shinyjs::addCssClass(
                id_list()$data_import$data_section$inputs$details_btn,
                "btn-warning"
              )
              shinyjs::removeCssClass(
                id_list()$data_import$data_section$inputs$details_btn,
                class = "btn-success btn-danger"
              )
            } else {
              output[[
                id_list()$data_import$data_section$outputs$import_status
              ]] <- renderUI(succ_banner)
              shinyjs::addCssClass(
                id_list()$data_import$data_section$inputs$details_btn,
                "btn-success"
              )
              shinyjs::removeCssClass(
                id_list()$data_import$data_section$inputs$details_btn,
                class = "btn-warning btn-danger"
              )
            }
            shinyjs::runjs(sprintf(
              "$('#%s').css('visibility', 'visible')",
              session$ns(
                id_list()$data_import$data_section$inputs$details_btn
              )
            ))
            shinyjs::runjs(sprintf(
              "$('#%s').css('visibility', 'visible')",
              session$ns(
                id_list()$data_import$data_section$inputs$details_collapse
              )
            ))
            shinyjs::enable(
              id_list()$data_import$data_section$inputs$import_btn)
          })
          to_return$matrices <- matrices
          return(to_return)
        }
    )
}
