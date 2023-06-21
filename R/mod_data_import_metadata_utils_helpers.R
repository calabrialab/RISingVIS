################################################################################
# Utility internal functions for metadata sub-module                           #
################################################################################

# Tooltip builders -------------------------------------------------------------

#' Builds the tooltip content for the metadata section
#' @noRd
.metadata_tooltip_builder <- function() {
    div(
        "The metadata file is a tabular file that contains",
        "information about the samples and the experiments",
        "that produced them.",
        br(),
        "The metadata file must contain at least the following",
        "ISAnalytics tags:",
        list_to_li(.global_required_af_tags()),
        br(),
        "It is highly recommended to perform file system alignment",
        "by providing a root folder.",
        "To know more about the file system alignment please take a look",
        "at the vignette at this link: ",
        tags$a(
            href = paste0(
                "https://calabrialab.github.io/ISAnalytics",
                "/articles/workflow_start.html",
                "#introduction-to-isanalytics-import-functions-family"
            ),
            "ISAnalytics import functions"
        )
    )
}

#' Builds the tooltip content for the project name section
#' @noRd
.project_name_tooltip_builder <- function() {
    paste(
        "The name of the project exacly as it is written in the",
        "dedicated field of the metadata file.",
        "The match is case sensitive."
    )
}

#' Builds the tooltip content for the dates format section
#' @noRd
.dates_format_tooltip_builder <- function() {
    div(
        "A general format that is used to parse all columns",
        "with a generic date type. It is possible to specify",
        "different formats for different columns directly in",
        "ISAnalytics options.",
        br(),
        "The formats are specified using the",
        "specifications from the package",
        tags$a(
            href = paste0(
                "https://lubridate.tidyverse.org/",
                "reference/parse_date_time.html"
            ),
            "lubridate"
        ),
        "."
    )
}

#' Builds the tooltip content for the sample id section
#' @noRd
.sample_id_tooltip_builder <- function() {
    div(
        "The name of the column(s) that uniquely identifies",
        "each independent sample. This column(s) will be used",
        "to group the data by sample.",
        br(),
        "Two independent samples, in principle, should not",
        "share integration sites.",
        br(),
        "In the vast majority of cases, the sample id corresponds",
        "to the column holding the subject identifier."
    )
}

#' Builds the tooltip content for the control line section
#' @noRd
.control_line_tooltip_builder <- function() {
    div(
        "Select one or more control cell lines.",
        br(),
        "To modify or delete cell lines go to the page",
        "'Control cell lines' in the navigation bar."
    )
}

# Other utilities --------------------------------------------------------------
#' Renders the choices in the control line selectize input as pills (like shown)
#' in the db page
#' @noRd
.render_cl_choices <- function(names, type = c("default", "user")) {
    pills <- if (type == "default") {
        purrr::map(names, ~ {
            sprintf(
                '<span class="badge rounded-pill text-bg-accent1">%s</span>',
                .x
            )
        }) |> purrr::list_c()
    } else {
        purrr::map(names, ~ {
            sprintf(
                '<span class="badge rounded-pill text-bg-accent2">%s</span>',
                .x
            )
        }) |> purrr::list_c()
    }
    return(pills)
}

# Observers --------------------------------------------------------------------

#' Activates the load button only when required fields are valid, disables
#' it otherwise.
#' @param input_validator An object of class shinyvalidate::InputValidator that
#' contains the validators for the required fields.
#' @param custom_validators A list of custom validators, aka reactiveValues that
#' can be only TRUE or FALSE.
#' @param input The shiny input object.
#' @noRd
.activate_load_observer <- function(
    input_validator,
    custom_validators,
    input) {
    all_valid <- reactive(
        input_validator$is_valid() &&
            all(custom_validators |> purrr::map_lgl(~ .x()))
    )
    observe({
        if (all_valid()) {
            shinyjs::enable(
                id = ids()$data_import$metadata_section$inputs[[
                    "import_btn"
                ]]
            )
        } else {
            shinyjs::disable(
                id = ids()$data_import$metadata_section$inputs[[
                    "import_btn"
                ]]
            )
        }
    })
}

#' Creates observers for the load button.
#' @noRd
.load_metadata_btn_observer <- function(
    input, folder_reactive) {
    # Component ids ----------------------------------------------------------
    confirm_alert_id <- ids()$data_import$metadata_section$inputs[[
        "empty_root_alert"
    ]]
    load_btn_id <- ids()$data_import$metadata_section$inputs[["import_btn"]]
    observeEvent(
        input[[confirm_alert_id]],
        {
            if (isTruthy(input[[confirm_alert_id]])) {
                gargoyle::trigger("begin-import-meta")
            }
        }
    )
    ## Attach load button observer ------------------------------------------
    observeEvent(input[[load_btn_id]], {
        # Check if root is set or not ---------------------------------------
        if (is.null(folder_reactive())) {
            ## Send confirm alert if root folder is not set -----------------
            shinyWidgets::confirmSweetAlert(
                inputId = confirm_alert_id,
                title = "Root folder not set",
                text = paste(
                    "Root folder for file system alignment is not set.",
                    "Proceed anyway?"
                ),
                btn_colors = sweetalert_btn_colors()
            )
        } else {
            gargoyle::trigger("begin-import-meta")
        }
    })
}

#' Performs quiet metadata import. If there are errors or
#' warnings shows an alert
#' @noRd
.meta_import <- function(
    path, root, dates_format,
    project_name, project_col,
    sep, report_path, checks_env,
    react_val) {
    withr::local_options(list(ISAnalytics.verbose = FALSE))
    ## Initialize environment for internal checks ---------------------------
    checks_env(rlang::new_environment())
    # Tries to import metadata quietly -----------------------------------------
    quiet_meta_import <- purrr::quietly(
        purrr::safely(ISAnalytics::import_association_file)
    )
    proj_filter <- list(project_name)
    names(proj_filter) <- project_col
    import_iss <- if (is.null(root)) FALSE else TRUE
    imported <- quiet_meta_import(
        path = path,
        root = root,
        dates_format = dates_format,
        filter_for = proj_filter,
        separator = sep,
        import_iss = import_iss,
        report_path = report_path,
        checks_env = checks_env
    ) |> purrr::list_flatten(name_spec = "{inner}")
    # If there are errors show an alert ----------------------------------------
    if (not_null(imported$error)) {
        shinyWidgets::sendSweetAlert(
            title = "Something went wrong",
            text = imported$error$message,
            type = "error",
            btn_colors = "#dee2e6",
            btn_labels = "OK"
        )
        react_val(NULL)
        gargoyle::trigger("end-import-meta")
        return(FALSE)
    }
    if (not_null(imported$warnings) & !purrr::is_empty(imported$warnings)) {
        shinyWidgets::alert(
            title = "Warning",
            text = imported$warnings,
            type = "warning",
            btn_colors = "#dee2e6",
            btn_labels = "OK"
        )
    }
    if (not_null(report_path)) {
        report_ok <- any(
            stringr::str_detect(imported$messages, "Report correctly saved")
        )
        saved_to <- stringr::str_extract(
            imported$messages[report_ok],
            "Report saved to: (.+)",
            group = 1
        )
        saved_to <- saved_to[!is.na(saved_to)] |> paste0(collapse = "")
        if (report_ok) {
            showNotification(
                ui = div(
                    strong("ISAnalytics report correctly saved"),
                    br(),
                    p("You can find it at:"),
                    p(
                        class = "overflow-scroll",
                        saved_to
                    )
                ),
                duration = NULL,
                type = "message"
            )
        } else {
            showNotification(
                ui = div(
                    h6("ISAnalytics report production failed"),
                    p(
                        "There was an unexpected error while producing",
                        "the report."
                    )
                ),
                duration = NULL,
                type = "error"
            )
        }
    }
    react_val(imported$result)
}

#' Performs additional checks on metadata, namely:
#' - if the metadata is empty
#' - if the required columns are present
#' - if the independent sample id is present
#' - if control cell lines are present and compliant with mand vars
#' @param imported_meta the imported metadata as a data frame
#' @param control_cell_line the control cell line(s) as a character vector
#' @param indep_sample_id the independent sample id column(s) as a character
#' vector
#' @param isa_opt the ISA options as list of dataframes
#' @param cldb the control line database object
#' @return a list of checks
#' - meta_empty: TRUE if the metadata is empty, FALSE otherwise -- always
#' present
#' - req_cols_present: the names of the required columns that are not present --
#' always present
#' - ind_smpl_id: TRUE if the independent sample id is present, FALSE otherwise
#'   -- always present
#' - control_cl: a data frame with this structure -- present only if previous
#' checks passed:
#'   - <pool column>: contains the names of the pools in the project
#'   - control_line: contains the names of the control lines selected
#'   - present: TRUE if corresponding control line is present in the pool,
#'              FALSE otherwise
#'   - compliant: TRUE if corresponding control line contains all mandatory IS
#'                vars (as per ISAnalytics options), FALSE otherwise
#' @noRd
.meta_additional_checks <- function(imported_meta,
                                    control_cell_line,
                                    indep_sample_id,
                                    isa_opt,
                                    add_checks_react,
                                    cldb) {
    # Init checks list ---------------------------------------------------------
    add_checks <- list()
    # Perform checks -----------------------------------------------------------
    ## Metadata is empty?
    add_checks$meta_empty <- nrow(imported_meta) == 0
    ## Required columns present? If not, which ones are missing?
    col_specs <- isa_opt$af_specs |>
        dplyr::filter(.data$tag %in% .global_required_af_tags())
    add_checks$req_cols_present <- col_specs$names[!col_specs$names %in%
        colnames(imported_meta)]
    ## Independent sample id fields present?
    add_checks$ind_smpl_id <- all(indep_sample_id %in% colnames(imported_meta))
    if (add_checks$meta_empty == FALSE &
        purrr::is_empty(add_checks$req_cols_present) &
        add_checks$ind_smpl_id == TRUE) {
        ## Get the pool column
        pool_col <- col_specs |>
            dplyr::filter(.data$tag == "vispa_concatenate") |>
            dplyr::pull(.data$names)
        ## Check for each cell line if it's present in the pool
        ## and if it's compliant
        control_cl <- imported_meta |>
            tidyr::unite(
                col = "ind_id",
                dplyr::all_of(indep_sample_id)
            ) |>
            dplyr::distinct(dplyr::across(dplyr::all_of(c(
                pool_col,
                "ind_id"
            )))) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(pool_col))) |>
            dplyr::reframe(
                control_line = control_cell_line,
                present = control_cell_line %in% .data[["ind_id"]]
            )
        line_compliant <- purrr::map(control_cell_line, ~ {
            tibble::tibble_row(
                control_line = .x,
                compliant = all(
                    isa_opt$mandatory_is_vars$names %in%
                        colnames(cldb$get_by_name(.x)$known_iss)
                )
            )
        }) |> purrr::list_rbind()
        control_cl <- control_cl |>
            dplyr::left_join(line_compliant, by = "control_line")
        add_checks$control_cl <- control_cl
    }
    add_checks_react(add_checks)
}

#' Evaluates the additional checks performed on metadata -
#' if critical checks fail, send a failure alert,
#' if issues are identified in control lines open the modal to
#' ask user input, otherwise return
#' @param checks the list of additional checks
#' @param input the shiny input object
#' @param output the shiny output object
#' @param session the shiny session object
#' @param cl_choices_react the reactive object containing the control line
#' choices (reactiveValues) - choices are NOT returned, they are set here
#' @return Nothing
#' @noRd
.handle_add_checks <- function(
    checks,
    input,
    output,
    session,
    cl_issues_modal,
    cl_choices_react) {
    # Send error alerts if there are errors ------------------------------------
    ## Metadata is empty?
    if (checks$meta_empty) {
        shinyWidgets::sendSweetAlert(
            title = "Something went wrong",
            text = paste(
                "The imported metadata is empty.",
                "Please check the file and the parameters and try again."
            ),
            type = "error"
        )
        gargoyle::trigger("end-import-meta")
        return()
    }
    ## Required columns present? If not, which ones are missing?
    if (!purrr::is_empty(checks$req_cols_present)) {
        shinyWidgets::sendSweetAlert(
            title = "Something went wrong",
            text = paste(
                "The following required columns are missing:",
                list_to_li(checks$req_cols_present)
            ),
            type = "error",
            html = TRUE
        )
        gargoyle::trigger("end-import-meta")
        return()
    }
    ## Independent sample id fields present?
    if (!checks$ind_smpl_id) {
        shinyWidgets::sendSweetAlert(
            title = "Something went wrong",
            text = paste(
                "Some or all independent sample id field(s) is/are missing.",
                "Please check the file and the parameters and try again."
            ),
            type = "error"
        )
        gargoyle::trigger("end-import-meta")
        return()
    }
    ## Check the control lines -------------------------------------------------
    ### If everything ok simply return
    cl_tmp_ok <- checks$control_cl |>
        dplyr::group_by(dplyr::across(
            dplyr::all_of(colnames(checks$control_cl)[1])
        )) |>
        dplyr::summarise(
            present = all(.data$present == TRUE),
            compliant = all(.data$compliant == TRUE),
            control_lines = paste(.data$control_line, collapse = ", "),
            .groups = "drop"
        )
    if (length(unique(cl_tmp_ok$control_lines)) == 1 &&
        all(cl_tmp_ok$present) &&
        all(cl_tmp_ok$compliant)) {
        cl_choices_react$cl_summary_tbl <- checks$control_cl
        cl_choices_react$cl_selection_tbl <- cl_tmp_ok |>
            dplyr::select(-"present", -"compliant") |>
            dplyr::mutate(
                control_lines = as.list(
                    stringr::str_split(.data$control_lines, ", ")[[1]]
                ),
                data = TRUE
            )
        cl_choices_react$exclude_data <- NULL
        cl_choices_react$exclude_cl <- NULL
        gargoyle::trigger("cl-issues-proceed")
        return()
    }
    ### If not, open a modal to let the user decide how to handle the issues
    showModal(cl_issues_modal)
}

#' Finalizes the import procedure by setting the results in the workflow
#' object and sending a notification
#' @noRd
.finalize_import <- function(
    import_results, was_aligned, file_path,
    project, separator, date_format, ind_sample_id,
    workflow, progress_obj) {
    progress_obj$inc(1, detail = "Producing summary report...")
    wf_params <- list(
        content = import_results$meta,
        was_aligned = was_aligned,
        file_path = file_path,
        project = project,
        separator = separator,
        date_format = date_format,
        ind_sample_id = ind_sample_id,
        lines_choices = import_results$checks$cl_choices,
        internal_checks = import_results$checks$internal_checks
    )
    workflow$set_metadata(wf_params)
    showNotification(
        ui = div(
            strong("Metadata imported successfully!"),
            br(),
            "You can check the import summary report in the ",
            strong("Import summary"), " tab."
        ),
        duration = NULL,
        type = "message"
    )
}

# Control line issues modal ----------------------------------------------------
#' Renders the reactable corresponding to the control line checks.
#' The table has this structure:
#' - <pool col>: name of the pool
#' - control_line: name of the control line
#' - present: whether the control line is present in the metadata for this pool
#' - compliant: whether the control line is compliant with ISAnalytics mand vars
#' @noRd
.cl_checks_table <- function(cl_table) {
    tbl <- reactable::reactable(
        data = cl_table,
        groupBy = colnames(cl_table)[1],
        defaultColDef = reactable::colDef(
            align = "center"
        ),
        columns = list(
            present = reactable::colDef(
                html = TRUE,
                name = "Present",
                cell = .render_boolean_col(as_icon = TRUE)
            ),
            compliant = reactable::colDef(
                html = TRUE,
                name = "Compliant",
                cell = .render_boolean_col(as_icon = TRUE)
            ),
            control_line = reactable::colDef(
                name = "Control line"
            )
        )
    )
    return(tbl)
}

#' Renders the reactable corresponding to the control line selection.
#' The table has this structure:
#' - <pool col>: name of the pool
#' - control_lines: list of control lines chosen for this pool
#' - data: whether the data for this pool should be kept or excluded
#' @noRd
.cl_post_sel_tbl <- function(sel_tbl) {
    render_data_col <- function(value) {
        if (value) {
            div(
                class = "badge rounded-pill bg-success",
                "Keep"
            )
        } else {
            div(
                class = "badge rounded-pill bg-danger",
                "Exclude"
            )
        }
    }

    tbl <- reactable::reactable(
        data = sel_tbl,
        defaultColDef = reactable::colDef(
            align = "center"
        ),
        columns = list(
            data = reactable::colDef(
                name = "Data",
                cell = render_data_col
            ),
            control_lines = reactable::colDef(
                name = "Control lines"
            )
        )
    )
    return(tbl)
}

#' Builds the UI portion of the modal for control line issues
#' @noRd
.cl_issues_modal_ui <- function(session) {
    tbl_out_id <- ids()$data_import$metadata_section$outputs[[
        "modal_cl_issues_tbl"
    ]]
    strategy_picker_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_stategy"
    ]]
    strategy_desc_id <- ids()$data_import$metadata_section$outputs[[
        "modal_cl_issues_strategy_desc"
    ]]
    out_sel_table_id <- ids()$data_import$metadata_section$outputs[[
        "modal_cl_issues_selection_tbl"
    ]]
    manual_input_panel_id <- ids()$data_import$metadata_section$outputs[[
        "modal_cl_issues_manual_panel"
    ]]
    radio_btns_data_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_radio_data"
    ]]
    radio_btns_cl_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_radio_cl"
    ]]
    confirm_btn_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_confirm_btn"
    ]]
    dismiss_btn_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_dismiss_btn"
    ]]

    cl_excluded_choices <- list(
        "Treat control line as normal sample" = "as_sample",
        "Remove all data (after recalibration)" = "remove_after",
        "Remove all data (before recalibration)" = "remove_before"
    )
    # Defines the ui of the modal ----------------------------------------------
    modal_ui <- modalDialog(
        title = "Control lines issues",
        size = "l",
        footer = tagList(
            tags$button(
                id = session$ns(dismiss_btn_id),
                class = "btn btn-gray",
                `data-dismiss` = "modal",
                `data-bs-dismiss` = "modal",
                "Cancel"
            ),
            actionButton(
                inputId = session$ns(confirm_btn_id),
                label = "Confirm"
            )
        ),
        div(
            div(
                class = "text-muted",
                "Here is a summary of issues detected.", br(),
                "The table reports, for each pool and each control line,",
                "if it was found in the metadata by matching the name against",
                "the independent sample id, and if it is compliant with",
                "the specified ISAnalytics mandatory IS vars configuration.",
                "Expand the rows for details.",
                br(), "Please choose below how to handle the issues.", br(),
                "If you want to modify the selection of control lines,",
                "or edit the cell lines, simply click \"Cancel\" and",
                "go back to the previous step."
            ),
            hr(),
            reactable::reactableOutput(
                outputId = session$ns(tbl_out_id)
            ),
            hr(),
            h5("Strategy"),
            div(
                class = "container",
                div(
                    class = "row",
                    div(
                        class = "col-4",
                        shinyWidgets::pickerInput(
                            inputId = session$ns(strategy_picker_id),
                            label = "Choose logic",
                            choices = list(
                                "Present & compliant all pools" = paste(
                                    "present_compliant_pools"
                                ),
                                "Present & compliant each" = paste(
                                    "present_compliant_each"
                                ),
                                "Manual" = paste(
                                    "manual"
                                )
                            )
                        )
                    ),
                    div(
                        class = "col-8",
                        div(
                            class = "text-muted mb-3",
                            textOutput(
                                outputId = session$ns(strategy_desc_id)
                            )
                        ),
                        shinyjs::hidden(
                            uiOutput(
                                outputId = session$ns(manual_input_panel_id)
                            )
                        ),
                        reactable::reactableOutput(
                            outputId = session$ns(out_sel_table_id)
                        )
                    )
                )
            ),
            h5("How to deal with excluded data?"),
            div(
                class = "text-muted",
                "Specify how to treat excluded pool data by selecting",
                "one of the options below."
            ),
            div(
                class = "mt-3",
                radioButtons(
                    inputId = session$ns(radio_btns_data_id),
                    label = NULL,
                    choices = list(
                        "Remove after recalibration" = "after_rec",
                        "Remove before recalibration" = "before_rec"
                    ),
                    selected = "after_rec"
                )
            ),
            h5("How to deal with excluded control lines?"),
            div(
                class = "text-muted",
                "Specify how to treat excluded control cell lines data.",
                "This refers to all the control lines that were present",
                "but were excluded with strategy or because non compliant.",
                br(),
                "NOTE: if a cell line is present but not compliant, you may",
                "want to edit the control lines database instead of removing",
                "it. To do so simply cancel the import and go in the control",
                "lines page."
            ),
            div(
                class = "mt-3",
                radioButtons(
                    inputId = session$ns(radio_btns_cl_id),
                    label = NULL,
                    choices = cl_excluded_choices,
                    selected = "remove_after"
                )
            )
        )
    )
    return(modal_ui)
}

#' Dynamically generates a series of inputs corresponding to the available
#' pools/lines for manual selection.
#' @noRd
.input_panel_generator <- function(cl_table, ns) {
    pool_col <- colnames(cl_table)[1]
    by_pool_tbl <- cl_table |>
        dplyr::group_by(dplyr::across(dplyr::all_of(pool_col))) |>
        dplyr::summarise(
            lines = list(
                dplyr::pick(dplyr::all_of(
                    c(
                        "control_line", "present",
                        "compliant"
                    )
                )) |>
                    dplyr::filter(
                        .data$present == TRUE,
                        .data$compliant == TRUE
                    ) |>
                    dplyr::pull(.data$control_line)
            ),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            n_lines = purrr::map_int(lines, length)
        )

    input_row_gen <- function(...) {
        row <- list(...)
        pool_text_classes <- if (row$n_lines == 0) {
            "text-muted"
        } else {
            ""
        }
        div(
            class = "row",
            id = ns(paste0("row-", row[[pool_col]])),
            div(
                class = "col-3 overflow-scroll",
                p(
                    class = pool_text_classes,
                    row[[pool_col]]
                )
            ),
            div(
                class = "col-3",
                if (row$n_lines == 0) {
                    shinyjs::disabled(
                        checkboxInput(
                            inputId = ns(paste0("checkbox-", row[[pool_col]])),
                            label = NULL
                        )
                    )
                } else {
                    checkboxInput(
                        inputId = ns(paste0("checkbox-", row[[pool_col]])),
                        label = NULL
                    )
                }
            ),
            div(
                class = "col",
                if (row$n_lines == 0) {
                    shinyjs::disabled(
                        shinyWidgets::pickerInput(
                            inputId = ns(paste0("picker-", row[[pool_col]])),
                            label = NULL,
                            choices = row$lines,
                            multiple = TRUE,
                            options = list(
                                `actions-box` = TRUE
                            ),
                            width = "100%"
                        )
                    )
                } else {
                    shinyWidgets::pickerInput(
                        inputId = ns(paste0("picker-", row[[pool_col]])),
                        label = NULL,
                        choices = row$lines,
                        multiple = TRUE,
                        options = list(
                            `actions-box` = TRUE
                        ),
                        width = "100%"
                    )
                }
            )
        )
    }
    input_list <- purrr::pmap(by_pool_tbl, input_row_gen)
    inputs_ids <- purrr::map(by_pool_tbl[[pool_col]], ~ {
        list(
            checkbox = paste0("checkbox-", .x),
            picker = paste0("picker-", .x)
        )
    }) |> purrr::set_names(by_pool_tbl[[pool_col]])
    input_panel <- div(
        class = "container",
        div(
            class = "row mb-2",
            div(
                class = "col-3 text-truncate",
                strong("Pool")
            ),
            div(
                class = "col-2 text-truncate",
                strong("Analyze")
            ),
            div(
                class = "col text-truncate",
                strong("Control lines")
            )
        ),
        input_list
    )
    return(list(ui = input_panel, ids = inputs_ids))
}

#' Defines the server-side logic for the modal
#' @noRd
.handle_cl_issues_modal <- function(
    input, output, session,
    cl_issues_choices,
    add_checks) {
    tbl_out_id <- ids()$data_import$metadata_section$outputs[[
        "modal_cl_issues_tbl"
    ]]
    strategy_picker_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_stategy"
    ]]
    strategy_desc_id <- ids()$data_import$metadata_section$outputs[[
        "modal_cl_issues_strategy_desc"
    ]]
    out_sel_table_id <- ids()$data_import$metadata_section$outputs[[
        "modal_cl_issues_selection_tbl"
    ]]
    manual_input_panel_id <- ids()$data_import$metadata_section$outputs[[
        "modal_cl_issues_manual_panel"
    ]]
    radio_btns_data_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_radio_data"
    ]]
    radio_btns_cl_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_radio_cl"
    ]]
    confirm_btn_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_confirm_btn"
    ]]
    dismiss_btn_id <- ids()$data_import$metadata_section$inputs[[
        "modal_cl_issues_dismiss_btn"
    ]]

    cl_table <- reactive({
        req(add_checks)
        add_checks()$control_cl
    })
    output[[tbl_out_id]] <- reactable::renderReactable({
        req(cl_table())
        .cl_checks_table(cl_table())
    })
    # Defines the logic of the modal -------------------------------------------
    observeEvent(input[[dismiss_btn_id]], {
        gargoyle::trigger("end-import-meta")
        return()
    })
    strategy_def <- list(
        present_compliant_pools = paste(
            "Keep only the control lines that are present and compliant",
            "across all pools. All pools analyses will include all of the",
            "selected control lines."
        ),
        present_compliant_each = paste(
            "Keep all compliant cell lines that are present in at least",
            "one pool.",
            "Each pool will include the analyses relative to the cell",
            "lines that are actually present in that pool."
        ),
        manual = paste(
            "Choose manually how to handle the issues.",
            "Use the inputs below and visualise a preview of the",
            "final selection"
        )
    )
    manual_inputs_panel <- reactive({
        req(cl_table())
        .input_panel_generator(
            cl_table = cl_table(),
            ns = session$ns
        )
    })
    manual_inputs_panel_ids <- reactive({
        req(manual_inputs_panel())
        manual_inputs_panel()$ids
    })
    manual_inputs_panel_ui <- reactive({
        req(manual_inputs_panel())
        manual_inputs_panel()$ui
    })
    output[[manual_input_panel_id]] <- renderUI({
        req(manual_inputs_panel_ui())
        manual_inputs_panel_ui()
    })
    selection_df <- reactiveVal(NULL)
    output[[out_sel_table_id]] <- reactable::renderReactable({
        req(selection_df())
        .cl_post_sel_tbl(selection_df())
    })
    observeEvent(selection_df(), {
        req(selection_df())
        reactable::updateReactable(
            outputId = out_sel_table_id,
            data = selection_df()
        )
    })
    gargoyle::init("up-selection-tbl")
    observeEvent(input[[strategy_picker_id]], {
        req(input[[strategy_picker_id]])
        output[[strategy_desc_id]] <- renderText({
            strategy_def[[input[[strategy_picker_id]]]]
        })
        if (input[[strategy_picker_id]] == "manual") {
            shinyjs::show(id = manual_input_panel_id)
            gargoyle::trigger("up-selection-tbl")
        } else {
            purrr::walk(
                purrr::list_flatten(
                    manual_inputs_panel_ids()
                ), ~ shinyjs::reset(id = .x)
            )
            shinyjs::hide(id = manual_input_panel_id)
            gargoyle::trigger("up-selection-tbl")
        }
    })
    observe({
        req(input[[strategy_picker_id]])
        req(cl_table())
        gargoyle::watch("up-selection-tbl")
        pool_col <- colnames(cl_table())[1]
        if (input[[strategy_picker_id]] == "present_compliant_pools") {
            temp <- cl_table() |>
                dplyr::filter(
                    .data$present == TRUE,
                    .data$compliant == TRUE
                ) |>
                dplyr::group_by(.data$control_line) |>
                dplyr::summarise(
                    n_pools = dplyr::n_distinct(.data[[pool_col]]),
                    .groups = "drop"
                ) |>
                dplyr::filter(.data$n_pools == length(unique(
                    cl_table()[[pool_col]]
                )))
            temp2 <- cl_table() |>
                dplyr::semi_join(temp, by = "control_line") |>
                dplyr::group_by(dplyr::across(
                    dplyr::all_of(pool_col)
                )) |>
                dplyr::summarise(
                    control_lines = list(.data$control_line),
                    .groups = "drop"
                ) |>
                dplyr::mutate(
                    data = TRUE
                )
            excluded_pools <- setdiff(cl_table()[[1]], temp2[[1]])
            for (pool in excluded_pools) {
                temp2 <- temp2 |>
                    dplyr::add_row(
                        !!pool_col := pool,
                        control_lines = NULL,
                        data = FALSE
                    )
            }
            selection_df(temp2)
        } else if (input[[strategy_picker_id]] == "present_compliant_each") {
            temp <- cl_table() |>
                dplyr::filter(
                    .data$present == TRUE,
                    .data$compliant == TRUE
                ) |>
                dplyr::group_by(dplyr::across(
                    dplyr::all_of(pool_col)
                )) |>
                dplyr::summarise(
                    control_lines = list(.data$control_line),
                    .groups = "drop"
                ) |>
                dplyr::mutate(
                    data = TRUE
                )
            excluded_pools <- setdiff(cl_table()[[1]], temp[[1]])
            for (pool in excluded_pools) {
                temp <- temp |>
                    dplyr::add_row(
                        !!pool_col := pool,
                        control_lines = NULL,
                        data = FALSE
                    )
            }
            selection_df(temp)
        } else {
            temp <- purrr::map2(
                manual_inputs_panel_ids(),
                names(manual_inputs_panel_ids()), ~ {
                    tibble::tibble_row(
                        !!pool_col := .y,
                        control_lines = list(input[[.x$picker]]),
                        data = input[[.x$checkbox]]
                    )
                }
            ) |> purrr::list_rbind()
            selection_df(temp)
        }
    })
    observeEvent(input[[confirm_btn_id]], {
        req(selection_df())
        if (all(selection_df()$data == FALSE)) {
            shinyWidgets::sendSweetAlert(
                title = "Error",
                text = paste(
                    "At least one pool has to be",
                    "included in the analysis"
                ),
                btn_colors = sweetalert_btn_colors()[1],
                type = "error"
            )
            return()
        }
        any_null_cl <- selection_df() |>
            dplyr::filter(.data$data == TRUE) |>
            dplyr::pull(.data$control_lines) |>
            purrr::map_lgl(~ is.null(.x)) |>
            any()
        if (any_null_cl) {
            shinyWidgets::sendSweetAlert(
                title = "Error",
                text = paste(
                    "All included pools must have at least",
                    "one control line associated"
                ),
                btn_colors = sweetalert_btn_colors()[1],
                type = "error"
            )
            return()
        }
        cl_issues_choices$cl_summary_tbl <- cl_table()
        cl_issues_choices$cl_selection_tbl <- selection_df()
        cl_issues_choices$exclude_data <- input[[radio_btns_data_id]]
        cl_issues_choices$exclude_cl <- input[[radio_btns_cl_id]]
        gargoyle::trigger("cl-issues-proceed")
        removeModal()
    })
}
