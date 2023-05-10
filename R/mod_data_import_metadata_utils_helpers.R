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
        "Control cell lines are cell lines with known",
        "integration sites. They are used by RISingVIS",
        "to check for the presence of contamination among samples.",
        br(),
        "It is possible to add a custom control line by clicking",
        "on the button 'Add': you must enter a unique name and",
        "you can import a tabular file that contains the known",
        "unique integration sites for the cell line.",
        br(),
        "Be aware that the file must be compliant to the",
        "specs you chose for ISAnalytics options (it must contain",
        "all mandatory variables).",
        br(),
        "Also note that the name of the cell line is matched exactly",
        "against the independent sample identifier: if more than",
        "one field identifies a sample use an underscore to concatenate",
        "the fields (e.g. CEM38_2)"
    )
}

#' Code at UI level for the control cell line selectize input
#' @noRd
.cell_line_selectize <- function(ns) {
    div(
        class = "d-flex w-100",
        tagAppendAttributes(
            selectizeInput(
                inputId = ns(
                    ids()$data_import$metadata_section$inputs$control_line
                ),
                label = .section_name_with_tooltip(
                    section_name = "Control cell line",
                    type = div,
                    tooltip_content_builder = .control_line_tooltip_builder
                ),
                choices = names(default_cell_lines()),
                multiple = TRUE
            ),
            class = "flex-grow-1"
        ),
        tags$button(
            id = ns(ids()$data_import$metadata_section$inputs$add_cell_line),
            class = paste(
                "btn btn-gray add-cl-btn text-nowrap",
                "shiny-bound-input action-button"
            ),
            name = "Add cell line",
            span(
                icon("plus"),
                span("Add")
            )
        )
    )
}

#' Creates the modal for the control cell line addition
#' and attaches observer to the add button
#' @noRd
.add_cl_btn_observer <- function(ns, input, workflow) {
    modal <- .add_cl_modal(ns, input, workflow)
    observeEvent(input[[
        ids()$data_import$metadata_section$inputs$add_cell_line
    ]], {
        showModal(modal)
    })
}



#' Defines the modal with inputs for control cell line addition
#' including observers for buttons
#' @noRd
.add_cl_modal <- function(ns, input, workflow) {
    modal_cl <- modalDialog(
        title = "New control cell line",
        footer = tagList(
            tags$button(
                id = ns(
                    ids()$data_import$metadata_section$inputs[[
                        "dismiss_cl_modal_btn"
                    ]]
                ),
                class = "btn btn-gray action-button",
                "Cancel"
            ),
            shinyjs::disabled(actionButton(
                inputId = ns(
                    ids()$data_import$metadata_section$inputs[[
                        "confirm_cl_modal_btn"
                    ]]
                ),
                label = "Create"
            ))
        ),
        textInput(
            inputId = ns(ids()$data_import$metadata_section$inputs$cl_name),
            label = "Name",
            placeholder = "Enter cell line name"
        ),
        fileInput(
            inputId = ns(
                ids()$data_import$metadata_section$inputs$cl_file
            ),
            label = "Known ISs file",
            placeholder = "Select file",
            multiple = FALSE,
            width = "100%"
        )
    )

    file_cleared <- reactiveVal(TRUE)
    known_iss_tbl <- reactiveVal(NULL)

    observeEvent(
        input[[ids()$data_import$metadata_section$inputs$cl_file]],
        {
            file_cleared(FALSE)
        }
    )
    observeEvent(
        input[[ids()$data_import$metadata_section$inputs[[
            "dismiss_cl_modal_btn"
        ]]]],
        {
            shinyjs::reset(ids()$data_import$metadata_section$inputs$cl_name)
            shinyjs::reset(ids()$data_import$metadata_section$inputs$cl_file)
            file_cleared(TRUE)
            known_iss_tbl(NULL)
            removeModal()
        }
    )
    observeEvent(
        {
            input[[
                ids()$data_import$metadata_section$inputs$cl_name
            ]]
            file_cleared()
        },
        {
            text_in <- input[[
                ids()$data_import$metadata_section$inputs$cl_name
            ]]
            if (not_null(text_in) &&
                nchar(text_in) > 0 & !file_cleared()) {
                shinyjs::enable(
                    id = ids()$data_import$metadata_section$inputs[[
                        "confirm_cl_modal_btn"
                    ]]
                )
            } else {
                shinyjs::disable(
                    id = ids()$data_import$metadata_section$inputs[[
                        "confirm_cl_modal_btn"
                    ]]
                )
            }
        }
    )

    observeEvent(input[[
        ids()$data_import$metadata_section$inputs[[
            "confirm_cl_modal_btn"
        ]]
    ]], {
        # Check if the name is already used
        if (input[[
            ids()$data_import$metadata_section$inputs$cl_name
        ]] %in% names(workflow()$get_available_cl())) {
            shinyWidgets::sendSweetAlert(
                title = "Something went wrong",
                type = "error",
                text = paste(
                    "The name provided is already used for another",
                    "control cell line. Please choose another name"
                ),
                btn_labels = "OK",
                btn_colors = "#dee2e6"
            )
            return()
        }
        # Import the file
        .import_known_iss(
            path = input[[
                ids()$data_import$metadata_section$inputs$cl_file
            ]]$datapath,
            res_reactive = known_iss_tbl,
            mand_vars_lu = workflow()$get_ISA_options()$options[[
                "mandatory_is_vars"
            ]]
        )
        if (not_null(known_iss_tbl())) {
            custom <- ControlLine$new(
                name = input[[
                    ids()$data_import$metadata_section$inputs$cl_name
                ]],
                known_iss = known_iss_tbl()
            )
            workflow()$add_control_cell_line(
                cl = custom,
                flag = "cl-list"
            )
            shinyjs::reset(ids()$data_import$metadata_section$inputs$cl_name)
            shinyjs::reset(ids()$data_import$metadata_section$inputs$cl_file)
            file_cleared(TRUE)
            known_iss_tbl(NULL)
            shinyWidgets::sendSweetAlert(
                title = "Success",
                type = "success",
                text = paste(
                    "The new control cell line has been successfully",
                    "added to the list"
                ),
                btn_labels = "OK",
                btn_colors = "#dee2e6"
            )
            removeModal()
        }
    })

    return(modal_cl)
}

#' Activates the load button only when required fields are valid
#' @noRd
.activate_load_observer <- function(input_validator, input) {
    observe({
        if (input_validator$is_valid()) {
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

#' Creates observers for the load button. If the root directory is empty,
#' it first shows an alert to confirm the action.
#' Keep in mind: no other field other than root folder can be empty,
#' so no other checks needed
#' @noRd
.load_metadata_btn_observer <- function(input, output, workflow) {
    gargoyle::init("meta-root-ok")
    observe({
        gargoyle::watch("meta-root-ok")
        if (isTruthy(input[[ids()$data_import$metadata_section$inputs[[
            "empty_root_alert"
        ]]]])) {
            # TODO : confirm action (import no align)
        }
    })
    observeEvent(
        input[[ids()$data_import$metadata_section$inputs[[
            "empty_root_alert"
        ]]]],
        {
            gargoyle::trigger("meta-root-ok")
        }
    )
    observeEvent(
        input[[ids()$data_import$metadata_section$inputs[[
            "import_btn"
        ]]]],
        {
            root_fold <- input[[
                ids()$data_import$metadata_section$inputs$root_dir
            ]]
            if (is.integer(root_fold)) {
                # Send confirm alert if root folder is not set
                shinyWidgets::confirmSweetAlert(
                    inputId = ids()$data_import$metadata_section$inputs[[
                        "empty_root_alert"
                    ]],
                    title = "Root folder not set",
                    text = paste(
                        "Root folder for file system alignment is not set.",
                        "Proceed anyway?"
                    ),
                    confirmBtnText = "Yes",
                    cancelBtnText = "No",
                    type = "info"
                )
            } else {
                # TODO: import
            }
        }
    )
}

#' Performs additional checks on metadata, namely:
#' - if the metadata is empty
#' - if the required columns are present
#' - if the independent sample id is present
#' - if control cell lines are present
#' @noRd
.meta_additional_checks <- function(imported_meta,
                                    control_cell_line,
                                    indep_sample_id,
                                    isa_opt) {
    add_checks <- list()
    add_checks$meta_empty <- nrow(imported_meta) == 0
    col_specs <- isa_opt |>
        dplyr::filter(.data$tag %in% .global_required_af_tags())
    add_checks$req_cols_present <- col_specs$names[!col_specs$names %in%
        colnames(imported_meta)]
    add_checks$ind_smpl_id <- all(indep_sample_id %in% colnames(imported_meta))
    if (add_checks$meta_empty == FALSE &
        purrr::is_empty(add_checks$req_cols_present)) {
        pool_col <- col_specs |>
            dplyr::filter(.data$tag == "vispa_concatenate") |>
            dplyr::pull(.data$names)
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
        add_checks$control_cl <- control_cl
    }
    return(add_checks)
}

#' Converts the result of the import step to a combination of numeric statuses
#' so it's easier to interpret and process by other functions
#' @noRd
## STATUS LEGEND:
## - 0: AF imported, all checks ok
## - 1: AF not imported due to error
## - 2: AF imported but with warnings to display
##
## SUB-STATUS:
## - 1.0: generic error
## - 1.1: fail with simple error - no additional info (empty metadata)
## - 1.2: fail because no pools found
.convert_import_status <- function(import_status) {
    result <- list()
    # Case import failed entirely ----------------------------------------------
    if (import_status$status == FALSE) {
        result$status <- 1
        result$substatus <- 0
        result$errors <- div(HTML(
            stringr::str_replace_all(
                import_status$error, "\n",
                " <br/> "
            )
        ))
        return(result)
    }
    # Import successful --------------------------------------------------------
    ## Error if metadata is empty ----------------------------------------------
    if (import_status$meta_empty) {
        result$status <- 1
        result$substatus <- 1
        result$error <- div(
            strong("Metadata is empty"), br(),
            "Did you set filters correctly?"
        )
        return(result)
    }
    ## Error if required columns are not present -------------------------------
    if (!purrr::is_empty(import_status$req_cols_present)) {
        result$status <- 1
        result$substatus <- 1
        result$error <- div(
            strong("Metadata is missing the following required columns"),
            br(),
            list_to_li(import_status$req_cols_present)
        )
        return(result)
    }
    ## Error if independent sample id is not present --------------------------
    if (import_status$ind_smpl_id == FALSE) {
        result$status <- 1
        result$substatus <- 1
        result$error <- div(
            strong("Missing independent sample id"),
            br(),
            p(
                "Independent sample id",
                " was not found in metadata columns"
            )
        )
        return(result)
    }
    ## Error if control cell lines are not present ----------------------------
    if (all(import_status$control_cl$control_line_present == FALSE)) {
        result$status <- 1
        result$substatus <- 1
        result$error <- div(
            strong("Missing control cell line"),
            br(),
            p("Control cell line selected is missing in all pools")
        )
        return(result)
    }
}

#' Actually imports metadata from file, creates the report and updates the
#' workflow if import was successful
#' @noRd
.load_metadata <- function(path,
                           root,
                           control_cell_line,
                           sep,
                           dates_format,
                           filter,
                           indep_sample_id,
                           isa_report,
                           isa_report_path,
                           workflow) {
    # Set local options for ISAnalytics
    wf_options <- workflow$get_ISA_options()
    names(wf_options) <- paste0("ISAnalytics.", names(wf_options))
    withr::local_options(append(
        list(
            ISAnalytics.reports = isa_report,
            ISAnalytics.verbose = FALSE
        ),
        wf_options
    ))
    # Env to extract internal checks data
    checks_res <- rlang::new_environment()
    ## Import VISPA2 iss if root is not NULL (alignment requested)
    iss_import <- !is.null(root)
    iss_failed <- FALSE
    # Perform import step - catch errors and signal additional warnings
    import_ok <- tryCatch(
        {
            withCallingHandlers(
                {
                    af <- ISAnalytics::import_association_file(
                        path = path, root = root,
                        dates_format = dates_format,
                        separator = sep,
                        filter_for = filter,
                        import_iss = iss_import,
                        report_path = isa_report_path,
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
            # Signal import is done
            list(
                status = TRUE,
                error = NULL,
                correctly_aligned = correctly_aligned
            )
        },
        error = function(e) {
            # Signal import failed
            list(status = FALSE, error = e$message)
        }
    )
    if (import_ok$status == FALSE) {
        # Import failed, return error
        return(.convert_import_status(import_ok))
    }
    import_ok <- append(import_ok, .meta_additional_checks(
        imported_meta = af,
        control_cell_line = control_cell_line,
        indep_sample_id = indep_sample_id,
        isa_opt = workflow$get_ISA_options()$af_specs
    ))
}
