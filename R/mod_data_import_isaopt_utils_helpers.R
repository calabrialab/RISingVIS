#' Builds the content of the tooltip to show next to section header
#' @noRd
.isa_opt_tooltip_builder <- function() {
    div(
        "ISAnalytics can use a JSON file to set the workflow options.",
        "To know more about the allowed options and formats please take a look",
        "at the vignette at this link: ",
        tags$a(
            href = paste0(
                "https://calabrialab.github.io/ISAnalytics",
                "/articles/workflow_start.html",
                "#setting-up-your-workflow-with-dynamic-vars"
            ),
            "ISAnalytics dynamic vars"
        ),
        br(),
        "Some ISAnalytics functions produce interactive reports, specify here",
        "where those reports should be saved."
    )
}

#' Generates specific info banner to notify which options are being used
#' @noRd
.isa_status_banner_info <- function(workflow) {
    banner_type <- "primary"
    content <- if (not_null(workflow$get_ISA_options()$json_source)) {
        content <- list(
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
    } else {
        list(div(
            strong(
                "Using",
                span("default",
                    style = "text-decoration: underline;"
                ),
                "ISAnalytics settings"
            )
        ))
    }
    .generate_status_banner(type = banner_type, content = content)
}

#' Observer for the toggle button
#' @noRd
.isa_toggle_observer <- function(input, workflow, upload_status_banner) {
    observeEvent(
        input[[
            ids()$data_import$isa_opt_section$inputs$toggle1
        ]],
        {
            req(workflow())
            toggle <- input[[
                ids()$data_import$isa_opt_section$inputs$toggle1
            ]]
            if (toggle == "Use defaults" & not_null(
                workflow()$get_ISA_options()$json_source
            )) {
                ## Revert back to defaults when previous configuration was
                ## imported
                upload_status_banner(NULL)
                shinyjs::reset(
                    ids()$data_import$isa_opt_section$inputs$json_file_input
                )
                workflow()$set_ISA_options(
                    opt = list(
                        json_source = NULL,
                        options = .get_isa_is_vars(TRUE)
                    ),
                    flag = "isa-opt"
                )
                showNotification(
                    "ISAnalytics options changed",
                    type = "message"
                )
            }
        },
        ignoreInit = TRUE
    )
}

#' Observer for the json file input
#' @noRd
.isa_file_input_observer <- function(input) {
    observeEvent(
        input[[
            ids()$data_import$isa_opt_section$inputs$json_file_input
        ]],
        {
            file_input <- input[[
                ids()$data_import$isa_opt_section$inputs$json_file_input
            ]]
            load_btn_id <- ids()$data_import$isa_opt_section$inputs[[
                "upload_config_btn"
            ]]
            if (!isTruthy(file_input)) {
                shinyjs::disable(load_btn_id)
            } else {
                shinyjs::enable(load_btn_id)
            }
        }
    )
}

#' Observer for the upload button
#' @noRd
.isa_upload_btn_observer <- function(input, upload_status_banner, workflow) {
    observeEvent(input[[
        ids()$data_import$isa_opt_section$inputs$upload_config_btn
    ]], {
        load_status <- .load_isa_opts(
            input[[
                ids()$data_import$isa_opt_section$inputs$json_file_input
            ]]$datapath
        )
        upload_status_banner(load_status$status_banner)
        if (load_status$flag) {
            workflow()$set_ISA_options(
                opt = list(
                    json_source = input[[
                        ids()$data_import$isa_opt_section$inputs$json_file_input
                    ]]$datapath,
                    options = load_status$vars_list
                ),
                flag = "isa-opt"
            )
            showNotification(
                "ISAnalytics options changed",
                type = "message"
            )
        } else {
            workflow()$set_ISA_options(
                opt = list(
                    json_source = NULL,
                    options = load_status$vars_list
                ),
                flag = "isa-opt"
            )
            showNotification(
                "ISAnalytics options reverted to default",
                type = "warning"
            )
        }
    })
}

#' Loads ISAnalytics options from provided json file.
#'
#' @return A list with 3 elements: `status_banner`, `flag`, `vars_list`. If
#' flag is `TRUE` then the options were loaded successfully, otherwise
#' options are reverted to default.
#' @noRd
.load_isa_opts <- function(file_path) {
    og_op <- options()
    options("ISAnalytics.verbose" = FALSE)
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
                            HTML(stringr::str_replace_all(
                                catched, "\n", " <br/> "
                            ))
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
    vars_list <- .get_isa_is_vars(FALSE)
    to_return <- append(to_return, list(vars_list = vars_list))
    withr::defer(options(og_op))
    return(to_return)
}
