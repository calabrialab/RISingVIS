################################################################################
# Utility internal functions for data submodule                                #
################################################################################

# Tooltip builders -----------------------------------------------------------
.data_tooltip_builder <- function() {
    div(
        "Data contains the actual information about the integration sites",
        "quantifications.", br(),
        "If system alignment was performed, integration matrices can be",
        "imported automatically. ",
        "If manual import is chosen, ensure all files are imported before",
        "proceeding."
    )
}

.par_workers_builder <- function() {
    div(
        "The maximum number of parallel workers to manage file",
        "import.", br(),
        "WARNING: setting this option to a high number may cause",
        "saturation of system memory depending on hardware",
        "configuration and file size."
    )
}

.import_params_tooltip_builder <- function() {
    div(
        h6("Annotation"),
        "If matrices are annotated with the closest gene,",
        "the option should be checked. NOTE: the columns",
        "must be named accordingly to the set ISAnalytics",
        "options, check your files to ensure compliance.",
        h6("Tidy vs. sparse format"),
        "In tidy format, all PCR replicates are contained in a single",
        "column and all values associated to quantifications are",
        "stored in a separate column.", br(),
        "On the contrary, sparse format matrices (like the ones produced",
        "by VISPA2), have a column for each PCR replicate.",
        h6("Multiple quantifications"),
        "When in tidy format, integration matrices can store multiple",
        "quantification types (e.g. seqCount and fragmentEstimate),",
        "each in a separate column.",
        "In this case, you should specify the name of the columns",
        "containing the corresponding quantifications if applicable.",
        "Note that sequence count is ALWAYS required while fragment",
        "estimate is optional."
    )
}


# Utilities ------------------------------------------------------------------
.manual_opts <- function(prefix) {
    btn_id <- paste0(prefix, "-m-import-btn")
    display_id <- paste0(prefix, "-m-import-display")

    div(
        shinyWidgets::materialSwitch(
            inputId = paste0(prefix, "-m-import-tidy-switch"),
            label = span("Tidy format", class = "ps-1"),
            right = TRUE,
            value = FALSE,
            status = "primary"
        ),
        # Displays if data is in tidy format -----------------------------------
        conditionalPanel(
            condition = paste0(
                "input['", paste0(prefix, "-m-import-tidy-switch"),
                "'] === true"
            ),
            shinyWidgets::materialSwitch(
                inputId = paste0(prefix, "-m-import-multiquant-switch"),
                label = span("Multiple quantifications",
                    class = "ps-1"
                ),
                right = TRUE,
                value = TRUE,
                status = "primary"
            ),
            ## Displays if multi-quant -----------------------------------------
            conditionalPanel(
                condition = paste0(
                    "input['", paste0(prefix, "-m-import-multiquant-switch"),
                    "'] === true"
                ),
                .file_input(
                    button_id = btn_id,
                    display_id = display_id,
                    label = "Choose file",
                    multiple = FALSE
                ),
                div(
                    class = "row",
                    div(
                        class = "col",
                        "SeqCount column name:"
                    ),
                    div(
                        class = "col",
                        textInput(
                            inputId = paste0(
                                prefix,
                                "-m-import-seqcount-cname"
                            ),
                            label = NULL,
                            value = "seqCount"
                        )
                    )
                ),
                div(
                    class = "row",
                    div(
                        class = "col",
                        "FragmentEstimate column name:"
                    ),
                    div(
                        class = "col",
                        textInput(
                            inputId = paste0(
                                prefix,
                                "-m-import-fe-cname"
                            ),
                            label = NULL,
                            value = "fragmentEstimate"
                        )
                    )
                )
            )
        ),
        # Displays if data is in sparse format or tidy but single quant ---
        conditionalPanel(
            condition = paste0(
                "input['", paste0(prefix, "-m-import-tidy-switch"),
                "'] === false || input['",
                paste0(prefix, "-m-import-multiquant-switch"),
                "'] === false"
            ),
            div(
                class = "row",
                div(
                    class = "col d-flex align-items-center",
                    "SeqCount matrix"
                ),
                div(
                    class = "col",
                    .file_input(
                        button_id = paste0(btn_id, "-seqcount"),
                        display_id = paste0(display_id, "-seqcount"),
                        label = "Choose file",
                        multiple = FALSE
                    )
                )
            ),
            div(
                class = "row",
                div(
                    class = "col d-flex align-items-center",
                    "FragmentEstimate matrix"
                ),
                div(
                    class = "col",
                    .file_input(
                        button_id = paste0(btn_id, "-fe"),
                        display_id = paste0(display_id, "-fe"),
                        label = "Choose file",
                        multiple = FALSE
                    )
                )
            )
        )
    )
}

.generate_manual_import_panel <- function(workflow, ns) {
    pools <- workflow$metadata |>
        dplyr::distinct(.data[[workflow$pool_col]]) |>
        dplyr::pull(.data[[workflow$pool_col]])
    pools_ui <- purrr::map(pools, ~ {
        div(
            class = "row border border-1 rounded-1 p-2 mb-2",
            strong(.x, class = "mb-2"),
            .manual_opts(ns(.x))
        )
    })
    div(
        class = "container",
        pools_ui
    )
}
