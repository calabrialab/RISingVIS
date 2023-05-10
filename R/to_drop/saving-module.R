# Saving module ---------------------------------------------------------------
SavingUI <- function(id) {
    ns <- NS(id)
    div(
        id = id,
        style = "display: none;",
        width = "100%",
        div("Save results",
            class = "display-2"
        ),
        div(
            align = "left",
            id = ns(id_list()$saving_section$inputs$dir_container),
            class = "input-mock-container",
            shinyFiles::shinyDirButton(
                id = ns(id_list()$saving_section$inputs$dir_path),
                title = "Directory",
                label = "Browse",
                icon = icon("folder"),
                buttonType = "primary",
                class = "choose-dir-btn"
            ),
            tagAppendAttributes(
                textOutput(
                    outputId = ns(id_list()$saving_section$outputs$dir_display)
                ),
                class = "text-out-mock-enabled"
            )
        ),
        div(
            br(),
            fluidRow(
                h2("Data"),
                column(3,
                       radioButtons(inputId = ns(id_list()$saving_section$inputs$data_form),
                                    label = "Data format",
                                    choices = list("Sparse" = 1, "Tidy" = 2),
                                    selected = 2)),
                column(3,
                       materialSwitch(inputId = ns(id_list()$saving_section$inputs$by_pool),
                                      label = "Separate by pool",
                                      status = "primary",
                                      right = FALSE)),
                column(3,
                       selectInput(inputId = ns(id_list()$saving_section$inputs$file_form),
                                   label = "File format",
                                   choices = list(".tsv" = 1, ".csv" = 2),
                                   selected = ".tsv"))
            )
        ),
        # div(
        #     h2("Report"),
        #     materialSwitch(inputId = ns(id_list()$saving_section$inputs$save_rep),
        #                    label = "Save a report of the workflow",
        #                    status = "primary",
        #                    right = FALSE),
        #     selectInput(inputId = ns(id_list()$saving_section$inputs$rep_form),
        #                 label = "File format",
        #                 choices = list("html" = 1),
        #                 selected = 1)
        # ),
        div(
            br(),
            align = "center",
            style = "width: 80%;",
            actionButton(
                inputId = ns(id_list()$saving_section$inputs$save_btn),
                label = "SAVE"
            ),
        ),
    )
}

SavingServer <- function(id, workflow, filtered_data, matrix_cols) {
    moduleServer(
        id,
        function(input, output, session) {
            # Choose directory
            volumes <- c(
                Home = fs::path_home(), "R Installation" = R.home(),
                shinyFiles::getVolumes()()
            )
            shinyFiles::shinyDirChoose(input,
                                       id_list()$saving_section$inputs$dir_path,
                                       roots = volumes,
                                       session = session
            )
            saving_path <- reactive({
                if (is.integer(input[[
                        id_list()$saving_section$inputs$dir_path
                    ]])) {
                    NULL
                } else {
                    shinyFiles::parseDirPath(volumes, input[[
                        id_list()$saving_section$inputs$dir_path
                    ]])
                }
            })
            output[[id_list()$saving_section$outputs$dir_display]] <-
                renderPrint({
                    if (!is.null(saving_path())) {
                        saving_path()
                    } else {
                        cat("Choose dir...")
                    }
                })

            # Test data
            # data("integration_matrix")
            # data("association_file")
            # filtered_data <- integration_matrix %>%
            #     dplyr::left_join(association_file,
            #                      by = "CompleteAmplificationID")
            # matrix_cols <- c("chr", "integration_locus", "strand",
            #                  "CompleteAmplificationID", "Value")

            # Download
            observeEvent(input[[id_list()$saving_section$inputs$save_btn]], {
                observeEvent(input[[id_list()$saving_section$inputs$by_pool]], {
                    if (isTRUE(input[[id_list()$saving_section$inputs$by_pool]])) {
                        pool_data <- .split_by_pool(filtered_data, session)
                        tab_names <- names(pool_data)
                        lapply(tab_names, function(x) {
                            curr <- pool_data[[x]]
                            if (input[[id_list()$saving_section$inputs$data_form]] == 1) {
                                # Convert to sparse
                                final_data <- .tidy_to_sparse(curr, session)
                            } else if (input[[id_list()$saving_section$inputs$data_form]] == 2) {
                                final_data <- curr %>%
                                    dplyr::select(dplyr::all_of(matrix_cols))

                            }
                            observeEvent(input[[id_list()$saving_section$inputs$file_form]], {
                                if (input[[id_list()$saving_section$inputs$file_form]] == 1) {
                                    write.table(final_data, file = paste0(
                                        saving_path(), "/",
                                        x, "_",
                                        "filtered_integration_matrix.tsv"),
                                        sep = "\t", quote = FALSE, row.names = FALSE)
                                } else if (input[[id_list()$saving_section$inputs$file_form]] == 2) {
                                    write.csv(final_data, file = paste0(
                                        saving_path(), "/",
                                        x, "_",
                                        "filtered_integration_matrix.csv"),
                                        row.names = FALSE)
                                }
                                showNotification("Data download complete",
                                                 type = "message")
                            })
                        })
                    }
                    else {
                        if (input[[id_list()$saving_section$inputs$data_form]] == 1) {
                            # Convert to sparse
                            final_data <- .tidy_to_sparse(filtered_data, session)
                        } else if (input[[id_list()$saving_section$inputs$data_form]] == 2) {
                            final_data <- filtered_data %>%
                                dplyr::select(dplyr::all_of(matrix_cols))
                        }
                        observeEvent(input[[id_list()$saving_section$inputs$file_form]], {
                            if (input[[id_list()$saving_section$inputs$file_form]] == 1) {
                                write.table(final_data, file = paste0(
                                    saving_path(),
                                    "/filtered_integration_matrix.tsv"),
                                    sep = "\t", quote = FALSE, row.names = FALSE)
                            } else if (input[[id_list()$saving_section$inputs$file_form]] == 2) {
                                write.csv(final_data, file = paste0(
                                    saving_path(),
                                    "/filtered_integration_matrix.csv"),
                                    row.names = FALSE
                                )
                            }
                            showNotification("Data download complete",
                                             type = "message")
                        })
                    }
                })
            })
    })
}
