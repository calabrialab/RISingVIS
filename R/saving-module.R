# Saving module ---------------------------------------------------------------

SavingUI <- function(id) {
    ns <- NS(id)
    div(
        id = id,
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
                                   selected = 1))
            )
        ),
        div(
            h2("Report"),
            materialSwitch(inputId = ns(id_list()$saving_section$inputs$save_rep),
                           label = "Save a report of the workflow",
                           status = "primary",
                           right = FALSE),
            selectInput(inputId = ns(id_list()$saving_section$inputs$rep_form),
                        label = "File format",
                        choices = list("html" = 1),
                        selected = 1)
        ),
        div(
            align = "center",
            style = "width: 80%;",
            downloadButton(
                outputId = ns(id_list()$saving_section$outputs$download_mat),
                label = "SAVE"
            )
        )
    )
}

SavingServer <- function(id, workflow, filtered_data) {
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
            # Data format - sparse or tidy
            observeEvent(input[[id_list()$saving_section$inputs$data_form]], {
                if (input[[id_list()$saving_section$inputs$data_form]] == 1) {
                    # Convert to sparse
                    filtered_data <- .tidy_to_sparse(filtered_data, session)
                 } 
                # else if (
                #     input[[id_list()$saving_section$inputs$data_form]] == 2) {
                #     NULL
                # } 
            })
            # Separate by pool
            observeEvent(input[[id_list()$saving_section$inputs$by_pool]], {
                if (isTRUE(input[[id_list()$saving_section$inputs$by_pool]])) {
                    filtered_data <- .split_by_pool(filtered_data, session)
                } 
                # else {
                #     NULL
                # }
            })
            # File format
            file_format <- reactive({
                if (input[[id_list()$saving_section$inputs$file_form]] == 1) {
                    ".tsv"
                } else {
                    ".csv"
                }
            })
            file_sep <- reactive({
                if (input[[id_list()$saving_section$inputs$file_form]] == 1) {
                    "\t"
                } else {
                    ","
                }
            })
            # Save filtered matrix
            if (!is.list(filtered_data)) {
                output$download_mat <- 
                    downloadHandler(
                        filename = function() {
                            paste0(saving_path(),
                                   "/filtered_integration_matrix", file_format())
                        },
                        content = function(file) {
                            write.table(filtered_data, file,
                                        quote = FALSE,
                                        sep = file_sep(), 
                                        col.names = NA)
                        }
                    )
            } else {
                sapply(names(filtered_data), function(x) {
                    output$download_mat <- 
                        downloadHandler(
                            filename = function() {
                                paste0(saving_path(),
                                       "/filtered_integration_matrix", 
                                       x, file_format())
                            },
                            content = function(file) {
                                write.table(filtered_data, file,
                                            quote = FALSE,
                                            sep = file_sep(), 
                                            col.names = NA)
                            }
                        )
                })
            }
              # observeEvent(input[[id_list()$saving_section$inputs$save_btn]], {
              #   if (!is.list(filtered_data)) {
              #       observeEvent(input[[id_list()$saving_section$inputs$file_form]], {
              #           if (input[[id_list()$saving_section$inputs$file_form]] == 1) {
              #               # .tsv
              #               id_list()$saving_section$outputs$download_mat <-
              #                   downloadHandler(
              #                       filename = function() {
              #                           paste0(saving_path(),
              #                                  "/filtered_integration_matrix.tsv")
              #                       },
              #                       content = function(file) {
              #                           write.table(filtered_data, file,
              #                                     quote = FALSE,
              #                                     sep = "\t",
              #                                     col.names = NA)
              #                       }
              #                   )
              #           } else if (
              #               input[[id_list()$saving_section$inputs$file_form]] == 2) {
              #               # .csv
              #               id_list()$saving_section$outputs$download_mat <-
              #                   downloadHandler(
              #                       filename = function() {
              #                           paste0(saving_path(),
              #                                  "/filtered_integration_matrix.csv")
              #                       },
              #                       content = function(file) {
              #                           write.csv(filtered_data, file,
              #                                     row.names = FALSE)
              #                       }
              #                   )
              #           }
              #        })
              #    }
                # else {
                #     observeEvent(input[[id_list()$saving_section$inputs$file_form]], {
                #         if (input[[id_list()$saving_section$inputs$file_form]] == 1) {
                #             # .tsv
                #             sapply(names(filtered_data), function(x) {
                #                 write.table(filtered_data[[x]],
                #                             file = paste0(saving_path,
                #                                           "/filtered_integration_matrix_",
                #                                           x, ".tsv"),
                #                             quote = FALSE,
                #                             sep = '\t',
                #                             col.names = NA)
                #             })
                #         } else if (
                #             input[[id_list()$saving_section$inputs$file_form]] == 2) {
                #             # .csv
                #             sapply(names(filtered_data), function(x) {
                #                 write.csv(filtered_data[[x]],
                #                           file = paste0(saving_path,
                #                                         "/filtered_integration_matrix_",
                #                                         x, ".csv"),
                #                           row.names = FALSE)
                #             })
                #         }
                #     })
                # }
                # Report
                observeEvent(input[[id_list()$saving_section$inputs$save_rep]], {
                    if (isTRUE(input[[id_list()$saving_section$inputs$save_rep]])) {
                        template <- system.file("rmd",
                                                "report_temp.rmd",
                                                package = "RISingVIS")

                        id_list()$saving_section$outputs$download_repo <-
                            downloadHandler(
                                filename = "report_RISingVIS.html",
                                content = function(file) {
                                    rmarkdown::render(
                                        input = template,
                                        #params = params,
                                        output_format = "html_document",
                                        output_file = "report_RISingVIS",
                                        output_dir = saving_path(),
                                        envir = new.env(),
                                        quiet = TRUE
                                    )
                                }
                            )
                    }
                    # else {
                    #     NULL
                    # }
                })
              # })
        })
    return()
}
