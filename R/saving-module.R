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
                                   choices = list(".tsv" = ".tsv", ".csv" = ".csv"),
                                   selected = ".tsv"))
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
            file_download <- reactiveValues(data = NULL, ext = NULL, 
                                            pool = NULL, sep = NULL,
                                            path = NULL)
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
            # Data format
            conv_data <- filtered_data
            observeEvent(input[[id_list()$saving_section$inputs$data_form]], {
                if (input[[id_list()$saving_section$inputs$data_form]] == 1) {
                    # Convert to sparse
                    conv_data <- .tidy_to_sparse(filtered_data, session)
                } else if (input[[id_list()$saving_section$inputs$data_form]] == 2) {
                    conv_data <- filtered_data
                 }
            })
            # Separate by pool
            final_data <- conv_data
            observeEvent(input[[id_list()$saving_section$inputs$by_pool]], {
                if (isTRUE(input[[id_list()$saving_section$inputs$by_pool]])) {
                    final_data <- .split_by_pool(conv_data, session)
                } else if (isFALSE(input[[id_list()$saving_section$inputs$by_pool]])) {
                    final_data <- conv_data
                }
            })
            # File format
            f_format <- reactive({
                if (input[[
                    id_list()$saving_section$inputs$file_form]] == 1) {
                    as.character(".tsv")
                } else if (input[[
                    id_list()$saving_section$inputs$file_form]] == 2) {
                    as.character(".csv")
                }
            })
            # Download
            data <- data.frame(x = 1, y = 1:10)
            output[[id_list()$saving_section$outputs$download_mat]] <-
                downloadHandler(
                    filename = function() { 
                        paste0(shinyFiles::parseDirPath(volumes, input[[
                            id_list()$saving_section$inputs$dir_path]]), 
                            "/filtered_integration_matrix",
                            input[[id_list()$saving_section$inputs$file_form]])
                    }, 
                    content = function(file) {
                        write.csv(data, file, row.names = FALSE)
                    }
                )
            # File format
            # observeEvent(input[[id_list()$saving_section$inputs$file_form]], {
            #     if (input[[id_list()$saving_section$inputs$file_form]] == 1) {
            #         file_download$ext <- ".tsv"
            #         file_download$sep <- "\t"
            #     } else if (input[[id_list()$saving_section$inputs$file_form]] == 2) {
            #         file_download$ext <- ".csv"
            #         file_download$sep <- ","
            #     }
            # })
            # file_download$data <- final_data
            # Download
            # if (!is.list(final_data)) {
            #     output[[id_list()$saving_section$outputs$download_mat]] <- 
            #         downloadHandler(
            #             filename = function() {
            #                 paste0(file_download$path, 
            #                        "/filtered_integration_matrix",
            #                        file_download$ext)
            #             },
            #             content = function(file) {
            #                 write.table(file_download$data, file, 
            #                             quote = FALSE,
            #                             row.names = FALSE, 
            #                             sep = file_download$sep)
            #             }
            #         )
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
                } else {
                    NULL
                }
            })
        })
    return()
}
