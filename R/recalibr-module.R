# Recalibration module ---------------------------------------------------------

RecUI <- function(id) {
  ns <- NS(id)
  div(
    id = id,
    style = "display: none;",
    width = "100%",
    div("Recalibration",
      class = "display-2"
    ),
    div(
      class = "alert alert-info",
      style = "width: 80%",
      icon(name = "circle-info"),
      "Warning: the recalibration step might require several",
      "minutes or hours for bigger datasets"
    ),
    h3("Recalibration parameters"),
    div(
      class = "input-row-container",
      tagAppendAttributes(
        numericInput(
          inputId = ns(id_list()$recalibration$inputs$threshold),
          label = "Threshold",
          value = 4,
          step = 1,
          width = "20%"
        ),
        style = "margin-right: 10px;"
      ),
      selectizeInput( # MUST UPDATE WHEN UPDATING ISAOPTIONS
        inputId = ns(id_list()$recalibration$inputs$is_tags),
        label = "IS identity tags",
        choices = ISAnalytics::mandatory_IS_vars(TRUE)$tag[
          ISAnalytics::mandatory_IS_vars(TRUE)$tag != "locus"
        ],
        selected = ISAnalytics::mandatory_IS_vars(TRUE)$tag[
          ISAnalytics::mandatory_IS_vars(TRUE)$tag != "locus"
        ],
        multiple = TRUE,
        width = "50%"
      )
    ),
    div(
      class = "input-row-container",
      selectizeInput(
        inputId = ns(id_list()$recalibration$inputs$criteria),
        label = "Keep criteria",
        choices = c("max_value", "keep_first"),
        selected = "max_value",
        multiple = FALSE,
        width = "25%"
      ),
      tagAppendAttributes(
        numericInput(
          inputId = ns(id_list()$recalibration$inputs$workers),
          label = "Parallel workers",
          value = 4,
          step = 1,
          width = "20%"
        ),
        style = "margin-left: 10px;"
      ),
    ),
    div(
      align = "left",
      shinyWidgets::materialSwitch(
        inputId = ns(id_list()$recalibration$inputs$rec_map_switch),
        label = "Save recalibration map",
        value = FALSE,
        status = "primary"
      ),
      div(
        align = "left",
        id = ns(id_list()$recalibration$inputs$rec_map_container),
        class = "input-mock-container",
        shinyFiles::shinyDirButton(
          id = ns(id_list()$recalibration$inputs$rec_map_path),
          title = "Save map to folder",
          label = "Choose",
          icon = icon("folder"),
          buttonType = "primary",
          class = "choose-dir-btn"
        ),
        tagAppendAttributes(
          textOutput(
            outputId = ns(id_list()$recalibration$outputs$rec_map_display)
          ),
          class = "text-out-mock-enabled",
          style = "width: 50%;"
        )
      )
    ),
    div(
      class = "input-row-container",
      style = paste(
        "justify-content: center;",
        "margin-top: 3rem;"
      ),
      tagAppendAttributes(
        actionButton(
          inputId = ns(id_list()$recalibration$inputs$rec_btn),
          label = "Recalibrate",
          width = "20%"
        ),
        class = "btn btn-secondary",
        style = "margin-right: 1rem;"
      ),
      tagAppendAttributes(
        actionButton(
          inputId = ns(id_list()$recalibration$inputs$skip_btn),
          label = "Skip",
          width = "20%"
        ),
        class = "btn btn-grey"
      )
    ),
    div(
      style = "margin-top: 10px; width:80%;",
      align = "left",
      uiOutput(ns(id_list()$recalibration$outputs$info_panel))
    ),
    div(
      align = "center",
      style = "width: 80%;",
      tagAppendAttributes(
        actionButton(
          inputId = ns(id_list()$recalibration$inputs$next_btn),
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

RecServer <- function(id, workflow) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input[[
      id_list()$recalibration$inputs$rec_map_switch
      ]], {
        if (isTRUE(input[[
        id_list()$recalibration$inputs$rec_map_switch
        ]])) {
          shinyjs::runjs(
            paste0(
              sprintf(
                "enablePseudoInput('%s', ['%s'], ['%s'])",
                session$ns(
                  id_list()$recalibration$inputs$rec_map_container
                ),
                session$ns(
                  id_list()$recalibration$inputs$rec_map_path
                ),
                session$ns(
                  id_list()$recalibration$outputs$rec_map_display
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
                  id_list()$recalibration$inputs$rec_map_container
                ),
                session$ns(
                  id_list()$recalibration$inputs$rec_map_path
                ),
                session$ns(
                  id_list()$recalibration$outputs$rec_map_display
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
                                 id_list()$recalibration$inputs$rec_map_path,
                                 roots = volumes,
                                 session = session
      )
      map_file_path <- reactive({
        if (isTRUE(input[[
          id_list()$recalibration$inputs$rec_map_switch
        ]])) {
          if (is.integer(input[[
            id_list()$recalibration$inputs$rec_map_path
          ]])) {
            NULL
          } else {
            shinyFiles::parseDirPath(volumes, input[[
              id_list()$recalibration$inputs$rec_map_path
            ]])
          }
        } else {
          NULL
        }
      })
      output[[
        id_list()$recalibration$outputs$rec_map_display
      ]] <- renderPrint({
        if (!is.null(map_file_path())) {
          map_file_path()
        } else {
          cat("Choose dir...")
        }
      })
      rec_data <- reactiveVal(NULL)
      observeEvent(rec_data(), {
        workflow$set_recalibr(rec_data())
        if (!is.null(rec_data())) {
          shinyjs::show(id = id_list()$recalibration$inputs$next_btn)
        } else {
          shinyjs::hide(id = id_list()$recalibration$inputs$next_btn)
        }
      })
      observeEvent(input[[
      id_list()$recalibration$inputs$rec_btn
      ]], {
        shinyjs::disable(id = id_list()$recalibration$inputs$rec_btn)
        shinyjs::disable(id = id_list()$recalibration$inputs$skip_btn)
        output[[
          id_list()$recalibration$outputs$info_panel
        ]] <- NULL
        to_rec <- workflow$get_data()
        recalibr_results <- .recalibrate(
          data = to_rec,
          map = input[[
            id_list()$recalibration$inputs$rec_map_switch
          ]],
          map_path = map_file_path(),
          workers = input[[
            id_list()$recalibration$inputs$workers
          ]],
          is_tags = input[[
            id_list()$recalibration$inputs$is_tags
          ]],
          criteria = input[[
            id_list()$recalibration$inputs$criteria
          ]],
          session = session
        )
        info_banner <- .recalibr_info_panel(recalibr_results)
        output[[
          id_list()$recalibration$outputs$info_panel
        ]] <- renderUI(info_banner)
        rec_data(recalibr_results$res$result)
        observeEvent(input[[
          id_list()$recalibration$inputs$next_btn
        ]], {
          shinyjs::runjs(sprintf("
                   $('#%s').slideUp('fast')
                   ", id_list()$recalibration$inputs$next_btn))
          shinyjs::show(id = id_list()$plot_section$section_id, asis = TRUE)
        })

        shinyjs::enable(id = id_list()$recalibration$inputs$rec_btn)
        shinyjs::enable(id = id_list()$recalibration$inputs$skip_btn)
      })
    }
  )
}
