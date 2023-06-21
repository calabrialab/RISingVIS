#' data_import_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_data_ui <- function(id) {
  ns <- NS(id)
  tab_1_btn_id <- ns(ids()$data_import$data_section$inputs$tab_1_btn)
  tab_1_content_id <- ns(
    ids()$data_import$data_section$inputs$tab_1_content
  )
  tab_2_btn_id <- ns(ids()$data_import$data_section$inputs$tab_2_btn)
  tab_2_content_id <- ns(
    ids()$data_import$data_section$inputs$tab_2_content
  )
  import_mode_picker_id <- ns(
    ids()$data_import$data_section$inputs$import_mode
  )
  div(
    id = id,
    style = "width: 80%;",
    class = "card mb-2",
    div(
      class = "card-header",
      tags$ul(
        class = "nav nav-pills card-header-pills",
        role = "tablist",
        tags$li(
          class = "nav-item",
          role = "presentation",
          tags$button(
            id = tab_1_btn_id,
            class = "nav-link active",
            `data-bs-toggle` = "pill",
            `data-bs-target` = paste0("#", tab_1_content_id),
            type = "button",
            role = "tab",
            `aria-controls` = tab_1_content_id,
            `aria-selected` = "true",
            "Import params"
          )
        ),
        tags$li(
          class = "nav-item",
          role = "presentation",
          shinyjs::disabled(
            tags$button(
              id = tab_2_btn_id,
              class = "nav-link",
              `data-bs-toggle` = "pill",
              `data-bs-target` = paste0("#", tab_2_content_id),
              type = "button",
              role = "tab",
              `aria-controls` = tab_2_content_id,
              `aria-selected` = "false",
              "Import summary"
            )
          )
        )
      )
    ),
    div(
      class = "card-body tab-content",
      div(
        id = tab_1_content_id,
        class = "tab-pane fade show active",
        role = "tabpanel",
        `aria-labelledby` = tab_1_btn_id,
        .section_name_with_tooltip(
          section_name = "Data",
          type = h2,
          tooltip_content_builder = .data_tooltip_builder
        ),
        div(
          class = "container",
          div(
            class = "row",
            div(
              class = "col-3",
              shinyWidgets::pickerInput(
                inputId = import_mode_picker_id,
                label = "Import mode",
                choices = c("Automatic", "Manual"),
                choicesOpt = list(
                  disabled = c("Automatic", "Manual") %in% "Automatic"
                ),
                width = "100%"
              )
            ),
            div(
              class = "col card",
              div(
                class = "card-body",
                .section_name_with_tooltip(
                  section_name = "Import parameters",
                  type = h4,
                  tooltip_content_builder = .import_params_tooltip_builder
                ),
                checkboxInput(
                  inputId = ns(
                    ids()$data_import$data_section$inputs$annotation
                  ),
                  label = "Matrices are annotated",
                  value = TRUE
                ),
                conditionalPanel(
                  condition = paste0(
                    "input['", import_mode_picker_id,
                    "'] === 'Manual'"
                  ),
                  uiOutput(
                    outputId = ns(
                      ids()$data_import$data_section$outputs[[
                        "manual_ui_panel"
                      ]]
                    )
                  )
                ),
                conditionalPanel(
                  condition = paste0(
                    "input['", import_mode_picker_id,
                    "'] === 'Automatic'"
                  ),
                  selectizeInput(
                    inputId = ns(
                      ids()$data_import$data_section$inputs$separator
                    ),
                    label = "File separator",
                    choices = c(
                      "tab" = "\t", "comma" = ",",
                      "semilcolon" = ";", "space" = " "
                    )
                  ),
                  numericInput(
                    inputId = ns(
                      ids()$data_import$data_section$inputs$workers
                    ),
                    label = .section_name_with_tooltip(
                      section_name = "Max parallel workers",
                      type = p,
                      tooltip_content_builder = .par_workers_builder
                    ),
                    value = 2,
                    min = 1,
                    step = 1
                  ),
                  shinyWidgets::pickerInput(
                    inputId = ns(
                      ids()$data_import$data_section$inputs$quant_picker
                    ),
                    label = "Quantification types",
                    choices = list("seqCount", "fragmentEstimate"),
                    multiple = TRUE,
                    options = shinyWidgets::pickerOptions(
                      actionsBox = TRUE
                    )
                  )
                )
              )
            )
          )
        )
      ),
      div(
        id = tab_2_content_id,
        class = "tab-pane fade",
        role = "tabpanel",
        `aria-labelledby` = tab_2_btn_id,
        h2("Import summary report"),
        uiOutput(
          outputId = ns(
            ids()$data_import$data_section$outputs[["data_report"]]
          )
        )
      )
    )
  )
}

#' data_import_data Server Functions
#'
#' @noRd
mod_data_import_data_server <- function(id, workflow) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Import method management -----------------------------------------------
    ## Activate option of auto import only when aligment was performed -------
    observe({
      req(workflow())
      gargoyle::watch("end-import-meta")
      aligned <- workflow()$was_system_aligned()
      if (is.null(aligned)) {
        aligned <- FALSE
      }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = ids()$data_import$data_section$inputs$import_mode,
        choices = c("Automatic", "Manual"),
        choicesOpt = list(
          disabled = c(!aligned, FALSE)
        )
      )
    })
    # Manual import panel management ------------------------------------------
    manual_import_panel_id <- ids()$data_import$data_section$outputs[[
      "manual_ui_panel"
    ]]
    manual_pool_files_ids <- workflow()$metadata |>
      dplyr::distinct(.data[[workflow()$pool_col]]) |>
      dplyr::pull(workflow()$pool_col)
    manual_import_panel_content <- reactive({
      req(workflow())
      gargoyle::watch("update-import-manual-panel")
      if (is.null(workflow()$metadata)) {
        return(
          div(
            class = "text-muted",
            "Import metadata file first"
          )
        )
      }
      .generate_manual_import_panel(
        workflow = workflow(),
        ns = ns
      )
    })
    output[[manual_import_panel_id]] <- renderUI({
      manual_import_panel_content()
    })

    # Automatic import panel management ---------------------------------------
  })
}

## To be copied in the UI
# mod_data_import_data_ui("data_import_data_1")

## To be copied in the server
# mod_data_import_data_server("data_import_data_1")
