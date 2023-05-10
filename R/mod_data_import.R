#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = id,
      div("Data import", class = "display-2"),
      mod_data_import_isaopt_ui(
        ns(ids()$data_import$isa_opt_section$section_id)
      ),
      mod_data_import_metadata_ui(
        ns(ids()$data_import$metadata_section$section_id)
      ),
      div(
        align = "center",
        style = "width: 80%;",
        shinyjs::disabled(
          tagAppendAttributes(
            actionButton(
              inputId = ns(ids()$data_import$inputs$next_btn),
              label = "NEXT"
            ),
            class = "btn btn-primary btn-lg",
            style = paste(
              "margin-top: 10px;"
            )
          )
        )
      )
    )
  )
}

#' data_import Server Functions
#'
#' @noRd
mod_data_import_server <- function(id, workflow, wf_flag_name, side_nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input[[ids()$data_import$inputs$next_btn]], {
      side_nav()$go_to_next()
    })
    mod_data_import_isaopt_server(
      ids()$data_import$isa_opt_section$section_id, workflow
    )
    mod_data_import_metadata_server(
      ids()$data_import$metadata_section$section_id, workflow
    )
  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_1")

## To be copied in the server
# mod_data_import_server("data_import_1")
