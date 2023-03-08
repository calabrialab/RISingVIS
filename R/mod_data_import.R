#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      id = id,
      div("Data import", class = "display-2"),
      div(
        align = "center",
        style = "width: 80%;",
        tagAppendAttributes(
          actionButton(
            inputId = ns(ids()$data_import$inputs$next_btn),
            label = "NEXT"
          ),
          class = "btn btn-primary btn-lg",
          style = paste(
            "margin-top: 10px;"#,
            #"display: none;"
          )
        )
      )
    )
  )
}

#' data_import Server Functions
#'
#' @noRd
mod_data_import_server <- function(id, workflow, wf_flag_name, side_nav){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input[[ids()$data_import$inputs$next_btn]], {
      side_nav()$go_to_next()
    })
  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_1")

## To be copied in the server
# mod_data_import_server("data_import_1")
