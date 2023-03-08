#' recalibration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_recalibration_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      div(
        id = id,
        div("Recalibration",
            class = "display-2"
        )
      )
    )
  )
}

#' recalibration Server Functions
#'
#' @noRd
mod_recalibration_server <- function(id, workflow,  wf_flag_name, side_nav){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_recalibration_ui("recalibration_1")

## To be copied in the server
# mod_recalibration_server("recalibration_1")
