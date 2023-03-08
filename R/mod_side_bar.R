#' side_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_side_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      id = id,
      class = "w-100 text-center",
      tags$button(
        id = ns(ids()$side_bar$inputs$wf_name_txt_btn),
        class = paste("btn btn-gray-outline wf-name-btn",
                      "shiny-bound-input action-button"),
        name = "Workflow name",
        div(
          class = "h5 m-0 text-center",
          textOutput(outputId = ids()$workflow_start$outputs$wf_name_out)
        )
      ),
      div(
        class = "svg-container",
        tags$svg(
          id = ns(ids()$side_bar$inputs$side_nav_element),
          xmlns = "http://www.w3.org/2000/svg",
          class = "svg-content-mini"
        )
      ),
      div(
        class = paste("w-100 p-2"),
        shinyjs::hidden(tags$button(
          id = ns(ids()$side_bar$inputs$back_btn),
          class = paste("btn btn-gray w-100 m-1",
                        "shiny-bound-input action-button"),
          name = "Back",
          icon("backward-step"),
          "Back"
        )),
        shinyjs::hidden(
          tags$button(
            id = ns(ids()$side_bar$inputs$new_btn),
            class = paste("btn btn-gray w-100 m-1",
                          "shiny-bound-input action-button"),
            name = "New workflow",
            icon("plus"),
            "New workflow"
          )
        )
      )
    )
  )
}

#' side_bar Server Functions
#'
#' @noRd
mod_side_bar_server <- function(id, workflow, wf_flag_name, side_nav){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    gargoyle::init("status-update")
    .wf_name_btn_observer(workflow, input, ns, wf_flag_name)
    side_nav_obj <- SideNav$new(
      nav_id = ns(ids()$side_bar$inputs$side_nav_element),
      status_flag = "status-update"
    )
    side_nav(side_nav_obj)
    .side_nav_status_observer(side_nav)
    .side_btn_observers(input, side_nav)
    .back_btn_observer(input, side_nav)
  })
}

## To be copied in the UI
# mod_side_bar_ui("side_bar_1")

## To be copied in the server
# mod_side_bar_server("side_bar_1")
