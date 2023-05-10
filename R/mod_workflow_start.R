#' workflow_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList div actionButton icon
#' @importFrom shinyjs disabled
mod_workflow_start_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = id,
      class = "vh-minus-navbar workflow_init_container",
      div(
        class = "workflow_init_item",
        tags$button(
          id = ns(ids()$workflow_start$inputs$new_wf_btn),
          class = paste(
            "btn border-1 rounded workflow_init_button",
            "shiny-bound-input action-button"
          ),
          name = "New workflow",
          icon("diagram-next",
            style = "font-size: 6em;",
            class = "align-middle mb-1"
          ),
          div("New workflow", class = "align-middle h4"),
          div("Create and run a new workflow",
            class = "align-middle text-muted"
          )
        )
      ),
      div(
        class = "workflow_init_item",
        shinyjs::disabled(
          tags$button(
            id = ns(ids()$workflow_start$inputs$import_wf_btn),
            class = paste(
              "btn border-1 rounded workflow_init_button",
              "shiny-bound-input action-button"
            ),
            name = "Import workflow",
            icon("upload",
              style = "font-size: 6em;",
              class = "align-middle mb-1"
            ),
            div("Import workflow", class = "align-middle h4"),
            div("Import a workflow from file (COMING SOON)",
              class = "align-middle text-muted"
            )
          )
        )
      )
    )
  )
}

#' workflow_start Server Functions
#'
#' @importFrom shiny moduleServer
#'
#' @noRd
mod_workflow_start_server <- function(id, workflow, wf_name_flag) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .create_wf_observer(input, session, workflow, wf_name_flag)
  })
}
