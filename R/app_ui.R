#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "RISingVIS",
      theme = app_theme(),
      window_title = "RISingVIS",
      bg = "#ffc812",
      position = "static-top",
      # Main workflow section ----
      bslib::nav(
        "Workflow",
        ## Workflow init ----
        mod_workflow_start_ui(ids()$workflow_start$section_id),
        ## Pages container ----
        shinyjs::hidden(
          div(
            id = "pages-container",
            class = "container-fluid",
            div(
              class = "row",
              div(
                ### Sidebar ----
                class = "col-2",
                mod_side_bar_ui(ids()$side_bar$section_id)
              ),
              div(
                ### Content ----
                class = "col-10",
                # mod_data_import_ui(ids()$data_import$section_id),
                # mod_recalibration_ui(ids()$recalibration$section_id)
              )
            )
          )
        )
      ),
      bslib::nav(
        "Control cell lines",
        mod_control_lines_db_ui(ids()$control_lines_db$section_id)
      ),
      bslib::nav("FAQs")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RISingVIS"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert()
  )
}
