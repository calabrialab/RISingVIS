
#' @importFrom shinyjs useShinyjs
#' @importFrom bslib page_navbar nav
ui <- tagList(
  shinyjs::useShinyjs(),
  tags$head(
    #tags$script(src = "node_clicked.js"),
    tags$script(src = "style_fileinput.js"),
    tags$script(src = "js_functions.js"),
  ),
  bslib::page_navbar(
    title = "RISingVIS",
    theme = app_theme(),
    window_title = "RISingVIS",
    bg = "#ffc812",
    position = "fixed-top",
    # --- Main workflow section
    bslib::nav(
      "Workflow",
      fluidPage(
        # ## --- Workflow home
        # fluidRow(
        #   style = "display: none;",
        #   column(
        #     width = 12,
        #     id = "first-page",
        #     div("New workflow",
        #         class = "display-2",
        #         align = "center"
        #     ),
        #     uiOutput("svg_full"),
        #   )
        # ),
        ## --- Container for all workflow pages
        fluidRow(
          ### --- Left side mini-graph
          column(
            width = 2,
            style = "padding-top: 2%;",
            align = "center",
            uiOutput(id_list()$side_graph)
          ),
          ### --- Pages content
          column(
            width = 10,
            class = "pages-container",
            dataImportUI(id_list()$data_import$section_id),
            RecUI(id_list()$recalibration$section_id)
          )
        )
      )
    )
  )
)
