#' save_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_save_results_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' save_results Server Functions
#'
#' @noRd 
mod_save_results_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_save_results_ui("save_results_1")
    
## To be copied in the server
# mod_save_results_server("save_results_1")
