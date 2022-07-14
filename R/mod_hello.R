#' hello UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hello_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' hello Server Functions
#'
#' @noRd 
mod_hello_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_hello_ui("hello_ui_1")
    
## To be copied in the server
# mod_hello_server("hello_ui_1")
