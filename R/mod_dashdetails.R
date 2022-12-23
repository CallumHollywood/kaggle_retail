#' dashdetails UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashdetails_ui <- function(id){
  ns <- NS(id)
  tagList(
    'mod_dashdetails_ui test'
  )
}

#' dashdetails Server Functions
#'
#' @noRd
mod_dashdetails_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_dashdetails_ui("dashdetails_1")

## To be copied in the server
# mod_dashdetails_server("dashdetails_1")
