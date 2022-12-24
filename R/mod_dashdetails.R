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
    br(),
    h5('This dashboard has been prepared as a portfolio example.'),
    br(),
    h5('The dashboard has been prepared with R’s Shiny Framework using Golem architecture.'),
    h5('The page applies a bs4Dash dashboard and uses reactable, echarts4r and higherchart visualisation technologies.'),
    br(),
    h5('Designed by Callum Hollywood'),
    h5('nexusdatascience.com'),
    br(),
    h5('Contact:'),
    h5('callumhollywood@gmail.com'),
    br(),
    h5('Check me out on Github'),
    h5('https://github.com/CallumHollywood'),
    br(),
    h5('The app code can be viewed at'),
    h5('https://github.com/CallumHollywood/gapminder/tree/master')
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
