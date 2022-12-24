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
    fluidRow(
      column(12,
             align = 'center',
             br(),
             h2('Dashboard Details')
      )
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(2),
      column(8,
             div(class = 'details_top',
                 style = 'height: 150px;',
                 align = 'center',
                 br(),
                 h3('This dashboard has been prepared as a portfolio example.'),
                 br(),
                 h5('The dashboard has been prepared with R’s Shiny Framework using Golem architecture.'),
                 br(),
                 h5('The dashboard applies a bs4Dash dashboard and uses reactable, echarts4r and higherchart visualisation technologies.'),
                 br()
             )
      ),
      column(2)
    ),
    br(),
    fluidRow(
      column(2),
      column(4,
             div(class = 'details_top',
                 style = 'height: 200px;',
                 align = 'center',
                 br(),
                 h4('Designed by Callum Hollywood'),
                 h5('nexusdatascience.com'),
                 br(),
                 h4('Contact:'),
                 h5('callumhollywood@gmail.com'),
                 br()
             )),
      column(4,
             div(class = 'details_top',
                 style = 'height: 200px;',
                 align = 'center',
                 br(),
                 h4('Check me out on Github'),
                 h5('https://github.com/CallumHollywood'),
                 br(),
                 h4('The app code can be viewed at'),
                 h5('https://github.com/CallumHollywood/gapminder/tree/master')
             )),
      column(2)
    )
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


