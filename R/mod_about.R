#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom magrittr %>%


mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = 'about',
        br(),
        fluidRow(
          column(12,
                 h3('Retail Dashboard'),
          )
        ),
        br(),
        fluidRow(
          column(4,
                 div(class = 'aboutdescription',
                     h4('About'),
                     br(),
                     h5('Sales details of stores of a USA supermarket chain.'),
                     p('Data sourced from...'),
                     div(id = 'href',
                         tags$a(href="https://www.kaggle.com/datasets/roopacalistus/superstore",
                                "Kaggle - Retail Supermarket",
                                target = '_blank'
                         )
                     )
                 )
          )
        )
    )
  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(
    id,
    superstore
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
