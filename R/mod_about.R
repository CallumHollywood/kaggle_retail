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
#' @import echarts4r
#' @import dplyr



mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = 'about',
        br(),
        fluidRow(
          column(12,
                 align = 'center',
                 h3('Retail Dashboard'),
          )
        ),
        br(),
        fluidRow(
          column(4,
                 column(12,
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
                 ),
                 column(12,
                        br(),
                        br(),
                        h4('Demo'),
                        br(),
                        h5('Nexus Data Science Portfolio Dashboards.'),
                        h5('For business metrics with an up-to-the-minute pulse, visit...'),
                        br(),
                        div(
                          tags$a(href="http://nexusdatascience.com",
                                 "www.nexusdatascience.com",
                                 target = '_blank'
                          )
                        ),
                        br(),
                        br(),
                        h5('BI Dashboards that put you in control!')
                 )

          ),
          box(
            id = "card4",
            title = h4("Top 10 States by Total Quantity"),
            width = 4,
            status = "primary",
            closable = FALSE,
            maximizable = FALSE,
            collapsible = FALSE,
            echarts4rOutput(ns('ot_state_quantity'))
          ),
          box(
            id = "card4",
            title = h4("Shipping Mode"),
            width = 4,
            status = "primary",
            closable = FALSE,
            maximizable = FALSE,
            collapsible = FALSE,
            echarts4rOutput(ns('ot_ship_mode'))
          ),
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



    #### <<<<    gargoyle        >>>>  ####
    #-------------------------------------#

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#

    count_ship_mode <- superstore %>%
      select(ship_mode) %>%
      group_by(ship_mode) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      distinct()


    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#



    output$ot_ship_mode=renderEcharts4r({

      count_ship_mode |>
        e_charts(ship_mode) |>
        e_pie(count, radius = c("50%", "70%")) |>
        # e_title("Shipping Mode", left = "center") %>%
        e_legend(bottom = 0)

    })

    output$ot_state_quantity <- renderEcharts4r({

      superstore %>%
        group_by(state) %>%
        transmute(quantity = sum(quantity)) %>%
        ungroup() %>%
        distinct() %>%
        arrange(desc(quantity)) %>%
        slice(1:10) %>%
        rename(states = state) %>%
        e_charts(states) %>%
        e_map_register("USA", json) %>%
        e_map(quantity , map = "USA") %>%
        e_visual_map(quantity )

    })

  })
}

