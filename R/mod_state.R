#' state UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets


mod_state_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             pickerInput(ns('slt_state'),
                         'Select State',
                         choices = NULL
             )
      ),
      column(3,
             align = 'center',
             uiOutput(ns('ot_info_1'))
      ),
      column(3,
             align = 'center',
             uiOutput(ns('ot_info_2'))
      ),
      column(3,
             align = 'center',
             uiOutput(ns('ot_info_3'))
      )
    ),
    br(),
    fluidRow(
      box(
        id = "card4",
        title = h4("Shipping Mode"),
        width = 3,
        status = "primary",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = FALSE,
        echarts4rOutput(ns('ot_ship_mode'))
      ),

    )
  )
}

#' state Server Functions
#'
#' @noRd
mod_state_server <- function(
    id,
    superstore
){
  moduleServer( id, function(input, output, session){

    ns <- session$ns


    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#

    # count_ship_mode <- superstore %>%
    #   filter(state == input$slt_state) %>%
    #   select(ship_mode) %>%
    #   group_by(ship_mode) %>%
    #   mutate(count = n()) %>%
    #   ungroup() %>%
    #   distinct()

    count_ship_mode_rctv <- reactive({

      superstore %>%
        filter(state == input$slt_state) %>%
        select(ship_mode) %>%
        group_by(ship_mode) %>%
        mutate(count = n()) %>%
        ungroup() %>%
        distinct()

    })

    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    observe({

      updatePickerInput(
        session,
        'slt_state',
        'Select State',
        choices = sort(unique(superstore$state))
      )

    })


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    output$ot_ship_mode <- renderEcharts4r({

      req(input$slt_state)
      req(count_ship_mode_rctv())

      count_ship_mode_rctv() |>
        e_charts(ship_mode) |>
        e_pie(count, radius = c("50%", "70%")) |>
        # e_title("Shipping Mode", left = "center") %>%
        e_legend(bottom = 0)

    })


    output$ot_info_1 <- renderUI({

      state_quantity <- superstore %>%
        filter(state == input$slt_state) %>%
        summarise(quantity = sum(quantity)) %>%
        pull(quantity)

      state_quantity <- format(round(mean(state_quantity), 0), big.mark=",", scientific=FALSE)

      state_quantity <- paste0(state_quantity, ' Units')

      infoBox(
        tabName = "cardsAPI",
        title = "Total Quantity",
        value = state_quantity,
        color = "indigo",
        icon = icon("laptop-code"),
        width = 12
      )

    })

    output$ot_info_2 <- renderUI({

      state_sales <- superstore %>%
        filter(state == input$slt_state) %>%
        summarise(sales = sum(sales)) %>%
        pull(sales)

      state_sales <- format(round(mean(state_sales), 0), big.mark=",", scientific=FALSE)

      state_sales <- paste0('$', state_sales)

      infoBox(
        tabName = "cardsAPI",
        title = "Total Sales",
        value = state_sales,
        color = "indigo",
        icon = icon("laptop-code"),
        width = 12
      )

    })

    output$ot_info_3 <- renderUI({

      state_profit <- superstore %>%
        filter(state == input$slt_state) %>%
        summarise(profit = sum(profit)) %>%
        pull(profit)

      state_profit <- format(round(mean(state_profit), 0), big.mark=",", scientific=FALSE)

      state_profit <- paste0('$', state_profit)

      infoBox(
        tabName = "cardsAPI",
        title = "Total Profit",
        value = state_profit,
        color = "indigo",
        icon = icon("laptop-code"),
        width = 12
      )

    })

  })
}
