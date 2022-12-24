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
      column(12,
             align = 'center',
             class = 'aboutcols',
             style = ' background-color: #007bff;',
             div(h3('State Profile', style = 'color: #ffffff;'))
      )
    ),
    br(),
    fluidRow(
      column(12,
             align = 'center',
             class = 'aboutcols2',
             style = ' background-color: #007bff;',
             fluidRow(
               column(3,
                      br(),
                      div(class = 'cls_slt_state',
                          pickerInput(ns('slt_state'),
                                      h6('Select State'
                                         # , style = 'color: #ffffff;'
                                      ),
                                      choices = NULL
                          )
                      )
               ),
               column(3,
                      align = 'center',
                      br(),
                      uiOutput(ns('ot_info_1'))
               ),
               column(3,
                      align = 'center',
                      br(),
                      uiOutput(ns('ot_info_2'))
               ),
               column(3,
                      align = 'center',
                      br(),
                      uiOutput(ns('ot_info_3'))
               )
             )
      )
    ),

    # fluidRow(
    #   column(3,
    #          pickerInput(ns('slt_state'),
    #                      'Select State',
    #                      choices = NULL
    #          )
    #   ),
    #   column(3,
    #          align = 'center',
    #          uiOutput(ns('ot_info_1'))
    #   ),
    #   column(3,
    #          align = 'center',
    #          uiOutput(ns('ot_info_2'))
    #   ),
    #   column(3,
    #          align = 'center',
    #          uiOutput(ns('ot_info_3'))
    #   )
    # ),
    br(),
    fluidRow(
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
      box(
        id = "card4",
        title = h4("Contribution to Overall Profit"),
        width = 4,
        status = "primary",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = FALSE,
        echarts4rOutput(ns('ot_state_contrib'))
      ),
      box(
        id = "card4",
        title = h4("State Category Profile"),
        width = 4,
        status = "primary",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = FALSE,
        echarts4rOutput(ns('ot_state_category'))
      )
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

    state_category_rctv <- reactive({

      req(input$slt_state)

      superstore %>%
        filter(state == input$slt_state) %>%
        select(category, sales) %>%
        group_by(category) %>%
        summarise(sales = sum(sales))

    })

    fks_state_contrib_rctv <- reactive({

      superstore %>%
        mutate(fks_state = ifelse(state == input$slt_state, 'fks_state', 'rest')) %>%
        group_by(fks_state) %>%
        summarise(sum_state = sum(profit)) %>%
        ungroup %>%
        pivot_wider(names_from = fks_state, values_from = sum_state) %>%
        rowwise() %>%
        mutate(total = fks_state + rest) %>%
        mutate(pc_contrib = round(fks_state / total * 100, 1))
    })


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




    output$ot_state_category <- renderEcharts4r({

      state_category_rctv() |>
        e_charts(category) |>
        e_bar(sales, name = "sales") |>
        e_legend(show = FALSE)

    })

    output$ot_state_contrib <- renderEcharts4r({

      e_charts() |>
        e_gauge(fks_state_contrib_rctv()$pc_contrib, "%")
      # |>
      #   e_title("Contribution to Overall Profit")

    })


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
        color = "primary",
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
        color = "primary",
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
        color = "primary",
        icon = icon("laptop-code"),
        width = 12
      )

    })

  })
}
