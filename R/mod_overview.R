#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import reactable
#' @import tidyr


mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             echarts4rOutput(ns('ot_guage_quantity'))
      ),
      column(4,
             echarts4rOutput(ns('ot_guage_sales'))
      ),
      column(4,
             echarts4rOutput(ns('ot_guage_profit'))
      )
    ),
    br(),
    fluidRow(
      infoBox(
        tabName = "cardsAPI",
        title = h4(tags$u("Top 5 Cities - Quantity")),
        value = tableOutput(ns('ot_top_5_quantity')),
        color = "indigo",
        icon = icon("bar-chart")
      ),
      infoBox(
        tabName = "cardsAPI",
        title = h4(tags$u("Top 5 Cities - Sales")),
        value = tableOutput(ns('ot_top_5_sales')),
        color = "indigo",
        icon = icon("usd")
      ),
      infoBox(
        tabName = "cardsAPI",
        title = h4(tags$u("Top 5 Cities - Profit")),
        value = tableOutput(ns('ot_top_5_profit')),
        color = "indigo",
        icon = icon("usd")
      )
    ),
    br(),
    fluidRow(
      column(4,
             box(
               id = "card4",
               title = h4("Total Profit by Region and Category"),
               width = 12,
               status = "primary",
               closable = FALSE,
               maximizable = FALSE,
               collapsible = FALSE,
               reactableOutput(ns('ot_regional_profit'))
             ),
             box(
               id = "card4",
               title = h4("Sub Category - % Stock Discounted"),
               width = 12,
               status = "primary",
               closable = FALSE,
               maximizable = FALSE,
               collapsible = FALSE,
               div(style = 'height: 350px;',
                   reactableOutput(ns('ot_most_discounted'))
               )
             )
      ),
      column(8,
             box(
               id = "card4",
               height = "800px",
               title = h4("Sub Category Shipping Modes"),
               width = 12,
               status = "primary",
               closable = FALSE,
               maximizable = FALSE,
               collapsible = FALSE,
               div(style = 'height: 350px;',
                   reactableOutput(ns('ot_ship_by'))
               )
             )
      )

    )
  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(
    id,
    superstore
){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#

    ship_mode_subcat <- superstore %>%
      select(ship_mode, sub_category) %>%
      group_by(sub_category, ship_mode) %>%
      summarise(count = n()) %>%
      pivot_wider(names_from = ship_mode, values_from = count) %>%
      select(sub_category, `Same Day`, `First Class`, `Second Class`, `Standard Class`) %>%
      janitor::clean_names() %>%
      select(sub_category, same_day, first_class, second_class, standard_class)

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

    output$ot_ship_by <- renderReactable({

      bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {

        bar <- div(style = list(background = fill, width = width, height = height))

        if(nchar(as.character(label)) == 1){

          marginleft <- "1.5rem"

        } else if(nchar(as.character(label)) == 2){

          marginleft <- "1rem"

        }  else if(nchar(as.character(label)) == 3){

          marginleft <- "0.5rem"

        }

        chart <- div(style = list(flexGrow = 1, marginLeft = marginleft, background = background), bar)
        div(style = list(display = "flex", alignItems = "center"), label, chart)
      }


      ship_mode_subcat %>%
        reactable(
          defaultPageSize = 17,
          showSortable = TRUE,
          columns = list(
            same_day = colDef(name = "same_day", align = "left", cell = function(value) {
              width <- paste0(value / max(ship_mode_subcat$same_day) * 100, "%")
              bar_chart(value, width = width, background = "#e1e1e1")
            }
            ),
            first_class = colDef(name = "first_class", align = "left", cell = function(value) {
              width <- paste0(value / max(ship_mode_subcat$first_class) * 100, "%")
              bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
            }
            ),
            second_class = colDef(name = "second_class", align = "left", cell = function(value) {
              width <- paste0(value / max(ship_mode_subcat$first_class) * 100, "%")
              bar_chart(value, width = width, fill = "#e6b122", background = "#e1e1e1")
            }
            ),
            standard_class = colDef(name = "standard_class", align = "left", cell = function(value) {
              width <- paste0(value / max(ship_mode_subcat$first_class) * 100, "%")
              bar_chart(value, width = width, fill = "#ab47bf", background = "#e1e1e1")
            }
            )
          )
        )


    })


    output$ot_most_discounted <- renderReactable({

      superstore %>%
        select(sub_category, discount) %>%
        mutate(discounted = ifelse(discount == 0, 'yes',	'no')) %>%
        group_by(sub_category, discounted) %>%
        summarise(count = n()) %>%
        pivot_wider(names_from = discounted, values_from = count) %>%
        rowwise() %>%
        mutate(total = no + yes) %>%
        mutate(prop_discounted = yes / total * 100) %>%
        ungroup() %>%
        arrange(desc(prop_discounted)) %>%
        mutate(prop_discounted = round(prop_discounted)) %>%
        rename(
          `Sub Category` = sub_category,
          `% of Sub Category Stock Discounted` = prop_discounted
        ) %>%
        select(-no, -yes, -total) %>%
        reactable(defaultPageSize = 4)

    })


    output$ot_guage_profit <- renderEcharts4r({

      e_charts() |>
        e_gauge(286397, "($)", min = 0, max = 30000) |>
        e_title("National Profit")

    })



    output$ot_guage_sales <- renderEcharts4r({

      e_charts() |>
        e_gauge(2297201, "($)", min = 0, max = 2500000) |>
        e_title("National Sales")

    })

    output$ot_guage_quantity <- renderEcharts4r({

      e_charts() |>
        e_gauge(37873, "Units", min = 0, max = 40000) |>
        e_title("National Quantity")

    })



    output$ot_top_5_quantity <- renderTable({

      tibble(
        Rank = 1:5,
        City = c('New York City',
                 'Los Angeles',
                 'Philadelphia',
                 'San Francisco',
                 'Seattle'
        ),
        `No Units`  = c(3417L,
                        2879L,
                        1981L,
                        1935L,
                        1590L
        )
      )

    })

    output$ot_top_5_sales <- renderTable({

      tibble(
        Rank = 1:5,
        City = c('New York City',
                 'Los Angeles',
                 'Seattle',
                 'San Francisco',
                 'Philadelphia'
        ),
        `Sales`  = c('$256,368',
                     '$175,851',
                     '$119,540',
                     '$112,669',
                     '$109,077'
        )
      )

    })


    output$ot_top_5_profit <- renderTable({

      tibble(
        Rank = 1:5,
        City = c('New York City',
                 'Los Angeles',
                 'Seattle',
                 'San Francisco',
                 'Detroit'
        ),
        `Sales`  = c('$62,036',
                     '$30,440',
                     '$29,156',
                     '$17,507',
                     '$13,181'
        )
      )

    })


    output$ot_regional_profit <- renderReactable({

      regional_profit <- superstore %>%
        group_by(region, category) %>%
        summarise(total_profit = sum(profit))

      regional_profit %>%
        arrange(total_profit, category) %>%
        mutate(total_profit = round(total_profit, 0)) %>%
        rename(
          `Region` = region,
          `Total Profit ($)` = total_profit,
          Category = category
        ) %>%
        reactable(groupBy = "Region",
                  columns = list(
                    `Total Profit ($)` = colDef(aggregate = "sum")
                  )
        )

    })

  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
