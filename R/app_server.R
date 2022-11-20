#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>%
#' @noRd


superstore <- read_csv('inst/app/data/SampleSuperstore.csv') %>%
  clean_names()

app_server <- function(input, output, session) {



  #### <<<<    gargoyle        >>>>  ####
  #-------------------------------------#

  #### <<<<    CALLMODULES     >>>>  ####
  #-------------------------------------#

  mod_about_server(
    "about_1"
    , superstore
    )

  #### <<<<    STATIC VALUES   >>>>  ####
  #-------------------------------------#


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

  output$plot <- renderEcharts4r({

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



}
