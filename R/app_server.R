#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>%
#' @noRd


superstore <- janitor::clean_names(readr::read_csv('inst/app/data/SampleSuperstore.csv'))


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





}
