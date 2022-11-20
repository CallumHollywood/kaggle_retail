#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    dashboardPage(
      header = dashboardHeader(
        title = h2(tags$u('Retail Analytics', style = 'color: white;'))
      ),
      sidebar = dashboardSidebar(

        div(class = 'sidebar',
        h3('stuff'),
        sidebarMenu(
          menuItem(
            text = "Item 1"
          ),
          menuItem(
            text = "Item 2"
          )
        )
            )

      ),
      body = dashboardBody(

      ),
    )
  )
}



#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "blank"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
