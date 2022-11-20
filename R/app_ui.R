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
            br(),
            h3('Menu'),
            sidebarMenu(
              menuItem(
                text = "About",
                tabName = "item_1"
              ),
              menuItem(
                text = "Overview",
                tabName = "item_2"
              ),
              menuItem(
                text = "State Profile",
                tabName = "item_3"
              )
            )
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName = "item_1",
            mod_about_ui("about_1")
          ),
          tabItem(
            tabName = "item_2",
            h1("Overview")
          ),
          tabItem(
            tabName = "item_3",
            h1("State")
          )
        )
      )
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
      app_title = "Kaggle Retail Analytics"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
