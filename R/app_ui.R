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
        title = tagList(
          div(class = 'centerup',
                h2(tags$u('Retail Analytics', style = 'color: white;'))
              )
        )
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
              ),
              menuItem(
                text = "Dashboard Details",
                tabName = "item_4"
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
            mod_overview_ui("overview_1")
          ),
          tabItem(
            tabName = "item_3",
            mod_state_ui("state_1")
          ),
          tabItem(
            tabName = "item_4",
            mod_dashdetails_ui("dashdetails_1")
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


  )
}
