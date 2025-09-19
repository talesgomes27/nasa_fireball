# DashboardHeader ---------------------------------------------------------



header <- dashboardHeader(
  title = span(tagList(icon("meteor"), "Fireballs Events")),
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Nasa",
      message = "You can view the International Space Station!",
      href = "https://spotthestation.nasa.gov/sightings/"
    ),
    # Add a second messageItem()
    messageItem(
      from = "Nasa",
      message = "Learn more about the International Space Station",
      href = "https://spotthestation.nasa.gov/faq.cfm"
    )
  )
)


# DashboardSidebar --------------------------------------------------------


sidebar <- dashboardSidebar(

  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
    menuItem("Table", tabName = "table", icon = icon("th")),
    menuItem("Map",
             tabName = "map",
             icon = icon("globe"),
             badgeLabel = "new",
             badgeColor = "green"
    ),
    menuItem("Plot", tabName = "plot", icon = icon("chart-line"))
  ),

  hr(),

  h4("Map/Table inputs"),

  dateRangeInput(
    "date",
    "Select a Date Range",
    start = min(nasa_fireball$date),
    end = max(nasa_fireball$date),
    min = min(nasa_fireball$date),
    max = max(nasa_fireball$date)
  ),

  br(),

  selectInput("impact_e",
              "Select the minimum impact energy shown:",
              choices = c("unlimited" = 0, "0.1kt" = 0.1,
                          "0.3kt" = 0.3, "1kt" = 1, "3kt" = 3,
                          "10kt" = 10, "30kt" = 30, "100kt" = 100)

  ),

  hr(),

  h4("Plot inputs"),

  selectInput(
    inputId = "variable_1",
    label = "select x-axis variable:",
    choices = c(
      "Total Radiated Energy (J)" = "energy",
      "Total Impact Energy (kt)" = "impact_e",
      "Altitude (km)" = "alt",
      "Velocity (km/s)" = "vel"
    ),
    selected = "vel"
  ),

  br(),

  selectInput(
    inputId ="variable_2",
    label ="select y-axis variable:",
    choices = c(
      "Total Radiated Energy (J)" = "energy",
      "Total Impact Energy (kt)" = "impact_e",
      "Altitude (km)" = "alt",
      "Velocity (km/s)" = "vel"
    ),
    selected = "impact_e"
  ),

  br(),

  sliderInput(
    inputId = "bin_value",
    label = "Select the Histogram's bin size:",
    min = 1,
    max = 100,
    value = 30,
    step = 0.5
  )



)


# DashboardBody -----------------------------------------------------------



body <- dashboardBody(
  shinyDashboardThemes(
    theme = "grey_light"
  ),



  tabItems(
    tabItem(tabName = "dashboard",
            fluidPage(
              fluidRow(
                column(
                  offset = 2,
                  width = 8,
                  box(
                    title = "Project",
                    width = NULL,
                    status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    uiOutput("project_objective")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  tabBox(
                    width = NULL,
                    id = "introduction",
                    tabPanel(
                      title = "Introduction",
                      icon = icon("globe-americas"),
                      uiOutput("resume")
                    ),
                    tabPanel(
                      title = "Fireball and Bolide Data Widget",
                      icon = icon("chart-bar"),
                      uiOutput("widget_description")
                    )
                  )
                )
              )
            )
    ),

    tabItem(tabName = "table",
            fluidRow(
              # Add a value box for maximum energy
              valueBox(
                value = max_energy,
                subtitle = "Maximum total radiated energy (Joules)",
                icon = icon("lightbulb")
              ),
              # Add a value box for maximum impact
              valueBox(
                value = max_impact_e,
                subtitle = "Maximum impact energy (kilotons of TNT)",
                icon = icon("star")
              ),
              # Add a value box for maximum velocity
              valueBox(
                value = max_vel,
                subtitle = "Maximum pre-impact velocity",
                icon = icon("fire")
              )
            ),

            fluidPage(
              fluidRow(
                column(
                  width = 12,
                  reactableOutput("table", width = "100%", height = "100%")
                )
              ),
              fluidRow(
                column(
                  width = 1,
                  downloadButton('download',"Download the data")
                )
              ),
              fluidRow(
                column(
                  offset = 2,
                  width = 8,
                  box(
                    title = "Table Column Descriptions",
                    width = NULL,
                    status = "danger",
                    icon = icon("scroll"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    uiOutput("table_description")
                  )
                )
              )
            )

    ),

    tabItem(tabName = "map",
            fillPage(
              tags$style(type = "text/css",
                         "#plot {height: calc(100vh - 80px) !important;}"),
              shinycustomloader::withLoader(plotlyOutput("plot",
                                                         width = "100%",
                                                         height = "100%"),
                                            type="html", loader="loader6")
            )
    ),

    tabItem(tabName = "plot",
            fluidPage(

              fluidRow(
                column(
                  width = 6,
                  box(
                    title = NULL,
                    width = NULL, status = "primary",
                    shinycustomloader::withLoader(plotlyOutput("histogram_v1"),
                                                  type="html", loader="loader6")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = NULL,
                    width = NULL, status = "primary",
                    shinycustomloader::withLoader(plotlyOutput("histogram_v2"),
                                                  type="html", loader="loader6")
                  )
                )
              ),
              fluidRow(
                column(
                  offset = 2, width = 8,
                  box(title = NULL,
                      width = NULL, status = "primary",
                      shinycustomloader::withLoader(plotlyOutput("scatter_plot"),
                                                    type="html", loader="loader6")
                  )
                )
              )
            )
    )

  )
)



# ShinyDashboard ----------------------------------------------------------



ui <- dashboardPage(
  tags$head(
    tags$style(
      HTML(
        ".sidebar {
                      height: 90vh; overflow-y: auto;}
                    .tab-content > .active {
                      height: 90vh; overflow: auto;}"
      ) # close HTML
    )# close tags$style
  ),# close tags#Head
  header = header,
  sidebar = sidebar,
  body = body
)
