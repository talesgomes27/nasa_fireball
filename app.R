# Load Library ------------------------------------------------------------


# Data used
# https://cneos.jpl.nasa.gov/fireballs/

library(tidyverse)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(reactable)


# Data Import -------------------------------------------------------------


data <- jsonlite::fromJSON(
  "https://ssd-api.jpl.nasa.gov/fireball.api",
  simplifyDataFrame = T
)


# Data Wrangling  ---------------------------------------------------------

nasa_fireball <- as_tibble(data$data)
names(nasa_fireball) <- data$fields
nasa_fireball <- janitor::clean_names(nasa_fireball)


nasa_fireball <- nasa_fireball |>
  mutate(
    date = lubridate::as_datetime(date),
    lat = paste(lat, lat_dir),
    lon = paste(lon, lon_dir),
    lat = parzer::parse_lat(lat),
    lon = parzer::parse_lon(lon)
  ) |>
  select(-c(lat_dir, lon_dir)) |>
  mutate_if(is.character, as.numeric) |> 
  relocate(c(lat, lon), .after = last_col())

# Global Variables --------------------------------------------------------


#Data from global maps
world <- ne_countries(scale = "medium", returnclass = "sf")


#Variable used

# Max velocity
max_vel <- max(na.omit(nasa_fireball$vel)) 
# MaX impact energy
max_impact_e <- max(na.omit(nasa_fireball$impact_e))
# Max energy radiated
max_energy <- max(na.omit(nasa_fireball$energy)) 
#Number of events
number_events <- as.numeric(data$count) - 1 
#Text for the description of the columns shown in the table
table_column_descriptions <- paste(readLines("table_column_descriptions.html"), collapse="\n")
# Text for the introdution text
fireball_introduction <- paste(readLines("fireball_introduction.html"), collapse="\n")
# Text for the Widget description text
widget_data_description <- paste(readLines("widget_data_description.html"), collapse="\n")
# Text for the Project objective text
project_objective <- paste(readLines("project_objective.html"), collapse="\n")


# Auxiliary variable used to plot axis labal plot
description <- c(
  "energy" = "Total Radiated Energy (J)",
  "impact_e" = "Total Impact Energy (kt)",
  "alt" = "Altitude (km)",
  "vel" = "Velocity (km/s)"
)


#Function used to plot the histogram plot
histogram <- function(col_name, fill = "blue", bins = 25){
  
  plot <- ggplot(nasa_fireball, aes_string(x = col_name)) +
    geom_histogram(bins = bins, fill = fill, color = "black") +
    scale_x_continuous(trans = 'log', labels = scales::number_format(accuracy = 0.1)) +
    theme_bw() +
    labs(
      title = paste0("Histogram of ", description[col_name]),
      x = description[col_name],
      y = NULL
    )
  
  return(ggplotly(plot))
  
}



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



# Server ------------------------------------------------------------------



server <- function(input, output) {
  
  nasa_fireball_r <- reactive({
    nasa_fireball |>
      filter(date >= (as.Date(input$date[1]) - 1),
             date <= (as.Date(input$date[2]) + 1),
             impact_e >= input$impact_e)
  })
  
  
  
  #### Maps ####
  output$plot <- renderPlotly({
    
    plot <- ggplot() +
      geom_sf(data = world,
              colour = "black",
              fill = "grey") +
      geom_point(
        data = nasa_fireball_r(),
        aes(
          x = lon,
          y = lat,
          size = impact_e,
          colour = impact_e,
          text = map(paste(
            '<b>Date:</b>', date,
            '<br>',
            '<b>total impact energy (kt):</b>', impact_e,
            '<br>',
            '<b>total radiated energy (J):</b>', energy,
            '<br>',
            '<b>Latitude:</b>', lat,
            '<br>',
            '<b>Longitude:</b>', lon), HTML)), alpha = .7) +
      scale_size(trans = 'log', range = c(1.5, 8)) +
      viridis::scale_color_viridis(trans = 'log') +
      ggthemes::theme_map() +
      labs(
        color = "Impact Energy",
        size = "Impact Energy"
      ) +
      theme(
        #legend.position = "none"
      )
    
    
    ####  Map - GGplot to PLotly ####
    ggplotly(plot, tooltip = "text") |> 
      layout(title = list(text = paste0('<br>',"<b>Fireballs Reported by US Government Sensors</b>",
                                        '<br>',
                                        '<sup>',
                                        input$date[1], 
                                        " to ",
                                        input$date[2],
                                        "; limited to events >= ",
                                        input$impact_e,
                                        " kt",
                                        '</sup>'),
                          titlefont = list(size = 30, family = 'sans serif')
      ),
      annotations = list(text = '<a href="https://cneos.jpl.nasa.gov/fireballs/"><b>Source: NASA/JPL Fireball Data API\nVersion: 1.0</b></a>', 
                         font = list(size = 14, family = 'sans serif')
                         ,showarrow = FALSE
                         ,xref = 'paper', x = 0
                         ,yref = 'paper', y = 0
      )
      )
    
  })
  
  
  #### Bulding Data Table ####
  output$table <- renderReactable({
    
    theme <-
      reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px"
      )
    
    reactable(
      nasa_fireball_r(),
      filterable = TRUE,
      searchable = FALSE,
      resizable = TRUE,
      paginationType = "jump",
      showPageSizeOptions = TRUE,
      borderless = TRUE,
      striped = TRUE,
      showSortable = TRUE,
      columns = list(
        date = colDef(name = "Peak Brightness Date/Time (UT)",
                      aggregate = "max"),
        energy = colDef(
          name = "Total Radiated Energy (J x 1e-10)",
          defaultSortOrder = "desc",
          aggregate = "mean"
        ),
        impact_e = colDef(name = "Calculated Total Impact Energy (kt)",
                          aggregate = "max"),
        alt = colDef(name = "Altitude (km)",
                     aggregate = "max"),
        vel = colDef(name = "Velocity (km/s)",
                     aggregate = "max"),
        lat = colDef(name = "Latitude"),
        lon = colDef(name = "Longitude")
      ),
      theme = theme
    )
    
  })
  
  #### Download Button ####
  output$download <- downloadHandler(
    filename = function(){"nasa_fireball.csv"}, 
    content = function(fname){
      write.csv(nasa_fireball_r(), fname)
    }
  )
  
  
  #### Bulding Plots #####
  output$histogram_v1 <- renderPlotly({
    histogram(input$variable_1, fill = "blue", bins = input$bin_value)
  })
  
  
  output$histogram_v2 <- renderPlotly({
    histogram(input$variable_2, fill = "red", bins = input$bin_value)
  })
  
  
  output$scatter_plot <- renderPlotly({
    
    plot3 <- ggplot(nasa_fireball, aes_string(input$variable_1, input$variable_2)) +
      geom_point(aes(color = impact_e, size = impact_e)) +
      geom_smooth(color = "red", se = FALSE) +
      scale_size_continuous(breaks = c(0.5, 5), trans = 'log') +
      scale_color_viridis_c(trans = 'log') +
      scale_x_continuous(trans = 'log', labels = scales::number_format(accuracy = 0.1)) +
      scale_y_continuous(trans = 'log', labels = scales::number_format(accuracy = 0.1)) +
      theme_bw() + 
      labs(
        title = paste0(description[input$variable_1], " Vs ", description[input$variable_2]),
        x = description[input$variable_1],
        y = description[input$variable_2],
        color = "Impact energy",
        size =  "Impact energy"
      )
    
    ggplotly(plot3)
    
    
  })
  
  output$resume <-  renderUI({
    
    HTML(fireball_introduction)
    
  })
  
  output$table_description <- renderUI({
    #Text input from a external file
    HTML(table_column_descriptions)
  })
  
  output$widget_description <- renderUI({
    #Text input from a external file
    HTML(widget_data_description)
  })
  
  output$project_objective <- renderUI({
    HTML(project_objective)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)