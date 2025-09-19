# Server ------------------------------------------------------------------



server <- function(input, output) {

  nasa_fireball_r <- reactive({
    nasa_fireball |>
      filter(date >= (as.Date(input$date[1]) - 1),
             date <= (as.Date(input$date[2]) + 1),
             impact_e >= input$impact_e)
  }) #|>
  #bindCache(nasa_fireball)



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
      geom_point(aes(color = impact_e,
                     size = impact_e,
                     text = map(paste(
                       '<b>Date:</b>', date,
                       '<br>',
                       '<b>total impact energy (kt):</b>', impact_e,
                       '<br>',
                       '<b>total radiated energy (J):</b>', energy,
                       '<br>',
                       '<b>Latitude:</b>', lat,
                       '<br>',
                       '<b>Longitude:</b>', lon), HTML)
      ), alpha = 0.7) +
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

    ggplotly(plot3, tooltip = "text")


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
