# NASA Fireballs & Bolides - Painel Analitico ----

library(shiny)
library(bslib)
library(plotly)
library(reactable)

# Modulos do projeto
source("R/data_fireball.R")
source("R/plots_fireball.R")
source("R/ui_components.R")

# Ingestão de dados no boot com fallback automático em cache
initial_data <- fetch_fireball_data()
nasa_fireball <- initial_data$data
data_source  <- initial_data$source

min_date <- min(as.Date(nasa_fireball$date), na.rm = TRUE)
max_date <- max(as.Date(nasa_fireball$date), na.rm = TRUE)

# UI Definition -----------------------------------------------------------

ui <- bslib::page_navbar(
  title = shiny::span(
    shiny::icon("meteor", class = "text-primary me-2"),
    "NASA Fireballs & Bolides"
  ),
  theme = build_space_theme(),
  id = "nav_active",
  sidebar = bslib::sidebar(
    width = 300,
    title = "Controles da Missão",
    shiny::h6("Filtros Globais", class = "text-uppercase small fw-bold mt-2"),
    shiny::dateRangeInput(
      "date_range",
      "Período de Observação",
      start     = min_date,
      end       = max_date,
      min       = min_date,
      max       = max_date,
      format    = "dd/mm/yyyy",
      language  = "pt-BR",
      separator = " até "
    ),
    shiny::selectInput(
      "min_impact_e",
      "Energia Mínima de Impacto",
      choices = c(
        "Todos os eventos (ilimitado)" = 0,
        "≥ 0,1 kt"                     = 0.1,
        "≥ 0,3 kt"                     = 0.3,
        "≥ 1,0 kt"                     = 1,
        "≥ 3,0 kt"                     = 3,
        "≥ 10,0 kt (Superbólidos)"     = 10,
        "≥ 15,0 kt (Escala Hiroshima)" = 15,
        "≥ 30,0 kt"                    = 30,
        "≥ 100,0 kt"                   = 100
      ),
      selected = 0
    ),
    shiny::radioButtons(
      "map_projection",
      "Projeção Espacial",
      choices = c(
        "Globo 3D Ortográfico" = "orthographic",
        "Mapa 2D Global"       = "natural earth"
      ),
      selected = "orthographic"
    ),
    shiny::hr(style = "border-color: #1E293B;"),
    shiny::uiOutput("filter_summary_ui")
  ),

  # Aba 1: Centro de Comando (Command Center) -----------------------------
  bslib::nav_panel(
    "Centro de Comando",
    icon = shiny::icon("dashboard"),
    shiny::div(
      class = "container-fluid px-0",
      # Indicadores principais (KPIs)
      bslib::layout_column_wrap(
        width = 1/4,
        class = "mb-3",
        bslib::value_box(
          title = "Total de Detecções",
          value = shiny::textOutput("kpi_total_count"),
          showcase = shiny::icon("meteor", class = "text-primary fs-3"),
          class = "vbox-orange",
          shiny::span(class = "small text-secondary", "Eventos no intervalo filtrado")
        ),
        bslib::value_box(
          title = "Maior Impacto Registrado",
          value = shiny::textOutput("kpi_max_impact"),
          showcase = shiny::icon("explosion", class = "text-danger fs-3"),
          class = "vbox-red",
          shiny::uiOutput("kpi_hiroshima_comp")
        ),
        bslib::value_box(
          title = "Pico de Velocidade",
          value = shiny::textOutput("kpi_max_vel"),
          showcase = shiny::icon("rocket", class = "text-info fs-3"),
          class = "vbox-cyan",
          shiny::span(class = "small text-secondary", "Velocidade pré-impacto (km/s)")
        ),
        bslib::value_box(
          title = "Altitude Média do Brilho",
          value = shiny::textOutput("kpi_avg_alt"),
          showcase = shiny::icon("mountain", class = "text-warning fs-3"),
          class = "vbox-amber",
          shiny::span(class = "small text-secondary", "Altitude média de fragmentação")
        )
      ),

      # Layout principal: Globo 3D e painel analitico
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            shiny::span(shiny::icon("globe-americas", class = "text-info me-2"), "Mapa Global de Bólidos"),
            shiny::span(class = "small text-secondary", "Escala de energia em kilotons (kt)")
          ),
          plotly::plotlyOutput("geo_plot", height = "560px")
        ),
        bslib::navset_card_underline(
          title = "Análise Científica",
          full_screen = TRUE,
          bslib::nav_panel(
            "Série Temporal",
            icon = shiny::icon("chart-line"),
            plotly::plotlyOutput("timeline_plot", height = "520px")
          ),
          bslib::nav_panel(
            "Perfil Atmosférico",
            icon = shiny::icon("layer-group"),
            plotly::plotlyOutput("atmosphere_plot", height = "520px")
          ),
          bslib::nav_panel(
            "Correlação",
            icon = shiny::icon("chart-simple"),
            shiny::div(
              class = "d-flex gap-2 mb-2 pt-1",
              shiny::selectInput(
                "corr_x", "Eixo X:",
                choices = c("Velocidade (km/s)" = "vel", "Energia Radiada" = "energy", "Altitude (km)" = "alt"),
                selected = "vel", width = "50%"
              ),
              shiny::selectInput(
                "corr_y", "Eixo Y:",
                choices = c("Energia de Impacto (kt)" = "impact_e", "Energia Radiada" = "energy", "Altitude (km)" = "alt"),
                selected = "impact_e", width = "50%"
              )
            ),
            plotly::plotlyOutput("correlation_plot", height = "450px")
          )
        )
      )
    )
  ),

  # Aba 2: Explorador de Dados --------------------------------------------
  bslib::nav_panel(
    "Explorador de Dados",
    icon = shiny::icon("table"),
    bslib::card(
      full_screen = TRUE,
      class = "mb-3",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span(shiny::icon("database", class = "text-primary me-2"), "Tabela Cronológica de Detecções"),
        shiny::downloadButton("download_csv", "Exportar CSV", class = "btn-outline-primary btn-sm")
      ),
      bslib::card_body(
        reactable::reactableOutput("fireball_table")
      )
    ),
    column_dictionary_ui()
  ),

  # Aba 3: Briefing da Missão ---------------------------------------------
  bslib::nav_panel(
    "Briefing da Missão",
    icon = shiny::icon("circle-info"),
    mission_brief_ui()
  ),

  # Elementos de Cabeçalho / Navbar ---------------------------------------
  bslib::nav_spacer(),
  bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "dark")),
  bslib::nav_item(shiny::uiOutput("header_badge_ui"))
)

# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {

  # Reativo central que filtra dados com base na sidebar
  fireballs_r <- shiny::reactive({
    filter_fireballs(nasa_fireball, input$date_range, input$min_impact_e)
  })

  is_dark_r <- shiny::reactive({
    !identical(input$dark_mode, "light")
  })

  # Indicadores de status e contagem de eventos
  output$header_badge_ui <- shiny::renderUI({
    render_status_badge(data_source, nrow(nasa_fireball))
  })

  output$filter_summary_ui <- shiny::renderUI({
    df <- fireballs_r()
    shiny::div(
      class = "small",
      shiny::p(
        class = "mb-1 text-white fw-semibold",
        shiny::span(class = "text-primary fs-5", format(nrow(df), big.mark = ".", decimal.mark = ",")),
        shiny::span(class = "text-secondary", paste0(" de ", format(nrow(nasa_fireball), big.mark = ".", decimal.mark = ","), " eventos"))
      ),
      shiny::div(
        class = "d-flex align-items-center gap-2 mt-2",
        shiny::icon("location-dot", class = "text-info small"),
        shiny::span(class = "text-secondary", "Geolocalizados: "),
        shiny::strong(class = "text-info", sum(!is.na(df$lat) & !is.na(df$lon)))
      )
    )
  })

  # Indicadores principais (KPIs) ----
  output$kpi_total_count <- shiny::renderText({
    df <- fireballs_r()
    format(nrow(df), big.mark = ".", decimal.mark = ",")
  })

  output$kpi_max_impact <- shiny::renderText({
    df <- fireballs_r()
    if (nrow(df) == 0 || all(is.na(df$impact_e))) return("—")
    max_val <- max(df$impact_e, na.rm = TRUE)
    paste0(format(max_val, big.mark = ".", decimal.mark = ","), " kt")
  })

  output$kpi_hiroshima_comp <- shiny::renderUI({
    df <- fireballs_r()
    if (nrow(df) == 0 || all(is.na(df$impact_e))) return(NULL)
    max_val <- max(df$impact_e, na.rm = TRUE)
    equiv <- round(max_val / HIROSHIMA_KT, 1)
    if (equiv >= 1) {
      shiny::span(
        style = "background: rgba(239, 68, 68, 0.15); color: #FCA5A5; border: 1px solid rgba(239, 68, 68, 0.35); border-radius: 999px; padding: 2px 8px; font-size: 0.76rem; font-weight: 600;",
        paste0("~", equiv, "x Hiroshima")
      )
    } else {
      shiny::span(class = "small text-secondary", "Abaixo de 1x Hiroshima")
    }
  })

  output$kpi_max_vel <- shiny::renderText({
    df <- fireballs_r()
    if (nrow(df) == 0 || all(is.na(df$vel))) return("—")
    max_val <- max(df$vel, na.rm = TRUE)
    paste0(format(max_val, big.mark = ".", decimal.mark = ","), " km/s")
  })

  output$kpi_avg_alt <- shiny::renderText({
    df <- fireballs_r()
    if (nrow(df) == 0 || all(is.na(df$alt))) return("—")
    avg_val <- mean(df$alt, na.rm = TRUE)
    paste0(format(round(avg_val, 1), big.mark = ".", decimal.mark = ","), " km")
  })

  # Graficos analiticos (Plotly) ----
  output$geo_plot <- plotly::renderPlotly({
    df <- fireballs_r()
    shiny::validate(shiny::need(nrow(df) > 0, "Nenhum evento encontrado para o período selecionado."))
    plot_fireball_geo(df, projection_type = input$map_projection, is_dark = is_dark_r())
  })

  output$timeline_plot <- plotly::renderPlotly({
    df <- fireballs_r()
    shiny::validate(shiny::need(nrow(df) > 0, "Nenhum evento encontrado para o período selecionado."))
    plot_fireball_timeline(df, is_dark = is_dark_r())
  })

  output$atmosphere_plot <- plotly::renderPlotly({
    df <- fireballs_r()
    shiny::validate(shiny::need(nrow(df) > 0, "Nenhum evento com dados de altitude e velocidade."))
    plot_atmosphere_profile(df, is_dark = is_dark_r())
  })

  output$correlation_plot <- plotly::renderPlotly({
    df <- fireballs_r()
    shiny::validate(shiny::need(nrow(df) > 0, "Nenhum dado para correlacionar."))
    plot_energy_correlation(df, var_x = input$corr_x, var_y = input$corr_y, is_dark = is_dark_r())
  })

  # Tabela interativa e exportacao ----
  output$fireball_table <- reactable::renderReactable({
    df <- fireballs_r()
    build_fireball_reactable(df, is_dark = is_dark_r())
  })

  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("nasa_fireballs_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      utils::write.csv(fireballs_r(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)