# Componentes de interface, tema visual e documentacao ----

build_space_theme <- function() {
  bslib::bs_theme(
    version               = 5,
    bg                    = "#0B0F19",
    fg                    = "#E2E8F0",
    primary               = "#F97316",
    secondary             = "#38BDF8",
    info                  = "#38BDF8",
    success               = "#10B981",
    warning               = "#F59E0B",
    danger                = "#EF4444",
    base_font             = bslib::font_google("Inter", local = FALSE),
    heading_font          = bslib::font_google("Space Grotesk", local = FALSE),
    code_font             = bslib::font_google("JetBrains Mono", local = FALSE),
    "card-border-radius"  = "0.75rem",
    "card-bg"             = "#111827",
    "card-border-color"   = "#1E293B",
    "navbar-bg"           = "#070A12",
    "sidebar-bg"          = "#0D1322",
    "sidebar-fg"          = "#E2E8F0",
    "sidebar-border-color"= "#1E293B"
  ) |>
    bslib::bs_add_rules("
      /* --- UNIFICAÇÃO DO TEMA ESCURO E SIDEBAR --- */
      body, .bslib-page-navbar {
        background-color: #0B0F19 !important;
        color: #E2E8F0 !important;
      }
      .bslib-sidebar-layout > .sidebar,
      .sidebar,
      .bslib-sidebar-layout .sidebar,
      aside.sidebar {
        background-color: #0D1322 !important;
        border-right: 1px solid #1E293B !important;
        color: #E2E8F0 !important;
      }
      .sidebar h6 {
        color: #38BDF8 !important;
        font-size: 0.72rem !important;
        letter-spacing: 0.08em;
      }
      .sidebar label, .sidebar .form-label, .sidebar .control-label {
        color: #CBD5E1 !important;
        font-weight: 500 !important;
        font-size: 0.85rem !important;
      }
      .sidebar .form-control, .sidebar .form-select, .sidebar .input-daterange input {
        background-color: #111827 !important;
        border: 1px solid #334155 !important;
        color: #F8FAFC !important;
        font-size: 0.88rem;
      }
      .sidebar .form-control:focus, .sidebar .form-select:focus {
        border-color: #F97316 !important;
        box-shadow: 0 0 0 2px rgba(249, 115, 22, 0.25) !important;
      }
      .sidebar .input-group-addon {
        background-color: #1E293B !important;
        border-color: #334155 !important;
        color: #94A3B8 !important;
      }

      /* --- CONTRASTE WCAG 2.2 AA PARA TEXTOS E LEITURA --- */
      .text-secondary, .text-muted {
        color: #94A3B8 !important;
      }
      p, li, dd {
        color: #CBD5E1 !important;
        line-height: 1.65;
        font-size: 0.95rem;
      }
      dt {
        color: #F8FAFC !important;
        font-weight: 600;
        font-size: 0.92rem;
        margin-top: 0.5rem;
      }
      h1, h2, h3, h4, h5, h6, .card-header {
        color: #F8FAFC !important;
        font-weight: 600;
        letter-spacing: -0.01em;
      }

      /* --- VALUE BOXES MODERNAS (SEM SATURAÇÃO SÓLIDA BERRANTE) --- */
      .bslib-value-box {
        background: #111827 !important;
        border: 1px solid #1E293B !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 20px -2px rgba(0, 0, 0, 0.45) !important;
        position: relative;
        overflow: hidden;
        transition: transform 0.2s ease, border-color 0.2s ease;
      }
      .bslib-value-box:hover {
        transform: translateY(-2px);
        border-color: #334155 !important;
      }
      .bslib-value-box::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
      }
      .vbox-orange::before { background: linear-gradient(90deg, #F97316, #FB923C); }
      .vbox-red::before    { background: linear-gradient(90deg, #EF4444, #F87171); }
      .vbox-cyan::before   { background: linear-gradient(90deg, #38BDF8, #7DD3FC); }
      .vbox-amber::before  { background: linear-gradient(90deg, #F59E0B, #FCD34D); }

      .bslib-value-box .value-box-value {
        color: #F8FAFC !important;
        font-family: 'JetBrains Mono', monospace !important;
        font-size: 2.1rem !important;
        font-weight: 700 !important;
        letter-spacing: -0.02em;
        margin-bottom: 0.2rem;
      }
      .bslib-value-box .value-box-title {
        color: #94A3B8 !important;
        font-size: 0.76rem !important;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        font-weight: 600;
      }
      .bslib-value-box .value-box-showcase {
        opacity: 0.9;
        background: rgba(255, 255, 255, 0.05);
        border-radius: 10px;
        padding: 8px 12px;
      }

      /* --- CARDS E ABAS --- */
      .card {
        background-color: #111827 !important;
        border: 1px solid #1E293B !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 20px -4px rgba(0, 0, 0, 0.45);
      }
      .card-header {
        background-color: rgba(255, 255, 255, 0.02) !important;
        border-bottom: 1px solid #1E293B !important;
        padding: 0.85rem 1.25rem !important;
      }
      .nav-tabs, .nav-underline {
        border-bottom: 1px solid #1E293B !important;
      }
      .nav-tabs .nav-link, .nav-underline .nav-link {
        color: #94A3B8 !important;
        font-weight: 500;
        font-size: 0.88rem;
        transition: all 0.2s ease;
      }
      .nav-tabs .nav-link:hover, .nav-underline .nav-link:hover {
        color: #F8FAFC !important;
      }
      .nav-tabs .nav-link.active, .nav-underline .nav-link.active {
        color: #F8FAFC !important;
        border-bottom: 2px solid #F97316 !important;
        background: transparent !important;
        font-weight: 600;
      }

      /* --- BADGES E STATUS --- */
      .status-pill {
        display: inline-flex;
        align-items: center;
        gap: 0.45rem;
        padding: 0.3rem 0.75rem;
        border-radius: 999px;
        font-size: 0.75rem;
        font-family: 'JetBrains Mono', monospace;
        font-weight: 600;
      }
      .status-live {
        background: rgba(16, 185, 129, 0.15) !important;
        color: #34D399 !important;
        border: 1px solid rgba(16, 185, 129, 0.4) !important;
      }
      .status-cache {
        background: rgba(245, 158, 11, 0.15) !important;
        color: #FBBF24 !important;
        border: 1px solid rgba(245, 158, 11, 0.4) !important;
      }
      .formula-box {
        font-family: 'JetBrains Mono', monospace;
        background: #0B132B !important;
        border: 1px solid rgba(56, 189, 248, 0.35) !important;
        border-left: 4px solid #38BDF8 !important;
        padding: 1rem 1.25rem;
        border-radius: 0.5rem;
        margin: 1rem 0;
        color: #F8FAFC !important;
      }
      .formula-box code {
        color: #38BDF8 !important;
        font-size: 1.15rem;
        font-weight: 600;
      }

      /* --- TABELA REACTABLE ESTILIZADA --- */
      .rt-search {
        background: #0B0F19 !important;
        border: 1px solid #334155 !important;
        color: #F8FAFC !important;
        border-radius: 8px !important;
        padding: 6px 12px !important;
      }
      .rt-search:focus {
        border-color: #F97316 !important;
        outline: none;
      }
      .reactable {
        font-size: 0.88rem;
      }
      .rt-th {
        color: #94A3B8 !important;
        font-weight: 600 !important;
        text-transform: uppercase !important;
        font-size: 0.74rem !important;
        letter-spacing: 0.05em;
        border-bottom: 2px solid #1E293B !important;
      }
      .rt-td {
        border-bottom: 1px solid #1E293B !important;
      }
      .case-banner {
        background: #0F172A !important;
        border: 1px solid #334155 !important;
        border-left: 4px solid #F97316 !important;
        border-radius: 10px;
        padding: 0.9rem 1.25rem;
        margin-bottom: 1rem;
      }
    ")
}

render_case_banner <- function(case_id) {
  if (is.null(case_id) || identical(case_id, "none")) return(NULL)

  switch(case_id,
    "chelyabinsk" = shiny::div(
      class = "case-banner",
      shiny::div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-1",
        shiny::span(style = "background: rgba(239, 68, 68, 0.2); color: #FCA5A5; border: 1px solid #EF4444; border-radius: 999px; padding: 2px 10px; font-size: 0.76rem; font-weight: 700;", "440 kt TNT • ~29x Hiroshima"),
        shiny::strong(class = "text-white", "15/02/2013 — Superbólido de Chelyabinsk, Rússia")
      ),
      shiny::p(
        class = "mb-0 small", style = "color: #CBD5E1;",
        "Asteroide de ~20 m fragmentado a 23,3 km de altitude. A onda de choque danificou edifícios e feriu cerca de 1.500 pessoas na região urbana, sem formação de cratera em solo."
      )
    ),
    "bering" = shiny::div(
      class = "case-banner",
      shiny::div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-1",
        shiny::span(style = "background: rgba(245, 158, 11, 0.2); color: #FCD34D; border: 1px solid #F59E0B; border-radius: 999px; padding: 2px 10px; font-size: 0.76rem; font-weight: 700;", "173 kt TNT • ~11x Hiroshima"),
        shiny::strong(class = "text-white", "18/12/2018 — Evento do Mar de Bering")
      ),
      shiny::p(
        class = "mb-0 small", style = "color: #CBD5E1;",
        "Evento de 173 kt ocorrido a 25,6 km de altitude sobre o Pacífico Norte. Detectado exclusivamente por sensores orbitais devido à localização remota."
      )
    ),
    "vinales" = shiny::div(
      class = "case-banner",
      shiny::div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-1",
        shiny::span(style = "background: rgba(56, 189, 248, 0.2); color: #7DD3FC; border: 1px solid #38BDF8; border-radius: 999px; padding: 2px 10px; font-size: 0.76rem; font-weight: 700;", "1.4 kt TNT • Ruptura a 8 km"),
        shiny::strong(class = "text-white", "01/02/2019 — Meteorito de Viñales, Cuba")
      ),
      shiny::p(
        class = "mb-0 small", style = "color: #CBD5E1;",
        "Fragmentação em baixa altitude (8 km, troposfera). A baixa velocidade residual permitiu a recuperação de múltiplos meteoritos na província de Pinar del Río."
      )
    ),
    "fastest" = shiny::div(
      class = "case-banner",
      shiny::div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-1",
        shiny::span(style = "background: rgba(168, 85, 247, 0.2); color: #D8B4FE; border: 1px solid #A855F7; border-radius: 999px; padding: 2px 10px; font-size: 0.76rem; font-weight: 700;", "49.0 km/s • 176.400 km/h"),
        shiny::strong(class = "text-white", "23/08/2015 — Evento de Alta Velocidade")
      ),
      shiny::p(
        class = "mb-0 small", style = "color: #CBD5E1;",
        "Entrada atmosférica a 49,0 km/s (176.400 km/h), próxima ao limite teórico para órbitas heliocêntricas capturadas pela Terra."
      )
    ),
    NULL
  )
}


render_status_badge <- function(source_type, total_rows) {
  if (identical(source_type, "live")) {
    shiny::span(
      class = "status-pill status-live",
      shiny::icon("satellite-dish"),
      paste0("NASA CNEOS LIVE • ", total_rows, " EVENTOS")
    )
  } else {
    shiny::span(
      class = "status-pill status-cache",
      shiny::icon("database"),
      paste0("CACHE OFFLINE • ", total_rows, " EVENTOS")
    )
  }
}

build_fireball_reactable <- function(df, is_dark = TRUE) {
  tbl_df <- df[, c("date", "impact_e", "hiroshima_eq", "energy", "vel", "alt", "atmo_layer", "location_label")]
  tbl_df$date <- format(tbl_df$date, "%Y-%m-%d %H:%M")

  rt_theme <- reactable::reactableTheme(
    color                = "#E2E8F0",
    backgroundColor      = "#111827",
    borderColor          = "#1E293B",
    stripedColor         = "#0D1322",
    highlightColor       = "#1E293B",
    inputStyle           = list(backgroundColor = "#0B0F19", color = "#F8FAFC", borderColor = "#334155"),
    selectStyle          = list(backgroundColor = "#0B0F19", color = "#F8FAFC", borderColor = "#334155"),
    pageButtonHoverStyle = list(backgroundColor = "#1E293B", color = "#F8FAFC"),
    pageButtonActiveStyle= list(backgroundColor = "#F97316", color = "#FFFFFF"),
    cellPadding          = "11px 14px"
  )

  reactable::reactable(
    tbl_df,
    searchable          = TRUE,
    filterable          = FALSE, # Remove filtros vazios feios abaixo do header
    highlight           = TRUE,
    striped             = TRUE,
    compact             = FALSE,
    defaultPageSize     = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions     = c(10, 25, 50, 100),
    defaultSorted       = list(date = "desc"),
    theme               = rt_theme,
    columns = list(
      date = reactable::colDef(
        name = "Data/Hora (UTC)",
        minWidth = 150,
        style = list(fontFamily = "JetBrains Mono, monospace", color = "#F8FAFC")
      ),
      impact_e = reactable::colDef(
        name = "Impacto (kt)",
        defaultSortOrder = "desc",
        minWidth = 140,
        cell = function(value) {
          # Barra de energia proporcional
          bar_pct <- min(100, max(5, round(log10(max(value, 0.05) / 0.05) / log10(500 / 0.05) * 100)))
          shiny::div(
            style = "display: flex; align-items: center; gap: 8px;",
            shiny::span(style = "font-family: JetBrains Mono; font-weight: 600; min-width: 50px; color: #F97316;",
                        format(round(value, 2), decimal.mark = ",")),
            shiny::div(
              style = "flex-grow: 1; height: 6px; background: rgba(255,255,255,0.08); border-radius: 3px; overflow: hidden;",
              shiny::div(style = paste0("width: ", bar_pct, "%; height: 100%; background: linear-gradient(90deg, #38BDF8, #F97316);"))
            )
          )
        }
      ),
      hiroshima_eq = reactable::colDef(
        name = "Equiv. Hiroshima",
        minWidth = 135,
        cell = function(value) {
          if (is.na(value)) return("—")
          if (value >= 1) {
            shiny::span(class = "badge bg-danger-subtle text-danger fw-bold", paste0(value, "x"))
          } else {
            shiny::span(style = "color: #94A3B8;", paste0(value, "x"))
          }
        }
      ),
      energy = reactable::colDef(
        name = "Radiada (10¹⁰ J)",
        minWidth = 130,
        format = reactable::colFormat(digits = 1),
        style = list(fontFamily = "JetBrains Mono, monospace")
      ),
      vel = reactable::colDef(
        name = "Velocidade",
        minWidth = 110,
        na = "—",
        format = reactable::colFormat(suffix = " km/s", digits = 1),
        style = list(fontFamily = "JetBrains Mono, monospace")
      ),
      alt = reactable::colDef(
        name = "Altitude",
        minWidth = 105,
        na = "—",
        format = reactable::colFormat(suffix = " km", digits = 1),
        style = list(fontFamily = "JetBrains Mono, monospace")
      ),
      atmo_layer = reactable::colDef(
        name = "Camada",
        minWidth = 145,
        cell = function(val) {
          col <- switch(val,
            "Troposfera (< 12 km)"     = "#38BDF8",
            "Estratosfera (12–50 km)"  = "#A855F7",
            "Mesosfera (50–85 km)"     = "#F97316",
            "#94A3B8"
          )
          shiny::span(
            style = paste0("color: ", col, "; font-weight: 500; font-size: 0.82rem;"),
            val
          )
        }
      ),
      location_label = reactable::colDef(
        name = "Coordenadas",
        minWidth = 150,
        style = list(fontFamily = "JetBrains Mono, monospace", color = "#CBD5E1", fontSize = "0.82rem")
      )
    )
  )
}

column_dictionary_ui <- function() {
  bslib::card(
    bslib::card_header(
      class = "d-flex align-items-center gap-2",
      shiny::icon("book-open", class = "text-info"),
      shiny::span("Dicionário de Variáveis")
    ),
    bslib::card_body(
      shiny::div(
        class = "row g-4",
        shiny::div(
          class = "col-md-6",
          shiny::tags$dl(
            shiny::tags$dt("Data/Hora (UTC)"),
            shiny::tags$dd("Instante do pico de emissão óptica registrado em Tempo Universal Coordenado (UTC)."),
            shiny::tags$dt("Energia de Impacto (kt)"),
            shiny::tags$dd("Energia cinética total estimada em kilotons de TNT (1 kt = 4,185 × 10¹² Joules), calculada a partir da relação de Brown et al. (2002)."),
            shiny::tags$dt("Equiv. Hiroshima"),
            shiny::tags$dd("Razão direta em relação ao referencial histórico de 15 kt de TNT (Little Boy, 1945), facilitando comparações de escala."),
            shiny::tags$dt("Energia Óptica Radiada (10¹⁰ J)"),
            shiny::tags$dd("Energia óptica integrada detectada na banda de passagem dos sensores orbitais.")
          )
        ),
        shiny::div(
          class = "col-md-6",
          shiny::tags$dl(
            shiny::tags$dt("Altitude (km) & Camada"),
            shiny::tags$dd("Altitude geométrica estimada no instante do pico de brilho (a maioria dos eventos fragmenta na Estratosfera, entre 15 e 50 km)."),
            shiny::tags$dt("Velocidade (km/s)"),
            shiny::tags$dd("Velocidade geocêntrica no momento de contato com a alta atmosfera (faixa física observada de 11 a 72 km/s)."),
            shiny::tags$dt("Coordenadas"),
            shiny::tags$dd("Latitude e longitude aproximadas obtidas por triangulação orbital. Valores ausentes indicam detecção parcial.")
          )
        )
      )
    )
  )
}

mission_brief_ui <- function() {
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header(
        class = "d-flex align-items-center gap-2",
        shiny::icon("meteor", class = "text-primary"),
        shiny::span("Dinâmica de Fragmentação Atmosférica")
      ),
      bslib::card_body(
        shiny::p(
          shiny::strong(class = "text-white", "Bólidos"), " e ", shiny::strong(class = "text-white", "bolas de fogo (fireballs)"),
          " são meteoros de magnitude visual igual ou superior a -3 no zênite, resultantes da entrada de pequenos asteroides ou fragmentos cometários na atmosfera da Terra a velocidades hipersônicas."
        ),
        shiny::p(
          "O atrito e a compressão adiabática a velocidades entre 11 e 72 km/s geram pressões mecânicas extremas sobre o corpo rochoso. Quando a pressão dinâmica supera a resistência estrutural do material, o objeto fragmenta-se abruptamente no ar (",
          shiny::strong(class = "text-white", "airburst"),
          "), dissipando a maior parte de sua energia cinética antes de atingir o solo."
        ),
        shiny::div(
          class = "formula-box",
          shiny::div(class = "small text-uppercase fw-bold text-info mb-1", "Relação Empírica de Brown et al. (Nature, 2002)"),
          shiny::code("E (kt) = 8.2508 × E₀^0.885"),
          shiny::div(class = "small text-secondary mt-1", "Onde E é a energia total de impacto em kilotons de TNT e E₀ é a energia óptica radiada detectada pelos sensores (em unidades de 10¹⁰ J).")
        ),
        shiny::p(
          class = "mb-0 small text-secondary",
          "Referência: Brown, P. et al., 'The flux of small near-Earth objects colliding with the Earth', Nature, vol. 420, pp. 294–296 (2002)."
        )
      )
    ),
    bslib::card(
      bslib::card_header(
        class = "d-flex align-items-center gap-2",
        shiny::icon("satellite", class = "text-info"),
        shiny::span("Rede de Sensores & Origem dos Dados")
      ),
      bslib::card_body(
        shiny::p(
          "Os registros são mantidos e catalogados publicamente pelo ",
          shiny::tags$a("Center for Near Earth Object Studies (CNEOS)", href = "https://cneos.jpl.nasa.gov/fireballs/", target = "_blank", class = "text-info text-decoration-none fw-semibold"),
          " do Jet Propulsion Laboratory (NASA/JPL), integrando sensores governamentais dos EUA e dados dos sensores ópticos GLM dos satélites meteorológicos GOES."
        ),
        shiny::hr(style = "border-color: #1E293B;"),
        shiny::h6("Eventos Históricos de Referência", class = "text-white"),
        shiny::tags$ul(
          class = "small ps-3",
          shiny::tags$li(
            shiny::strong(class = "text-danger", "Superbólido de Chelyabinsk (15/02/2013):"),
            " ~440 kt liberados a 23,3 km de altitude na Rússia (~29x Hiroshima)."
          ),
          shiny::tags$li(
            shiny::strong(class = "text-warning", "Mar de Bering (18/12/2018):"),
            " ~173 kt de impacto a 25,6 km de altitude sobre o Pacífico Norte."
          ),
          shiny::tags$li(
            shiny::strong(class = "text-info", "Referência Hiroshima (1945):"),
            " ~15 kt de TNT (equivalente a 6,3 × 10¹³ Joules)."
          )
        ),
        shiny::hr(style = "border-color: #1E293B;"),
        shiny::p(
          class = "small text-secondary mb-0",
          "Aplicação analítica desenvolvida em R por ",
          shiny::tags$strong("Tales Gomes", class = "text-white"),
          ". Projeto originado a partir do curso de Dashboards da ",
          shiny::tags$a("Curso-R", href = "https://curso-r.com/", target = "_blank", class = "text-primary text-decoration-none"),
          "."
        )
      )
    )
  )
}
