# Visualizacoes graficas interativas com Plotly ----

plotly_theme_colors <- function(is_dark = TRUE) {
  if (isTRUE(is_dark)) {
    list(
      text      = "#F1F5F9",
      muted     = "#94A3B8",
      grid      = "rgba(255, 255, 255, 0.07)",
      land      = "#131C2E",
      ocean     = "#090E17",
      coast     = "#24334C",
      border    = "#1E2A3E",
      hover_bg  = "#0F172A"
    )
  } else {
    list(
      text      = "#0F172A",
      muted     = "#475569",
      grid      = "rgba(0, 0, 0, 0.08)",
      land      = "#E2E8F0",
      ocean     = "#F8FAFC",
      coast     = "#94A3B8",
      border    = "#CBD5E1",
      hover_bg  = "#FFFFFF"
    )
  }
}

build_event_tooltip <- function(df) {
  paste0(
    "<b>", format(df$date, "%d/%m/%Y %H:%M UTC"), "</b><br>",
    "📍 Coordenadas: <b>", df$location_label, "</b><br>",
    "💥 Energia de Impacto: <b style='color:#F97316;'>", format(df$impact_e, big.mark = ".", decimal.mark = ","), " kt</b>",
    ifelse(
      !is.na(df$hiroshima_eq) & df$hiroshima_eq >= 0.1,
      paste0(" (<b>", df$hiroshima_eq, "x</b> Hiroshima)"),
      ""
    ), "<br>",
    "⚡ Energia Radiada: ", format(df$energy, big.mark = ".", decimal.mark = ","), " × 10¹⁰ J<br>",
    "🏔️ Altitude: ", ifelse(is.na(df$alt), "Não registrada", paste0(df$alt, " km (", df$atmo_layer, ")")), "<br>",
    "🚀 Velocidade: ", ifelse(is.na(df$vel), "Não registrada", paste0(df$vel, " km/s"))
  )
}

plot_fireball_geo <- function(df, projection_type = "orthographic", is_dark = TRUE) {
  geo_df <- dplyr::filter(df, !is.na(lat), !is.na(lon), !is.na(impact_e))
  pal <- plotly_theme_colors(is_dark)

  log_e <- log10(pmax(geo_df$impact_e, 0.05))
  # Ajusta o tamanho dos pontos em escala logaritmica conforme a energia
  marker_size <- scales::rescale(log_e, to = c(4, 18), from = c(log10(0.05), log10(500)))

  geo_cfg <- list(
    projection     = list(type = projection_type),
    showland       = TRUE,
    landcolor      = pal$land,
    showocean      = TRUE,
    oceancolor     = pal$ocean,
    showcountries  = TRUE,
    countrycolor   = pal$border,
    showcoastlines = TRUE,
    coastlinecolor = pal$coast,
    showframe      = FALSE,
    bgcolor        = "rgba(0,0,0,0)"
  )

  plotly::plot_geo(geo_df, lat = ~lat, lon = ~lon) |>
    plotly::add_markers(
      text = build_event_tooltip(geo_df),
      hoverinfo = "text",
      marker = list(
        size       = marker_size,
        color      = log_e,
        colorscale = list(
          c(0, "#38BDF8"),
          c(0.40, "#F59E0B"),
          c(0.70, "#F97316"),
          c(1, "#EF4444")
        ),
        cmin       = log10(0.07),
        cmax       = log10(450),
        opacity    = 0.76,
        line       = list(color = "rgba(0,0,0,0.4)", width = 0.6),
        colorbar   = list(
          title     = list(text = "Impacto (kt)", font = list(color = pal$text, size = 11)),
          tickvals  = log10(c(0.1, 1, 10, 100, 440)),
          ticktext  = c("0,1 kt", "1 kt", "10 kt", "100 kt", "440 kt"),
          tickfont  = list(color = pal$muted, size = 10),
          thickness = 10,
          len       = 0.60
        )
      )
    ) |>
    plotly::layout(
      geo           = geo_cfg,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(family = "Inter, sans-serif", color = pal$text),
      margin        = list(l = 0, r = 0, t = 10, b = 10),
      hoverlabel    = list(bgcolor = pal$hover_bg, bordercolor = "#334155", font = list(color = pal$text, size = 12))
    ) |>
    plotly::config(displaylogo = FALSE)
}

plot_fireball_timeline <- function(df, is_dark = TRUE) {
  pal <- plotly_theme_colors(is_dark)
  yearly <- df |>
    dplyr::filter(!is.na(year)) |>
    dplyr::summarise(
      eventos    = dplyr::n(),
      max_impact = max(impact_e, na.rm = TRUE),
      .by = year
    ) |>
    dplyr::arrange(year)

  plotly::plot_ly(yearly, x = ~year) |>
    plotly::add_bars(
      y = ~eventos,
      name = "Eventos Detectados",
      marker = list(
        color = "rgba(56, 189, 248, 0.45)",
        line = list(color = "#38BDF8", width = 1)
      ),
      hovertemplate = "<b>Ano %{x}</b><br>Detecções: %{y}<extra></extra>"
    ) |>
    plotly::add_lines(
      y = ~max_impact,
      yaxis = "y2",
      name = "Pico de Impacto (kt)",
      line = list(color = "#F97316", width = 2.2),
      marker = list(color = "#F97316", size = 5),
      hovertemplate = "<b>Ano %{x}</b><br>Maior Impacto: %{y:.2f} kt<extra></extra>"
    ) |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(family = "Inter, sans-serif", color = pal$text),
      xaxis         = list(title = "Ano de Detecção", gridcolor = pal$grid, zeroline = FALSE, tickfont = list(color = pal$muted)),
      yaxis         = list(title = "Nº de Eventos", gridcolor = pal$grid, zeroline = FALSE, tickfont = list(color = pal$muted)),
      yaxis2        = list(
        title      = "Pico Anual de Impacto (kt)",
        overlaying = "y",
        side       = "right",
        type       = "log",
        showgrid   = FALSE,
        tickfont   = list(color = "#F97316")
      ),
      legend        = list(orientation = "h", x = 0, y = 1.12, font = list(color = pal$text, size = 11)),
      margin        = list(l = 50, r = 55, t = 30, b = 45),
      hovermode     = "x unified",
      hoverlabel    = list(bgcolor = pal$hover_bg, bordercolor = "#334155")
    ) |>
    plotly::config(displaylogo = FALSE)
}

plot_atmosphere_profile <- function(df, is_dark = TRUE) {
  pal <- plotly_theme_colors(is_dark)
  atmo_df <- dplyr::filter(df, !is.na(alt), !is.na(vel), !is.na(impact_e))

  log_e <- log10(pmax(atmo_df$impact_e, 0.05))
  sz <- scales::rescale(log_e, to = c(5, 18), from = c(log10(0.05), log10(500)))

  plotly::plot_ly(atmo_df, x = ~vel, y = ~alt) |>
    plotly::add_markers(
      text = build_event_tooltip(atmo_df),
      hoverinfo = "text",
      marker = list(
        size       = sz,
        color      = log_e,
        colorscale = "Inferno",
        opacity    = 0.78,
        line       = list(color = "rgba(0,0,0,0.3)", width = 0.6)
      ),
      showlegend = FALSE
    ) |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(family = "Inter, sans-serif", color = pal$text),
      xaxis         = list(title = "Velocidade de Entrada (km/s)", gridcolor = pal$grid, zeroline = FALSE, tickfont = list(color = pal$muted)),
      yaxis         = list(title = "Altitude de Fragmentação (km)", gridcolor = pal$grid, range = c(0, 95), tickfont = list(color = pal$muted)),
      shapes        = list(
        list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 12, y1 = 12,
             line = list(color = "#38BDF8", dash = "dot", width = 1.2)),
        list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 50, y1 = 50,
             line = list(color = "#A855F7", dash = "dot", width = 1.2)),
        list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 85, y1 = 85,
             line = list(color = "#F97316", dash = "dot", width = 1.2))
      ),
      annotations   = list(
        list(x = 0.98, y = 6,  xref = "paper", yref = "y", text = "Troposfera (<12 km)",
             showarrow = FALSE, xanchor = "right", font = list(size = 11, color = "#38BDF8")),
        list(x = 0.98, y = 31, xref = "paper", yref = "y", text = "Estratosfera (12–50 km)",
             showarrow = FALSE, xanchor = "right", font = list(size = 11, color = "#A855F7")),
        list(x = 0.98, y = 68, xref = "paper", yref = "y", text = "Mesosfera (50–85 km)",
             showarrow = FALSE, xanchor = "right", font = list(size = 11, color = "#F97316"))
      ),
      margin        = list(l = 55, r = 20, t = 20, b = 45),
      hoverlabel    = list(bgcolor = pal$hover_bg, bordercolor = "#334155")
    ) |>
    plotly::config(displaylogo = FALSE)
}

plot_energy_correlation <- function(df, var_x, var_y, is_dark = TRUE) {
  pal <- plotly_theme_colors(is_dark)
  sub_df <- df[!is.na(df[[var_x]]) & !is.na(df[[var_y]]) & df[[var_x]] > 0 & df[[var_y]] > 0, ]

  log_e <- log10(pmax(sub_df$impact_e, 0.05))
  sz <- scales::rescale(log_e, to = c(5, 18), from = c(log10(0.05), log10(500)))

  shapes_list <- list()
  ann_list    <- list()

  if (var_y == "impact_e") {
    shapes_list <- list(
      list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = HIROSHIMA_KT, y1 = HIROSHIMA_KT,
           line = list(color = "#F59E0B", dash = "dash", width = 1.3)),
      list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = CHELYABINSK_KT, y1 = CHELYABINSK_KT,
           line = list(color = "#EF4444", dash = "dash", width = 1.3))
    )
    ann_list <- list(
      list(x = 0.02, y = log10(HIROSHIMA_KT), xref = "paper", yref = "y",
           text = "Hiroshima (~15 kt)", showarrow = FALSE, xanchor = "left",
           yshift = 10, font = list(size = 11, color = "#F59E0B")),
      list(x = 0.02, y = log10(CHELYABINSK_KT), xref = "paper", yref = "y",
           text = "Chelyabinsk 2013 (440 kt)", showarrow = FALSE, xanchor = "left",
           yshift = -12, font = list(size = 11, color = "#EF4444"))
    )
  }

  plotly::plot_ly(sub_df, x = sub_df[[var_x]], y = sub_df[[var_y]]) |>
    plotly::add_markers(
      text = build_event_tooltip(sub_df),
      hoverinfo = "text",
      marker = list(
        size       = sz,
        color      = log_e,
        colorscale = "Plasma",
        opacity    = 0.78,
        line       = list(color = "rgba(0,0,0,0.3)", width = 0.6)
      ),
      showlegend = FALSE
    ) |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(family = "Inter, sans-serif", color = pal$text),
      xaxis         = list(title = unname(VAR_LABELS[var_x]), type = "log", gridcolor = pal$grid, tickfont = list(color = pal$muted)),
      yaxis         = list(title = unname(VAR_LABELS[var_y]), type = "log", gridcolor = pal$grid, tickfont = list(color = pal$muted)),
      shapes        = shapes_list,
      annotations   = ann_list,
      margin        = list(l = 55, r = 20, t = 20, b = 45),
      hoverlabel    = list(bgcolor = pal$hover_bg, bordercolor = "#334155")
    ) |>
    plotly::config(displaylogo = FALSE)
}
