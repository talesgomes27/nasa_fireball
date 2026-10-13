# Ingestao e tratamento de dados da API CNEOS/NASA ----

NASA_API_URL <- "https://ssd-api.jpl.nasa.gov/fireball.api"
CACHE_PATH   <- file.path("data", "fireball_cache.rds")
HIROSHIMA_KT <- 15.0    # Referencia historica: ~15 kt TNT
CHELYABINSK_KT <- 440.0 # Superbolido de Chelyabinsk (2013): ~440 kt TNT

# Rótulos das variáveis numéricas em Português (PT-BR)
VAR_LABELS <- c(
  "impact_e" = "Energia de Impacto (kt TNT)",
  "energy"   = "Energia Radiada (10¹⁰ J)",
  "vel"      = "Velocidade Pré-Impacto (km/s)",
  "alt"      = "Altitude de Pico (km)"
)

classify_atmosphere <- function(alt_km) {
  dplyr::case_when(
    is.na(alt_km)  ~ "Não rastreada",
    alt_km < 12    ~ "Troposfera (< 12 km)",
    alt_km <= 50   ~ "Estratosfera (12–50 km)",
    alt_km <= 85   ~ "Mesosfera (50–85 km)",
    TRUE           ~ "Termosfera (> 85 km)"
  )
}

format_coords <- function(lat, lon) {
  ifelse(
    !is.na(lat) & !is.na(lon),
    sprintf(
      "%.1f°%s, %.1f°%s",
      abs(lat), ifelse(lat >= 0, "N", "S"),
      abs(lon), ifelse(lon >= 0, "E", "W")
    ),
    "Sem coordenadas"
  )
}

parse_fireball_payload <- function(raw) {
  df <- as.data.frame(raw$data, stringsAsFactors = FALSE)
  names(df) <- raw$fields

  lat_num <- suppressWarnings(as.numeric(df[["lat"]]))
  lon_num <- suppressWarnings(as.numeric(df[["lon"]]))
  lat_val <- ifelse(df[["lat-dir"]] == "S", -lat_num, lat_num)
  lon_val <- ifelse(df[["lon-dir"]] == "W", -lon_num, lon_num)
  alt_val <- suppressWarnings(as.numeric(df[["alt"]]))
  vel_val <- suppressWarnings(as.numeric(df[["vel"]]))
  imp_val <- suppressWarnings(as.numeric(df[["impact-e"]]))
  eng_val <- suppressWarnings(as.numeric(df[["energy"]]))
  dt_val  <- as.POSIXct(df[["date"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  out <- tibble::tibble(
    date           = dt_val,
    year           = as.integer(format(dt_val, "%Y")),
    energy         = eng_val,
    impact_e       = imp_val,
    hiroshima_eq   = round(imp_val / HIROSHIMA_KT, 2),
    alt            = alt_val,
    vel            = vel_val,
    lat            = lat_val,
    lon            = lon_val,
    atmo_layer     = classify_atmosphere(alt_val),
    location_label = format_coords(lat_val, lon_val)
  )

  out[order(out$date, decreasing = TRUE), ]
}

fetch_fireball_data <- function(api_url = NASA_API_URL, cache_path = CACHE_PATH) {
  # Consulta a API da NASA com User-Agent para evitar bloqueio 403 e fallback em cache local
  live_result <- tryCatch({
    old_ua <- getOption("HTTPUserAgent")
    on.exit(options(HTTPUserAgent = old_ua), add = TRUE)
    options(HTTPUserAgent = "Mozilla/5.0 (compatible; NASA-Fireball-App/1.0)")

    raw <- jsonlite::fromJSON(api_url, simplifyDataFrame = TRUE)
    df <- parse_fireball_payload(raw)
    if (dir.exists(dirname(cache_path))) {
      try(saveRDS(df, cache_path), silent = TRUE)
    }
    list(data = df, source = "live", updated_at = Sys.time())
  }, error = function(e) NULL)

  if (!is.null(live_result)) return(live_result)

  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    mtime <- file.info(cache_path)$mtime
    return(list(data = df, source = "cache", updated_at = mtime))
  }

  stop("Não foi possível obter os dados da NASA API nem localizar o cache local.")
}

filter_fireballs <- function(df, date_range, min_impact_e) {
  min_e <- as.numeric(min_impact_e)
  start_dt <- as.POSIXct(paste0(as.Date(date_range[1]), " 00:00:00"), tz = "UTC")
  end_dt   <- as.POSIXct(paste0(as.Date(date_range[2]), " 23:59:59"), tz = "UTC")

  dplyr::filter(
    df,
    !is.na(date),
    date >= start_dt,
    date <= end_dt,
    !is.na(impact_e) & impact_e >= min_e
  )
}
