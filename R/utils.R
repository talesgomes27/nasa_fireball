# Data used in the project
# https://cneos.jpl.nasa.gov/fireballs/

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
