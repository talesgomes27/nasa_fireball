# Load Library ------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(reactable)

# Load auxiliary files
source("R/utils.R", local = T)
source("R/plots.R", local = T)
