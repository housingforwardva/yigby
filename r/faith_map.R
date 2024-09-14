library(tidyverse)
library(mapgl)
library(tigris)
library(sf)

Sys.getenv("MAPBOX_PUBLIC_TOKEN")

faith <- st_read("data/va_statewide_geo.gpkg")

va <- counties(state = "VA", year = 2020)

mapboxgl(style = mapbox_style("light"),
         bounds = faith) |> 
  add_fill_layer(id = "faith",
                 source = faith,
                 fill_color = "blue",
                 fill_opacity = 0.8,
                 tooltip = "owner") |> 
  add_line_layer(id = "boundaries",
                 source = va,
                 line_color = "#808080",
                 line_width = 0.5,
                 tooltip = "NAME")
