library(tidyverse)
library(mapgl)
library(tigris)
library(sf)

Sys.setenv(MAPBOX_PUBLIC_TOKEN = 'pk.eyJ1IjoiZXJpY3ZtYWkiLCJhIjoiY2xvZDVuM3BpMDNyajJrbzNxdGV2cDdsZiJ9.j5UmQvRd_iU12LtmCbjiEw')

faith <- st_read("data/va_statewide_geo.gpkg")

va <- counties(state = "VA", year = 2020)

mapboxgl(style = mapbox_style("light"),
         bounds = faith) |> 
  add_fill_layer(id = "faith",
                 source = faith,
                 fill_color = "blue",
                 fill_opacity = 0.8) |> 
  add_fill_layer(id = "boundaries",
                 source = va,
                 fill_color = "blue",
                 fill_opacity = 0.1,
                 tooltip = "NAME")
