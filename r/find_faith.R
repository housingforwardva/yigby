library(arrow)
library(tidyverse)

va <- open_csv_dataset("data/va_statewide.csv")

# Read everything in as character (as some fields are getting incorrectly
# flagged as nulls)
#
# If you want, convert numeric fields back after you collect or 
# set up a more detailed schema.  
va_schema <- schema(
  map(names(va), ~Field$create(name = .x, type = string()))
)

# Open the dataset again
va <- open_csv_dataset("data/va_statewide.csv", schema = va_schema)

# We can write to a compressed parquet file that saves the schema if we want
write_parquet(va, "data/va_statewide.parquet")

# The parquet file is only 909MB as opposed to the 4.7GB CSV file.

# The way I like to work with arrow is to treat the dataset as if it were a 
# database using dplyr.  You can query the larger file, then "collect"
# the information you need into your R session.  
#
# Let's try this for the baptist example.
faith_based_orgs <- open_dataset("data/va_statewide.parquet") |> 
  filter(str_detect(owner, regex("baptist|methodist|episcopal|presbyterian|lutheran|catholic|church|temple|synagogue|mosque|congregation|ministry|chapel|christian|evangelical|pentecostal|adventist|assembly of god|jehovah|latter-day saints|mormon|mennonite|quaker|friends meeting|unitarian|orthodox", ignore_case = TRUE))) |> 
  collect() |> 
  select(1:2, 11:16, 19:66)

# We've cut the original dataset with 4.2 million rows down to 11,541 rows
# with "(Bb)aptist" in their `owner` column. 
baptist

# Working with GeoPackages is a little trickier as we can't use 
# `arrow::open_dataset()` on them yet. However, there are some ways we 
# can crack into the data.

# Let's take a look first at the layers in the GeoPackage:
library(sf)

st_layers("va_statewide.gpkg")

query <- "SELECT * FROM va_statewide WHERE owner LIKE '%BAPTIST%'"

baptist_sf <- st_read("va_statewide.gpkg", query = query)

# We'll trim down our data a bit and take a look at it: 
library(mapview)

baptist_sf_min <- baptist_sf |> 
  select(parcelnumb, zoning_description, yearbuilt, 
         owner, parval)

mapview(baptist_sf_min)