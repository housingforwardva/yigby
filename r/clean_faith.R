library(tidyverse)
library(geojsonsf)
library(sf)


faith <- st_read("data/va_statewide_geo.gpkg") |> 
  st_drop_geometry() |> 
  janitor::clean_names() 

lookup <- read_csv("data/local_lookup.csv") |> 
  janitor::clean_names() |> 
  mutate(fips_full = as.character(fips_full))

remove_NA <- function(string) {
  gsub("\\s+", " ", gsub("\\bNA\\b", "", string, ignore.case = FALSE))
}


faith_mapped <- faith |> 
  left_join(lookup, by = c("geoid" = "fips_full" )) |> 
  select(ll_uuid, geoid, jurisdiction = name_long, cbsa =  cbsa_title, pdc, owner, 
         usecode, usedesc, struct, improvval,
         landval, parval, owntype, saddno, saddpref,
         saddsttyp, saddstsuf, sunit, scity, county,
         szip, lat, lon, ll_gisacre, ll_gissqft, ll_bldg_count, 
         ll_bldg_footprint_sqft, acres) |> 
  mutate(address = paste(saddno, saddpref,
                         saddsttyp, saddstsuf, sunit, scity, county, szip, sep = " ")) |> 
  mutate(address = toupper(trimws(remove_NA(address)))) |> 
  mutate(bld_coverage = ll_bldg_footprint_sqft/ll_gissqft) |> 
  drop_na(ll_uuid)

write_rds(faith_mapped, "data/faith_mapped.rds")

faith_summary <- faith_mapped |> 
  group_by(jurisdiction) |> 
  summarise(total_acres = format(sum(acres), scientific = FALSE),
            avg_parcel_size = mean(acres))

faith_summary_cbsa <- faith_mapped |> 
  group_by(cbsa) |> 
  summarise(total_acres = format(sum(acres), scientific = FALSE),
            avg_parcel_size = mean(acres))

faith_owner_summary <- faith_mapped |> 
  group_by(owner) |> 
  summarise(total_acres = format(sum(acres), scientific = FALSE))



