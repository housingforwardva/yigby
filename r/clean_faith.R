library(tidyverse)
library(geojsonsf)
library(sf)
library(arrow)

full <- faith_parcels <- open_dataset("data/va_statewide.parquet") %>%
  filter(rdi != "Y"| is.na(rdi)) %>% # Remove known residential properties with RDI and keep NULLS
  filter(lbcs_activity %in% lbcs_keep | is.na(lbcs_activity)) |> # Remove known non-church or unclassified properties based on LBCS and keep NULLS
  collect() |> 
  select(ll_uuid, usps_vacancy, lbcs_structure, lbcs_structure_desc, 
         lbcs_site, lbcs_site_desc)

faith <- st_read("data/va_statewide_geo.gpkg") |> 
  st_drop_geometry() |> 
  janitor::clean_names() |> 
  left_join(full, by = "ll_uuid")

count_vacancy <- as.data.frame(table(faith$usps_vacancy, useNA = "always"))
count_structure <- as.data.frame(table(faith$lbcs_site.x, useNA = "always"))
count_site <- as.data.frame(table(faith$lbcs_structure, useNA = "always"))


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



