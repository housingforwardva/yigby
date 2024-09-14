library(tidyverse)
library(sf)
library(scales)


# To conduct an analysis of faith-based parcels in Northern Virginia with the
# Northern Virginia Zoning Atlas, you need to conduct a spatial join. The parcels 
# should be spatially joined after clipping the NOVA zoning atlas geojson based on 
# a mask of the parcels. Then conduct a spatial join. 

# nova <- st_read("data/faith_mapped.gpkg") |> 
#   filter(pdc == "Northern Virginia")
# 
# st_write(nova, "data/nova_faith.gpkg", driver = "GPKG")

# Based on the join, there are 4 out 653 parcels that were not joined because they
# were in an area that was deemed not developable by the Virginia Zoning Atlas.

nova_zoning <- st_read("data/nova_faith_zoning.gpkg") 

count_type <- as.data.frame(table(nova_zoning$jurisdiction_2, useNA = "always")) 

count_type_area <- nova_zoning |> 
  group_by(jurisdiction, type) |> 
  summarise(acres = sum(acres),
            count = n_distinct(ll_uuid)) |> 
  drop_na(jurisdiction)
  

nova_zoning_apts <- nova_zoning |> 
  filter(family4_treatment == "allowed")

# Out of 649 properties, only 205 of them allow for multifamily housing by-right.

nova_zoning_no_housing <- nova_zoning |> 
  filter(family1_treatment == "prohibited" & family2_treatment == "prohibited" & family3_treatment == "prohibited" & family4_treatment == "prohibited")

# Out of 649 properties, there are 132 where no housing is allowed at all, either through public hearing or by-right.

nova_zoning_lot <- nova_zoning |> 
  filter(family4_treatment == "allowed" | family4_treatment == "hearing") |> 
  mutate(lot_fit = case_when(
    acres >= min4 ~ TRUE,
    TRUE ~ FALSE
  )) |> 
  select(jurisdiction, family4_treatment, abbrvname, ll_uuid, owner, acres, min4, lot_fit) |> 
  group_by(lot_fit) |> 
  summarise(count = n_distinct(ll_uuid))

# Based on the lots where multifamily is allowed by-right or through a public hearing, 196 
# of those lots meet the minimum lot requirements, whereas 30 do not meet the minimum lot 
# size requirements.


# NOVA has decent coverage for the building count and building footprint data from the Regrid
# dataset. You can calculate the current lot coverage for all buildings and potentially identify
# vacant lots or lots with a significant amount of available land that can be developed/redeveloped.

nova_building <- nova_zoning |> 
  select(jurisdiction, owner, abbrvname, type, family1_treatment,
         family2_treatment, family3_treatment, family4_treatment,
         ll_bldg_count, ll_bldg_footprint_sqft, acres) |> 
  mutate(bld_acres = ll_bldg_footprint_sqft/43560) |> 
  mutate(bld_acres = ifelse(is.na(bld_acres), 0, bld_acres)) |> 
  mutate(bld_coverage = bld_acres/acres)

# A map can be created with the mapgl package and used to verify the results.

nova_zoning_summary <- nova_zoning |> 
  group_by(jurisdiction) |> 
  mutate(total_area = n_distinct(ll_uuid)) |> 
  mutate(family1_ct = case_when(
    family1_treatment == "allowed" ~ 1,
    TRUE ~ 0
  ))|> 
  mutate(family2_ct = case_when(
    family2_treatment == "allowed" ~ 1,
    TRUE ~ 0 
  )) |> 
  mutate(family3_ct = case_when(
    family3_treatment == "allowed" ~ 1,
    TRUE ~ 0 
  )) |> 
  mutate(family4_ct = case_when(
    family4_treatment == "allowed" ~ 1,
    TRUE ~ 0 
  )) |> 
  select(jurisdiction, family1_ct, family2_ct,
         family3_ct, family4_ct, total_area)

nova_summary <- nova_zoning_summary |> 
  group_by(jurisdiction, total_area) |> 
  summarise(family1_byright = sum(family1_ct),
            family2_byright = sum(family2_ct),
            family3_byright = sum(family3_ct),
            family4_byright = sum(family4_ct),
            .groups = "drop") |> 
  mutate(across(family1_byright:family4_byright, ~ .x / total_area)) |> 
  mutate(across(family1_byright:family4_byright, ~ scales::percent(.x, accuracy = 0.01)))

across()
  

  

count_structure <- as.data.frame(table(faith$lbcs_site.x, useNA = "always"))
count_site <- as.data.frame(table(faith$lbcs_structure, useNA = "always"))



