library(tidyverse)
library(sf)
library(geojsonsf)
library(scales)


hdistricts <- geojson_sf("data/faith_gen_assembly.geojson") |> 
  st_drop_geometry() |> 
  rename(house = NAMELSAD)|> 
  rename(senate = NAMELSAD_2) |> 
  distinct(ll_uuid, .keep_all = TRUE)

faith_summary_ga <- hdistricts |> 
  group_by(house) |> 
  summarise(total_acres = as.numeric(format(sum(acres), scientific = FALSE)),
            avg_parcel_size = mean(acres),
            med_parcel_size = median(acres),
            count = as.numeric(n_distinct(ll_uuid)))

write_csv(faith_summary_ga, "data/faith_house.csv")


sdistricts <- geojson_sf("data/faith_gen_assembly.geojson") |> 
  st_drop_geometry() |> 
  rename(house = NAMELSAD)|> 
  rename(senate = NAMELSAD_2) |> 
  distinct(ll_uuid, .keep_all = TRUE)

faith_summary_ga <- sdistricts |> 
  group_by(senate) |> 
  summarise(total_acres = as.numeric(format(sum(acres), scientific = FALSE)),
            avg_parcel_size = mean(acres),
            med_parcel_size = median(acres),
            count = as.numeric(n_distinct(ll_uuid)))

write_csv(faith_summary_ga, "data/faith_senate.csv")



hd <- geojson_sf("data/geo/faith_house_clip.geojson") |> 
  mutate(across(3:5, .fns = ~as.numeric(.x))) |> 
  mutate(label_count = comma(faith_house_count)) |> 
  mutate(NAMELSAD = str_remove(NAMELSAD, "State House District ")) |> 
  mutate(faith_house_total_acres = round(faith_house_total_acres, digits = 2)) |> 
  mutate(label_acres = paste(comma((faith_house_total_acres)), "acres", sep = " "))

med_size <- read_csv("data/faith_house.csv") |> 
  mutate(house = str_remove(house, "State House District ")) |> 
  select(NAMELSAD = house, med_parcel_size)

hd <- hd |> 
  left_join(
    med_size, 
    by = "NAMELSAD"
  ) |> 
  mutate(
    med_parcel_size = round(med_parcel_size, 2)
  ) |> 
  mutate(label_size = round(med_parcel_size, digits = 2))

write_rds(hd, "data/rds/faith_house.rds")


sd <- geojson_sf("data/geo/faith_senate_clip.geojson") |> 
  mutate(across(3:5, .fns = ~as.numeric(.x))) |> 
  mutate(label_count = comma(faith_senate_count)) |> 
  mutate(NAMELSAD = str_remove(NAMELSAD, "State Senate District ")) |> 
  mutate(faith_senate_total_acres = round(faith_senate_total_acres, digits = 2)) |> 
  mutate(label_acres = paste(comma((faith_senate_total_acres)), "acres", sep = " "))

med_size <- read_csv("data/faith_senate.csv") |> 
  mutate(senate = str_remove(senate, "State Senate District ")) |> 
  select(NAMELSAD = senate, med_parcel_size)

sd <- sd |> 
  left_join(
    med_size, 
    by = "NAMELSAD"
  ) |> 
  mutate(
    med_parcel_size = round(med_parcel_size, 2)
  ) |> 
  mutate(label_size = round(med_parcel_size, digits = 2))

write_rds(sd, "data/rds/faith_senate.rds")