library(tidyverse)
library(sf)
library(geojsonsf)
library(scales)
library(rmapshaper)


# hdistricts <- geojson_sf("data/geo/faith_join_ga_21.geojson") |>
#   st_drop_geometry() |>
#   rename(house = hd)|>
#   rename(senate = sd) |>
#   distinct(ll_uuid, .keep_all = TRUE)
# 
# faith_summary_ga <- hdistricts |>
#   group_by(house) |>
#   summarise(total_acres = as.numeric(format(sum(acres), scientific = FALSE)),
#             avg_parcel_size = mean(acres),
#             med_parcel_size = median(acres),
#             count = as.numeric(n_distinct(ll_uuid)))
# 
# write_csv(faith_summary_ga, "data/faith_house.csv")
# 
# 
# sdistricts <- geojson_sf("data/geo/faith_join_ga_21.geojson") |>
#   st_drop_geometry() |>
#   rename(house = hd)|>
#   rename(senate = sd) |>
#   distinct(ll_uuid, .keep_all = TRUE)
# 
# faith_summary_ga <- sdistricts |>
#   group_by(senate) |>
#   summarise(total_acres = as.numeric(format(sum(acres), scientific = FALSE)),
#             avg_parcel_size = mean(acres),
#             med_parcel_size = median(acres),
#             count = as.numeric(n_distinct(ll_uuid)))
# 
# write_csv(faith_summary_ga, "data/faith_senate.csv")



hd <- geojson_sf("data/geo/faith_house_21.geojson") |>
  ms_simplify(keep = 0.1, keep_shapes = TRUE) |> 
  mutate(across(4:7, .fns = ~as.numeric(.x))) |> 
  mutate(label_count = comma(faith_house_count)) |> 
  mutate(district = DISTRICT) |> 
  mutate(faith_house_total_acres = round(faith_house_total_acres, digits = 2)) |> 
  mutate(label_acres = paste(comma((faith_house_total_acres)), "acres", sep = " ")) |> 
  mutate(
    med_parcel_size = round(faith_house_med_parcel_size, 2)
  ) |> 
  mutate(label_size = round(med_parcel_size, digits = 2))


write_rds(hd, "data/rds/faith_house.rds")


sd <- geojson_sf("data/geo/faith_senate_21.geojson") |>
  ms_simplify(keep = 0.1, keep_shapes = TRUE) |> 
  mutate(across(3:7, .fns = ~as.numeric(.x))) |> 
  mutate(label_count = comma(faith_senate_count)) |> 
  mutate(district = DISTRICT) |> 
  mutate(faith_senate_total_acres = round(faith_senate_total_acres, digits = 2)) |> 
  mutate(label_acres = paste(comma((faith_senate_total_acres)), "acres", sep = " ")) |> 
  mutate(
    med_parcel_size = round(faith_senate_med_parcel_size, 2)
  ) |> 
  mutate(label_size = round(med_parcel_size, digits = 2))

write_rds(sd, "data/rds/faith_senate.rds")
