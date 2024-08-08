library(arrow)
library(tidyverse)
library(fuzzyjoin)
library(readr)
library(tidytext)


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


# Load in HIFLD data for Places of Worship in Virginia. Keep only the fields you
# want or need.

hifld_worship <- read_csv("data/va_worship.csv") |> 
  janitor::clean_names() |> 
  mutate(name = str_to_upper(name)) |>   # Convert to uppercase for consistency
  select(1:7)

# Find the most common terms among places of worship in Virginia. 

word_counts <- hifld_worship %>%
  unnest_tokens(word, name) %>%
  anti_join(stop_words) %>%  # Remove common stop words
  count(word, sort = TRUE)

common_words <- word_counts %>%
  filter(n > 30) %>%
  pull(word)

contains_two_common_words <- function(name, common_words) {
  words <- unlist(strsplit(tolower(name), "\\W+"))
  sum(words %in% common_words) >= 2
}

filtered_data <- hifld_worship %>%
  filter(sapply(name, contains_two_common_words, common_words = common_words))


# Consider identifying terms or columns that help indicate that a parcel 
# is not a faith-based organization. 


# rdi = Y
# lbcs_activity = https://www.planning.org/lbcs/standards/activity/

lbcs_keep <- c("9100", "9200", "9300", "9900")

faith_parcels <- open_dataset("data/va_statewide.parquet") %>%
  filter(rdi != "Y"| is.na(rdi)) %>% # Remove known residential properties with RDI and keep NULLS
  filter(lbcs_activity %in% lbcs_keep | is.na(lbcs_activity)) |> # Remove known non-church or unclassified properties based on LBCS and keep NULLS
  collect() 




# Let's try this for the baptist example.
faith_based_orgs <- open_dataset("data/va_statewide.parquet") |> 
  filter(str_detect(owner, regex("baptist|methodist|episcopal|presbyterian|lutheran|catholic|church|temple|synagogue|mosque|congregation|ministry|chapel|christian|evangelical|pentecostal|adventist|assembly of god|jehovah|latter-day saints|mormon|mennonite|quaker|friends meeting|unitarian|orthodox", ignore_case = TRUE))) |> 
  collect() |> 
  select(1:2, 11:16, 19:66)

faith_based_orgs <- open_dataset("data/va_statewide.parquet") |> 
  select(1:2, 11:16, 19:66) |> 
  mutate(
    is_faith_based = str_detect(owner, regex("baptist|methodist|episcopal|presbyterian|lutheran|catholic|church|temple|synagogue|mosque|congregation|ministry|chapel|(^|\\s)christian|evangelical|pentecostal|adventist|assembly of god|jehovah|latter-day saints|lds|mormon|mennonite|quaker|friends meeting|unitari(an|um)|orthodox", ignore_case = TRUE)),
    potential_false_negative = str_detect(owner, regex("\\b(christian|christ)(sen|son|ensen|enson)\\b", ignore_case = TRUE))
  ) |> 
  filter(is_faith_based | potential_false_negative) |>
  collect()

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