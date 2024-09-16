library(tidyverse)
library(geojsonsf)
library(sf)
library(arrow)

lbcs_keep <- c("6600", "5200", "5210", "9100", "9200", "9900")

full <- faith_parcels <- open_dataset("data/va_statewide.parquet") %>%
  filter(rdi != "Y"| is.na(rdi)) %>% # Remove known residential properties with RDI and keep NULLS
  filter(lbcs_activity %in% lbcs_keep | is.na(lbcs_activity)) |> # Remove known non-church or unclassified properties based on LBCS and keep NULLS
  collect() |> 
  select(ll_uuid, usps_vacancy, lbcs_structure, lbcs_structure_desc, 
         lbcs_site, lbcs_site_desc)

faith <- st_read("data/va_statewide_geo.gpkg") |> 
  # st_drop_geometry() |> 
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

remove <- c("ACADEMY", "SCHOOL", "UNIVERSITY", "COLLEGE", "BROADCASTING", "RADIO",
            "NETWORK INC", "HOMES", "HOLDING CORP", "CEMETERY", "CEMETERIES", 
            "CEMETARY", "CEM", "SCHOOLS")

words_boundaries <- paste0("\\b", remove, "\\b")

pattern <- paste(words_boundaries, collapse = "|")

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
  mutate(owner = toupper(owner)) |> 
  mutate(bld_coverage = ll_bldg_footprint_sqft/ll_gissqft) |> 
  drop_na(ll_uuid) |> 
  filter(!str_detect(owner, pattern)) |> 
  distinct(ll_uuid, .keep_all = TRUE)
  

write_rds(faith_mapped, "data/faith_mapped.rds")
sf::st_write(faith_mapped, "data/faith_mapped.gpkg", driver = "GPKG")

faith_mapped <- read_rds("data/faith_mapped.rds") |> 
  filter(!str_detect(owner, pattern))

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


# Function to calculate sample size
calculate_sample_size <- function(population_size, confidence_level, margin_of_error) {
  # Z-score dictionary
  z_scores <- c(
    "0.90" = 1.645,
    "0.95" = 1.96,
    "0.99" = 2.576
  )
  
  # Calculate sample size
  z <- z_scores[as.character(confidence_level)]
  p <- 0.5  # Use 0.5 for maximum sample size
  e <- margin_of_error
  
  numerator <- (z^2 * p * (1-p)) / e^2
  denominator <- 1 + (z^2 * p * (1-p)) / (e^2 * population_size)
  
  sample_size <- numerator / denominator
  return(ceiling(sample_size))
}

# Example usage
population_size <- 22466
confidence_level <- 0.95  # 95% confidence level
margin_of_error <- 0.05  # 5% margin of error

sample_size <- calculate_sample_size(population_size, confidence_level, margin_of_error)
cat(sprintf("For a population of %d, you need a sample size of %d\n", population_size, sample_size))

# Calculate for different scenarios
scenarios <- list(
  c(0.95, 0.05),
  c(0.99, 0.05),
  c(0.95, 0.03),
  c(0.99, 0.03)
)

for (scenario in scenarios) {
  conf_level <- scenario[1]
  margin <- scenario[2]
  size <- calculate_sample_size(population_size, conf_level, margin)
  cat(sprintf("For %.0f%% confidence level and %.0f%% margin of error: %d\n", 
              conf_level * 100, margin * 100, size))
}

set.seed(123)  # for reproducibility
sample_faith <- faith_mapped %>% sample_n(378)
write_csv(sample_faith, "data/sample_faith.csv")


# Load necessary libraries
library(broom)
library(dplyr)

# Function to calculate confidence interval for a proportion
calculate_ci <- function(successes, n, conf_level = 0.95) {
  binom.test(successes, n, conf.level = conf_level) %>%
    tidy() %>%
    select(estimate, conf.low, conf.high)
}

# Function to assign confidence level based on accuracy
assign_confidence_level <- function(accuracy) {
  case_when(
    accuracy >= 0.95 ~ "Very High Confidence",
    accuracy >= 0.90 ~ "High Confidence",
    accuracy >= 0.80 ~ "Moderate Confidence",
    accuracy >= 0.70 ~ "Low Confidence",
    TRUE ~ "Very Low Confidence"
  )
}

# Sample size
n <- 378

# The sample shows 100% of entries sampled were faith-based organizations.
set.seed(123)
correct_classifications <- 378

# Calculate confidence interval
ci_results <- calculate_ci(378, n)

# Print results
cat("Sample size:", n, "\n")
cat("Correct classifications:", correct_classifications, "\n")
cat("Accuracy:", round(ci_results$estimate, 4), "\n")
cat("95% Confidence Interval: [", 
    round(ci_results$conf.low, 4), ", ", 
    round(ci_results$conf.high, 4), "]\n")

# Assign confidence level
confidence_level <- assign_confidence_level(ci_results$estimate)
cat("Confidence Level:", confidence_level, "\n")

# Calculate confidence intervals for different sample sizes
sample_sizes <- c(100, 200, 378, 500, 1000)
results <- data.frame()

for (size in sample_sizes) {
  successes <- round(size * ci_results$estimate)
  ci <- calculate_ci(successes, size)
  results <- rbind(results, data.frame(
    sample_size = size,
    accuracy = ci$estimate,
    conf_low = ci$conf.low,
    conf_high = ci$conf.high,
    confidence_level = assign_confidence_level(ci$estimate)
  ))
}

# Print results table
print(results)

