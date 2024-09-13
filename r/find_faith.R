library(arrow)
library(tidyverse)
library(fuzzyjoin)
library(readr)
library(tidytext)
library(babynames)
library(lexicon)
library(sf)



# va <- open_csv_dataset("data/va_statewide.csv")
# 
# # Read everything in as character (as some fields are getting incorrectly
# # flagged as nulls)
# #
# # If you want, convert numeric fields back after you collect or 
# # set up a more detailed schema.  
# va_schema <- schema(
#   map(names(va), ~Field$create(name = .x, type = string()))
# )
# 
# # Open the dataset again
# va <- open_csv_dataset("data/va_statewide.csv", schema = va_schema)
# 
# # We can write to a compressed parquet file that saves the schema if we want
# write_parquet(va, "data/va_statewide.parquet")

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

## ---- START HERE ---- 

# Consider identifying terms or columns that help indicate that a parcel 
# is not a faith-based organization. 


# rdi = Y
# lbcs_activity = https://www.planning.org/lbcs/standards/activity/

## THE BELOW LBCS ACTIVITY CODES SIGNIFY
lbcs_keep <- c("6600", "5200", "5210", "9100", "9200", "9900")

faith_parcels <- open_dataset("data/va_statewide.parquet") %>%
  filter(rdi != "Y"| is.na(rdi)) %>% # Remove known residential properties with RDI and keep NULLS
  filter(lbcs_activity %in% lbcs_keep | is.na(lbcs_activity)) |> # Remove known non-church or unclassified properties based on LBCS and keep NULLS
  collect() 

## The above reduces the entry count to 811,545 parcels. Much more manageable. 

# Create a word list based on common faith-related words from the HIFLD data.

# word_list <- c("church", "baptist", "ministries", "christian", "god",
#                     "christ", "fellowship", "faith", "lutheran", "assembly",
#                     "presbyterian", "iglesia", "pentecostal", "grace", "outreach",
#                     "gospel", "ministry", "hope", "temple", "holiness", "worship",
#                     "chapel", "bible", "tabernacle", "methodist", "mount", "covenant",
#                     "jesus", "deliverance", "evangelical", "holy", "korean", "calvary",
#                     "zion", "prophecy", " apostolic", "bahais", "episcopal", "kingdom",
#                     "dios", "spiritual", "trinity", "prayer", "bethel", "catholic", 
#                     "orthodox", "missionary", "saint", "spirit", "islamic", "nazarene",
#                     "congregation", "cristo", "evangelistic", "revival", "antioch", "peace",
#                     "agape", "buddhist", "lord", "mercy", "assemblies", "miniesterio", 
#                     "evangelica", "unity", "saints", "mision", "emmanuel", "imanuel",
#                     "anglican", "cathedral", "coptic", "redeem", "bethlehem", "shalom", "muslim",
#                     "resurrection", "mennonite", "adventist", "diocese", "disciples", "ebenezer",
#                     "heaven", "religious", "cristiano", "emanuel", "israel", "cielo", "chruch",
#                     "reformation", "universalist"
#                     )

# Try a word list that removes common words that could lead to false positives.

word_list <- c("ame", "baha'i", "congregacion", "church", "baptist", "ministries", "christian", "god",
               "christ", "fellowship", "lutheran", "assembly", "diocesan",
               "presbyterian", "iglesia", "pentecostal", 
               "gospel", "ministry", "temple", "holiness", "worship",
               "chapel", "bible", "tabernacle", "methodist", "covenant",
               "jesus", "deliverance", "evangelical", "holy", "korean", "calvary",
               "zion", "prophecy", " apostolic", "bahais", "episcopal", "kingdom",
               "dios", "spiritual", "prayer", "bethel", "catholic", 
               "orthodox", "missionary", "islamic", "nazarene",
               "congregation", "cristo", "evangelistic", "revival", "antioch", "peace",
               "agape", "buddhist", "lord", "mercy", "assemblies", "miniesterio", 
               "evangelica", "unity", "saints", "mision", 
               "anglican", "cathedral", "coptic", "redeem", "bethlehem", "shalom", "muslim",
               "resurrection", "mennonite", "adventist", "diocese", "disciples", "ebenezer",
               "heaven", "religious", "cristiano", "cielo", "chruch",
               "reformation", "universalist", "masjid", "mission", "mosque", "quaker", "shrine",
               "synagogue", "buddhist", "buddha"
)

# word_list <- as.data.frame(word_list)
# 
# write_csv(word_list, "word_list.csv")

words_boundaries <- paste0("\\b", word_list, "\\b")

pattern <- paste(words_boundaries, collapse = "|")


## BASED ON THE WORD LIST. FILTER THE OWNER NAME COLUMN BASED ON WORDS ASSOCIATED
## WITH FAITH-BASED ORGANIZATIONS
  
faith_found <- faith_parcels |> 
  filter(str_detect(owner, regex(pattern, ignore_case = TRUE))) 

## THE ABOVE REDUCES THE PARCEL COUNT FROM 811,545 TO 25,121. 
## THE USEDESC COLUMN WILL BE HELPFUL TO FINDING PARCELS THAT ARE CONFIRMED 
## AS RELIGIOUS OR A CEMETERY. CEMETERY PARCELS CAN BE REMOVED BECAUSE THEY ARE
## UNLIKELY TO EVER BE REDEVELOPED.


use_religious <- c("RELIGIOUS", "CHURCH", "WORSHIP", "SANCTUARY", "SANCT")
use_religious_pattern <- paste(use_religious, collapse = "|")

use_cemetery <- c("CEMETERY", "CEMETERIES", "CEMETARY")
use_cemetery_pattern <- paste(use_cemetery, collapse = "|")

faith_found_2 <- faith_found |> 
  select(file_name, geoid, parcelnumb, parcelnumb_no_formatting, owner, usecode, usedesc, struct,
         improvval, landval, parval, owntype, owner, saddno,
         saddpref, saddstr, saddsttyp, saddstsuf, sunit, scity, city, county, szip,
         lat, lon, ll_uuid, ll_gisacre, ll_gissqft, ll_bldg_count, ll_bldg_footprint_sqft, 
         rdi, lbcs_activity, lbcs_function_desc, lbcs_function, lbcs_site, lbcs_site_desc,
         lbcs_ownership, lbcs_ownership_desc) |> 
  mutate(usedesc = toupper(usedesc)) |> 
  mutate(religious_use = str_detect(usedesc, use_religious_pattern)) |> 
  mutate(cemetery_use = str_detect(usedesc, use_cemetery_pattern)) 


## SEPARATE DATA FRAMES OF PARCELS THAT ARE CONFIRMED AS RELIGIOUS BASED ON THEIR
## USE DESCRIPTIONS.
faith_confirmed <- faith_found_2 |> 
  filter(religious_use == TRUE)

## FILTER OUT PARCELS THAT WERE REMOVED FROM THE PREVIOUS AND ALSO REMOVE PARCELS 
## THAT ARE LIKELY CEMETERIES.
faith_found_3 <- faith_found_2 |> 
  filter(cemetery_use != TRUE | is.na(cemetery_use)) |> 
  filter(religious_use != TRUE | is.na(religious_use)) |> 
  mutate(owner_cemetery = str_detect(owner, use_cemetery_pattern)) |> 
  filter(owner_cemetery != TRUE | is.na(owner_cemetery))

## SOME WORDS FROM THE WORD LIST ARE MORE LIKELY TO BE FAITH-BASED ORGANIZATIONS 
## THAN OTHERS. YOU CAN USE THESE WORDS TO REMOVE PARCELS FROM THE SEARCH AND LATER 
## MERGE THEM WITH FAITH_CONFIRMED ABOVE.

# WORDS THAT WILL LIKELY NOT RESULT IN FALSE POSITIVES:

positive_words <- c("BAPTIST", "LUTHERAN", "EPISCOPAL", "PRESBYTERIAN", "METHODIST", 
                    "WORSHIP", "CATHOLIC", "DIOCESAN", "DIOCESE", "MASJID", "MOSQUE",
                    "SYNAGOGUE", "ISLAMIC", "PENTECOSTAL", "TABERNACLE", "CHRIST", 
                    "BAPT", "METH", "DIOCESEAN", "BIBLE", "UNIVERSALIST", "MENNOITE",
                    "MENNONITE", "PENTACOSTAL", "CHURCH OF GOD", "COMMUNITY CHURCH",
                    "ORTHODOX CHURCH", "CHRISTIAN CHURCH", "CHAPEL CHURCH", "BRETHREN CHURCH",
                    "CHURCH OF", "ASSEMBLY OF")

words_boundaries <- paste0("\\b", positive_words, "\\b")

pattern <- paste(words_boundaries, collapse = "|")

# CREATE A BINARY COLUMN THAT DENOTES WHETHER THE OWNER NAME CONTAINS 
# ONE OF THE POSITIVE WORDS LISTED ABOVE. THESE ARE LIKELY FAITH-ORGS AND 
# SHOULD BE RETAINED.

faith_found_4 <- faith_found_3 |> 
  mutate(faith_positive = str_detect(owner, regex(pattern, ignore_case = TRUE))) 

# KEEP ENTRIES WHERE A POSITIVE WORD IS PRESENT.

faith_confirmed_2 <- faith_found_4 |> 
  filter(faith_positive == TRUE)

# FILTER OUT ENTRIES THAT WERE RETAINED AND THEN CREATE A BINARY TO IDENTIFY 
# ENTRIES WHERE THE USECODE COLUMN INDICATES A RELIGIOUS ENTITY.

faith_found_5 <- faith_found_4 |> 
  filter(faith_positive != TRUE | is.na(faith_positive)) |> 
  mutate(usecode = toupper(usecode)) |> 
  mutate(usecode_religious = str_detect(usecode, use_religious_pattern))

count_unique <- as.data.frame(table(faith_found_5$usecode, useNA = "always"))

# KEEP ENTRIES WHERE A RELIGIOUS ENTITY IS FOUND USING THE USECODE COLUMN.

faith_confirmed_3 <- faith_found_5 |> 
  filter(usecode_religious == TRUE)

## THERE ARE CERTAIN WORDS AND PHRASES THAT ARE NOT CHURCH ORGS OR LEAD TO FALSE 
## POSITIVES. USE THOSE WORDS TO REMOVE THEM FROM DATA FRAME.

## CHOOSING TO REMOVE YMCA ORGANIZATIONS.

negative_phrases <- c("FALLS CHURCH", "FIRE", "HOMEOWNERS", "COMMUNITY ASSOCIATION",
                      "COMMUNITY ASSOC", "MOTOR", "CHURCH STREET", "BETHEL ROAD", 
                      "CHURCH ROAD", "CHURCH ST", "CHURCH AVE", "CHURCH BLVD",
                      "COMMUNITY ASSN", "OWNERS ASSN", "843 CHURCH LLC", "7490 BETHLEHEM LLC",
                      "725 CHURCH LLC", "5907 GOSPEL STREET LLC", "5404 ANTIOCH RD LLC", "53 WEST CHURCH LLC",
                      "420 WEST CHURCH AVENUE CONDOMINIUM", "333 CHURCH LLC", "316 CHURCH INC",
                      "26 CHURCH LLC", "251 GARBERS CHURCH FARM LLC", "23 WEST CHURCH AVENUE CONDOMINIUM",
                      "220 CHURCH LLC", "16 CHURCH LANE LLC", "1330 EBENEZER, LLC", "1330 EBENEZER LLC",
                      "11859 LORD FAIRFAX HWY LLC", "ZION CROSSROADS", "CAPITAL GROUP",
                      "TRANSPORT", "DEVELOPMENT", "CHURCH, ", ", CHRISTIAN", "CHURCH,",
                      "PROPERTY", "MANAGEMENT", "DEVELOPMENT", ",JESUS", "PROFESSIONAL",
                      "PROPERTIES", "COMPANY", "BORN AGAIN CHRISTIAN CONST CO", "CHRISTIAN,",
                      "HEALTH", "ALL SAINTS CHILD CARE CENTER", "YOUNG MENS", "YOUNG MEN'S", 
                      "APARTMENTS", "VENTURES", "OWNERS ASSOC", "HOLDINGS", "INVESTING", "REALTY",
                      "ACQUISITION", "REAL ESTATE", "RENTALS", "CONSTRUCTION", "INVESTMENTS", 
                      "BUILDERS", "MASONIC", "WILLIAM", "GEORGE", "ROBERT", "GOD'S PIT CREW INC", "COAL PIT MINISTRY INC",
                      "AGAPE SEVEN LLC", "CEMETERY", "CEMET", "CEMETE", "THE ASSEMBLY INC", "THE ASSEMBLY INCORPORATED",
                      "ACCA", "REBKEE", "LODGE", "MOCHA")

words_boundaries <- paste0("\\b", negative_phrases, "\\b")

pattern <- paste(words_boundaries, collapse = "|")


faith_found_6 <- faith_found_5 |> 
  filter(usecode_religious != TRUE | is.na(usecode_religious)) |> 
  mutate(owner = toupper(owner)) |> 
  filter(!str_detect(owner, regex(pattern, ignore_case = TRUE)))

## THE VIRGINIA SCC LIST OF RELIGIOUS ORG CAN HELP IDENTIFY FAITH ORGS IF THEY 
## MATCH THE OWNER NAME.
  
scc <- read_csv("data/va_scc_religious.csv") |>
  janitor::clean_names() |>
  mutate(owner = toupper(entity_name)) |>
  drop_na(owner) |> 
  distinct(owner, .keep_all = TRUE)

faith_found_7 <- faith_found_6 |> 
  left_join(scc, by = "owner") 

## RETAIN ENTRIES WHERE THERE WAS A SUCCESSFUL JOIN.

faith_confirmed_4 <- faith_found_7 |> 
  filter(!is.na(status))

## FILTER OUT ENTRIES THAT WERE RETAINED PREVIOUSLY AND KEEP ONLY 
## THOSE WHERE THERE WAS NO SUCCESSFUL JOIN.

faith_found_8 <- faith_found_7 |> 
  filter(is.na(status))

## ENTRIES THAT END WITH CHURCH ARE MORE LIKELY TO BE A CHURCH AND NOT A PERSON.

faith_found_9 <- faith_found_8 |> 
  mutate(end_church = str_ends(owner, "CHURCH"))

count_unique <- as.data.frame(table(faith_found_9$end_church, useNA = "always"))

## RETAIN ENTRIES THAT END WITH CHURCH

faith_confirmed_5 <- faith_found_9 |> 
  filter(end_church == TRUE)

## CREATE A PATTERN TO EASILY FILTER ORGANIZATIONS.

orgwords_to_detect <- c("LLC", "INC", "LLP")

org_pattern <- paste(orgwords_to_detect, collapse = "|")

## CREATE A COLUMN TO DETECT WHETHER OWNER NAME ENDS WITH CHAPEL. THESE ENTRIES 
## ARE MORE LIKELY TO BE A FAITH-BASED ORG.

faith_found_10 <- faith_found_9 |> 
  filter(end_church != TRUE) |> 
  mutate(investigate = str_detect(owner,"[,&]")) |> 
  mutate(org = str_detect(owner, org_pattern)) |> 
  mutate(end_chapel = str_ends(owner, "CHAPEL"))

## RETAIN ENTRIES THAT END WITH CHAPEL.

faith_confirmed_6 <- faith_found_10 |> 
  filter(end_chapel == TRUE)

## CREATE A LIST OF WORDS THAT ARE DEFINITIVELY A FAITH-BASED ORG IN VIRGINIA. THIS IS BASED ON 
## A SERIES OF TRIAL AND ERROR EXPLORATION OF THE DATA.

positive_words2 <- c("HOUSE OF", "CONGREGATION", "MINISTRIES", "TRUSTEES", "TRS", "TRUST",
                     "SHRINE OF", "MINIST", "LUTHERANCHURCH", "METHODISTCHURCH", "BRETHRENCHURCH",
                     "ORTHODOX", "UMC", "FELLOWSHIP", "CHRISTIAN OUTREACH", "CHRISTIAN CENTER",
                     "REVIVAL CENTER", "EVANGELICAL", "PRAYER", "MISSIONARY", "BRANCH CHRISTIAN",
                     "CHURCH TRS", "CHURCH TRUST", "CHURCH TRUSTEES", "KINGS CHAPEL CHRISTIAN", 
                     "DELIVERANCE", "CHURCH INC", "OF GOD", "CALVARY CHAPEL", "CAMP OAK HILL", 
                     "LIFE CENTER", "ZION CHURCH", "AME", "BETHEL CHURCH", "MINISTRY", "HOLINESS",
                     "CH", "ANGLICAN", "GOSPEL", "FRIENDS", "NAZARENE", "A.M.E.", "MISSION", "CONGREGATIONAL",
                     "ASSEMBLY", "UNION", "CROSS", "MUSLIM", "BORN", "ADVENT", "ADVENTIST", "UNITED", "FAITH",
                     "CATHEDRAL", "IGLESIA", "MISION", "MISIONERA", "SHRINE MONT", "APOSTOLIC"
                     )

words_boundaries <- paste0("\\b", positive_words2, "\\b")

pattern <- paste(words_boundaries, collapse = "|")

## RETAIN ENTRIES THAT CONTAIN THOSE POSITIVE WORDS, BUT FILTER OUT KNOWN ENTRIES 
## THAT ARE NOT FAITH-BASED ORGS.

faith_confirmed_7 <- faith_found_10 |> 
  filter(owner != "TULL BRENDA TEMPLE ET AL TRUSTEES") |> 
  filter(str_detect(owner, regex(pattern, ignore_case = TRUE))) |> 
  filter(owner != "WILLIAMS MARTIN H & WILLIAMS NAZARENE D") |> 
  filter(owner != "AMERICAN NATIONAL RED CROSS 352 W CHURCH AVENUE")

## THE PRECEDING HAS REDUCED THE NUMBER OF ENTRIES TO A MANAGEABLE AMOUNT OF ENTRIES.
## WHAT CAN FOLLOW IS A MANUAL REVIEW OF ENTRIES TO REMOVE ENTRIES THAT ARE LIKELY 
## NOT FAITH-BASED ORGS OR ARE OBVIOUSLY PEOPLE.

faith_found_11 <- faith_found_10 |> 
  filter(!str_detect(owner, regex(pattern, ignore_case = TRUE))) |> 
  filter(end_chapel != TRUE) |> 
  filter(owner != "BUNDY SILAS & SHAKIR MUSLIM") |> 
  mutate(owner = str_replace(owner, ",", ", ")) |> 
  filter(!str_detect(owner, "CHRISTIAN, ")) |> 
  filter(!str_detect(owner, "LORD, ")) |> 
  mutate(end_christian = str_ends(owner, "CHRISTIAN")) |> 
  filter(owner != "LIBERTY SAINTS LLC") 

## AT THIS POINT THE NUMBER OF ENTRIES IS DOWN TO 1,318 AND THERE ARE FEW PATTERNS 
## TO HELP REMOVE OR RETAIN ENTRIES. IT MAY BE BEST TO EXPORT OUT TO A .CSV AND 
## MANUALLY REMOVE ENTRIES THAT ARE CLEARLY A PERSON OR A NON-FAITH-BASED ORGANIZATION.

# write_csv(faith_found_11, "data/finding_faith.csv")

## THE PREVIOUS ANALYSIS FAILED TO INCLUDE THE LL_UUID COLUMN WHICH SERVES AS A 
## UNIQUE IDENTIFIER. IN ORDER TO RETAIN THE ENTRIES FROM THE MANUAL REVIEW, A RIGHT JOIN 
## BELOW WAS MADE TO THE FAITH_FOUND_11 OBJECT. THIS RESULTS IN THE SAME NUMBER OF ENTRIES (415)
## BEING RETAINED.

faith_confirmed_8 <- read_csv("data/finding_faith.csv") |>
mutate(geoid = as.character(geoid)) |>
  mutate(parcelnumb = as.character(parcelnumb)) |>
  mutate(ll_gissqft = as.character(ll_gissqft))

faith_found_xx <- faith_found_11 |> 
  right_join(faith_confirmed_8, by = c("geoid", "parcelnumb", "owner", "saddno", "ll_gissqft")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x"))


# found_faith <- bind_rows(faith_confirmed, faith_confirmed_2, faith_confirmed_3,
#                      faith_confirmed_4, faith_confirmed_5, faith_confirmed_6,
#                      faith_confirmed_7)

# write_rds(found_faith, "data/found_faith.rds")


found_faith <- read_rds("data/found_faith.rds")

## MERGE THE CONFIRMED AND THE LAST DATA FRAME TOGETHER. USE THE FOLLOWING TO 
## FORCE DATA TYPES TO MATCH. 
## 
## NOTE THAT SOME ENTRIES WERE RETAINED THAT SHOULD BE REVIEWED WITH VIRGINIA 
## INTERFAITH. THIS INCLUDES SCHOOLS, CAMPS, ACADEMIES, MEDIA ORGANIZATIONS LIKE
## CHRISTIAN BROADCASTING, RADIO, ETC.

faith_table <- list(found_faith, faith_found_xx) %>% 
  data.table::rbindlist(fill = TRUE) 

faith_full <- faith_table |> 
  mutate(join_id = paste(geoid, parcelnumb, owner, sep = "-"))

# write_csv(faith_table, "data/faith_parcels.csv")

faith_full <- read_csv("data/faith_parcels.csv")


query <- "SELECT * FROM va_statewide WHERE lbcs_activity = 6600 OR lbcs_activity = 5200 OR lbcs_activity = 5210 OR lbcs_activity = 9100 OR lbcs_activity = 9200 OR lbcs_activity = 9900 OR lbcs_activity IS NULL"

faith_sf <- st_read("data/va_statewide.gpkg", query = query)


faith_sf_reduced <- faith_sf|> 
  mutate(join_id = paste(geoid, parcelnumb, owner, sep = "-")) |> 
  filter(rdi != "Y"| is.na(rdi)) # Remove known residential properties with RDI and keep NULLS
  

faith_sf_join <- faith_sf_reduced |> 
  right_join(faith_full, by = "ll_uuid") %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) |> 
  select(file_name, geoid, parcelnumb, parcelnumb_no_formatting, owner, usecode, usedesc, struct,
         improvval, landval, parval, owntype, owner, saddno,
         saddpref, saddstr, saddsttyp, saddstsuf, sunit, scity, city, county, szip,
         lat, lon, ll_uuid, ll_gisacre, ll_gissqft, ll_bldg_count, ll_bldg_footprint_sqft, 
         rdi, lbcs_activity, lbcs_function_desc, lbcs_function, lbcs_site, lbcs_site_desc,
         lbcs_ownership, lbcs_ownership_desc)

write_rds(faith_sf_join, "data/va_statewide_geo.rds")

faith_sf_join <- read_rds("data/va_statewide_geo.rds")

library(sf)

st_write(faith_sf_join, "data/va_statewide_geo.gpkg", driver = "GPKG")

mapview::mapview(faith_sf_join)
