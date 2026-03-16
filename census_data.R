install.packages("cancensus")
install.packages("ggplot2")
library(cancensus)
library(dplyr)
library(ggplot2)
library(sf)


# Get a free API key at: https://censusmapper.ca/api
# Then store it permanently so you don't need to set it each session:
set_cancensus_api_key("CensusMapper_93e8ef8d4bddfb762de8b82485d963d2", install = TRUE, overwrite = TRUE)

set_cancensus_cache_path("~/cancensus_cache", install = TRUE, overwrite = TRUE)

# Search all variables containing "mother tongue" in the 2021 census
mother_tongue_vars <- find_census_vectors(
  "mother tongue",
  dataset = "CA21",
  type = "total"
)

# List all CMAs and search for Montreal
regions <- list_census_regions("CA21") %>%
  filter(level == "CMA", grepl("Montr", name, ignore.case = TRUE))

print(regions)

# Define the variables you want
mt_vectors <- c(
  "v_CA21_1174",  # Total - Mother tongue
  "v_CA21_1183",  # English
  "v_CA21_1186",  # French
  "v_CA21_1189",  # Non-official languages
  "v_CA21_1516",  # Arabic
  "v_CA21_1924",  # Spanish
  "v_CA21_1915",  # Italian
  "v_CA21_2053",  # Mandarin
  "v_CA21_1633"  # Creole
)

# Pull data — geo_format = "sf" returns an sf spatial object with CT polygons
montreal_mt <- get_census(
  dataset    = "CA21",
  regions    = list(CMA = "24462"),
  vectors    = mt_vectors,
  level      = "CT",           # Census Tract level
  geo_format = "sf",           # Returns geometry — no separate shapefile needed
  labels     = "short"         # Shorter column names
)

# Quick look at what came back
glimpse(montreal_mt)

montreal_mt <- montreal_mt %>%
  mutate(
    pct_english      = 100 * v_CA21_1183 / v_CA21_1174,
    pct_french       = 100 * v_CA21_1186 / v_CA21_1174,
    pct_non_official = 100 * v_CA21_1189 / v_CA21_1174,
    pct_arabic       = 100 * v_CA21_1516 / v_CA21_1174,
    pct_spanish      = 100 * v_CA21_1924 / v_CA21_1174,
    pct_italian      = 100 * v_CA21_1915 / v_CA21_1174,
    pct_mandarin     = 100 * v_CA21_2053 / v_CA21_1174,
    pct_creole       = 100 * v_CA21_1633 / v_CA21_1174
  )

ggplot(montreal_mt) +
  geom_sf(aes(fill = pct_french), colour = NA) +
  scale_fill_viridis_c(name = "% French\nmother tongue", option = "magma") +
  labs(title = "French mother tongue by census tract",
       subtitle = "Greater Montréal CMA, 2021 Census") +
  theme_minimal()


st_write(montreal_mt, "montreal_mother_tongue_ct.geojson", delete_dsn = TRUE)


