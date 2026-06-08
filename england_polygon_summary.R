# 08/06/2026
# Josh Jones

# summarise access points by:
# LSOA - urban vs rural
# westminster cons

# libraries
librarian::shelf(tidyverse, sf)
source("../../../LINK Chemicals/Scripts & tools/spatial-functions.R")

# Extract
# full res urban LSOA
urban_lsoa <- st_read("../../Data/Source/Urban_LSOA/Urban_LSOA/Urban_LSOAs.shp")

# all LSOA
proj_db <- "../../Maps/BI_15_minute/BI_15_minute.gdb"
lsoa <- st_read(dsn = proj_db, "LSOA_2021_EW_BSC_V4")

# Westminster
westmin <- load_flayer("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BSC/FeatureServer/0")

# England access points
nodes <- st_read("../../Data/Source/Blue_space_access_points_in_England.gpkg/Blue_space_access_points_in_England.gpkg")

# England boundary
eng <- st_read("C:/Users/JoshJones/The Rivers Trust/TechData - Themes/Boundaries/UK/Countries_December_2022_GB_BFE_454709407110229782/0d337352-d3cc-4874-90ea-8091d66163c0.gdb") %>%
  filter(CTRY22NM == "England")

# Transform
# Label low res LSOA with urban 
lsoa_urban_rural <- lsoa %>%
  mutate(urban_rural = case_when(LSOA21CD %in% urban_lsoa$LSOA11CD ~ "urban",
                                 TRUE ~ "rural"))

# Join nodes to polygons
# LSOA
urban_rural_count <- nodes %>%
  st_join(lsoa_urban_rural) %>%
  st_drop_geometry() %>%
  group_by(LSOA21CD, urban_rural) %>%
  summarise(n_points = n(),
            accessible_length = sum(accessible_waterside_collection_length))  %>% 
  ungroup()

urban_rural_count_lsoa <- lsoa %>%
  mutate(ctry = substr(LSOA21CD, 1, 4)) %>% 
  filter(ctry == "E010") %>%
  left_join(urban_rural_count, by = "LSOA21CD") 

# ggplot(urban_rural_count_lsoa ) +
#   geom_sf(color = "white", aes(fill = accessible_length))

# Westmin
westmin_count <- nodes %>%
  st_join(westmin %>% st_make_valid()) %>%
  st_drop_geometry() %>%
  group_by(PCON24CD) %>%
  summarise(n_points = n(),
            accessible_length = sum(accessible_waterside_collection_length)) %>% 
  ungroup() 

count_westmin <- westmin %>%
  left_join(westmin_count, by = "PCON24CD") %>%
  mutate(ctry = substr(PCON24CD, 1,1)) %>%
  filter(ctry == "E")

ggplot(count_westmin) +
  geom_sf(color = "white", aes(fill = n_points))

ggplot(count_westmin) +
  geom_sf(color = "white", aes(fill = accessible_length))

# Load
st_write(count_westmin, dsn = proj_db, layer = "westminster_con_access_summary")
st_write(urban_rural_count_lsoa, dsn = proj_db, layer = "urban_rural_access_summary")
