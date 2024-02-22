# 22/02/2024
# Josh Jones

# Determine a duplicate exclusion threshold

librarian::shelf(dplyr, sf)

out_gpkg <- "../../Data/Analysis/BI_r_outputs.gpkg"

check_duplicates <- st_read(out_gpkg,
           "potential_prow_duplicates_subset100")

colnames(check_duplicates)

dist <- check_duplicates %>%
  st_drop_geometry() %>%
  select(d2nodes, duplicate)

hist(dist$d2nodes)

summary(dist$d2nodes)

# take the 98th percentile as the duplicate exclusion distance 
# Belletti et al., 2023
top80_duplicates <- dist %>% 
  filter(duplicate == TRUE) %>% 
  select(d2nodes) %>%
  slice_min(d2nodes, prop = 0.8)

summary(top80_duplicates)

hist(top80_duplicates$d2nodes)
