# 20/12/2023
# Josh Jones

# compare waterside length from three datasets
# MasterMap
# waterlink
# waterlinkset

librarian::shelf(tidyverse, sf, tictoc)

# Function to load data and transform it 
read_and_transform <- function(dsn, ...) {
    st_read(dsn = dsn, layer = ...) %>%
        st_transform(proj_epsg) %>%
        st_zm(drop = TRUE, what = "ZM")
}

# A helper function that erases all of y from x: 
st_erase <- function(x, y) {
    st_difference(x, st_union(st_combine(y)))
}

# coordinate reference system
proj_epsg <- 7405

# Paths
prj_path <- "../Maps/BI_15_minute/BI_15_minute.gdb"
# Output gpkg path, because sf can't write to gdb
out_gpkg <- "../Data/Analysis/BI_r_outputs.gpkg"

osngd_wtr_fts_path <- "../Data/Source/OS Hub Download/OS Hub Download/wtr_fts_water/wtr_fts_water.gpkg"
wtr_ntwk_waterlink_path <- "../Data/Source/OS Hub Download/OS Hub Download/wtr_ntwk_waterlink/wtr_ntwk_waterlink.gpkg"
wtr_ntwk_waterlinkset_path <- "../Data/Source/OS Hub Download/OS Hub Download/wtr_ntwk_waterlinkset/wtr_ntwk_waterlinkset.gpkg"

# Load waterways
osngd_wtr_fts <- read_and_transform(osngd_wtr_fts_path)
wtr_ntwk_waterlink <- read_and_transform(wtr_ntwk_waterlink_path)
wtr_ntwk_waterlinkset <- read_and_transform(wtr_ntwk_waterlinkset_path)

# Load current waterside
current_waterside <- read_and_transform(prj_path, "Inland_Waterside_Distribution_AIre_Calder")

# get OS NGD waterside
water_feature_boundary <- st_boundary(osngd_wtr_fts) %>%
    mutate(wb_src = "wtr_fts_water") %>%
    select(wb_src, description, osid)

tic()
waterlink_not_in_feature <- st_erase(wtr_ntwk_waterlink, osngd_wtr_fts) %>% 
    mutate(wb_src = "wtr_ntwk_waterlink") %>%
    select(wb_src, description, osid)
toc()
# 1691.19 sec elapsed

tic()
waterlinkset_not_in_feature <- st_erase(wtr_ntwk_waterlinkset, osngd_wtr_fts) %>% 
    mutate(wb_src = "wtr_ntwk_waterlink") %>%
    select(wb_src, description, osid)
toc()
# xxxx sec elapsed

# combine area boundary and lines
waterside_OS_NGD_waterlink <- rbind(water_feature_boundary, waterlink_not_in_feature)
waterside_OS_NGD_waterlinkset <- rbind(water_feature_boundary, waterlinkset_not_in_feature)

st_write(waterside_OS_NGD_waterlink, 
         dsn = out_gpkg, layer = "aire_calder_waterside_OS_NGD_waterlink", 
         append = FALSE)

st_write(waterside_OS_NGD_waterlinkset, 
         dsn = out_gpkg, layer = "aire_calder_waterside_OS_NGD_waterlinkset", 
         append = FALSE)

# Summarise path lengths
# List of function calls
line_list <- list(
    current_waterside,
    waterside_OS_NGD_waterlink,
    waterside_OS_NGD_waterlinkset
)

# Applying the calc len diff function to each element in the list
results_list <- lapply(line_list, function(func) {
    calculate_length_difference(func, lsoa)
})

# Creating a dataframe from the results
results_df <- data.frame(
    Dataset = c("current_waterside", "waterside_OS_NGD_waterlink", 
                "waterside_OS_NGD_waterlinkset"),
    bind_rows(results_list)
)

# Print
print(results_df)
# Dataset Result.total_km Result.urban_km Result.rural_km
# 1             current_waterside      6438.4 [m]      2897.8 [m]      3540.6 [m]
# 2    waterside_OS_NGD_waterlink      7815.2 [m]      3688.8 [m]      4126.4 [m]
# 3 waterside_OS_NGD_waterlinkset      5149.7 [m]      2680.7 [m]      2469.0 [m]

write_csv(results_df, "../Data/Outputs/aire_calder_waterside_distribution_length_comparisons.csv")
