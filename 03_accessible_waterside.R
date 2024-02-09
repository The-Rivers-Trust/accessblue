# 11/12/2023
# Josh Jones

# Make updated waterside
# Get access nodes 
# Summarise waterside accessibility

# subset df
# df[1:1000, ]

librarian::shelf(tidyverse, sf, tictoc)

# Path
prj_path <- "../Maps/BI_15_minute/BI_15_minute.gdb"
# Output gpkg path, because sf can't write to gdb
out_gpkg <- "../Data/Analysis/BI_r_outputs.gpkg"

osngd_wtr_fts_path <- "../Data/Source/OS Hub Download/OS Hub Download/wtr_fts_water/wtr_fts_water.gpkg"
wtr_ntwk_waterlink_path <- "../Data/Source/OS Hub Download/OS Hub Download/wtr_ntwk_waterlink/wtr_ntwk_waterlink.gpkg"
wtr_ntwk_waterlinkset_path <- "../Data/Source/OS Hub Download/OS Hub Download/wtr_ntwk_waterlinkset/wtr_ntwk_waterlinkset.gpkg"

# urban LSOA 
lsoa_path <- "../Data/Source/Urban_LSOA/Urban_LSOA/Urban_LSOAs.shp"

# coordinate reference system
proj_epsg <- 7405

# Function to load data and transform it 
read_and_transform <- function(dsn, ...) {
    st_read(dsn = dsn, layer = ...) %>%
        st_transform(proj_epsg) %>%
        st_zm(drop = TRUE, what = "ZM")
}

# Function to buffer routes by 10 m and clip waterside
buffer_clip <- function(waterside, routes) {
    # buffer the routes by 10 m 
    # we don't need any columns as they aren't preserved
    routes_10 <- routes %>% 
        st_buffer(10) %>%
        select(osid) %>%
        rename(waterside_route_osid = osid)
    
    # %>%
    #     rename(route_description = description,
    #            waterside_route_osid = osid,
    #            route_src = source) %>%
    #     select(waterside_route_osid, route_src, route_description)
    
    waterside <- waterside %>%
        rename(wb_description = description,
               waterside_osid = osid)
    
    # st_intersection is the same as clip in Arc or QGIS
    waterside_clip <- st_intersection(waterside, routes_10)
    
    # retain waterside info whilst removing overlaps
    # we don't need route
    waterside_clean <- waterside_clip %>%
        group_by(wb_src, waterside_osid, wb_description) %>%
        summarise()
    
    return(waterside_clip)
}

# Function to update geom length and add lsoa to compare rural vs urban
calculate_length_difference <- function(df, lsoa) {
    df_total <- df %>%
        ungroup() %>%
        mutate(geom_length_m = st_length(.)) %>%
        st_drop_geometry() %>%
        summarise(total_km = round(sum(geom_length_m) / 1000, 1))
    
    # st_intersection is the same as clip in Arc or QGIS
    df_lsoa <- df %>%
        st_intersection(lsoa) %>%
        mutate(geom_length_m = st_length(.)) %>%
        st_drop_geometry() %>%
        summarise(urban_km = round(sum(geom_length_m) / 1000, 1))
    
    urban <- df_lsoa
    rural <- abs(df_lsoa - df_total) %>%
        rename(rural_km = urban_km)
    
    result_df <- data.frame(df_total, urban, rural)
    
    return(result_df)
}

# Load data
path_road <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_road_PROW")
path_road_no_b_road <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_road_no_B_road_PROW")
path <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_PROW")
current_accessible_waterside <- read_and_transform(prj_path, "InlandWatersidePROW_ANG_aire_calder") %>%
    mutate(roadclassification  = "PROW or ANG",
           wb_src = "OS MasterMap",
           description = "all")
current_waterside <- read_and_transform(prj_path, "Inland_Waterside_Distribution_AIre_Calder")
urban_accessible_waterside <- read_and_transform(prj_path, "Accessible_urban_waterside_aire_calder")
lsoa <- read_and_transform(lsoa_path) %>%
    select(LSOA11NM)
waterside_OS_NGD_waterlink <- st_read(dsn = out_gpkg, 
                                      layer = "aire_calder_waterside_OS_NGD_waterlink")
green_space <- read_and_transform(prj_path, "AGI_aire_calder")

# Get waterside paths with all the attributes
waterside_path <- buffer_clip(waterside_OS_NGD_waterlink, path)
waterside_path_road <- buffer_clip(waterside_OS_NGD_waterlink, path_road)
waterside_path_road_no_b_road <- buffer_clip(waterside_OS_NGD_waterlink, path_road_no_b_road)

# Export waterside
st_write(waterside_path, 
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path", 
         append = FALSE)
st_write(waterside_path_road, 
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path_road", 
         append = FALSE)
st_write(waterside_path_road_no_b_road, 
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path_road_no_b_road", 
         append = FALSE)

# Export buffered waterside for visualization
st_write(st_buffer(waterside_path, 10), 
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path_buffer10m", 
         append = FALSE)
st_write(st_buffer(waterside_path_road, 10), 
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path_road_buffer10m", 
         append = FALSE)
st_write(st_buffer(waterside_path_road_no_b_road, 10), 
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path_road_no_b_road_buffer10m", 
         append = FALSE)

# Summarise path lengths
# List of function calls
line_list <- list(
    waterside_path,
    waterside_path_road,
    waterside_path_road_no_b_road,
    urban_accessible_waterside,
    current_accessible_waterside
)

# Applying the calc len diff function to each element in the list
results_list <- lapply(line_list, function(func) {
    calculate_length_difference(func, lsoa)
})

# Creating a dataframe from the results
# not comparing with current accessible waterside because 
# that uses GI and we're already tagging nodes within GI
results_df <- data.frame(
    Dataset = c("waterside_path", 
                "waterside_path_road", 
                "waterside_path_road_no_b_road", 
                "urban_accessible_waterside",
                "current_accessible_waterside"),
    bind_rows(results_list)
)

# Print
print(results_df)

# Dataset   total_km   urban_km   rural_km
# 1                waterside_path 2199.8 [m] 1321.9 [m]  877.9 [m]
# 2           waterside_path_road 2741.0 [m] 1596.2 [m] 1144.8 [m]
# 3 waterside_path_road_no_b_road 2727.0 [m] 1589.3 [m] 1137.7 [m]
# 4    urban_accessible_waterside  970.8 [m]  921.9 [m]   48.9 [m]
# 5  current_accessible_waterside 1902.5 [m]  724.0 [m] 1178.5 [m]

write_csv(results_df, "../Data/Outputs/aire_calder_waterside_length_comparisons.csv")

