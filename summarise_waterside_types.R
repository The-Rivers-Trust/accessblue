
librarian::shelf(tidyverse, sf, writexl)

# Output gpkg path, because sf can't write to gdb
out_gpkg <- "../Data/Analysis/BI_r_outputs.gpkg"

# coordinate reference system
proj_epsg <- 7405

# Function to load data and transform it 
read_and_transform <- function(dsn, ...) {
    st_read(dsn = dsn, layer = ...) %>%
        st_transform(proj_epsg) %>%
        st_zm(drop = TRUE, what = "ZM")
}

summarise_waterside <- function(x) {
    x %>%
        mutate(km = st_length(.)/1000) %>%
        st_drop_geometry() %>%
        group_by(wb_description) %>%
        summarise(km = sum(km)) %>%
        mutate(percentage = (km/sum(km)) * 100) %>%
        arrange(desc(km)) %>%
        print(n = 40)
}

# Export buffered waterside for visualization
waterside_path <- read_and_transform( 
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path")
waterside_path_road <- read_and_transform( 
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path_road" 
         )
waterside_path_road_no_b_road <- read_and_transform(
         dsn = out_gpkg, 
         layer = "aire_calder_accessible_waterside_path_road_no_b_road" 
         )

head(waterside_path_road)

summarise_waterside(waterside_path_road)

summarise_waterside(waterside_path)

summarise_waterside(waterside_path_road_no_b_road)


