# 11/01/2024
# Josh Jones
library(sf, tidyverse)

# Path
prj_path <- "C:/Users/JoshJones/The Rivers Trust/Technical Team Data - Projects/Blue Space 15 minute neghbourhoods/Maps/BI_15_minute/BI_15_minute.gdb"
# Output gpkg path, because sf can't write to gdb
out_gpkg <- "C:/Users/JoshJones/The Rivers Trust/Technical Team Data - Projects/Blue Space 15 minute neghbourhoods//Data/Analysis/BI_r_outputs.gpkg"

# coordinate reference system
proj_epsg <- 7405

# Function to load data and transform it 
read_and_transform <- function(dsn, ...) {
    st_read(dsn = dsn, layer = ...) %>%
        st_transform(proj_epsg) %>%
        st_zm(drop = TRUE, what = "ZM")
}

# Assuming you have two sf data frames named sf_data1 and sf_data2 with POINT geometry
# Replace "geometry_column" with the actual column name containing your spatial coordinates

# Create or load your spatial datasets
brw <- read_and_transform(prj_path, "BRW_Aire_Calder")
path_nodes <- read_and_transform(out_gpkg, "aire_calder_path_access_nodes")  # Replace "your_file2.shp" with your actual file

green_space <- read_and_transform(prj_path, "AGI_aire_calder")

# Create a matrix of distances between points in sf_data1 and sf_data2
distances_matrix <- st_distance(brw, path_nodes)

# Find the minimum distance for each point in sf_data1
nearest_distances <- apply(distances_matrix, 1, min, na.rm = TRUE)

length(nearest_distances)
summary(nearest_distances)

brw_tidy <- brw %>%
    dplyr::select(id)

# brw_tidy$d2access <- nearest_distances

brw_tidy$in_GI <- lengths(st_intersects(brw_tidy, green_space)) > 0

brw_tidy %>% filter(in_GI == TRUE)

brw_tidy %>%
    st_drop_geometry() %>%
    filter(d2access < 50) 