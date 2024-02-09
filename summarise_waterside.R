# Summarise waterside versions

# devtools::install_github("a-benini/sfhelpers")

librarian::shelf(tidyverse, sf)

# Path
prj_path <- "../Maps/BI_15_minute/BI_15_minute.gdb"
# Output gpkg path, because sf can't write to gdb
out_gpkg <- "../Data/Analysis/BI_r_outputs.gpkg"

osngd_wtr_fts_path <- "../Data/Source/os-ngd-water-features/os_ngd_water_features/GPKG/wtr_fts_water.gpkg"
wtr_ntwk_waterlink_path <- "../Data/Source/os-ngd-water-network/GPKG/wtr_ntwk_waterlink.gpkg"

# Function to load data and transform it 
read_and_transform <- function(dsn, layer) {
    st_read(dsn = dsn, layer = layer) %>%
        st_transform(27700) %>%
        st_zm(drop = TRUE, what = "ZM")
}

# Function to buffer routes by 10 m, merge with green space, and clip waterside
buffer_clip <- function(waterside, routes, green_space) {
    routes_10 <- routes %>% st_buffer(10)
    
    # route_gi <- bind_rows(routes_10, green_space) %>%
    #     st_transform(27700)
    
    # st_intersection is the same as clip in Arc or QGIS
    waterside_clip <- st_intersection(waterside, routes_10)
    
    return(waterside_clip)
}

# Function to update geom length and add lsoa to compare rural vs urban
calculate_length_difference <- function(df, lsoa) {
    df_total <- df %>%
        mutate(geom_length_m = st_length(.)) %>%
        st_drop_geometry() %>%
        summarise(total_km = sum(geom_length_m) / 1000)
    
    # st_intersection is the same as clip in Arc or QGIS
    df_lsoa <- df %>%
        st_intersection(lsoa) %>%
        mutate(geom_length_m = st_length(.)) %>%
        st_drop_geometry() %>%
        summarise(urban_km = sum(geom_length_m) / 1000)
    
    urban <- df_lsoa
    rural <- abs(df_lsoa - df_total) %>%
        rename(rural_km = urban_km)
    
    result_df <- data.frame(df_total, urban, rural)
    
    return(result_df)
}

get_access_nodes <- function(waterside_path, routes) {
    route_desc <- routes %>% 
        rename(inter_route = description) %>%
        select(inter_route)
    
    # buffer waterside by 10 m
    buffer <- st_buffer(waterside_path, 10) %>%
        select(roadclassification, wb_src, description) %>%
        rename(waterside_route = roadclassification,
               wb_description = description) %>%
        group_by(waterside_route, wb_src, wb_description) %>%
        summarise()
    
    # dissolve adjacent boundaries
    unioned <- st_union(buffer)
    
    # Convert the polygon to a line
    # without attributes
    boundary_line <- st_boundary(unioned)
    # with attributes
    boundary_line_att <- st_boundary(buffer)
    
    # intersect boundary_line and routes
    access_node_geom <- st_intersection(boundary_line, route_desc) %>%
        st_cast("MULTIPOINT") %>%
        st_cast("POINT") %>%
        st_as_sf() 
    
    access_node_att <- st_intersection(boundary_line_att, route_desc)
    
    access_node_geom_att <- access_node_geom %>%
        st_join(access_node_att) %>%
        na.omit() 
    
    access_node_geom_att$in_GI <- lengths(st_intersects(access_node_geom_att, green_space)) > 0
    
    return(access_node_geom_att)
}

# A helper function that erases all of y from x: 
st_erase <- function(x, y) {
    st_difference(x, st_union(st_combine(y)))
    }

# Load data
path_road <- read_and_transform(out_gpkg, "OS_NGD_path_road_PROW")
path_road_no_b_road <- read_and_transform(out_gpkg, "OS_NGD_path_road_no_B_road_PROW")
path <- read_and_transform(out_gpkg, "OS_NGD_path_PROW")
current_waterside <- read_and_transform(prj_path, "InlandWatersidePROW_ANG_Exeter") %>%
    mutate(roadclassification  = "PROW or ANG",
           wb_src = "OS MasterMap",
           description = "all")
urban_accessible_waterside <- read_and_transform(prj_path, "Accessible_urban_waterside_exeter")
green_space <- read_and_transform(prj_path, "AGI_exeter")
GI_waterside <- read_and_transform(prj_path, "InlandWatersideDistribution_exeter")
lsoa <- read_and_transform(prj_path, "Urban_LSOAs_exeter") %>%
    select(LSOA11NM)
osngd_wtr_fts <- read_and_transform(osngd_wtr_fts_path)
wtr_ntwk_waterlink <- read_and_transform(wtr_ntwk_waterlink_path)

# get OS NGD waterside
water_feature_boundary <- st_boundary(osngd_wtr_fts) %>%
    mutate(wb_src = "wtr_fts_water") %>%
    select(wb_src, description)

water_line_not_in_feature <- st_erase(wtr_ntwk_waterlink, osngd_wtr_fts) %>% 
    mutate(wb_src = "wtr_ntwk_waterlink") %>%
    select(wb_src, description)

waterside_OS_NGD <- rbind(water_feature_boundary, water_line_not_in_feature)

# Get waterside paths
waterside_path_gi <- buffer_clip(waterside_OS_NGD, path, green_space)
waterside_path_road_gi <- buffer_clip(waterside_OS_NGD, path_road, green_space)
waterside_path_road_no_b_road_gi <- buffer_clip(waterside_OS_NGD, path_road_no_b_road, green_space)

# Export buffered waterside for visualization
st_write(st_buffer(waterside_path_gi, 10), 
         dsn = out_gpkg, layer = "waterside_path_buffer10m", 
         append = FALSE)
st_write(st_buffer(waterside_path_road_gi, 10), 
         dsn = out_gpkg, layer = "waterside_path_road_buffer10m", 
         append = FALSE)
st_write(st_buffer(waterside_path_road_no_b_road_gi, 10), 
         dsn = out_gpkg, layer = "waterside_path_road_no_b_road_buffer10m", 
         append = FALSE)

# Summarise path lengths
# List of function calls
line_list <- list(
    waterside_path_gi,
    waterside_path_road_gi,
    waterside_path_road_no_b_road_gi,
    current_waterside,
    GI_waterside,
    waterside_OS_NGD,
    urban_accessible_waterside
)

# Applying the calc len diff function to each element in the list
results_list <- lapply(line_list, function(func) {
    calculate_length_difference(func, lsoa)
})

# Creating a dataframe from the results
results_df <- data.frame(
    Dataset = c("waterside_path_gi", "waterside_path_road_gi", 
                "waterside_path_road_no_b_road_gi",
               "current_waterside", "GI_waterside_distribution", "waterside_OS_NGD", "urban_accessible_waterside"),
    Result = bind_rows(results_list)
)

# Print
print(results_df)

# Get access nodes for paths, paths and road, paths and road (no B roads), 
# and existing waterside and updated paths
path_access_nodes <- get_access_nodes(waterside_path_gi, 
                                      path)
path_road_access_nodes <- get_access_nodes(waterside_path_road_gi, 
                                           path_road)
path_road_no_b_road_access_nodes <- get_access_nodes(waterside_path_road_no_b_road_gi, 
                                                     path_road_no_b_road)
current_path_access_nodes <- get_access_nodes(current_waterside, 
                                              path_road_no_b_road)

# Export access nodes for visualization
st_write(path_access_nodes, dsn = out_gpkg, 
         layer = "path_access_nodes", append = FALSE)
st_write(path_road_access_nodes, dsn = out_gpkg, 
         layer = "path_road_access_nodes", append = FALSE)
st_write(path_road_no_b_road_access_nodes, dsn = out_gpkg, 
         layer = "path_road_no_b_road_access_nodes", append = FALSE)
st_write(current_path_access_nodes, dsn = out_gpkg, 
         layer = "current_path_access_nodes", append = FALSE)

# apply attributes to access points
# Water body - polygon or line - done
# Water body description - done
# Node in GI - inside or outside - done
# Waterside route - description - done
# Intersecting route - description

waterside_path <- current_waterside
routes <- path_road_no_b_road

route_desc <- routes %>% 
    rename(inter_route = description) %>%
    select(inter_route)

# buffer waterside by 10 m
buffer <- st_buffer(waterside_path, 10) %>%
    select(roadclassification, wb_src, description) %>%
    rename(waterside_route = roadclassification,
           wb_description = description) %>%
    group_by(waterside_route, wb_src, wb_description) %>%
    summarise()

# dissolve adjacent boundaries
unioned <- st_union(buffer)

# Convert the polygon to a line
# without attributes
boundary_line <- st_boundary(unioned)
# with attributes
boundary_line_att <- st_boundary(buffer)

# intersect boundary_line and routes
access_node_geom <- st_intersection(boundary_line, route_desc) %>%
    st_cast("MULTIPOINT") %>%
    st_cast("POINT") %>%
    st_as_sf() 

access_node_att <- st_intersection(boundary_line_att, route_desc)

access_node_geom_att <- access_node_geom %>%
    st_join(access_node_att) %>%
    na.omit() 

access_node_geom_att$in_GI <- lengths(st_intersects(access_node_geom_att, green_space)) > 0

st_write(
    access_node_att, 
    dsn = out_gpkg, 
    layer = "access_node_att", 
    append = FALSE
)

st_write(
    access_node_geom, 
         dsn = out_gpkg, 
         layer = "access_node_geom", 
         append = FALSE
         )

st_write(buffer, dsn = out_gpkg, 
         layer = "buffer", append = FALSE)
# I think this, above, works
# no need for whats below
# check in a GIS if the points make sense
# might have to use the land coast ocean trick to remove internal nodes
