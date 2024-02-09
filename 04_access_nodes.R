# 11/12/2023
# Josh Jones

# Get access nodes 

librarian::shelf(tidyverse, sf, tictoc, lwgeom, terra)

# Path
prj_path <- "../Maps/BI_15_minute/BI_15_minute.gdb"
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

remove_exact_duplicate_nodes <- function(df) {
    # Remove exact duplicates, by geometry, by taking the 
    # first row in each pair
    # keep pavements over roads by using on inter_route_source
    
    # Define the order of preference for each group
    order_of_preference <- c("trn_ntwk_pavlink",  "trn_ntwk_roadlink", 
                             "connectinglink", "trn_ntwk_pathlink", "PROW")
    
    # Use dplyr functions to retain one value over another based on order of preference
    df %>%
        group_by(geom) %>%
        arrange(match(inter_route_source, order_of_preference)) %>%
        slice(1) %>%
        ungroup()
}

remove_near_duplicate_nodes <- function(df) {
    # Remove near duplicates, by geometry, by rounding the 
    # coordinates to 1 meter. 
    df %>%
        mutate(x = round(st_coordinates(.)[,1],0),
               y = round(st_coordinates(.)[,2],0),
               coords = paste0(x, " - ", y)) %>%
        group_by(coords) %>%
        slice(1) %>%
        ungroup() %>%
        select(-x, -y, -coords)
}

# An sf object with POINT and MULTIPOINT geoms causes issues.
# We need every point as a unique row i.e. all POINT geoms
# Identify rows with MULTIPOINT geometries
split_points <- function(access_node_geom) {
    multipoint_rows <- st_geometry_type(access_node_geom) == "MULTIPOINT"
    
    # Split MULTIPOINT geometries into individual POINTs
    split_sf <- st_cast(access_node_geom[multipoint_rows, ], "POINT")
    
    # Combine the resulting POINTs with the original POINT rows
    access_node_geom_POINT <- rbind(access_node_geom[!multipoint_rows, ], split_sf)
    
    return(access_node_geom_POINT)
}

get_access_nodes <- function(accessible_waterside, routes) {
    # subset the route columns
    # retains id, source and description + class  
    route_desc <- routes %>% 
        mutate(inter_route = paste0(description, " - ", roadclassification)) %>%
        rename(inter_route_osid = osid,
               inter_route_source = source) %>%
        select(inter_route, inter_route_osid, inter_route_source)
    
    # buffer waterside by 10 m
    # retains wb source, id, and description
    buffer <- st_buffer(accessible_waterside, 10) %>%
        select(waterside_osid, wb_src, wb_description)
    
    # dissolve adjacent boundaries
    # st_union gets rid of any attributes so there is a 
    # two step process to get points with attributes
    # see step 1 and step 2 below
    
    diss <- aggregate(vect(buffer), dissolve = TRUE) %>% 
        st_as_sf() %>% 
        st_cast("POLYGON")
    
    # Convert the polygon to a line
    # without attributes
    boundary_line <- st_boundary(diss) 
    # with attributes
    boundary_line_att <- st_boundary(buffer)
    
    # step 1
    # intersect boundary_line and routes
    # this gets the access node geometry with the fewest points
    # as the boundary_line has been unioned/dissolved
    # and split MULTIPOINT geoms into POINT geoms
    access_node_geom <- st_intersection(boundary_line, 
                                        st_geometry(route_desc)) %>%
        split_points()
    
    # Because there are overlapping, but not exact duplicate, route geoms
    # there will be some duplicate nodes.
    # Round coordinates to the nearest meter and only keep one per rounded geom 
    # We do this because whilst duplicate linestrings have been removed 
    # and exact duplicate geoms can be removed there are slight differences 
    # in route geoms that are <1 m.
    # This may also catch duplicate PROW and OS NGD points.
    access_node_geom <- access_node_geom %>%
        remove_near_duplicate_nodes() %>%
        mutate(dum = 1) # dummy column so we can join later
    
    # step 2
    # intersect boundary_line_att and routes
    # this gets the access node multiple points with geometries inside buffers
    # we don't need these but we need the attributes from the correct
    # geometries at the buffer edge 
    access_node_att <- st_intersection(boundary_line_att, route_desc) %>%
        st_set_geometry("geom") %>%
        remove_exact_duplicate_nodes() %>%
        split_points()
    
    # Give the attributes back to the correct geometry
    # on a sample dataset left_join() using coordinates was 37% 
    # faster than st_join()
    access_node_geom_att <- access_node_geom %>% 
        mutate(x = st_coordinates(.)[,1],
               y = st_coordinates(.)[,2]) %>%
        left_join(access_node_att %>% 
                      mutate(x = st_coordinates(.)[,1],
                             y = st_coordinates(.)[,2]) %>%
                      st_drop_geometry(), 
                  by = c("x" = "x", "y" = "y")) %>%
        select(-dum,-x, -y) %>%
        st_set_geometry("geom") %>%
        remove_exact_duplicate_nodes()
    
    # assign true or false if the geom is within Green Infrastructure
    access_node_geom_att$in_GI <- lengths(st_intersects(access_node_geom_att, green_space)) > 0
    
    return(access_node_geom_att)
}

# Load data
path_road <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_road_PROW")
path_road_no_b_road <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_road_no_B_road_PROW")
path <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_PROW")
current_accessible_waterside <- read_and_transform(prj_path, "InlandWatersidePROW_ANG_aire_calder") %>%
    mutate(roadclassification  = "PROW or ANG",
           wb_src = "OS MasterMap",
           description = "all")
green_space <- read_and_transform(prj_path, "AGI_aire_calder")
urban_accessible_waterside <- read_and_transform(prj_path, 
                                                 "Accessible_urban_waterside_aire_calder")
waterside_path <- read_and_transform(out_gpkg, 
                          layer = "aire_calder_accessible_waterside_path")
waterside_path_road <- read_and_transform(out_gpkg, 
                               layer = "aire_calder_accessible_waterside_path_road")
waterside_path_road_no_b_road <- read_and_transform(out_gpkg, 
                                         layer = "aire_calder_accessible_waterside_path_road_no_b_road")

# Get access nodes for paths, paths and road, paths and road (no B roads), 
# and existing waterside and updated paths
tic()
path_access_nodes <- get_access_nodes(waterside_path, 
                                      path)
toc()
# 533.62 sec elapsed 

# x <- path_access_nodes %>%
#     filter(n_matching_geom > 1)
# 
# x %>%
#     st_drop_geometry() %>%
#     group_by(inter_route) %>%
#     summarise(n = n())

# the upshot of the above is that there are duplicates because of overlapping,
# but not duplicate geometries in the route data
# so it's safe to remove duplicate coordinates from final access nodes
# this happens in get_access_nodes

tic()
path_road_access_nodes <- get_access_nodes(waterside_path_road, 
                                           path_road)
toc()
# 777.47 sec elapsed

tic()
path_road_no_b_road_access_nodes <- get_access_nodes(waterside_path_road_no_b_road, 
                                                     path_road_no_b_road)
toc()
# 826.92 sec elapsed

tic()
# current_accessible_waterside doesn't have OS NGD columns so need to set these
current_path_access_nodes <- get_access_nodes(current_accessible_waterside %>%
                                                  rename(waterside_osid = ID,
                                                         wb_description = description,
                                                         route_description = Access) %>%
                                                  mutate(waterside_route_osid = "no_ID"), 
                                              path_road_no_b_road)
toc()
# 111.61 sec elapsed

# Export access nodes for visualization
st_write(path_access_nodes, dsn = out_gpkg, 
         layer = "aire_calder_path_access_nodes", append = FALSE)
st_write(path_road_access_nodes, dsn = out_gpkg, 
         layer = "aire_calder_path_road_access_nodes", append = FALSE)
st_write(path_road_no_b_road_access_nodes, dsn = out_gpkg, 
         layer = "aire_calder_path_road_no_b_road_access_nodes", append = FALSE)
st_write(current_path_access_nodes, dsn = out_gpkg, 
         layer = "aire_calder_current_path_access_nodes", append = FALSE)

