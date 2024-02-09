# 18/01/2024
# Josh Jones
librarian::shelf(tidyverse, sf, tictoc)

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

path_nodes <- read_and_transform(out_gpkg, "aire_calder_path_access_nodes") %>%
    select(waterside_osid, wb_src, wb_description, inter_route, inter_route_osid, inter_route_source, in_GI)
path_road_nodes <- read_and_transform(out_gpkg, "aire_calder_path_road_access_nodes")  %>%
    select(waterside_osid, wb_src, wb_description, inter_route, inter_route_osid, inter_route_source, in_GI)
nobroad_nodes <- read_and_transform(out_gpkg, "aire_calder_path_road_no_b_road_access_nodes") %>%
    select(waterside_osid, wb_src, wb_description, inter_route, inter_route_osid, inter_route_source, in_GI)
current_nodes <- read_and_transform(out_gpkg, "aire_calder_current_path_access_nodes") %>%
    select(waterside_osid, wb_src, wb_description, inter_route, inter_route_osid, inter_route_source, in_GI)

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

dist_prow_nodes <- function(nodes) {
    # add ID for splitting later on
    nodes <- nodes %>%
        mutate(ID = row_number())
    
    # get on PROW derived nodes
    PROW_nodes <- nodes %>% 
        filter(inter_route_source == "PROW")
    
    # get non-PROW nodes
    non_PROW_nodes <- nodes %>% 
        filter(inter_route_source != "PROW") %>%
        mutate(d2nodes = NA)
    
    # get PROW nodes within 10 m of non-PROW nodes
    PROW_nodes_within10 <- st_intersection(PROW_nodes, 
                                           non_PROW_nodes %>% 
                                               st_buffer(10) %>%
                                               st_geometry())
    
    PROW_nodes_outside10 <- PROW_nodes %>%
        filter(!ID %in% PROW_nodes_within10$ID) %>%
        mutate(d2nodes = NA) 
    
    #find the nearest node name and save in PROW_nodes_within10
    nodename <- st_nearest_feature(PROW_nodes_within10, non_PROW_nodes)
    PROW_nodes_within10$nodename <- nodename

    # Find distances and azimuths
    PROW_nodes_within10$d2nodes = st_distance(PROW_nodes_within10, 
                                        non_PROW_nodes[nodename, ], 
                                      by_element = TRUE)

    # stich them back together and remove any stray duplicates
    out_nodes <- bind_rows(PROW_nodes_within10, PROW_nodes_outside10) %>%
        bind_rows(non_PROW_nodes) %>%
        select(-ID, -nodename) %>%
        remove_exact_duplicate_nodes()
    
    return(out_nodes)
}

path_nodes_with_dist <- dist_prow_nodes(path_nodes) 

# path_nodes_with_dist %>% filter(is.na(d2nodes) | as.numeric(d2nodes) > 5)

###################

path_nodes %>%
    dist_prow_nodes() %>%
    st_write(out_gpkg,
             "aire_calder_path_access_nodes",
             append = FALSE)

path_road_nodes %>%
    dist_prow_nodes() %>%
    st_write(out_gpkg,
             "aire_calder_path_road_access_nodes",
             append = FALSE)

nobroad_nodes %>%
    dist_prow_nodes() %>%
    st_write(out_gpkg,
             "aire_calder_path_road_no_b_road_access_nodes",
             append = FALSE)

current_nodes %>%
    dist_prow_nodes() %>%
    st_write(out_gpkg,
             "aire_calder_current_path_access_nodes",
             append = FALSE)
