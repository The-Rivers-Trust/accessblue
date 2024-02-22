# 22/01/2024
# Josh Jones

# r network find closest target point on network for each source point

librarian::shelf(tidyverse, sf, tictoc, sfnetworks, tidygraph, parallel)

set.seed(345)

# Path
prj_path <- "../Maps/BI_15_minute/BI_15_minute.gdb"
# Output gpkg path, because sf can't write to gdb
out_gpkg <- "../Data/Analysis/BI_r_outputs.gpkg"

# coordinate reference system
proj_epsg <- 7405

# Set the number of cores to use
num_cores <- detectCores() - 1  # Use all available cores except one
# num_cores=2
# Function to load data and transform it 
read_and_transform <- function(dsn, ...) {
    st_read(dsn = dsn, layer = ...) %>%
        st_transform(proj_epsg) %>%
        st_zm(drop = TRUE, what = "ZM")
}

get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

get_min_distance <- function(id, src_in, trg_in, net_in) {

    origin <- src_in %>%
        filter(grid_id == id) %>%
        st_as_sfc() %>%
        st_as_sf() %>%
        mutate(node_type = "source") 

    origin_buffer <- origin %>%
        st_buffer(2000) %>%
        st_union() %>%
        st_cast("POLYGON")
    
    # only investigate BI nodes within 1000 m buffer
    destination <- trg_in %>%
        st_intersection(origin_buffer) %>%
        select(grid_id) %>%
        st_as_sfc() %>%
        st_as_sf() %>%
        mutate(node_type = "target")

    # origin <- origin %>%
    #     st_geometry()
    
    points <- rbind(origin, destination)

    # only investigate BI nodes using network within 1000 m buffer
    network_clip <- net_in %>%
        st_intersection(origin_buffer)  %>%
        st_union() %>%
        st_cast("LINESTRING") 
    
    network <- network_clip %>%
        as_sfnetwork(directed = FALSE) 
    
    network_clean <- network %>% 
        convert(to_spatial_subdivision, .clean = TRUE) %>% 
        convert(to_spatial_smooth, .clean = TRUE) 

    network_blend <- st_network_blend(network_clean, points) %>%
        activate("edges") %>%
        mutate(weight = edge_length())

    cost_matrix <- st_network_cost(network_blend,
                                   from = origin,
                                   to = destination,
                                   direction = "all",
                                   weights = "weight")

    min_distances <- apply(cost_matrix, 1, min)

    out_source <- src_in %>%
        filter(grid_id == id) %>%
        st_set_geometry("geom")

    out_source$d2BInode <- min_distances

    return(out_source)
}

# Split the address data by grid ID and split the BI nodes
# and network into buffered areas around those split address points
# Send the split data to compute nodes and run the minimum distance 
# analysis in parallel
grid_parallel <- function(network_sf, target_points) {
    message("Loading network and source points...")
    message("")
    
    target_calder <- target_points %>%
        st_intersection(basin) 
    
    #########
    # make test data so less has to go to each compute node
    #########
    # all of them
    sample_grid_ids <- calder_grid$grid_id
    
    message("Generating sample datasets to send to each compute node...")
    message("")
    
    # ######
    # # test
    # sample_grid_ids <- sample(calder_grid$grid_id, 2, replace = FALSE)
    # message("sample grid IDs = ", sample_grid_ids)
    # ######
    
    sample_grid <- calder_grid %>%
        filter(grid_id %in% sample_grid_ids)

    sample_source <- source_calder %>%
        filter(grid_id %in% sample_grid_ids) %>%
        select(grid_id)

    sample_source_buffer <- st_buffer(sample_source, 2000) %>%
        st_union() %>%
        st_cast("POLYGON")
    
    sample_target <- target_calder %>%
        st_intersection(sample_source_buffer) %>%
        st_intersection(sample_grid) %>%
        select(grid_id)
    
    sample_network <- network_sf  %>%
        st_intersection(sample_source_buffer) 
    
    #########
    
    message("Getting minimum network distance for each address...")
    message("")
    
    # Set up parallel processing
    cl <- makeCluster(num_cores)
    
    # Export required libraries to the cluster
    clusterEvalQ(cl, {
        library(sf)
        library(sfnetworks)
        library(dplyr)
        library(tidygraph)
    })
    
    clusterExport(cl, c("sample_source", "sample_target", "sample_network"))
    
    # Perform parallel computation
    min_distances_parallel <- parLapply(cl, 
                                        sample_grid_ids, 
                                        get_min_distance, 
                                        sample_source, 
                                        sample_target, 
                                        sample_network)
    
    # 5.86 sec elapsed
    # 15 secs for 15 grids at 100m2 (0.03%)
    # 25 secs for 15 grids at 1000m2 (3%)
    # 91 secs for 15 grids at 2000m2 (16%)
    # 140 secs for 20 grids at 2000m2 (22%)
    # 602 secs for all grids at 2000m2 (100%)
    
    # Clean up parallel processing
    stopCluster(cl)
    
    return(min_distances_parallel)
}

run_scenarios <- function(routes, nodes) {
    # Get variable name for output later
    node_string <- deparse(substitute(nodes))
    node_out_string <- paste0(node_string, "_with_routing_distance")
    # 
    # routes_string <- deparse(substitute(routes))
    # message(routes_string)
    # message(node_string)
    
    # Run the network analysis in parallel to find the closest Bi to each address
    message("Gridding and parallelizing the analysis...")
    res <- grid_parallel(routes, nodes)

    # Combine the results
    message("Binding results together...")
    res_out <- bind_rows(res) %>%
        st_join(lsoa) %>%
        mutate(d2BInode = replace(d2BInode , is.infinite(d2BInode), NA))

    # Write the results
    message("Writing the results...")
    st_write(res_out, out_gpkg, node_out_string, append = FALSE)
}

# load data
path_node <- read_and_transform(out_gpkg, "aire_calder_path_access_nodes")
path_road_node <- read_and_transform(out_gpkg, "aire_calder_path_road_access_nodes")
path_road_no_b_road_node <- read_and_transform(out_gpkg, "aire_calder_path_road_no_b_road_access_nodes")
current_node <- read_and_transform(out_gpkg, "aire_calder_current_path_access_nodes")

path_road <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_road_PROW") %>%
    select(-coords)
path_road_no_b_road <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_road_no_B_road_PROW") %>%
    select(-coords)
path <- read_and_transform(out_gpkg, "Aire_Calder_OS_NGD_path_PROW") %>%
    select(-coords)

lsoa <- read_and_transform(prj_path, "aire_calder_urban_rural") %>%
    select(urbanrural) %>%
    st_set_geometry("geom")

basin <- st_read("../Maps/BI_15_minute/Aire_Calder_Colne_basin.shp") %>%
    st_transform(proj_epsg) %>%
    select(ShortName) %>%
    filter(ShortName != "Aire RT") %>%
    st_geometry()

source_points <- read_and_transform(prj_path,
                                    "Postal_address_Aire_Calder") %>%
    st_intersection(basin) 

# make a grid of the colne and calder catchment
calder_grid <- st_make_grid(basin, cellsize = 2000) %>%
    st_intersection(basin) %>%
    st_as_sf() %>%
    mutate(grid_area = as.numeric(st_area(.))) %>%
    filter(grid_area == get_mode(grid_area)) %>%
    mutate(grid_id = row_number())

st_write(calder_grid, out_gpkg, "calder_grid",
         append = FALSE)
         
# clip source and target points to the basin
# add grid ids
source_calder <- source_points %>%
    st_set_geometry("geom") %>%
    select(PRIMARYCLASSDESC, POSTCODE) %>%
    filter(PRIMARYCLASSDESC == "Residential") %>%
    st_intersection(basin) %>%
    st_intersection(calder_grid)

# Run the network analysis to find the closest Bi to each address
# test <- grid_parallel(path, path_node)

# run everything
tic()
run_scenarios(path, path_node)
toc()
# 778.72 sec elapsed
tic()
run_scenarios(path_road, path_road_node)
toc()
# 1431.13 sec elapsed
tic()
run_scenarios(path_road_no_b_road, path_road_no_b_road_node)
toc()
# 1280.01 sec elapsed
tic()
run_scenarios(path, current_node)
toc()
# 854.33 sec elapsed


