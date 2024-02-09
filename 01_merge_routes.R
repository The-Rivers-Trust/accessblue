# 16/10/2023
# Josh Jones
# Route combining

# attribute, merge and clip transport networks
# size scenarios can be created here
# also, play around with different road classes

librarian::shelf(tidyverse, sf, tictoc, lwgeom)

# paths
prj_path <- "../Maps/BI_15_minute/BI_15_minute.gdb"
# Output gpkg path, because sf can't write to gdb
out_gpkg <- "../Data/Analysis/BI_r_outputs.gpkg"
os_ngd_path <- "../Data/Source/OS Hub Download/OS Hub Download/"

prow_path <- "C:/Users/JoshJones/The Rivers Trust/BI Experiential Indicator - Documents/Scoping/ProProjects/BI data processing/prow.gdb"
path_path <- "trn_ntwk_pathlink"
pave_path <- "trn_ntwk_pavementlink"
conn_path <- "trn_ntwk_connectinglink"
road_path <- "trn_ntwk_roadlink"
prj_path <- "../Maps/BI_15_minute/BI_15_minute.gdb"

# out gpkg path, because sf can't write to gbd
out_gpkg <- "../Data/Analysis/BI_r_outputs.gpkg"

# Function to read OS NGD data
read_osngd <- function(path){
    full_path <- paste0(os_ngd_path, path, "/", path, ".gpkg")
    
    st_read(full_path) %>%
        st_transform(7405) %>%
        st_zm(drop = TRUE, what = "ZM")
}

# summarise length
calculate_length <- function(df) {
    df_total <- df %>%
        mutate(geom_length_m = st_length(.)) %>%
        st_drop_geometry() %>%
        summarise(total_km = sum(geometry_length_m) / 1000)
    
    return(df_total)
}

# remove duplicate linestring geometry
linestring_geom_clean <- function(df) {
    df %>%
        mutate(start = as.character(st_startpoint(.)),
               end = as.character(st_endpoint(.)),
               coords = paste0(start, " - ", end)) %>%
        select(-start, -end) %>%
        group_by(coords) %>%
        slice(1) %>%
        ungroup()
}

# load 
roadlink <- read_osngd(road_path)
pathlink <- read_osngd(path_path)
pavlink <- read_osngd(pave_path)
connlink <- read_osngd(conn_path)
boundary <- st_read(prj_path, layer = "Rivers_Trust_Boundaries") %>%
    st_transform(7405) %>%
    st_zm(drop = TRUE, what = "ZM")
prow <- st_read(prow_path, layer = "ProW") %>%
    st_transform(7405)  %>%
    st_zm(drop = TRUE, what = "ZM")

# get columns, give source, filter and clip
exclude <- c("A Road", "Motorway")

# roadlink %>% filter(roadclassification == "B Road") %>%
#     select(description) %>%
#     distinct(description)

tic()
rd <- roadlink %>%
    select(osid, description, roadclassification, geometry_length_m) %>%
    mutate(source = "trn_ntwk_roadlink") %>%
    filter(!roadclassification %in% exclude) %>%
    st_crop(boundary)  %>%
    st_cast("MULTILINESTRING") 

pth <- pathlink %>%
    select(osid, geometry_length, description) %>%
    rename(geometry_length_m = geometry_length) %>%
    mutate(source = "trn_ntwk_pathlink",
           roadclassification = "path") %>%
    st_crop(boundary) %>%
    st_cast("MULTILINESTRING") 

pv <- pavlink %>%
    select(osid, geometry_length_m, description) %>%
    mutate(source = "trn_ntwk_pavlink",
           roadclassification = "pavement") %>%
    st_transform(7405) %>%
    st_crop(boundary) %>%
    st_cast("MULTILINESTRING") 

conn <- connlink %>%
    select(osid, geometry_length, description)  %>%
    rename(geometry_length_m = geometry_length) %>%
    mutate(source = "connectinglink",
           roadclassification = "connecting") %>%
    st_crop(boundary) %>%
    st_cast("MULTILINESTRING") 

pr <- prow %>%
    select(Type, Shape_Length, Name) %>%
    rename(geometry_length_m = Shape_Length,
           description = Type,
           osid = Name,
           geometry = Shape) %>%
    mutate(source = "PROW",
           roadclassification = "PROW") %>%
    st_transform(crs = 7405) %>%
    st_crop(boundary) %>%
    st_cast("MULTILINESTRING")

st_write(pr, dsn = out_gpkg, layer = "Aire_Calder_PROW", append = FALSE)

toc()
# 154.79 sec elapsed

tic()
# bind OS NGD paths, PROW and OS NGD roads
roads_paths <- do.call("rbind", list(rd, pth, pv, conn, pr))
toc()
# 12.16 sec elapsed

unique(roads_paths$description)

# only keep some paths
# lists for filtering
all_routes <- c("Footpath", "Bridleway", "Path", "Pavement",
                "Connecting Link", "Path With Steps", "Track", 
                "Footbridge", "Shared Use Carriageway", 
                "Path With Ford", "Subway", 
                "Path With Level Crossing", "Not Classified",
                "Unknown", "Unclassified",  "Classified Unnumbered")

pedestrian_paths <- c("Footpath", "Bridleway", "Path", "Pavement",
                      "Connecting Link", "Path With Steps", "Track", 
                      "Footbridge", "Shared Use Carriageway", 
                      "Path With Ford", "Subway", 
                      "Path With Level Crossing")

road_with_potential <- c("Not Classified",
                         "Unknown",
                         "Unclassified",
                         "B Road",
                         "Classified Unnumbered")

no_B_road <- c("Not Classified",
               "Unknown",
               "Unclassified",
               "Classified Unnumbered",
               "path",
               "pavement",
               "connecting",
               "PROW")

tic()
# add scenario
roads_paths_scenario <- roads_paths %>%
    filter(description %in% all_routes | roadclassification %in% road_with_potential) %>%
    mutate(pedestrian = case_when(description %in% pedestrian_paths ~ 1,
                                  TRUE ~ 0),
           road_pedestrian = case_when(description %in% pedestrian_paths | 
                                           roadclassification %in% road_with_potential ~ 1,
                                       TRUE ~ 0),
           no_B_road = case_when(road_pedestrian == 1 & roadclassification %in% no_B_road ~ 1,
                                 TRUE ~ 0))
toc()
# 17.09 sec elapsed

# roads_paths_scenario %>%
#     filter(road_pedestrian == 1) %>%
#     st_drop_geometry() %>%
#     distinct(roadclassification)

# remove duplicate geometries
# do it for each one in case pavements and roads share geom

# filter
all_routes_out <- roads_paths_scenario %>%
    filter(road_pedestrian == 1)  %>%
    linestring_geom_clean()

all_routes_no_B_roads_out <- roads_paths_scenario %>%
    filter(no_B_road == 1)  %>%
    linestring_geom_clean()

pedest_routes_out <- roads_paths_scenario %>%
    filter(pedestrian == 1)  %>%
    linestring_geom_clean()

st_write(pedest_routes_out, dsn = out_gpkg, layer = "Aire_Calder_OS_NGD_path_PROW", append = FALSE)

st_write(all_routes_out, dsn = out_gpkg, layer = "Aire_Calder_OS_NGD_path_road_PROW", append = FALSE)

st_write(all_routes_no_B_roads_out, dsn = out_gpkg, layer = "Aire_Calder_OS_NGD_path_road_no_B_road_PROW", append = FALSE)

# Summarise path lengths
# List of function calls
line_list <- list(
    pedest_routes_out,
    all_routes_out,
    all_routes_no_B_roads_out
)

# Applying the calc len diff function to each element in the list
results_list <- lapply(line_list, function(func) {
    calculate_length(func)
})

# Creating a dataframe from the results
results_df <- data.frame(
    Dataset = c("pedest_routes_out", 
                "all_routes_out", 
                "all_routes_no_B_roads_out"),
    bind_rows(results_list)
)

# Print
print(results_df)

# Dataset total_km
# 1         pedest_routes_out 29474.88
# 2            all_routes_out 38789.45
# 3 all_routes_no_B_roads_out 38539.12

write_csv(results_df, "../Data/Outputs/aire_calder_route_length_comparisons.csv")