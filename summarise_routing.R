# 08/02/2024
# Josh JOnes

# combine nodes with attributes and BI distance

librarian::shelf(tidyverse, sf, writexl, viridis)

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
    # Remove exact duplicates by geometry
    df %>%
        group_by(geom) %>%
        slice(1) %>%
        ungroup()
}


# Export access nodes for visualization
path_access_nodes <- read_and_transform(dsn = out_gpkg, 
                                        layer = "aire_calder_path_access_nodes")
path_road_access_nodes <- read_and_transform(dsn = out_gpkg, 
                                             layer = "aire_calder_path_road_access_nodes")
path_road_no_b_road_access_nodes <- read_and_transform(dsn = out_gpkg, 
                                                       layer = "aire_calder_path_road_no_b_road_access_nodes")
current_path_access_nodes <- read_and_transform(dsn = out_gpkg, 
                                                layer = "aire_calder_current_path_access_nodes")

# have to add attribute that signals it was in the routing analysis
path_node_with_routing_distance <- read_and_transform(dsn = out_gpkg, 
                      layer = "path_node_with_routing_distance") %>%
    mutate(inCC = TRUE)
path_road_node_with_routing_distance <- read_and_transform(dsn = out_gpkg, 
                      layer = "path_road_node_with_routing_distance")
path_road_no_b_road_node_with_routing_distance <- read_and_transform(dsn = out_gpkg, 
                      layer = "path_road_no_b_road_node_with_routing_distance")
current_node_with_routing_distance <- read_and_transform(dsn = out_gpkg, 
                      layer = "current_node_with_routing_distance")

lsoa <- read_and_transform(prj_path, "aire_calder_urban_rural") %>%
    select(urbanrural) %>%
    st_set_geometry("geom")

path_nodup <- path_node_with_routing_distance %>%
    remove_exact_duplicate_nodes()

road_nodup <- path_road_node_with_routing_distance %>%
    remove_exact_duplicate_nodes() 

no_b_road_nodup <- path_road_no_b_road_node_with_routing_distance %>%
    remove_exact_duplicate_nodes() 

current_nodup <- current_node_with_routing_distance %>%
    remove_exact_duplicate_nodes()

nrow(path_nodup)
nrow(road_nodup) 
nrow(no_b_road_nodup)
nrow(current_nodup)

# rural vs urban diff
path_nodup %>%
    st_drop_geometry() %>%
    group_by(urbanrural) %>%
    summarise(n = n(),
              meand2BI = mean(d2BInode, na.rm = T))

road_nodup %>% 
    filter(d2BInode < 2000) %>%
    st_drop_geometry() %>%
    group_by(urbanrural) %>%
    summarise(n = n(),
              meand2BI = mean(d2BInode, na.rm = T))

no_b_road_nodup %>%
    st_drop_geometry() %>%
    group_by(urbanrural) %>%
    summarise(n = n(),
              meand2BI = mean(d2BInode, na.rm = T))

# Define a function for summarizing NA distances
summarize_na_distances <- function(df, scenario_name) {
    total <- nrow(df)
    
    df %>%
        st_drop_geometry() %>%
        filter(is.na(d2BInode)) %>%
        group_by(urbanrural) %>%
        summarise(n = n()) %>%
        mutate(scenario = scenario_name,
               Percentage = (n/total) * 100)
}

# Individual calls to the function
result_R1 <- summarize_na_distances(path_nodup, "R1")
result_R2 <- summarize_na_distances(road_nodup, "R2")
result_R3 <- summarize_na_distances(no_b_road_nodup, "R3")
result_Current <- summarize_na_distances(current_nodup, "Current waterside")

# Combine the results into a single dataframe
combined_results <- bind_rows(result_R1, result_R2, result_R3, result_Current)

# Pivot the result to have "urban" and "rural" on the top
pivoted_results <- combined_results %>%
    pivot_wider(names_from = urbanrural, values_from = c(n, Percentage), values_fill = 0)

# Display the pivoted results
print(pivoted_results)

# Define the breaks
breaks <- c(0, 300, 600, 900, 1200, 2001)

# Use dplyr to create intervals and get summary counts
summarise_by_distance <- function(x, scen) {
    path_nodup %>%
        st_drop_geometry() %>%
        mutate(d2BInode = if_else(d2BInode > 2000, NA, d2BInode),
               Interval = cut(d2BInode, 
                              breaks = breaks, 
                              include.lowest = TRUE, 
                              right = FALSE)) %>%
        group_by(Interval) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = (Count / sum(Count)) * 100,
               Scenario = scen)
}

list_of_dfs <- c(path_nodup, road_nodup, no_b_road_nodup, current_nodup)
list_of_scenarios <- c("R1", "R2", "R3", "current")

# Function definition
summarise_by_distance <- function(df, scen) {
    df %>%
        st_drop_geometry() %>%
        mutate(
            d2BInode = if_else(d2BInode > 2000, NA, d2BInode),
            Interval = cut(d2BInode, breaks = breaks, include.lowest = TRUE, right = FALSE)
        ) %>%
        group_by(Interval) %>%
        summarise(Count = n()) %>%
        mutate(
            Percentage = (Count / sum(Count)) * 100,
            Scenario = scen
        )
}

# Individual calls to summarise_by_distance
result_R1 <- summarise_by_distance(path_nodup, "R1")
result_R2 <- summarise_by_distance(road_nodup, "R2")
result_R3 <- summarise_by_distance(no_b_road_nodup, "R3")
result_R4 <- summarise_by_distance(current_nodup, "Current waterside")

# Combine the results into a single dataframe
results <- bind_rows(result_R1, result_R2, result_R3, result_R4)

# Create a barplot using ggplot2 with a colorblind-friendly palette
ggplot(results, aes(x = as.factor(Interval), y = Percentage, fill = Scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Distance (m) interval to nearest blue space access point",
         y = "Percentage") +
    ylim(0, 100) +
    scale_fill_viridis(discrete = TRUE) +  # Use viridis color palette
    scale_x_discrete(labels = function(x) ifelse(is.na(x), "Address not connected", x)) +
    theme_minimal(base_size = 16)  +  # Adjust the base font size
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees


