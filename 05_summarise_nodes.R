# 12/12/2023
# Josh Jones

# summarise access nodes under each scenario of route and waterbody.

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

# Export access nodes for visualization
path_access_nodes <- read_and_transform(dsn = out_gpkg, 
                                        layer = "aire_calder_path_access_nodes")
path_road_access_nodes <- read_and_transform(dsn = out_gpkg, 
         layer = "aire_calder_path_road_access_nodes")
path_road_no_b_road_access_nodes <- read_and_transform(dsn = out_gpkg, 
         layer = "aire_calder_path_road_no_b_road_access_nodes")
current_path_access_nodes <- read_and_transform(dsn = out_gpkg, 
         layer = "aire_calder_current_path_access_nodes")

colnames(path_road_access_nodes)

# get number of nodes for each group in each column

(total_nodes <- nrow(path_road_access_nodes))
nrow(path_access_nodes)
nrow(path_road_no_b_road_access_nodes)
nrow(current_path_access_nodes)

summary_counts <- function(x) {
    # Initialize an empty list to store individual grouped results
    individual_grouped_results <- list()
    
    # Iterate over each column
    for (col in names(x)) {
        # Group by the current column and get the count
        grouped_result <- x %>%
            group_by(!!sym(col)) %>%
            summarise(Count = n())  %>%
            mutate(Percentage = round((Count/sum(Count)) * 100, 2)) %>%
            arrange(desc(Percentage))
        
        # Store the result in the list
        individual_grouped_results[[col]] <- grouped_result
    }
    
    return(individual_grouped_results)
}

for_sum_path_road_access_nodes <- path_road_access_nodes %>%
    select(-waterside_osid, -d2nodes, -geom, -inter_route_osid) %>%
    st_drop_geometry()

summaryresults <- summary_counts(for_sum_path_road_access_nodes)

extra_summary <- for_sum_path_road_access_nodes %>%
    group_by(wb_src, wb_description) %>%
    summarise(Count = n())  %>%
    ungroup() %>%
    mutate(Percentage = round((Count/sum(Count)) * 100, 2)) %>%
    arrange(desc(Percentage))
    
summaryresults[["wb_src_description"]] <- extra_summary

# Natural List
natural_list <- c(
    "Watercourse",
    "Reeds In Water",
    "Marsh",
    "Spring",
    "Waterfall",
    "Scattered Non-Coniferous Trees In Water And Reeds",
    "Non-Coniferous Trees In Water",
    "Reeds In Watercourse"
)

both <- c("Still Water")

# Artificial List
artificial_list <- c(
    "Drain",
    "Spring And Trough",
    "Moat",
    "Cascade",
    "Canal",
    "Leat",
    "Open Water Tank",
    "Open Reservoir",
    "Overflow",
    "Lock",
    "Mill Leat",
    "Settling Pond",
    "Buried Open Reservoir",
    "Canal And Reeds",
    "Open Tank Reservoir",
    "Paddling Pool",
    "Canal Feeder",
    "Open Reservoir And Reeds",
    "Swimming Pool"
)

wb_description_df <- data.frame(
    Class = c(rep("Natural", length(natural_list)), 
              rep("Both", length(both)), 
              rep("Artificial", length(artificial_list))),
    Description = c(natural_list, both, artificial_list)
)

write_csv(wb_description_df, "../Data/Outputs/wb_description_class.csv")

art_nat_summary <- for_sum_path_road_access_nodes %>%
    mutate(artificial_natural = case_when(wb_description %in% artificial_list ~ "artificial",
                                          wb_description %in% natural_list ~ "natural",
                                          wb_description %in% both ~ "both")) %>%
    group_by(artificial_natural) %>%
    summarise(Count = n())  %>%
    mutate(Percentage = round((Count/sum(Count)) * 100, 2)) %>%
    arrange(desc(Percentage))

summaryresults[["artificial_natural"]] <- art_nat_summary

gi_summary <- for_sum_path_road_access_nodes %>%
    group_by(in_GI, wb_description) %>%
    summarise(Count = n())  %>%
    mutate(Percentage = round((Count/sum(Count)) * 100, 2)) %>%
    arrange(desc(Percentage)) %>%
    print(n = 41)

# for_sum_path_road_access_nodes %>%
#     filter(in_GI) %>%
#     group_by(in_GI, wb_description) %>%
#     summarise(Count = n())  %>%
#     ungroup() %>%
#     mutate(perc = round((Count/total_nodes) * 100, 2)) %>%
#     arrange(desc(perc)) %>%
#     print(n = 41)

summaryresults[["GI_description"]] <- gi_summary

write_xlsx(summaryresults, "../Data/Outputs/path_road_access_nodes_count_summary.xlsx")




