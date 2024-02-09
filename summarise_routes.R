# 06/02/2024
# Josh Jones

# Summarise route lengths
librarian::shelf(dplyr, readr, sf)

out_gpkg <- "../Data/Analysis/BI_r_outputs.gpkg"

# pedest_routes_out <- st_read(dsn = out_gpkg, layer = "Aire_Calder_OS_NGD_path_PROW")

all_routes_out <- st_read(dsn = out_gpkg, layer = "Aire_Calder_OS_NGD_path_road_PROW")

# all_routes_no_B_roads_out <- st_read(dsn = out_gpkg, layer = "Aire_Calder_OS_NGD_path_road_no_B_road_PROW")

head(all_routes_out)

all_routes_km <- sum(st_length(all_routes_out)/1000)

all_routes_out %>%
    mutate(len_km = st_length(.)/1000) %>%
    st_drop_geometry() %>%
    group_by(description) %>%
    summarise(km = sum(len_km)) %>%
    ungroup() %>%
    mutate(desc_perc = (km/all_routes_km) * 100) %>%
    arrange(desc(desc_perc))  %>%
    write_csv("../Data/Outputs/all_routes_desc_sum.csv")

all_routes_out %>%
    mutate(len_km = st_length(.)/1000) %>%
    st_drop_geometry() %>%
    group_by(roadclassification) %>%
    summarise(km = sum(len_km)) %>%
    ungroup() %>%
    mutate(desc_perc = (km/all_routes_km) * 100) %>%
    arrange(desc(desc_perc))  %>%
    write_csv("../Data/Outputs/all_routes_desc_roadclass.csv")


all_routes_out %>%
    mutate(len_km = st_length(.)/1000) %>%
    st_drop_geometry() %>%
    group_by(source) %>%
    summarise(km = sum(len_km)) %>%
    ungroup() %>%
    mutate(desc_perc = (km/all_routes_km) * 100) %>%
    arrange(desc(desc_perc))  %>%
    write_csv("../Data/Outputs/all_routes_src_sum.csv")
