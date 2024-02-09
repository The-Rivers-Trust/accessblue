librarian::shelf(tidyverse, sf, tictoc)

waterside_OS_NGD_waterlink <- st_read(
    dsn = out_gpkg, 
         layer = "aire_calder_waterside_OS_NGD_waterlink")

waterside_OS_NGD_waterlinkset <- st_read(dsn = out_gpkg, 
                                         layer = "aire_calder_waterside_OS_NGD_waterlinkset")


head(waterside_OS_NGD_waterlink)

# "wb_src"      "description" "osid" 

waterside_OS_NGD_waterlink %>%
    mutate(length = st_length(.)) %>%
    st_drop_geometry() %>%
    group_by(wb_src) %>%
    summarise(total = sum(length)/1000)

waterside_OS_NGD_waterlinkset %>%
    mutate(length = st_length(.)) %>%
    st_drop_geometry() %>%
    group_by(wb_src) %>%
    summarise(total = sum(length)/1000)

waterlink_sum <- waterside_OS_NGD_waterlink %>%
    mutate(length = st_length(.)) %>%
    st_drop_geometry() %>%
    group_by(wb_src, description) %>%
    summarise(link_src_desc_len = sum(length)/1000) %>%
    ungroup() %>%
    mutate(link_total_length = sum(st_length(waterside_OS_NGD_waterlink))/1000,
           link_src_desc_perc = (link_src_desc_len/link_total_length) * 100) %>%
    arrange(desc(link_src_desc_perc)) %>%
    print(n = 42)

waterlinkset_sum <-waterside_OS_NGD_waterlinkset %>%
    mutate(length = st_length(.)) %>%
    st_drop_geometry() %>%
    group_by(wb_src, description) %>%
    summarise(waterlinkset_src_desc_len = sum(length)/1000) %>%
    ungroup() %>%
    mutate(linkset_total_length = sum(st_length(waterside_OS_NGD_waterlinkset))/1000,
           linkset_src_desc_perc = (waterlinkset_src_desc_len/linkset_total_length) * 100) %>%
    arrange(desc(linkset_src_desc_perc))  %>%
    print(n = 42)

waterlink_sum %>%
    filter(wb_src == "wtr_ntwk_waterlink") %>%
    write_csv("../Data/Outputs/waterlink_sum.csv")

 waterlinkset_sum %>%
     filter(wb_src == "wtr_ntwk_waterlink") %>%
     write_csv("../Data/Outputs/waterlinkset_sum.csv")
