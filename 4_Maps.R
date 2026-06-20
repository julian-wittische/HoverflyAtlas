source("2_LuxembourgBorders.R")
source("3_CombineData.R")


###### Make DB spatial ----
DB_sf <- st_as_sf(DB,coords=c("Long", "Lat"), crs = st_crs(4326))

DB_sf <- DB_sf %>%
  st_crop(bbox) %>%
  st_transform("EPSG:2169")


DB_sf22 <- DB_sf[DB_sf$Year %in% as.character(1950:2022),]
DB_sf23 <- DB_sf[DB_sf$Year %in% as.character(1800:2023),]
DB_sf24 <- DB_sf[DB_sf$Year %in% as.character(1800:2024),]
DB_sf25 <- DB_sf[DB_sf$Year %in% as.character(1800:2025),]
DB_sf26 <- DB_sf[DB_sf$Year %in% as.character(1800:2026),]

###### Plotting ----
LuxAtlasMap <- function(x) {
  ggplot() +
    geom_sf(data = GR2169_c, fill = "white", color = "grey") +
    geom_text(data = country_labels, aes(x = x, y = y, label = name),
              size = 3, color = "grey40", fontface = "italic") +
    scale_fill_viridis_c(option = "viridis", name = expression("")) +
    labs(title = NULL) + #labs(title = as.character(max(as.numeric(x$Year,na.rm=T))))
    theme(legend.position = "right",
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.title=element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.title=element_blank())+
    geom_sf(data=x[x$Source %in% c("Citizen science","MNHNL"),],aes(color=Source), size=1.5, alpha=0.5)+
    geom_sf(data=x[!(x$Source %in% c("Citizen science","MNHNL")),], aes(color=Source), size=2.75, alpha=0.5)+
    scale_color_manual(
      values = c(
        "Citizen science" = "grey35",
        "MNHNL" = "blue",
        "Hand netting" = "lightblue",
        "Malaise traps" = "orange",
        "Pan traps" = "brown"
        
      ) 
    ) +
    annotation_north_arrow(
      location = "tr",      # top-right
      which_north = "true",
      style = north_arrow_fancy_orienteering( fill = c("white", "grey"),  line_col = "grey",
                                              text_col = "grey35"),
      height = unit(1.2, "cm"),
      width = unit(1.2, "cm"),
      pad_x = unit(1.5, "cm"),
      pad_y = unit(1.5, "cm"),

    ) +
    annotation_scale(
      location = "br",      # bottom-right
      width_hint = 0.25,
      pad_x = unit(1, "cm"),
      pad_y = unit(1, "cm"),
      bar_cols = c("grey", "white"),
      text_col = "grey35",
      line_col = "grey35"
    )
  
}
LuxAtlasMap(DB_sf_sp)

LuxAtlasMap(DB_sf[DB_sf$Year %in% as.character(1950:2020),])
LuxAtlasMap(DB_sf[DB_sf$Year %in% as.character(1950:2021),])
LuxAtlasMap(DB_sf[DB_sf$Year %in% as.character(1950:2022),])
LuxAtlasMap(DB_sf[DB_sf$Year %in% as.character(1950:2023),])
LuxAtlasMap(DB_sf[DB_sf$Year %in% as.character(1950:2024),])
LuxAtlasMap(DB_sf[DB_sf$Year %in% as.character(1950:2025),])
LuxAtlasMap(DB_sf[DB_sf$Year %in% as.character(1950:2026),])


##### Syritta pipiens example ----

DB_sf_sp <- DB_sf[DB_sf$ID %in% "Syritta pipiens",]

LuxAtlasMapSimple <- function(x) {
  ggplot() +
    geom_sf(data = GR2169_c, fill = "white", color = "black") +
    geom_text(data = country_labels, aes(x = x, y = y, label = name),
              size = 3, color = "black", fontface = "italic") +
    scale_fill_viridis_c(option = "viridis", name = expression("")) +
    labs(title = NULL) + #labs(title = as.character(max(as.numeric(x$Year,na.rm=T))))
    theme(legend.position = "right",
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.title=element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.title=element_blank())+
    geom_sf(data=x[x$Source %in% c("Citizen science","MNHNL"),],aes(color=Source), size=1.5, alpha=0.5)+
    geom_sf(data=x[!(x$Source %in% c("Citizen science","MNHNL")),], aes(color=Source), size=1.5, alpha=0.5)+
    scale_color_manual(
      values = c(
        "Citizen science" = "black",
        "MNHNL" = "black",
        "Hand netting" = "black",
        "Malaise traps" = "black",
        "Pan traps" = "black"
        
      ) 
    ) +
    annotation_north_arrow(
      location = "tr",      # top-right
      which_north = "true",
      style = north_arrow_fancy_orienteering( fill = c("white", "black"),  line_col = "black",
                                              text_col = "black"),
      height = unit(1.2, "cm"),
      width = unit(1.2, "cm"),
      pad_x = unit(1.5, "cm"),
      pad_y = unit(1.5, "cm"),
      
    ) +
    annotation_scale(
      location = "br",      # bottom-right
      width_hint = 0.25,
      pad_x = unit(1, "cm"),
      pad_y = unit(1, "cm"),
      bar_cols = c("grey35", "white"),
      text_col = "black",
      line_col = "black"
    )
  
}
LuxAtlasMapSimple(DB_sf_sp)

