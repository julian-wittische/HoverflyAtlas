source("2_LuxembourgBorders.R")
source("3_CombineData.R")


###### Make DB spatial ----
DB_sf <- st_as_sf(DB,coords=c("Long", "Lat"), crs = st_crs(4326))

DB_sf <- DB_sf %>%
  st_crop(bbox) %>%
  st_transform("EPSG:2169")


###### Plotting ----
ggplot() +
  geom_sf(data = GR2169_c, fill = "white", color = "grey") +
  geom_text(data = country_labels, aes(x = x, y = y, label = name),
            size = 3, color = "grey40", fontface = "italic") +
  scale_fill_viridis_c(option = "viridis", name = expression("")) +
  labs(title = "Map of Luxembourg showing data by source") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title=element_blank()) +
  geom_sf(data=DB_sf[DB_sf$Source %in% c("Citizen science","MNHNL"),],aes(color=Source), size=2)+
  geom_sf(data=DB_sf[!(DB_sf$Source %in% c("Citizen science","MNHNL")),], aes(color=Source), size=2.75)+
  scale_color_manual(
    values = c(
      "Citizen science" = "grey",
      "MNHNL" = "brown",
      "Hand netting" = "lightblue",
      "Malaise traps" = "orange",
      "Pan traps" = "green"
      
    ) 
  )


##### Syritta pipiens example ----

DB_sf_sp <- DB_sf[DB_sf$ID %in% "Syritta pipiens",]

DB_sf_sp2023 <- DB_sf_sp[DB_sf_sp$Year %in% "2023",]
DB_sf_sp2024 <- DB_sf_sp[DB_sf_sp$Year %in% "2024",]
DB_sf_sp2025 <- DB_sf_sp[DB_sf_sp$Year %in% "2025",]
DB_sf_sp2026 <- DB_sf_sp[DB_sf_sp$Year %in% "2026",]
DB_sf_spold  <- DB_sf_sp[!(DB_sf_sp$Year %in% c("2023","2024","2025","2026")), ] <- "Before 2023"


Map_sp_allyears <- ggplot() +
  geom_sf(data = GR2169_c, fill = "white", color = "grey") +
  geom_text(data = country_labels, aes(x = x, y = y, label = name),
            size = 3, color = "grey40", fontface = "italic") +
  scale_fill_viridis_c(option = "viridis", name = expression("")) +
  labs(title = "Map of Luxembourg showing Syritta pipiens distribution") +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title=element_blank()) +
  geom_sf(data=DB_sf_sp[DB_sf_sp$Source %in% c("Citizen science","MNHNL"),],aes(color=Source), size=2)+
  geom_sf(data=DB_sf_sp[!(DB_sf_sp$Source %in% c("Citizen science","MNHNL")),], aes(color=Source), size=2.75)+
  scale_color_manual(
    values = c(
      "Citizen science" = "grey",
      "MNHNL" = "brown",
      "Hand netting" = "lightblue",
      "Malaise traps" = "orange",
      "Pan traps" = "green"
      
    ) 
  )
