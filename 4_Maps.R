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
  labs(title = "") +
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

