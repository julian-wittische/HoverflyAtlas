######################## PROJECT: HoverflyAtlas 
# Author: Hinatea Ariey & Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Hinatea Ariey
# Start: Spring 2026
# Data: MNHNL
# Script objective : create map of all hoverfly data in Luxembourg through time

library(readxl)
library(stringr::str_conv)

library(dplyr)
library(latticeExtra)
library(mapview)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(reshape2)
library(rgeos)
library(RgoogleMaps)
library(sf)
library(sp)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(fasterize)
library(stars)
library(terra)
library(rgeoboundaries)


############ Load data ----

##### DISCLAIMER : Several docs were modified at excel lvl and some insignifiant data were ignored, do not use this database outside of ISS13 presentation

#import database from different tab of the doc
BC1 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Bycatch sorting_20260611.xlsx",sheet=3)
BC2 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Bycatch sorting_20260611.xlsx",sheet=7)
BC3 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Bycatch sorting_20260611.xlsx",sheet=8)
BC6 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Bycatch sorting_20260611.xlsx",sheet=11)
BC7 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Bycatch sorting_20260611.xlsx",sheet=12)

HN1 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Hand netting atlas_20260611.xlsx",sheet=1)
HN2 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Hand netting atlas_20260611.xlsx",sheet=2)
HN3 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Hand netting atlas_20260611.xlsx",sheet=3)
HN4 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Hand netting atlas_20260611.xlsx",sheet=4)
HN5 <- read_xlsx("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/ID_Hand netting atlas_20260611.xlsx",sheet=5)

MD <- read.csv("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/Mdata.csv", header=TRUE)

# combine them
BC <- rbind(BC1, BC2)
BC <- rbind(BC, BC3)
BC <- rbind(BC, BC6)
BC <- rbind(BC, BC7)

HN <- rbind(HN1, HN2)
HN <- rbind(HN, HN4)
HN <- rbind(HN, HN5)

DB <- rbind(BC, HN)

# check the number of individuals
sum(DB[, 19], na.rm = TRUE)



###### Important as fgb is not recognised by pandoc
mapviewOptions(fgb = FALSE)

lux5km <- raster(nrows=12, ncols=17, xmn=48000, xmx=108000, ymn=55000, ymx=140000,
                 crs=CRS('+init=EPSG:2169'), resolution=5000, vals=1:204)

lux_borders <- geoboundaries("Luxembourg", adm_lvl="adm0")
lux_borders <- st_transform(lux_borders, crs="EPSG:2169")
lux_borders <- as(lux_borders, "Spatial")
lux_raster <- rasterize(lux_borders, lux5km, mask=TRUE, getCover=TRUE)

plot(lux_raster)
lux_raster[lux_raster==0] <- NA

plot(lux_borders)

cell_number_lux <- lux_raster

cell_number_lux[!is.na(cell_number_lux)] <- 1:length(cell_number_lux[!is.na(cell_number_lux)])

rtp <- rasterToPolygons(cell_number_lux, digits=20)

tableL<- as.data.frame(tableL)
#table_swlux <- table_swlux[-nrow(table_swlux),]

cells_done_lux <- unique(tableL$`CELL`)
cells_done_lux <- cells_done_lux[!is.na(cells_done_lux)]
effort <- cell_number_lux
effort[] <- NA
effort[which(values(cell_number_lux)%in%cells_done_lux)] <- cells_done_lux
rtp_effort <- rasterToPolygons(effort, digits=20)

satorosm <-  "OpenStreetMap" #"Esri.WorldImagery" "Esri.WorldImagery" #  "OpenStreetMap" #








###### Draft - Basket

#hn <- read_xlsx(path, sheet="premiere gooood sheet")
#sheets <- c("bbb","ccc") # reste des bonnes sheeeeeeets
#for (i in 1: length(sheets)){
#  hn <-rbind(hn,read_xlsx(path, sheet=sheets[i]))
#}



