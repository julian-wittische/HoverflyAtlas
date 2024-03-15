################################################################################
############# GOAL: interactive mapping of interesting biotopes  ###############
################################################################################
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: HOVERFLY ATLAS PROJECT
# Start: WINTER 2024
# Data: ANF
################################################################################

############ TO DO LIST
# 

############ Loading libraries
library(sf)
library(rgeoboundaries)
library(mapview)
library(leaflet)

############ Loading biotope shapefile (polygons)

# Attempt to read it directly
biot <- st_read(dsn="C:/Users/YNM724/Downloads/wbk-2020-vf-data-public/", layer="WBK_2020_polygones")
biot <- biot[biot$E_Btyp1_co=="91E0"|biot$E_Btyp1_co=="91D0",]
# Keep only interesting biotopes
plot(biot["E_Btyp1_co"])

###### Important as fgb is not recognised by pandoc
mapviewOptions(fgb = FALSE)

lux_borders <- geoboundaries("Luxembourg", adm_lvl="adm0")
lux_borders <- st_transform(lux_borders, crs="EPSG:2169")
lux_borders <- as(lux_borders, "Spatial")

satorosm <-  "Esri.WorldImagery" #"OpenStreetMap" #"Esri.WorldImagery" 

biotmsat <- mapview(biot,
                 method = "ngb", 
                 na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
                 query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
                 trim = TRUE,
                 legend = FALSE, #no need for legend
                 map.types = satorosm ,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
                 alpha.regions = 0.45,
                 lwd=5,
                 col.regions = "red",
                 color="red") #get rid of color

mapshot2(biotmsat, url="91E0&91D0_sat.html")

satorosm <-  "OpenStreetMap"

biotmosm <- mapview(biot,
                    method = "ngb", 
                    na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
                    query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
                    trim = TRUE,
                    legend = FALSE, #no need for legend
                    map.types = satorosm ,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
                    alpha.regions = 0.45,
                    lwd=5,
                    col.regions = "red",
                    color="red") #get rid of color

mapshot2(biotmosm, url="91E0&91D0_osm.html")

