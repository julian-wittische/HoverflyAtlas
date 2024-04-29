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
library(raster)

############ Loading biotope shapefile (polygons)

# Attempt to read it directly

# Remarkable trees
library(readxl)
trees <- read_excel("S:/BDPatNat/_Julian/Zoology/hoverfly atlas/Arbres-rem-ANF-181008.xlsx")
trees <- st_as_sf(trees, coords=c("Longitude", "Latitude"), crs="EPSG:2169")

# There is problem accessing server directories; dirty FIX: go on open file and click on server until it becomes green
biot <- st_read(dsn="S:/BDPatNat/_Julian/Zoology/hoverfly atlas/Biotope/wbk-2020-vf-data-public/", layer="WBK_2020_polygones")

# Quercus biotopes BK13, BK14, BK15, BK16, BK17, BK23, 9160
querc <- biot[biot$E_Btyp1_co %in% c("BK13", "BK14", "BK15", "BK16", "BK17", "BK23", "9160"),]
# Wetland biotopes BK05, BK06, BK08, BK10, BK11, 7140, 6430, 3260, 3150, 3130, 91D0, 91E0
wetland <- biot[biot$E_Btyp1_co  %in% c("BK05", "BK06", "BK08", "BK10", "BK11", "7140", "6430", "3260", "3150", "3130", "91D0", "91E0"),]

# Keep only interesting biotopes
plot(querc["E_Btyp1_co"])
plot(wetland["E_Btyp1_co"])

###### Important as fgb is not recognised by pandoc
mapviewOptions(fgb = FALSE)

lux5km <- raster(nrows=12, ncols=17, xmn=48000, xmx=108000, ymn=55000, ymx=140000,
                 crs=CRS('+init=EPSG:2169'), resolution=5000, vals=1:204)

lux1km <- raster(nrows=12, ncols=17, xmn=48000, xmx=108000, ymn=55000, ymx=140000,
                 crs=CRS('+init=EPSG:2169'), resolution=1000, vals=1:5100)

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

# tableL<- as.data.frame(tableL)
# #table_swlux <- table_swlux[-nrow(table_swlux),]
# 
# cells_done_lux <- unique(tableL$`CELL`)
# cells_done_lux <- cells_done_lux[!is.na(cells_done_lux)]
# effort <- cell_number_lux
# effort[] <- NA
# effort[which(values(cell_number_lux)%in%cells_done_lux)] <- cells_done_lux
# rtp_effort <- rasterToPolygons(effort, digits=20)

satorosm <-  "Esri.WorldImagery"

satorosm <-  "OpenStreetMap"

##### Plotting procedure
m <- mapview(rtp,
             method = "ngb", 
             na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
             query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
             trim = TRUE,
             legend = FALSE, #no need for legend
             map.types = satorosm ,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
             alpha.regions = 0,
             lwd=2,
             color="white") #get rid of color

treesm <- mapview(trees, col.regions = "red", color="red")


# biotm <- mapview(biot,
#                     method = "ngb",
#                     na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
#                     query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
#                     trim = TRUE,
#                     legend = FALSE, #no need for legend
#                     map.types = satorosm ,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
#                     alpha.regions = 0.45,
#                     lwd=5,
#                     col.regions = "red",
#                     color="red") #get rid of color

quercm <- mapview(querc,
                 method = "ngb",
                 na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
                 query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
                 trim = TRUE,
                 legend = FALSE, #no need for legend
                 map.types = satorosm ,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
                 alpha.regions = 0.45,
                 lwd=5,
                 col.regions = "yellow",
                 color="yellow") #get rid of color

wetlandm <- mapview(wetland,
                  method = "ngb",
                  na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
                  query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
                  trim = TRUE,
                  legend = FALSE, #no need for legend
                  map.types = satorosm ,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
                  alpha.regions = 0.45,
                  lwd=5,
                  col.regions = "blue",
                  color="blue") #get rid of color

mapshot2(m + quercm + wetlandm + treesm, url="quercus&wetland_osm.html")

