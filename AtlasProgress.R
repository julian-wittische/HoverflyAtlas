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
library(googledrive)
library(googlesheets4)
library(ggplot2)
library(fasterize)
library(stars)
library(terra)
library(rgeoboundaries)

tableL <- read_sheet("https://docs.google.com/spreadsheets/d/1Ws6ROUVHCIgfvM7GwVXAZrXQ677aTO24QbuX1PHtpYE/edit?usp=sharing", na="")

# tab <- apply(as.matrix(tableL[,2:5]), 2, unlist)
# sum(as.numeric(tab$`A. cineraria`), na.rm=T)
# sum(as.numeric(tab$`B. major`), na.rm=T)

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

tableL<- as.data.frame(tableL)
#table_swlux <- table_swlux[-nrow(table_swlux),]

cells_done_lux <- unique(tableL$`CELL`)
cells_done_lux <- cells_done_lux[!is.na(cells_done_lux)]
effort <- cell_number_lux
effort[] <- NA
effort[which(values(cell_number_lux)%in%cells_done_lux)] <- cells_done_lux
rtp_effort <- rasterToPolygons(effort, digits=20)

satorosm <-  "OpenStreetMap" #"Esri.WorldImagery" "Esri.WorldImagery" #  "OpenStreetMap" #

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
             color="red") #get rid of color

eff <- mapview(rtp_effort,
                  method = "ngb", 
                  na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
                  query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
                  trim = TRUE,
                  legend = FALSE, #no need for legend
                  map.types = satorosm,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
                  alpha.regions = 0.25,
                  col.regions = "blue",
                  lwd=2,
                  color="blue") #get rid of color

comb <- m + eff
comb

mapshot(comb, url="lux_effort_osm.html")

# sum(unlist(as.numeric(as.character(table_swlux$`B. major`))), na.rm=TRUE)
# length(which(unlist(as.numeric(as.character(table_swlux$`B. major`)))>=1))
# length(which(unlist(as.numeric(as.character(table_swlux$`B. major`)))>=1))/457

# People
species <- table_swlux[, 2:5]
species  <- apply(species, 2, FUN=function(x){as.numeric(as.character(x))})
table(table_swlux[rowSums(species, na.rm=TRUE) != 0,"BOOKED BY"])

### Cheers

AC21 <- read.csv("Data/Acineraria2021.csv")
AC21 <- st_as_sf(AC21[,2:3], coords = c("Longitude", "Latitude"), crs = 4326)
AC21 <- st_transform(AC21, crs=2169)

AC21map <- mapview(AC21, na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
                      query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
                      trim = TRUE,
                      legend = FALSE, #no need for legend
                      map.types = satorosm,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
                      alpha.regions = 0.5,
                      col.regions = "blue",
                      lwd=2,
                      color="blue")

comb_AC + AC21map

################################################################################
tableL <- as.data.frame(tableL)
colnames(tableL)[2] <- "LATITUDE"
coor <- tableL[, c("LONGITUDE","LATITUDE")]
coor$LONGITUDE <- as.numeric(coor$LONGITUDE)
coor$LATITUDE <- as.numeric(coor$LATITUDE)

visits <- st_as_sf(coor, coords = c("LONGITUDE","LATITUDE"), crs = 4326)

visitsmap <- mapview(visits, na.color = rgb(0, 0, 255, max = 255, alpha = 0), #get rid of color
                   query.type = "click", #CLICK ON A PLACE TO KNOW WHICH CELL YOU ARE IN
                   trim = TRUE,
                   legend = FALSE, #no need for legend
                   map.types = satorosm,#"Esri.WorldImagery",#, # CHANGE TO "Esri.WorldImagery" IF YOU WANT
                   alpha.regions = 0.5,
                   col.regions = "blue",
                   lwd=2,
                   color="red")

mapshot2(visitsmap, url="visits2023map4CG.html")
