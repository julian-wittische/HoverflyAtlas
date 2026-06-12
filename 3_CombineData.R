######################## PROJECT: HoverflyAtlas 
# Author: Hinatea Ariey & Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Hinatea Ariey
# Start: Spring 2026
# Data: MNHNL
# Script objective : load and combine all hoverfly data in Luxembourg through time

# load libraries and datapath before this script
source("1_Config&Libraries.R")

############ Load data ----

##### DISCLAIMER : Several docs were modified at excel lvl and some insignifiant data were ignored, do not use this database outside of ISS13 presentation

#import database from different tab of the doc
BC1 <- read_xlsx(paste0(DATAPATH, "ID_Bycatch sorting_20260611.xlsx"),sheet=3)
BC2 <- read_xlsx(paste0(DATAPATH,"ID_Bycatch sorting_20260611.xlsx"),sheet=7)
BC3 <- read_xlsx(paste0(DATAPATH, "ID_Bycatch sorting_20260611.xlsx"),sheet=8)
BC6 <- read_xlsx(paste0(DATAPATH, "ID_Bycatch sorting_20260611.xlsx"),sheet=11)
BC7 <- read_xlsx(paste0(DATAPATH, "ID_Bycatch sorting_20260611.xlsx"),sheet=12)

HN1 <- read_xlsx(paste0(DATAPATH, "ID_Hand netting atlas_20260611.xlsx"),sheet=1)
HN2 <- read_xlsx(paste0(DATAPATH, "ID_Hand netting atlas_20260611.xlsx"),sheet=2)
HN3 <- read_xlsx(paste0(DATAPATH, "ID_Hand netting atlas_20260611.xlsx"),sheet=3)
HN4 <- read_xlsx(paste0(DATAPATH, "ID_Hand netting atlas_20260611.xlsx"),sheet=4)
HN5 <- read_xlsx(paste0(DATAPATH, "ID_Hand netting atlas_20260611.xlsx"),sheet=5)

# add origine of the data = Source
BC1$Source <-"WBA"
BC2$Source <- "LMsTh"
BC3$Source <- "LBB"
BC6$Source <- "Insk"
BC7$Source <- "Insk"

MD <- read.csv("W:/02_Shared/HinateaAriey/HoverflyAtlasDATA/Mdata.csv", header=TRUE, encoding="latin1")
colnames(MD)[17] <- "Source"
MD$Year <- format(as.Date(MD$Sample_Date, format="%d/%m/%Y"),"%Y")

# combine them
BC <- rbind(BC1, BC2)
BC <- rbind(BC, BC3)
BC <- rbind(BC, BC6)
BC <- rbind(BC, BC7)

HN <- rbind(HN1, HN2)
HN <- rbind(HN, HN4)
HN <- rbind(HN, HN5)
# add origine of the data = Source
HN$Source <-"HN"

DB <- rbind(BC, HN)

# check the number of individuals
sum(DB[, 19], na.rm = TRUE)

# Keep only necessary data
DB<- DB[,c(2,3,17,29,8)]
colnames(DB)[1] <- "Lat"
colnames(DB)[2] <- "Long"
colnames(DB)[5] <- "Year"

MD <- MD[,c(11,12,5,17,28)]
colnames(MD)[3] <- "ID"

DB <- rbind(DB,MD)

###### Draft - Basket

#hn <- read_xlsx(path, sheet="premiere gooood sheet")
#sheets <- c("bbb","ccc") # reste des bonnes sheeeeeeets
#for (i in 1: length(sheets)){
#  hn <-rbind(hn,read_xlsx(path, sheet=sheets[i]))
#}



