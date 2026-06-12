######################## PROJECT: HoverflyAtlas 
# Author: Hinatea Ariey & Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Hinatea Ariey
# Start: Spring 2026
# Data: MNHNL
# Script objective : load and combine all hoverfly data in Luxembourg through time

############ Load data ----

##### DISCLAIMER : Several docs were modified at excel lvl and some insignifiant data were ignored, do not use this database outside of ISS13 presentation
paste0(DATAPATH, "fichier1.csv")
#import database from different tab of the doc
BC1 <- read_xlsx(paste0(DATAPATH, "ID_Bycatch sorting_20260611.xlsx"),sheet=3)
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





###### Draft - Basket

#hn <- read_xlsx(path, sheet="premiere gooood sheet")
#sheets <- c("bbb","ccc") # reste des bonnes sheeeeeeets
#for (i in 1: length(sheets)){
#  hn <-rbind(hn,read_xlsx(path, sheet=sheets[i]))
#}



