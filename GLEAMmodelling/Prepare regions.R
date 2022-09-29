.libPaths(c(.libPaths()[2], .libPaths()[3]))
######
library(dplyr)
library(terra)
library(sf)

IPCC <- terra::vect("../Mapping/IPCC_WGS_land.shp")

IPCC <- IPCC[IPCC$Type %in% c("Land", "Land-Ocean") & !(IPCC$Acronym %in% c("WAN", "EAN")),]

#Exclude the Sahara (SAH) and East Asia (China and Japan = EAS)
IPCChigh <- IPCC[!(IPCC$Acronym %in% c("GIC", "WNA", "NCA", "NEU", "WSB", "NAU", "SAU", "NWN", "CNA", "WCE", "ESB", "ARP", "CAU", "NZ", "NEN", "ENA", "EEU", "RAR", "RFE", "EAU", "EAS", "SAH")),]
IPCChigh <- terra::as.data.frame(IPCChigh)

#Include East Asia (China and Japan)
IPCClow <- IPCC[!(IPCC$Acronym %in% c(IPCChigh$Acronym, "GIC", "SAH")),]
IPCClow <- as.data.frame(IPCClow)



interventionsHigh <- read.csv('Interventions high potential init.csv')

GLEAMhighDB <- data.frame()
for(i in 1:nrow(IPCChigh)){
  tmp <- cbind("High potential", IPCChigh[i, c(2,4,5)], interventionsHigh)
  
  GLEAMhighDB <- rbind(GLEAMhighDB, tmp)
}

names(GLEAMhighDB)[1] <- "Group"

write.csv(GLEAMhighDB, 'GLEAM interventions high potential regions.csv')


interventionsLow <- read.csv('Interventions low potential init.csv')

GLEAMlowDB <- data.frame()
for(i in 1:nrow(IPCClow)){
  tmp <- cbind("Low potential", IPCClow[i, c(2,4,5)], interventionsLow)
  
  GLEAMlowDB <- rbind(GLEAMlowDB, tmp)
}

names(GLEAMlowDB)[1] <- "Group"

write.csv(GLEAMlowDB, 'GLEAM interventions low potential regions.csv')


GLEAMall <- rbind(GLEAMhighDB, GLEAMlowDB)

GLEAMinterventions <- GLEAMall[!duplicated(GLEAMall[, c(1,5,7:9)]), c(1,5,7:9)]

write.csv(GLEAMinterventions, 'GLEAM intervention list.csv')
