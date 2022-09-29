######
##Regional GHG mitigation potential in agriculture
#        _____
#    ,-:` \;',`'-, 
#  .'-;_,;  ':-;_,'.
# /;   '/    ,  _`.-\
#| '`. (`     /` ` \`|
#|:.  `\`-.   \_   / |
#|     (   `,  .`\ ;'|
# \     | .'     `-'/
#  `.   ;/        .'
#    `'-._____.
#
#Authors: Jon Hillier, Pete Smith, Dali Nayak & Simon Fraval
#Version: 0.01
#
######
#Rice emissions

library(dplyr)
library(terra)
library(sf)



####Spatial data import - 2000 - 2010. 0.5 resolution
###Climate
if("cli_HI.tiff" %in% list.files(path = "Climatic/")  ){ #Check if all pre-processed layers are available for import. If not then process
  HI <- rast("Climatic/cli_HI.tiff")
  #Koeppen Geiger Climate Classification
  KG_class <- rast("Climatic/cli_KG.tiff")
  temperature <- rast("Climatic/cli_TMP.tiff")
  climate_code <- rast("Climatic/cli_cliCode.tiff")
  
  } else { #End climate imports
  
  #Climatic #CRU - 10 yr averages
  tmp <- rast("Climatic/Climatic Research Unit/cru_ts4.01.2001.2010.tmp.dat.nc", "tmp")
  MAT <- mean(tmp, na.rm = T) #Mean annual temerature?
  MAT[MAT < 0.01] <- 0.01 #To match Jon's excel model
  
  pre <- rast("Climatic/Climatic Research Unit/cru_ts4.01.2001.2010.pre.dat.nc", "pre")
  PRE <- mean(pre, na.rm = T) #Precipitation?
  HI <- PRE/MAT # Humidity Index
  
  #Koeppen Geiger Climate Classification
  KG_class <- rast("Climatic/World_koppen/vat.adf")
  
  #Elevation - meters
  ELE <- rast("Climatic/Elevation/vat.adf") 
  #  if("cli_MAT.tiff" %in% list.files("Climatic/")) {rast("Climatic/cli_MAT.tiff")} else{
  #    MAT <- resample(MAT, ELE, filename = "Climatic/cli_MAT.tiff")
  #    MAT <- rast("Climatic/cli_MAT.tiff")}
  
  if("cli_ELE.tiff" %in% list.files("Climatic/")) {
    ELE <-rast("Climatic/cli_ELE.tiff")} else{
    ELE <- resample(ELE, MAT, filename = "Climatic/cli_ELE.tiff")
    ELE <- rast("Climatic/cli_ELE.tiff")}
  
  
  #1 = "Tropical montane", 2 = "Tropical", 3 ="Warm temperate", 4 = "Cool temperate", 5 = "Boreal"))))
  temperature <- ifel(MAT > 18 & ELE > 1000, 1, ifel(MAT > 18 & ELE <= 1000, 2, ifel(MAT > 10 & MAT <= 18, 3, ifel(MAT > 0 & MAT <= 10, 4, 5))))
  
  pet <- rast("Climatic/Climatic Research Unit/cru_ts4.01.2001.2010.pet.dat.nc", "pet")
  PET <- mean(pet, na.rm = T) #Potential evapo transpiration?
  #1 = Wet, 2 = moist, 3 = dry
  climate_code <- ifel(MAT > 18 & PRE > 2000, 1, ifel(MAT > 18 & PRE <= 2000 & PRE > 1000, 2, ifel(PRE > PET, 2, 3)))
  
  cropArea <- rast('Cropping/Pesticides/CROPS/NC/Rice_HarvestedArea.nc')
  
  HI <- resample(HI, cropArea, filename = 'Climatic/cli_HI.tiff', overwrite = T)
  KG_class <- resample(KG_class, cropArea, method = "near", filename = 'Climatic/cli_KG.tiff', overwrite = T)
  temperature <- resample(temperature, cropArea, method = "near", filename = 'Climatic/cli_TMP.tiff', overwrite = T)
  climate_code <- resample(climate_code, cropArea, method = "near", filename = 'Climatic/cli_CliCode.tiff', overwrite = T)
  
  writeRaster(HI, "Climatic/cli_HI.tiff", overwrite = T)
  writeRaster(KG_class, "Climatic/cli_KG.tiff", overwrite = T)
  writeRaster(temperature, "Climatic/cli_TMP.tiff", overwrite = T)
  writeRaster(climate_code, "Climatic/cli_cliCode.tiff", overwrite = T)
  
  rm(tmp, MAT, pre, PRE, pet, PET, ELE)
} #End climate layer import/ processing

###Cropping
if("i_N_rice.tiff" %in% list.files(path = "Cropping/") ){ #Check if pre-processed layers are available for import. If not then process
  input_N_rice <- rast("Cropping/i_N_rice.tiff")
  input_P_rice <- rast("Cropping/i_P_rice.tiff")
  soil_manure_etc_rice <- rast("Cropping/i_manure_rice.tiff")
  input_fuel_l_rice <- rast("Cropping/i_fuel_rice.tiff")
  input_pest_kg_rice <- rast("Cropping/i_pest_rice.tiff")
  soil_till_rice <- rast("Cropping/i_till_rice.tiff")
  N_residue_rice <- rast("Cropping/i_resN_rice.tiff")
  N_cover_rice <- rast("Cropping/i_covN_rice.tiff")
  soil_residue_rice <- rast("Cropping/i_res_rice.tiff")
  soil_cover_rice <- rast("Cropping/i_cov_rice.tiff")
  
  cropArea_rice <- rast('Cropping/Pesticides/CROPS/NC/Rice_HarvestedArea.nc')
  
  IPCC <- terra::vect("Mapping/IPCC_WGS_land.shp")
  IPCC <- IPCC[IPCC$Type %in% c("Land", "Land-Ocean") & !(IPCC$Acronym %in% c("WAN", "EAN")),]
  riceSeasons <- st_read("Rice/RiceCalendar_v1.gpkg") #@ Temporary as terra has mem issues with this vec
  irriEquip <- rast("Rice/gmia_v5_aei_pct.asc")
  irriUse <- rast("Rice/gmia_v5_aai_pct_aei.asc")
  
} else{ #end crop imports
##Crop area - Use Maggi or Waha? Maggi uses Monfreda, C., Ramankutty 2008. Farming the planet: 2. Waha uses Portmann et al., 2010 MIRCA2000
#Monfreda ha -#! sum of all is less than the grid cell area - 8100 ha
  cropArea_rice <- rast('Cropping/Pesticides/CROPS/NC/Rice_HarvestedArea.nc')
  
  IPCC <- terra::vect("Mapping/IPCC_WGS_land.shp")
  IPCC <- IPCC[IPCC$Type %in% c("Land", "Land-Ocean") & !(IPCC$Acronym %in% c("WAN", "EAN")),]
  riceSeasons <- st_read("Rice/RiceCalendar_v1.gpkg")
  irriEquip <- rast("Rice/gmia_v5_aei_pct.asc")
  irriUse <- rast("Rice/gmia_v5_aai_pct_aei.asc")


  ##Inputs
  #input_N_ha, input_P_ha, soil_manure_etc, soil_till, input_pest_rice_kg, input_pest_crop_kg

  #Fertiliser N - KG N / m^2 / year
  F_N1 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2000.asc")
  F_N2 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2001.asc")
  F_N3 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2002.asc")
  F_N4 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2003.asc")
  F_N5 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2004.asc")
  F_N6 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2005.asc")
  F_N7 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2006.asc")
  F_N8 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2007.asc")
  F_N9 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2008.asc")
  F_N10 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2009.asc")
  F_N11 <- rast("Cropping/Fert_N_P/Nfer_ASCII/nfery2010.asc")
  
  F_N <- c(F_N1, F_N2, F_N3, F_N4, F_N5, F_N6, F_N7, F_N8, F_N9, F_N10, F_N11)
  input_N_m <- mean(F_N, na.rm= T)
  input_N_m <- resample(input_N_m, cropArea)
  
  input_N_rice <- (input_N_m / 1000) * cropArea_rice #Convert km2 to ha
  
  rm(list=ls(pattern="^F_N"))
  
  #Fertiliser P
  F_P1 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2000.asc")
  F_P2 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2001.asc")
  F_P3 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2002.asc")
  F_P4 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2003.asc")
  F_P5 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2004.asc")
  F_P6 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2005.asc")
  F_P7 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2006.asc")
  F_P8 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2007.asc")
  F_P9 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2008.asc")
  F_P10 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2009.asc")
  F_P11 <- rast("Cropping/Fert_N_P/Pfer_ASCII/pfery2010.asc")
  
  
  F_P <- c(F_P1, F_P2, F_P3, F_P4, F_P5, F_P6, F_P7, F_P8, F_P9, F_P10, F_P11)
  input_P_ha <- mean(F_P, na.rm= T)
  input_P_ha <- resample(input_P_ha, cropArea) #0.00004 difference in resolution
  
  input_P_rice <- (input_P_ha / 1000) * cropArea_rice #Convert m2 to ha

  rm(list=ls(pattern="^F_P"))
  
  #Manure Zhang et al. Manure nitrogen applied to cropland and rangeland in kg N/km**2/yr
  m1 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2000.asc")
  m2 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2001.asc")
  m3 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2002.asc")
  m4 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2003.asc")
  m5 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2004.asc")
  m6 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2005.asc")
  m7 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2006.asc")
  m8 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2007.asc")
  m9 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2008.asc")
  m10 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2009.asc")
  m11 <- rast("Cropping/Fert_N_manure/ManNitProCrpRd/yy2010.asc")
  
  m <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)
  soil_manure_etc <- mean(m, na.rm = T)
  soil_manure_etc <- resample(soil_manure_etc, cropArea) #0.00004 difference in resolution
  
  N_manure_etc_rice <- (soil_manure_etc/100) * cropArea_rice #Convert km2 to ha
  
  soil_manure_etc_rice <- N_manure_etc_rice * 50 #! Check with Aimable Uwizeye on N content and manure type globally

  
  rm(m, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)
  

#Pesticides - application rate (APR) = kg/ha per year
#HHHLLL. So take the first band
  pest <- rast("Cropping/Pesticides/APR/NC/APR_Alfalfa_2,4-d_H_L.nc")[[1]]
  for(i in 2:length(list.files("Cropping/Pesticides/APR/NC/"))){
    pest <- c(pest, rast(list.files("Cropping/Pesticides/APR/NC/", full.names = T)[i])[[1]])
  
  }
  names(pest) <- gsub(".nc", "", list.files("Cropping/Pesticides/APR/NC/"))

  

  pest_rice <- max(pest[[grep("Rice", names(pest))]], na.rm = T) ##!These should all be sum. Can't remove neg values because of memory issues

  
  ###!Temporary until memory issue sorted
  pest_rice <- classify(pest_rice, cbind(-Inf, 0, 0)) 

  ###
  
  pest_rice <- resample(pest_rice, cropArea) # Minor difference
  
  pest_rice_kg <- cropArea$rice * pest_rice
  
  input_pest_kg_rice <- pest_rice_kg
  
  input_fuel_l_rice <- input_pest_kg_rice #!!! Temporary to build model
  
  
  rm(list = ls(pattern = "^pest_"))
  rm(pest)
  
  #Soil management
  #Tillage Vera et al. 2019
  #1 = conventional annual tillage;2 = traditional annual tillage;3 = reduced tillage;4 = Conservation Agriculture;5 = rotational tillage;6 = traditional rotational tillage;7 = Scenario Conservation Agriculture area
  soil_till <- rast('Cropping/Tillage/tillage_revised.nc4', c("rice_till", "whea_till", "maiz_till", "cott_till", "pmil_till", "soyb_till", "vege_till", "rest_till"))
  soil_till <- resample(soil_till, cropArea, method = "near") 
  
  # ha * kg ha-1
  #!@ Adjust residue amounts based on crop 
  soil_residue_rice <- cropArea$rice * ifel(soil_till[["rice_till"]] == 4, 0, 0) # Rice is different https://www.tandfonline.com/doi/abs/10.1080/03650340.2019.1661994?journalCode=gags20
  
  #!@ Adjust residue amounts based on crop
  soil_cover_rice <- cropArea$rice * ifel(soil_till[["rice_till"]] == 4, 3000, 0) # ha * kg ha-1
  
  N_residue_rice <- soil_residue_rice * 0.02 #! from OM_added tab of Jon's model. Marked for 'fixing'!
  N_cover_rice <- soil_cover_rice * 0.02 #! from OM_added tab of Jon's model. Marked for 'fixing'!

  
  soil_till_rice <- soil_till[["rice_till"]]
  

  rm(cropArea)
  
  writeRaster(input_N_rice, "Cropping/i_N_rice.tiff", overwrite = T)
  writeRaster(input_P_rice, "Cropping/i_P_rice.tiff", overwrite = T)
  writeRaster(soil_manure_etc_rice, "Cropping/i_manure_rice.tiff", overwrite = T)
  writeRaster(input_fuel_l_rice, "Cropping/i_fuel_rice.tiff", overwrite = T)
  writeRaster(input_pest_kg_rice, "Cropping/i_pest_rice.tiff", overwrite = T)
  writeRaster(soil_till_rice, "Cropping/i_till_rice.tiff", overwrite = T)
  writeRaster(N_residue_rice, "Cropping/i_resN_rice.tiff", overwrite = T)
  writeRaster(N_cover_rice, "Cropping/i_covN_rice.tiff", overwrite = T)
  writeRaster(soil_residue_rice, "Cropping/i_res_rice.tiff", overwrite = T)
  writeRaster(soil_cover_rice, "Cropping/i_cov_rice.tiff", overwrite = T)

  
  rm(list = ls(pattern = "^soil_res_"))
  rm(list = ls(pattern = "^soil_cov_"))
} #End crop input processing




#######################
####Model
###Emissions

##Baseline emissions for soil management emissions - inhibition, crop residue incorporation, tillage... 
##Parameters
effN2O <- 298 #@GWP N2O 100 year
effCO2_l_diesel <- 2.68
effCO2_l_pest <- 21
brentrupEmbedded <- data.frame(compound = c("Urea", "Ammonium Nitrate", "TSP", "MoP"), Europe = c(1.62, 1.18, 0.18, 0.23),
                               Russia = c(1.91, 2.85, 0.25, 0.23), USA = c(1.91, 2.52, 0.19, 0.23), 
                               China = c(3.24, 3.47, 0.26, 0.23), Hydrolysis = c(0.73, NA, NA, NA)) 

N2O <- read.csv('EmissionFactors/N2O.csv', stringsAsFactors = F)	

##Int calcs
#! Simplified by assuming all fert is ammonium nitrate
N2O_Ndirect_rice <- ifel(climate_code == 1, input_N_rice * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_rice * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_rice * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_rice + N_cover_rice) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_rice + N_cover_rice) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_rice + N_cover_rice) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

N2O_volat_rice <- ifel(climate_code == 1, input_N_rice * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_rice * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_rice * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_rice + N_cover_rice) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_rice + N_cover_rice) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_rice + N_cover_rice) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

N2O_leach_rice <- ifel(climate_code == 1, input_N_rice * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_rice * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_rice * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_rice + N_cover_rice) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_rice + N_cover_rice) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_rice + N_cover_rice) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

##Emission calcs for non-CH4 emissions
#rice
CO2eq_direct_rice <- N2O_Ndirect_rice * effN2O
CO2eq_indirect_rice <- (N2O_volat_rice + N2O_leach_rice) * effN2O
CO2eq_fuel_rice <- input_fuel_l_rice * effCO2_l_diesel
CO2eq_embed_fert_N_rice <- input_N_rice * brentrupEmbedded[brentrupEmbedded$compound == "Ammonium Nitrate", "Europe"] / 0.335 #!!! using Europe for all for testing purposes.
CO2eq_embed_fert_P_rice <- input_P_rice * brentrupEmbedded[brentrupEmbedded$compound == "TSP", "Europe"] / 0.48
CO2eq_embed_fert_K_rice <- CO2eq_embed_fert_P_rice #!!! temporary
pesticides_rice <- input_pest_kg_rice * effCO2_l_pest
CO2eq_SOC_manure_rice <- -(0.038 * (soil_manure_etc_rice) - 0.0538 * 1000) * 44/12
CO2eq_SOC_residue_rice <- -(0.115 * soil_residue_rice + 0.192 * 1000) * 44/12 #@Residue not incorporated into soil in baseline
CO2eq_SOC_cover_rice <- -(0.038 * soil_cover_rice - 0.0538 * 1000) * 44/12

rm(N2O_Ndirect_rice, N2O_volat_rice, N2O_leach_rice, input_N_rice, N_residue_rice, N_cover_rice)

###Rice CH4 emission estimates
##Time component of equation 5.1
riceSeasons$daysS1 <- ifelse(riceSeasons$HARV_END1 < riceSeasons$PLANT_PK1, riceSeasons$HARV_END1+365 - riceSeasons$PLANT_PK1, riceSeasons$HARV_END1 - riceSeasons$PLANT_PK1)
riceSeasons$daysS2 <- ifelse(riceSeasons$HARV_END2 < riceSeasons$PLANT_PK2, riceSeasons$HARV_END2+365 - riceSeasons$PLANT_PK2, riceSeasons$HARV_END2 - riceSeasons$PLANT_PK2)
riceSeasons$daysS3 <- ifelse(riceSeasons$HARV_END3 < riceSeasons$PLANT_PK3, riceSeasons$HARV_END3+365 - riceSeasons$PLANT_PK3, riceSeasons$HARV_END3 - riceSeasons$PLANT_PK3)

riceSeasons <- terra::vect(riceSeasons)
rice_t <- c(rasterize(riceSeasons, cropArea_rice, "NUM_CROP"), rasterize(riceSeasons, cropArea_rice, "daysS1"), rasterize(riceSeasons, cropArea_rice, "daysS2"), rasterize(riceSeasons, cropArea_rice, "daysS3"))


##Emission factor calculation - EFbase (table 5.11) * SFwater * SFpre * SFo https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch05_Cropland.pdf
#Soil type and cultivar? = Tier 2
#Base emission factor for cont flood without manure
#!No specific factors for Oceania, Central America, and 'other Asia'
IPCC$EFbase <- ifelse(IPCC$Continent == "AFRICA", 1.19, ifelse(IPCC$Continent %in% c("EUROPE", "EUROPE-AFRICA"), 1.56, ifelse(IPCC$Continent %in% c("NORTH-AMERICA", "OCEANIA"), 0.65, ifelse(IPCC$Continent %in% c("CENTRAL-AMERICA", "SOUTH-AMERICA"), 1.26, NA))))
IPCC$EFbase <- ifelse(is.na(IPCC$EFbase) & IPCC$Name %in% c("E.Asia", "E.C.Asia", "W.C.Asia", "Tibetan-Plateau", "Arabian-Peninsula", "W.Siberia", "E.Siberia", "Russian-Far-East", "Russian-Arctic"), 1.32, ifelse(is.na(IPCC$EFbase) & IPCC$Name == "S.E.Asia", 1.22, ifelse(is.na(IPCC$EFbase) & IPCC$Name == "S.Asia", 0.85, IPCC$EFbase)))

riceCH4EFbase <- rasterize(IPCC, cropArea_rice, "EFbase")

#Scaling factor for water regime during cultivation period 
irriEquip <- resample(irriEquip, cropArea_rice, method = "ngb")
irriUse <- resample(irriUse, cropArea_rice, method = "ngb")
irrigation <- irriEquip * (irriUse/100) / 100 # % of gridcell likely to be irrigated
irrigation[is.na(irrigation)] <- 0

rm(irriEquip, irriUse, riceSeasons)

SFw_irri <- 0.6 #irrigated scaling factor is 0.6 - IPCC 2019 Table 5.12
SFw_rain <- 0.45 #rainfed scaling factor is 0.45 - IPCC 2019 Table 5.12
SFp <- 1.22 # IPCC 2019 Table 5.13
SFo <- 1 + ((0 * 0.19) + (soil_manure_etc_rice/1000/365 * 0.21)) #IPCC 2019 Table 5.13. Assume no straw incorporated in baseline (i.e. 0 * 0.19). KG of manure converted to tonnes per ha per day

##Emission estimate for CH4 - set max of 160 days based on IPCC 2019 Table 5.11a
riceCH4irri <- riceCH4EFbase * SFw_irri * SFp * SFo * min(rice_t$daysS1 + rice_t$daysS2 + rice_t$daysS3, 160, na.rm = T) * (cropArea_rice * irrigation) # convert to Gg = * 10^-6
riceCH4rain <- riceCH4EFbase * SFw_rain * SFp * SFo * min(rice_t$daysS1 + rice_t$daysS2 + rice_t$daysS3, 160, na.rm = T) * (cropArea_rice * (1-irrigation)) # convert to Gg = * 10^-6 * 10^-6

riceCH4_kg_year <- riceCH4irri + riceCH4rain 
CO2eq_riceCH4_kg_year <- riceCH4_kg_year * 28

global(riceCH4_kg_year, sum, na.rm = T)*1e-9 #@ in Tg #@ gosat estimates = 41 Tg. (Lena) Höglund-Isaksson 2020 = 30 Tg (2015). If 365 days allowed then our estimate is 30-35x higher than these. @160 day max = 21 Tg


###Total emissions for rice in CO2-eq
CO2eq_rice_kg_year <- sum(CO2eq_riceCH4_kg_year, CO2eq_direct_rice, CO2eq_indirect_rice, CO2eq_fuel_rice, CO2eq_embed_fert_N_rice, CO2eq_embed_fert_P_rice, CO2eq_embed_fert_K_rice, pesticides_rice, CO2eq_SOC_manure_rice, CO2eq_SOC_residue_rice, CO2eq_SOC_cover_rice, na.rm = T)

global(CO2eq_rice_kg_year, sum, na.rm = T)*1e-9 # in Tg

writeRaster(CO2eq_riceCH4_kg_year, 'cropping/base_total_rice_CH4.tiff', overwrite = T)
writeRaster(CO2eq_rice_kg_year, 'cropping/base_total_rice_CO2e.tiff', overwrite = T)
