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
#Authors: Jon Hillier, Pete Smith, Simon Fraval
#Version: 0.01
#
######
#QC checks - N2O mathematical model vs empirical model EF
######

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
  
  cropArea <- rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Maize_Total.tif')
  
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
##Crop area - GAEZ 2015 https://dataverse.harvard.edu/dataverse/GAEZ_plus_2015
cropArea <- c(rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Rice_Total.tif'),
              rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Maize_Total.tif'),
              #rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Cotton_Total.tif'),
              #rast('Cropping/Pesticides/CROPS/NC/PasHay_HarvestedArea.nc'), #Pasture and hay not included
              #rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Vegetables_Total.tif'),
              rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Wheat_Total.tif'))

names(cropArea) <- c("rice", "maize", "wheat")
  
 
cropArea <- cropArea*1000 # Unit is 1000s of Ha. Convert to Ha
  


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
  
input_N_rice <- (input_N_m / 1000) * cropArea$rice #Convert km2 to ha
input_N_maize <- (input_N_m / 1000) * cropArea$maize #Convert km2 to ha
input_N_wheat <- (input_N_m / 1000) * cropArea$wheat #Convert km2 to ha
  
rm(list=ls(pattern="^F_N"))
  
  
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
  
N_manure_etc_rice <- (soil_manure_etc/100) * cropArea$rice #Convert km2 to ha
N_manure_etc_maize <- (soil_manure_etc/100) * cropArea$maize #Convert km2 to ha
N_manure_etc_wheat <- (soil_manure_etc/100) * cropArea$wheat #Convert km2 to ha
  
soil_manure_etc_rice <- N_manure_etc_rice * 50 #! Check with Aimable Uwizeye on N content and manure type globally
soil_manure_etc_maize <- N_manure_etc_maize * 50
soil_manure_etc_wheat <- N_manure_etc_wheat * 50
  
rm(m, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)
  
  
#Soil management
#Tillage Vera et al. 2019
#1 = conventional annual tillage;2 = traditional annual tillage;3 = reduced tillage;4 = Conservation Agriculture;5 = rotational tillage;6 = traditional rotational tillage;7 = Scenario Conservation Agriculture area
soil_till <- rast('Cropping/Tillage/tillage_revised.nc4', c("rice_till", "whea_till", "maiz_till"))
soil_till <- resample(soil_till, cropArea, method = "near") 
  
rm(cropArea)
  
#residue = production * (1/HI-1) - HI = harvest index
#proportion incorporated dependent on till practice
soil_residue_rice <- ifel(soil_till[["rice_till"]] == 4, 0, 0) * classify(rast('Cropping/GAEZ2015/GAEZ._2015_crop_production/GAEZAct2015_Production_Rice_Total.tif'), cbind(-Inf, 0, 0)) * 1000 * 1000 * (1/0.4-1) # Rice is different https://www.tandfonline.com/doi/abs/10.1080/03650340.2019.1661994?journalCode=gags20
soil_res_maize_kg <- ifel(soil_till[["maiz_till"]] == 4, 1, 0.1) * classify(rast('Cropping/GAEZ2015/GAEZ._2015_crop_production/GAEZAct2015_Production_Maize_Total.tif'), cbind(-Inf, 0, 0)) * 1000 * 1000 * (1/0.3-1)
soil_res_wheat_kg <- ifel(soil_till[["whea_till"]] == 4, 1, 0.1) * classify(rast('Cropping/GAEZ2015/GAEZ._2015_crop_production/GAEZAct2015_Production_Wheat_Total.tif'), cbind(-Inf, 0, 0)) * 1000 * 1000 * (1/0.5-1)
 

  
crop_N <- read.csv('Cropping/N_content/IPCC_Table1_11a.csv')
  
N_residue_rice <- (soil_residue_rice*crop_N$aboveN[crop_N$cropAbr =="rice"]) + (soil_residue_rice*crop_N$ratioBelow[crop_N$cropAbr =="rice"]*crop_N$belowN[crop_N$cropAbr =="rice"])
N_residue_maize  <- (soil_res_maize_kg*crop_N$aboveN[crop_N$cropAbr =="maize"]) + (soil_res_maize_kg*crop_N$ratioBelow[crop_N$cropAbr =="maize"]*crop_N$belowN[crop_N$cropAbr =="maize"])
N_residue_wheat <- (soil_res_wheat_kg*crop_N$aboveN[crop_N$cropAbr =="wheat"]) + (soil_res_wheat_kg*crop_N$ratioBelow[crop_N$cropAbr =="wheat"]*crop_N$belowN[crop_N$cropAbr =="wheat"]) 

  
#In CA locations: Fodder yield * area of annual crop
#!@ May overestimate locations with double cropping. 
#!@ Assumes all locations with conservation agriculture use cover cropping
#!@ Assumes all cover cropping is fodder 
#@ Yield converted to kg
#@ Area converted to ha
soil_cover_rice <- ifel(soil_till[["rice_till"]] == 4, 0, 0) * classify(rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Rice_Total.tif'), cbind(-Inf, 0, 0)) *1000 # Rice is different https://www.tandfonline.com/doi/abs/10.1080/03650340.2019.1661994?journalCode=gags20
soil_cov_maize_kg <- ifel(soil_till[["maiz_till"]] == 4, (classify(rast('Cropping/GAEZ2015/GAEZ._2015_crop_yield/GAEZAct2015_Yield_Foddercrops_Mean.tif'), cbind(-Inf, 0, 0)) * 1000), 0) * classify(rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Maize_Total.tif'), cbind(-Inf, 0, 0)) *1000
soil_cov_wheat_kg <- ifel(soil_till[["whea_till"]] == 4, (classify(rast('Cropping/GAEZ2015/GAEZ._2015_crop_yield/GAEZAct2015_Yield_Foddercrops_Mean.tif'), cbind(-Inf, 0, 0)) * 1000), 0) * classify(rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Wheat_Total.tif'), cbind(-Inf, 0, 0)) *1000


returnPropRice <- 0.5 #! assume 50% of above ground biomass is returned to soil!
returnProp <- 0.5 #! assume 50% of above ground biomass is returned to soil!
  
soil_cover_rice <- soil_cover_rice * returnPropRice
soil_cover_maize <- soil_cov_maize_kg * returnProp 
soil_cover_wheat <- soil_cov_wheat_kg * returnProp 
  
N_cover_rice <- (soil_cover_rice*crop_N$aboveN[crop_N$cropAbr =="cover"]) + (soil_cover_rice*(1/returnPropRice)*crop_N$ratioBelow[crop_N$cropAbr =="cover"]*crop_N$belowN[crop_N$cropAbr =="cover"])
N_cover_maize <- (soil_cov_maize_kg*crop_N$aboveN[crop_N$cropAbr =="cover"]) + (soil_cov_maize_kg*(1/returnProp)*crop_N$ratioBelow[crop_N$cropAbr =="cover"]*crop_N$belowN[crop_N$cropAbr =="cover"])
N_cover_wheat <- (soil_cov_wheat_kg*crop_N$aboveN[crop_N$cropAbr =="cover"]) + (soil_cov_wheat_kg*(1/returnProp)*crop_N$ratioBelow[crop_N$cropAbr =="cover"]*crop_N$belowN[crop_N$cropAbr =="cover"])
 
##Read N2O-N EF data from Cui et al. https://www.nature.com/articles/s43016-021-00384-9#data-availability
EF_rice <- rast('NETCDF:QC__checks/Benchmark_N2O/RICE_EF_MEAN_CI.NC:N2O emission factor') 
EF_maize <- rast('QC__checks/Benchmark_N2O/MAIZE_EF_MEAN_CI.NC')[[1]] #subset to get mean
EF_wheat <- rast('QC__checks/Benchmark_N2O/WHEAT_EF_MEAN_CI.NC')[[1]]

EF_rice <- flip(t(EF_rice), direction = "horizontal")
EF_maize <- flip(t(EF_maize), direction = "horizontal")
EF_wheat <- flip(t(EF_wheat), direction = "horizontal")

ext(EF_rice) <- c(-180, 180, -90, 90)
ext(EF_maize) <- c(-180, 180, -90, 90)
ext(EF_wheat) <- c(-180, 180, -90, 90)

EF_rice <- ifel(is.na(EF_rice), as.numeric(global(EF_rice, mean, na.rm = T)), EF_rice)
EF_maize <- ifel(is.na(EF_maize), as.numeric(global(EF_maize, mean, na.rm = T)), EF_maize)
EF_wheat <- ifel(is.na(EF_wheat), as.numeric(global(EF_wheat, mean, na.rm = T)), EF_wheat)

#######################
####Model

###Emissions
##Baseline emissions for soil management emissions
##Parameters

N2O <- read.csv('EmissionFactors/N2O.csv', stringsAsFactors = F)	

cropArea_rice_irr <- rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Rice_Irrigated.tif')*1000
cropArea_rice_rain <- rast('Cropping/GAEZ2015/GAEZ._2015_crop_harvest_area/GAEZAct2015_HarvArea_Rice_Rainfed.tif')*1000

##Intermediate calcs
#! Simplified by assuming all fert is ammonium nitrate
#Rice int calcs
#! Add rice specific emission factor
N2O_Ndirect_rice <- sum(ifel(climate_code == 1, input_N_rice * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_rice * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_rice * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_rice + N_cover_rice) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_rice + N_cover_rice) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_rice + N_cover_rice) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)

N2O_Ndirect_riceFlood <- sum((input_N_rice*(cropArea_rice_irr/sum(cropArea_rice_irr, cropArea_rice_rain, na.rm = T)) * N2O$Direct_emissions[N2O$var == "Flood"]),
                             ((soil_manure_etc_rice/50)*cropArea_rice_irr/sum(cropArea_rice_irr, cropArea_rice_rain, na.rm = T) * N2O$Direct_emissions[N2O$var == "Flood"]),
                             ((N_residue_rice + N_cover_rice)*cropArea_rice_irr/sum(cropArea_rice_irr, cropArea_rice_rain, na.rm = T) * N2O$Direct_emissions[N2O$var == "Flood"]),
                             na.rm = T)

N2O_Ndirect_riceDrain <- sum((input_N_rice*(cropArea_rice_irr/sum(cropArea_rice_irr, cropArea_rice_rain, na.rm = T)) * N2O$Direct_emissions[N2O$var == "Drain"]),
                             ((soil_manure_etc_rice/50)*cropArea_rice_irr/sum(cropArea_rice_irr, cropArea_rice_rain, na.rm = T) * N2O$Direct_emissions[N2O$var == "Drain"]),
                             ((N_residue_rice + N_cover_rice)*cropArea_rice_irr/sum(cropArea_rice_irr, cropArea_rice_rain, na.rm = T) * N2O$Direct_emissions[N2O$var == "Drain"]),
                             na.rm = T)

N2O_volat_rice <- sum(ifel(climate_code == 1, input_N_rice * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_rice * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_rice * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_rice + N_cover_rice) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_rice + N_cover_rice) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_rice + N_cover_rice) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)

N2O_leach_rice <- sum(ifel(climate_code == 1, input_N_rice * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_rice * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_rice * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_rice + N_cover_rice) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_rice + N_cover_rice) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_rice + N_cover_rice) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)

#Maize int calcs 
N2O_Ndirect_maize <- sum(ifel(climate_code == 1, input_N_maize * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_maize * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_maize * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_maize/50) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_maize + N_cover_maize) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_maize + N_cover_maize) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_maize + N_cover_maize) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)

N2O_volat_maize <- sum(ifel(climate_code == 1, input_N_maize * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_maize * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_maize * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_maize/50) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_maize + N_cover_maize) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_maize + N_cover_maize) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_maize + N_cover_maize) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)

N2O_leach_maize <- sum(ifel(climate_code == 1, input_N_maize * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_maize * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_maize * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_maize/50) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_maize + N_cover_maize) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_maize + N_cover_maize) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_maize + N_cover_maize) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)

#Wheat int calcs
N2O_Ndirect_wheat <- sum(ifel(climate_code == 1, input_N_wheat * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_wheat * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_wheat * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_wheat/50) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_wheat + N_cover_wheat) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_wheat + N_cover_wheat) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_wheat + N_cover_wheat) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)

N2O_volat_wheat <- sum(ifel(climate_code == 1, input_N_wheat * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_wheat * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_wheat * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_wheat/50) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_wheat + N_cover_wheat) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_wheat + N_cover_wheat) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_wheat + N_cover_wheat) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)

N2O_leach_wheat <- sum(ifel(climate_code == 1, input_N_wheat * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_wheat * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_wheat * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])),
  ifel(climate_code == 1, (soil_manure_etc_wheat/50) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])),
  ifel(climate_code == 1, (N_residue_wheat + N_cover_wheat) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_wheat + N_cover_wheat) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_wheat + N_cover_wheat) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Crop residues"])),
  na.rm = T)


##Emission calcs
#crops
N2O_rice_Hillier <- sum(N2O_Ndirect_rice, N2O_Ndirect_riceFlood, N2O_Ndirect_riceDrain, N2O_volat_rice, na.rm = T) #N2O_leach_rice
N2O_maize_Hillier <- sum(N2O_Ndirect_maize, N2O_volat_maize, na.rm = T) #N2O_leach_maize
N2O_wheat_Hillier <- sum(N2O_Ndirect_wheat, N2O_volat_wheat, na.rm = T)# N2O_leach_wheat

N2O_rice_Cui <- sum(input_N_rice, (soil_manure_etc_rice/50), N_residue_rice, N_cover_rice, na.rm = T) * (EF_rice/100) * (44/28) #44/28 = conversion from N2O-N to N2O
N2O_maize_Cui <- sum(input_N_maize, (soil_manure_etc_rice/50), N_residue_maize, N_cover_maize, na.rm = T) * (EF_maize/100) * (44/28) #Divide EF by 100 to convert % to fraction
N2O_wheat_Cui <- sum(input_N_wheat, (soil_manure_etc_rice/50), N_residue_wheat, N_cover_wheat, na.rm = T) * (EF_wheat/100) * (44/28)

plot(N2O_rice_Hillier - N2O_rice_Cui)
plot(N2O_maize_Hillier - N2O_maize_Cui)
plot(N2O_wheat_Hillier - N2O_wheat_Cui)

writeRaster(N2O_rice_Hillier, 'Hillier_rice.tiff', overwrite = T)
writeRaster(N2O_maize_Hillier, 'Hillier_maize.tiff', overwrite = T) 
writeRaster(N2O_wheat_Hillier, 'Hillier_wheat.tiff', overwrite = T)

writeRaster(N2O_rice_Cui/100, 'Cui_rice.tiff', overwrite = T) 
writeRaster(N2O_maize_Cui/100, 'Cui_maize.tiff', overwrite = T) 
writeRaster(N2O_wheat_Cui/100, 'Cui_wheat.tiff', overwrite = T) 
