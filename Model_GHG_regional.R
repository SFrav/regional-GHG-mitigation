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
  input_N_crop <- rast("Cropping/i_N_crop.tiff")
  input_P_rice <- rast("Cropping/i_P_rice.tiff")
  input_P_crop <- rast("Cropping/i_P_crop.tiff")
  soil_manure_etc_rice <- rast("Cropping/i_manure_rice.tiff")
  soil_manure_etc_crop <- rast("Cropping/i_manure_crop.tiff")
  input_fuel_l_rice <- rast("Cropping/i_fuel_rice.tiff")
  input_fuel_l_crop <- rast("Cropping/i_fuel_crop.tiff")
  input_pest_kg_rice <- rast("Cropping/i_pest_rice.tiff")
  input_pest_kg_crop <- rast("Cropping/i_pest_crop.tiff")
  soil_till_rice <- rast("Cropping/i_till_rice.tiff")
  soil_till_crop <- rast("Cropping/i_till_crop.tiff")
  N_residue_rice <- rast("Cropping/i_resN_rice.tiff")
  N_residue_crop <- rast("Cropping/i_resN_crop.tiff")
  N_cover_rice <- rast("Cropping/i_covN_rice.tiff")
  N_cover_crop <- rast("Cropping/i_covN_crop.tiff")
  soil_residue_rice <- rast("Cropping/i_res_rice.tiff")
  soil_residue_crop <- rast("Cropping/i_res_crop.tiff")
  soil_cover_rice <- rast("Cropping/i_cov_rice.tiff")
  soil_cover_crop <- rast("Cropping/i_cov_crop.tiff")
  
  
} else{ #end crop imports
##Crop area - Use Maggi or Waha? Maggi uses Monfreda, C., Ramankutty 2008. Farming the planet: 2. Waha uses Portmann et al., 2010 MIRCA2000
#Monfreda ha -#@ sum of all is less than the grid cell area - 8100 ha
  cropArea <- c(rast('Cropping/Pesticides/CROPS/NC/Rice_HarvestedArea.nc'),
              #rast('Cropping/Pesticides/CROPS/NC/Alfalfa_HarvestedArea.nc'), #Feed
              rast('Cropping/Pesticides/CROPS/NC/Corn_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/Cotton_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/OrcGra_HarvestedArea.nc'), 
              #rast('Cropping/Pesticides/CROPS/NC/PasHay_HarvestedArea.nc'), #Pasture and hay not included
              rast('Cropping/Pesticides/CROPS/NC/Soybean_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/VegFru_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/Wheat_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/Other_HarvestedArea.nc'))

  names(cropArea) <- c("rice", "corn", "cotton", "orcGra", "soybean", "vegFru", "wheat", "other")

  cropArea_total <- sum(cropArea, na.rm = T)
  cropArea_rice <- sum(cropArea[[1]], na.rm = T)
  cropArea_other <- sum(cropArea[[2:8]], na.rm = T)


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
  input_N_crop <- (input_N_m / 1000) * cropArea_other #Convert km2 to ha
  
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
  input_P_crop <- (input_P_ha / 1000) * cropArea_other #Convert m2 to ha

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
  N_manure_etc_crop <- (soil_manure_etc/100) * cropArea_other #Convert km2 to ha
  
  soil_manure_etc_rice <- N_manure_etc_rice * 50 #! Check with Aimable Uwizeye on N content and manure type globally
  soil_manure_etc_crop <- N_manure_etc_crop * 50
  
  rm(m, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)
  

#Pesticides - application rate (APR) = kg/ha per year
#HHHLLL. So take the first band
  pest <- rast("Cropping/Pesticides/APR/NC/APR_Alfalfa_2,4-d_H_L.nc")[[1]]
  for(i in 2:length(list.files("Cropping/Pesticides/APR/NC/"))){
    pest <- c(pest, rast(list.files("Cropping/Pesticides/APR/NC/", full.names = T)[i])[[1]])
  
  }
  names(pest) <- gsub(".nc", "", list.files("Cropping/Pesticides/APR/NC/"))
  

  pest_rice <- max(pest[[grep("Rice", names(pest))]], na.rm = T) ##!These should all be sum. Can't remove neg values because of memory issues
  #pest_alfalfa <- max(pest[[grep("Alfalfa", names(pest))]], na.rm = T)
  pest_corn <- max(pest[[grep("Corn", names(pest))]], na.rm = T)
  pest_cotton <- max(pest[[grep("Cotton", names(pest))]], na.rm = T)
  pest_orcGra <- max(pest[[grep("OrcGra", names(pest))]], na.rm = T)
  #pest_pasHay <- sum(pest[[grep("PasHay", names(pest))]], na.rm = T) #! Not used
  pest_soybean <- max(pest[[grep("Soybean", names(pest))]], na.rm = T)
  pest_vegFru <- max(pest[[grep("VegFru", names(pest))]], na.rm = T)
  pest_wheat <- max(pest[[grep("Wheat", names(pest))]], na.rm = T)
  
  ###!Temporary until memory issue sorted
  pest_rice <- classify(pest_rice, cbind(-Inf, 0, 0)) 
  #pest_alfalfa <- classify(pest_alfalfa, cbind(-Inf, 0, 0))
  pest_corn <- classify(pest_corn, cbind(-Inf, 0, 0))
  pest_cotton <- classify(pest_cotton, cbind(-Inf, 0, 0))
  pest_orcGra <- classify(pest_orcGra, cbind(-Inf, 0, 0))
  #pest_pasHay <- sum(pest[[grep("PasHay", names(pest))]], na.rm = T) #! Not used
  pest_soybean <- classify(pest_soybean, cbind(-Inf, 0, 0))
  pest_vegFru <- classify(pest_vegFru, cbind(-Inf, 0, 0))
  pest_wheat <- classify(pest_wheat, cbind(-Inf, 0, 0))
  ###
  
  pest_rice <- resample(pest_rice, cropArea) # Minor difference
  #pest_alfalfa <- resample(pest_alfalfa, cropArea) # Minor difference
  pest_corn <- resample(pest_corn, cropArea) # Minor difference
  pest_cotton <- resample(pest_cotton, cropArea) # Minor difference
  pest_orcGra <- resample(pest_orcGra, cropArea) # Minor difference
  pest_soybean <- resample(pest_soybean, cropArea) # Minor difference
  pest_vegFru <- resample(pest_vegFru, cropArea) # Minor difference
  pest_wheat <- resample(pest_wheat, cropArea) # Minor difference
  
  pest_rice_kg <- cropArea$rice * pest_rice
  #pest_alf_kg <- cropArea$alfalfa * pest_alfalfa
  pest_corn_kg <- cropArea$corn * pest_corn
  pest_cotton_kg <- cropArea$cotton * pest_cotton
  pest_orcGra_kg <- cropArea$orcGra * pest_orcGra
  pest_soy_kg <- cropArea$soybean * pest_soybean
  pest_veg_kg <- cropArea$vegFru * pest_vegFru
  pest_wheat_kg <- cropArea$wheat * pest_wheat
  pest_other_kg <- cropArea$other * pest_corn # No application rate for 'other'
  
  input_pest_kg_rice <- pest_rice_kg
  input_pest_kg_crop <- sum(pest_corn_kg, pest_cotton_kg, pest_orcGra_kg, pest_soy_kg, pest_veg_kg, pest_veg_kg, pest_wheat_kg, pest_other_kg, na.rm = T)
  #input_pest_kg <- resample(input_pest_kg, MAT)
  
  input_fuel_l_rice <- input_pest_kg_rice #!!! Temporary to build model
  input_fuel_l_crop <- input_pest_kg_crop #!!! Temporary to build model
  
  
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
  #soil_res_alf_kg <- cropArea$alfalfa * ifel(soil_till[["whea_till"]] == 4, 5000, 500) # alfalfa = feed
  soil_res_corn_kg <- cropArea$corn * ifel(soil_till[["maiz_till"]] == 4, 5000, 500)
  soil_res_cotton_kg <- cropArea$cotton * ifel(soil_till[["cott_till"]] == 4, 5000, 500)
  soil_res_orcGra_kg <- cropArea$orcGra * ifel(soil_till[["pmil_till"]] == 4, 5000, 500) #using pearl millet
  soil_res_soy_kg <- cropArea$soybean * ifel(soil_till[["soyb_till"]] == 4, 5000, 500)
  soil_res_veg_kg <- cropArea$vegFru * ifel(soil_till[["vege_till"]] == 4, 5000, 500) #Overestimate for orchards
  soil_res_wheat_kg <- cropArea$wheat * ifel(soil_till[["whea_till"]] == 4, 5000, 500)
  soil_res_other_kg <- cropArea$other * ifel(soil_till[["rest_till"]] == 4, 5000, 500) #!rest = other?
  
  
  soil_residue_crop <- sum(eval(parse(text = ls(pattern = "^soil_res_"))), na.rm = T)
  
  
  #!@ Adjust residue amounts based on crop
  soil_cover_rice <- cropArea$rice * ifel(soil_till[["rice_till"]] == 4, 3000, 0) # ha * kg ha-1
  #soil_cov_alf_kg <- cropArea$alfalfa * ifel(soil_till[["whea_till"]] == 4, 3000, 0) # alfalfa = feed
  soil_cov_corn_kg <- cropArea$corn * ifel(soil_till[["maiz_till"]] == 4, 3000, 0)
  soil_cov_cotton_kg <- cropArea$cotton * ifel(soil_till[["cott_till"]] == 4, 3000, 0)
  soil_cov_orcGra_kg <- cropArea$orcGra * ifel(soil_till[["pmil_till"]] == 4, 3000, 0) #using pearl millet
  soil_cov_soy_kg <- cropArea$soybean * ifel(soil_till[["soyb_till"]] == 4, 3000, 0)
  soil_cov_veg_kg <- cropArea$vegFru * ifel(soil_till[["vege_till"]] == 4, 3000, 0) #Overestimate for orchards
  soil_cov_wheat_kg <- cropArea$wheat * ifel(soil_till[["whea_till"]] == 4, 3000, 0)
  soil_cov_other_kg <- cropArea$other * ifel(soil_till[["rest_till"]] == 4, 3000, 0) #!rest = other?
  
  soil_cover_crop <- sum(eval(parse(text = ls(pattern = "^soil_cov_"))), na.rm = T)
  
  N_residue_rice <- soil_residue_rice * 0.02 #! from OM_added tab of Jon's model. Marked for 'fixing'!
  N_residue_crop <- soil_residue_crop * 0.02 #! from OM_added tab of Jon's model. Marked for 'fixing'!
  N_cover_rice <- soil_cover_rice * 0.02 #! from OM_added tab of Jon's model. Marked for 'fixing'!
  N_cover_crop <- soil_cover_crop * 0.02 #! from OM_added tab of Jon's model. Marked for 'fixing'!
  
  soil_till_rice <- soil_till[["rice_till"]]
  soil_till_crop <- soil_till[["maiz_till"]] #@Simplification
  

  rm(cropArea)
  
  writeRaster(input_N_rice, "Cropping/i_N_rice.tiff", overwrite = T)
  writeRaster(input_N_crop, "Cropping/i_N_crop.tiff", overwrite = T)
  writeRaster(input_P_rice, "Cropping/i_P_rice.tiff", overwrite = T)
  writeRaster(input_P_crop, "Cropping/i_P_crop.tiff", overwrite = T)
  writeRaster(soil_manure_etc_rice, "Cropping/i_manure_rice.tiff", overwrite = T)
  writeRaster(soil_manure_etc_crop, "Cropping/i_manure_crop.tiff", overwrite = T)
  writeRaster(input_fuel_l_rice, "Cropping/i_fuel_rice.tiff", overwrite = T)
  writeRaster(input_fuel_l_crop, "Cropping/i_fuel_crop.tiff", overwrite = T)
  writeRaster(input_pest_kg_rice, "Cropping/i_pest_rice.tiff", overwrite = T)
  writeRaster(input_pest_kg_crop, "Cropping/i_pest_crop.tiff", overwrite = T)
  writeRaster(soil_till_rice, "Cropping/i_till_rice.tiff", overwrite = T)
  writeRaster(soil_till_crop, "Cropping/i_till_crop.tiff", overwrite = T)
  writeRaster(N_residue_rice, "Cropping/i_resN_rice.tiff", overwrite = T)
  writeRaster(N_residue_crop, "Cropping/i_resN_crop.tiff", overwrite = T)
  writeRaster(N_cover_rice, "Cropping/i_covN_rice.tiff", overwrite = T)
  writeRaster(N_cover_crop, "Cropping/i_covN_crop.tiff", overwrite = T)
  writeRaster(soil_residue_rice, "Cropping/i_res_rice.tiff", overwrite = T)
  writeRaster(soil_residue_crop, "Cropping/i_res_crop.tiff", overwrite = T)
  writeRaster(soil_cover_rice, "Cropping/i_cov_rice.tiff", overwrite = T)
  writeRaster(soil_cover_crop, "Cropping/i_cov_crop.tiff", overwrite = T)
  
  rm(list = ls(pattern = "^soil_res_"))
  rm(list = ls(pattern = "^soil_cov_"))
} #End crop input processing



###Livestock
##Wolf et al. 2017 https://daac.ornl.gov/CMS/guides/CMS_Global_Livestock_CH4_CO2.html - in Mg. Representative of 2000-2013
int_co2_carbon <- rast("Livestock/Global_Livestock_Gases_Carbon_Flux_Per_Gridcell_2000-2013.nc4", "totCO2_C") #Comprised of respiration (rsp) and manure management (mm)
int_co2_carbon <- mean(int_co2_carbon[[1:10]], na.rm = T)*1000 # convert Mg to kg and take average
CO2eq_lvst_C <- int_co2_carbon * (16.04246/12.0111) #CO2 is 44.0095 g/mol, C is 12.0111 g/mol = 27.27% Carbon

int_ch4_carbon <- rast("Livestock/Global_Livestock_Gases_Carbon_Flux_Per_Gridcell_2000-2013.nc4", "totCH4_C") #Comprised of enteric fermentation (ef) and manure management (mm)
int_ch4_carbon <- mean(int_ch4_carbon[[1:10]], na.rm = T)*1000 # convert Mg to kg and take average
int_ch4 <- int_ch4_carbon * (16.04246/12.0111) #CH4 is 16.04246 g/mol, C in 12.0111 g/mol = 75% Carbon
CO2eq_lvst_CH4 <- int_ch4 * 28 # AR5 GWP100
#global(int_ch4, fun = "sum", na.rm = T) Wolf et al. estimate 119 Tg of methane in 2011




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

N2O_Ndirect_crop <- ifel(climate_code == 1, input_N_crop * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_crop + N_cover_crop) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_crop + N_cover_crop) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_crop + N_cover_crop) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

N2O_volat_crop <- ifel(climate_code == 1, input_N_crop * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_crop + N_cover_crop) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_crop + N_cover_crop) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_crop + N_cover_crop) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

N2O_leach_crop <- ifel(climate_code == 1, input_N_crop * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_crop + N_cover_crop) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_crop + N_cover_crop) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_crop + N_cover_crop) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 



#crops (excluding rice)
CO2eq_direct_crop <- N2O_Ndirect_crop * effN2O
CO2eq_indirect_crop <- (N2O_volat_crop + N2O_leach_crop) * effN2O
CO2eq_fuel_crop <- input_fuel_l_crop * effCO2_l_diesel
CO2eq_embed_fert_N_crop <- input_N_crop * brentrupEmbedded[brentrupEmbedded$compound == "Ammonium Nitrate", "Europe"] / 0.335 #!!! Europe
CO2eq_embed_fert_P_crop <- input_P_crop * brentrupEmbedded[brentrupEmbedded$compound == "TSP", "Europe"] / 0.48 #!!! Europe
CO2eq_embed_fert_K_crop <- CO2eq_embed_fert_P_crop #!!! temporary
pesticides_crop <- input_pest_kg_crop * effCO2_l_pest
CO2eq_SOC_manure_crop <- -(0.038 * (soil_manure_etc_crop) - 0.0538 * 1000) * 44/12
CO2eq_SOC_residue_crop <- -(0.115 * soil_residue_crop + 0.192 * 1000) * 44/12
CO2eq_SOC_cover_crop <- -(0.038 * soil_cover_crop - 0.0538 * 1000) * 44/12


CO2eq_crop <- sum(CO2eq_direct_crop, CO2eq_indirect_crop, CO2eq_fuel_crop, CO2eq_embed_fert_N_crop, CO2eq_embed_fert_P_crop, CO2eq_embed_fert_K_crop, pesticides_crop, CO2eq_SOC_manure_crop, CO2eq_SOC_residue_crop, CO2eq_SOC_cover_crop, na.rm = T)
CO2eq_lvst <- sum(CO2eq_lvst_C, CO2eq_lvst_CH4, na.rm = T)
CO2eq_lvst <- resample(CO2eq_lvst, CO2eq_crop)*((0.08335263/0.05)^2) # resample and weight to "sum" pixels. https://gis.stackexchange.com/questions/317129/rescale-raster-to-custom-resolution-using-sum-as-the-function-to-aggregate

CO2eq_total <- sum(CO2eq_rice, CO2eq_crop, CO2eq_lvst, na.rm = T) ##@ check against published figures at pixel and national level

#rm(list = setdiff(ls(), c("CO2eq_rice", "CO2eq_crop", "CO2eq_lvst", "CO2eq_total")))

##############
###Mitigation
##Crop##

##Agroforestry

##Fuel
writeRaster(CO2eq_fuel_rice, 'Mitigation/mit_crop_other_energy_rice.tiff', overwrite = T)
writeRaster(CO2eq_fuel_crop, 'Mitigation/mit_crop_other_energy_crop.tiff', overwrite = T)

##N inhibitors

mit_crop_soil_inhib_crop <- ifel(input_N_crop > 0, 1*0.38*CO2eq_direct_crop, 1*0.24*CO2eq_direct_crop)

writeRaster(mit_crop_soil_inhib_crop, 'Mitigation/mit_crop_soil_inhib_crop.tiff', overwrite = T)

##Residue incorporation
#HI <- rast("Climatic/cli_HI.tiff")
cropArea <- c(rast('Cropping/Pesticides/CROPS/NC/Rice_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/Alfalfa_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/Corn_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/Cotton_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/OrcGra_HarvestedArea.nc'), 
              #rast('Cropping/Pesticides/CROPS/NC/PasHay_HarvestedArea.nc'), #Pasture and hay not included
              rast('Cropping/Pesticides/CROPS/NC/Soybean_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/VegFru_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/Wheat_HarvestedArea.nc'),
              rast('Cropping/Pesticides/CROPS/NC/Other_HarvestedArea.nc'))

names(cropArea) <- c("rice", "alfalfa", "corn", "cotton", "orcGra", "soybean", "vegFru", "wheat", "other")

cropArea_total <- sum(cropArea, na.rm = T)
cropArea_other <- sum(cropArea[[2:9]], na.rm = T)


#!@ Adjust residue amounts based on crop
soil_res_alf_kg <- cropArea$alfalfa * 5000 # No alfalfa
soil_res_corn_kg <- cropArea$corn * 5000
soil_res_cotton_kg <- cropArea$cotton * 5000
soil_res_orcGra_kg <- cropArea$orcGra * 5000 
soil_res_soy_kg <- cropArea$soybean * 5000
soil_res_veg_kg <- cropArea$vegFru * 5000 #Overestimate for orchards
soil_res_wheat_kg <- cropArea$wheat * 5000
soil_res_other_kg <- cropArea$other * 5000 #!rest = other?


soil_residue_crop <- sum(eval(parse(text = ls(pattern = "^soil_res_"))), na.rm = T)


N_residue_crop <- soil_residue_crop * 0.02 #! from OM_added tab of Jon's model. Marked for 'fixing'!


N2O_Ndirect_crop <- ifel(climate_code == 1, input_N_crop * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_crop) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_crop) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_crop) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

N2O_volat_crop <- ifel(climate_code == 1, input_N_crop * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_crop) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_crop) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_crop) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

N2O_leach_crop <- ifel(climate_code == 1, input_N_crop * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_residue_crop) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_residue_crop) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_residue_crop) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 



#Reduction in mitigation due to increase in N2O
mitCO2eq_SOC_residue_rice <- -(0.115 * soil_residue_rice + 0.192 * 1000) * (44.0095/12.0111)


#Increase in mitigation due to sequestration

mitCO2eq_direct_crop <- N2O_Ndirect_crop * effN2O
mitCO2eq_indirect_crop <- (N2O_volat_crop + N2O_leach_crop) * effN2O

#Reduction in mitigation due to increase in N2O
mitCO2eq_N_crop <- sum(CO2eq_direct_crop, CO2eq_indirect_crop, na.rm = T) - sum(mitCO2eq_direct_crop, mitCO2eq_indirect_crop, na.rm = T)

mitCO2eq_SOC_residue_crop <- -(0.115 * soil_residue_crop + 0.192 * 1000) * (44.0095/12.0111)

#Increase in mitigation due to sequestration
mitCO2eq_SOC_crop <- abs(mitCO2eq_SOC_residue_crop) - abs(CO2eq_SOC_residue_crop)

mitCO2eq_SOC_N_crop <- sum(mitCO2eq_N_crop, mitCO2eq_SOC_crop, na.rm = T)

writeRaster(mitCO2eq_SOC_N_crop, 'Mitigation/mitCO2eq_SOC_N_residue_crop.tiff', overwrite = T)

## cover cropping
#!@ Adjust residue amounts based on crop
soil_cov_alf_kg <- cropArea$alfalfa * 3000 # No alfalfa
soil_cov_corn_kg <- cropArea$corn * 3000
soil_cov_cotton_kg <- cropArea$cotton * 3000
soil_cov_orcGra_kg <- cropArea$orcGra * 3000 #using pearl millet
soil_cov_soy_kg <- cropArea$soybean * 3000
soil_cov_veg_kg <- cropArea$vegFru * 3000 #Overestimate for orchards
soil_cov_wheat_kg <- cropArea$wheat * 3000
soil_cov_other_kg <- cropArea$other * 3000 #!rest = other?

soil_cover_crop <- sum(eval(parse(text = ls(pattern = "^soil_cov_"))), na.rm = T)

N_cover_crop <- soil_cover_crop * 0.02 #! from OM_added tab of Jon's model. Marked for 'fixing'!


N2O_Ndirect_crop <- ifel(climate_code == 1, input_N_crop * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_cover_crop) * N2O$Direct_emissions[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_cover_crop) * N2O$Direct_emissions[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_cover_crop) * N2O$Direct_emissions[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

N2O_volat_crop <- ifel(climate_code == 1, input_N_crop * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_cover_crop) * N2O$Factor_volatised[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_cover_crop) * N2O$Factor_volatised[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_cover_crop) * N2O$Factor_volatised[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 

N2O_leach_crop <- ifel(climate_code == 1, input_N_crop * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Ammonium Nitrate"], ifel(climate_code == 2, input_N_crop * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Ammonium Nitrate"], input_N_crop * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Ammonium Nitrate"])) +
  ifel(climate_code == 1, (soil_manure_etc_crop/50) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Manure/compost, etc"], ifel(climate_code == 2, (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Manure/compost, etc"], (soil_manure_etc_rice/50) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Manure/compost, etc"])) +
  ifel(climate_code == 1, (N_cover_crop) * N2O$Factor_leached[N2O$Climate == "wet" & N2O$var == "Crop residues"], ifel(climate_code == 2, (N_cover_crop) * N2O$Factor_leached[N2O$Climate == "moist" & N2O$var == "Crop residues"], (N_cover_crop) * N2O$Factor_leached[N2O$Climate == "dry" & N2O$var == "Crop residues"])) 


  
mitCO2eq_direct_crop <- N2O_Ndirect_crop * effN2O
mitCO2eq_indirect_crop <- (N2O_volat_crop + N2O_leach_crop) * effN2O

#Reduction in mitigation due to increase in N2O
mitCO2eq_N_crop <- sum(CO2eq_direct_crop, CO2eq_indirect_crop, na.rm = T) - sum(mitCO2eq_direct_crop, mitCO2eq_indirect_crop, na.rm = T)

mitCO2eq_SOC_cover_crop <- -(0.038 * soil_cover_crop - 0.0538 * 1000) * (44.0095/12.0111)

#Increase in mitigation due to sequestration
mitCO2eq_SOC_crop <- abs(mitCO2eq_SOC_cover_crop) - abs(CO2eq_SOC_cover_crop)

mitCO2eq_SOC_N_crop <- sum(mitCO2eq_N_crop, mitCO2eq_SOC_crop, na.rm = T)

writeRaster(mitCO2eq_SOC_N_crop, 'Mitigation/mitCO2eq_SOC_N_cover_crop.tiff', overwrite = T)


##Tillage
soil_till_crop <- rast("Cropping/i_till_crop.tiff") #1 = conventional annual tillage;2 = traditional annual tillage;3 = reduced tillage;4 = Conservation Agriculture;5 = rotational tillage;6 = traditional rotational tillage;7 = Scenario Conservation Agriculture area
mit_rice_soil_notill <- ifel(HI <= 89, 0.4 * -1000 * (44.0095/12.0111) * cropArea_rice, 0.19 * -1000 * (44.0095/12.0111) * cropArea_rice)

mit_crop_soil_notill <- ifel(HI <= 89, 0.4 * -1000 * (44.0095/12.0111) * cropArea_other, 0.19 * -1000 * (44.0095/12.0111) * cropArea_other)
mitCO2eq_crop_soil_notill <- ifel(soil_till_crop != 4, mit_crop_soil_notill, 0)

writeRaster(mitCO2eq_crop_soil_notill, 'Mitigation/mitCO2eq_notill_crop.tiff', overwrite = T)




##Livestock##
##Agroforestry
mit_lvst_agro <- st_read("Agroforestry/mit_agroforestry_past_biomass_Mg.geojson")

#Grassland sequestration
mit_lvst_soil_grazing <- rast('Livestock/Mitigation/Soil sequestration_pasture rangelands_modified.tif')
crs(mit_lvst_soil_grazing) <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #esri:54030
mit_lvst_soil_grazing <- ifel(mit_lvst_soil_grazing[[1]] > 120 & mit_lvst_soil_grazing[[1]] < 135 & mit_lvst_soil_grazing[[2]] > 120 & mit_lvst_soil_grazing[[2]] < 135 & mit_lvst_soil_grazing[[3]] > 120 & mit_lvst_soil_grazing[[3]] < 135, NA, ifel(mit_lvst_soil_grazing[[1]] > 240 & mit_lvst_soil_grazing[[2]] > 240 & mit_lvst_soil_grazing[[3]] > 240, NA, ifel(mit_lvst_soil_grazing[[1]] == mit_lvst_soil_grazing[[2]] & mit_lvst_soil_grazing[[2]] == mit_lvst_soil_grazing[[3]], NA, sqrt((mit_lvst_soil_grazing[[1]]**2)+(mit_lvst_soil_grazing[[2]]**2)+(mit_lvst_soil_grazing[[3]]*2))/3))) #Brightness Index
mit_lvst_soil_grazing <- lapp(mit_lvst_soil_grazing, fun = function(x){5000 + (x - min(x, na.rm = T)) * ((0 - 5000) / (max(x, na.rm = T) - min(x, na.rm = T)))}) #Rescale and invert (inverted by (0, 5000), instead of (5000, 0))
#epsg:4326
mit_lvst_soil_grazing <- project(mit_lvst_soil_grazing, "epsg:4326", method = "near") #From Robinson to WGS84
writeRaster(mit_lvst_soil_grazing, 'Mitigation/mit_lvst_soil_grazing.tiff', overwrite = T)

##Grassland legume
mit_lvst_soil_legume <- rast('Livestock/Mitigation/Legume sowing_modified.tif')
crs(mit_lvst_soil_legume) <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #esri:54030
mit_lvst_soil_legume <- ifel(mit_lvst_soil_legume[[1]] > 120 & mit_lvst_soil_legume[[1]] < 140 & mit_lvst_soil_legume[[2]] > 120 & mit_lvst_soil_legume[[2]] < 140 & mit_lvst_soil_legume[[3]] > 50 & mit_lvst_soil_legume[[3]] < 140, NA, ifel(mit_lvst_soil_legume[[1]] > 200 & mit_lvst_soil_legume[[2]] > 200 & mit_lvst_soil_legume[[3]] > 200, NA, ifel(mit_lvst_soil_legume[[1]] == mit_lvst_soil_legume[[2]] & mit_lvst_soil_legume[[2]] == mit_lvst_soil_legume[[3]], NA, (mit_lvst_soil_legume[[2]] - mit_lvst_soil_legume[[1]]) / (mit_lvst_soil_legume[[2]] + mit_lvst_soil_legume[[1]])))) #Brightness Index
mit_lvst_soil_legume <- lapp(mit_lvst_soil_legume, fun = function(x){0 + (x - min(x, na.rm = T)) * ((1500 - 0) / (max(x, na.rm = T) - min(x, na.rm = T)))}) #Rescale
mit_lvst_soil_legume <- project(mit_lvst_soil_legume, "epsg:4326", method = "near") #From Robinson to WGS84

writeRaster(mit_lvst_soil_legume, 'Mitigation/mit_lvst_soil_legume.tiff', overwrite = T)
