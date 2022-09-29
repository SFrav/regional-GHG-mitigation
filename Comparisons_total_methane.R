library(openxlsx)
library(dplyr)
library(tidyr)
library(terra)
#library(sf)

IPCC <- terra::vect("Mapping/IPCC_WGS_land.shp")
IPCC <- IPCC[IPCC$Type %in% c("Land", "Land-Ocean") & !(IPCC$Acronym %in% c("WAN", "EAN")),]
IPCC$area <- terra::expanse(IPCC, unit = "m")

## ESSD dataset
datESSD <- openxlsx::read.xlsx("QC__checks/Benchmark_baseline//Minx/essd_ghg_data.xlsx", sheet = 3)
datESSD <- datESSD[datESSD$sector_title == "AFOLU",]
datESSD <- datESSD[datESSD$year %in% 2000:2010,]
datESSD$CO2e <- datESSD$value * datESSD$gwp100_ar5
datESSD <- group_by(datESSD, ISO, country, region_ar6_22, year, subsector_title)
datESSD <- summarise(datESSD, CO2eYear = sum(CO2e, na.rm = T)) #Sum gasses across year, subsector and country
datESSD <- group_by(datESSD, ISO, country, region_ar6_22, subsector_title)
datESSD <- summarise(datESSD, CO2e = mean(CO2eYear, na.rm = T)) #Average CO2e bw 2000-2010 by subsector and country
datESSD$CO2e_kg <- datESSD$CO2e / 1000 #Tonnes to KG

datESSD <- group_by(datESSD, ISO)
datEsum <- summarise(datESSD, CO2eESSD = sum(CO2e_kg, na.rm = T))

#Total emissions by sub-sector
datESSD <- group_by(datESSD, subsector_title)
datEsec_sum <- summarise(datESSD, CO2eESSD = sum(CO2e_kg, na.rm = T))


##PRIMAP dataset
datPRIMAP <- read.csv('QC__checks/Benchmark_baseline/PRIMAP-hist/Guetschow-et-al-2021-PRIMAP-hist_v2.3.1_20-Sep_2021.csv')
datPRIMAP <- datPRIMAP[datPRIMAP$category..IPCC2006_PRIMAP. %in% c("3.A", "M.AG.ELV") & datPRIMAP$entity %in% c("CO2","CH4", "N2O"), ]
#mean for gas, year, subsector and gas
datPRIMAP$value <- rowSums(datPRIMAP[, c("X2000","X2001","X2002", "X2003","X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010")])/11
datPRIMAP$CO2eGas <- ifelse(datPRIMAP$entity == "CH4", datPRIMAP$value*28, ifelse(datPRIMAP$entity == "N2O", datPRIMAP$value * 265, datPRIMAP$value))
datPRIMAP <- group_by(datPRIMAP, area..ISO3., category..IPCC2006_PRIMAP.)
datPRIMAP <- summarise(datPRIMAP, CO2e_Gg = sum(CO2eGas, na.rm = T)) # in Gg CO2e


datPRIMAP <- group_by(datPRIMAP, area..ISO3.)
datPsum <- summarise(datPRIMAP, CO2ePRIMAP = sum(CO2e_Gg))

datCompare <- left_join(datEsum, datPsum, by = c("ISO" = "area..ISO3."))

plot(datCompare$CO2eESSD, datCompare$CO2ePRIMAP)
hist(datCompare$CO2eESSD / datCompare$CO2ePRIMAP)

#Total emissions by sub-sector
datPRIMAP <- group_by(datPRIMAP, category..IPCC2006_PRIMAP.)
datPRIMAPsub <- datPRIMAP[!(datPRIMAP$area..ISO3. %in% c("ANNEXI", "NONANNEXI", "AOSIS", "BASIC", "EARTH", "EU27BX", "LDC", "UMBRELLA")),]
datPsec_sum <- summarise(datPRIMAPsub, CO2ePRIMAP = sum(CO2e_Gg))




###Wolf et al. 2017 vs GOSAT livestock
wolf_lvstCH4 <- rast("QC__checks/Benchmark_baseline/Livestock/Wolf_Global_Livestock_Gases_Carbon_Flux_Per_Area_2000-2013.nc4") #Comprised of enteric fermentation (ef) and manure management (mm)
wolf_lvstCH4 <- subset(wolf_lvstCH4, c("totCH4_C_10", "totCH4_C_11", "totCH4_C_12", "totCH4_C_13"))
wolf_lvstCH4_mean <- mean(wolf_lvstCH4, na.rm = T)/1000 # convert g to kg and take mean of 2010-2013
wolf_lvstCH4_mean <- wolf_lvstCH4_mean * (16.04246/12.0111) #CH4 is 16.04246 g/mol, C in 12.0111 g/mol = 75% Carbon
tmpArea <- terra::cellSize(wolf_lvstCH4_mean, unit = "m") # area of 0.05 deg gridcells
wolf_lvstCH4_2010_2013 <- wolf_lvstCH4_mean * tmpArea

gosat_lvstCH4 <- rast('../../GOSAT_CH4_ag/GlobalInv_GOSAT_posterior_annual_Livestock_v1.nc') 
ext(gosat_lvstCH4) <- ext(-180, 180, -90, 90)
#gosat_lvstCH4 <- subset(gosat_lvstCH4, 1) * subset(gosat_lvstCH4, 2:5) #KG CH4 per gridcell in 2010
gosat_lvstCH4_mean <- mean(terra::subset(gosat_lvstCH4, 2:5), na.rm = T) #take mean of 2010-2013
gosat_lvstCH4_mean <- resample(gosat_lvstCH4_mean, wolf_lvstCH4_mean, method = "ngb")
gosat_lvstCH4_mean <- gosat_lvstCH4_mean * 31557600 # * seconds in a year
#gosat_lvstCH4_mean <- gosat_lvstCH4_mean * (44.0095/12.0111) #@ Confirmed with Yuzhong
gosat_lvstCH4_2010_2013 <- gosat_lvstCH4_mean * tmpArea

plot(gosat_lvstCH4_2010_2013 - wolf_lvstCH4_2010_2013)

IPCC$wolf <- extract(wolf_lvstCH4_2010_2013, IPCC, fun = sum, na.rm = T)[,2]
IPCC$gosat <- extract(gosat_lvstCH4_2010_2013, IPCC, fun = sum, na.rm = T)[,2]

plot(IPCC$wolf ~ IPCC$gosat)

#checks
wolf_lvstCH4_carbon_grid <- rast("Livestock/Global_Livestock_Gases_Carbon_Flux_Per_Gridcell_2000-2013.nc4") #Comprised of enteric fermentation (ef) and manure management (mm)
wolf_lvstCH4_carbon_grid <- subset(wolf_lvstCH4_carbon_grid, c("totCH4_C_10", "totCH4_C_11", "totCH4_C_12", "totCH4_C_13"))
wolf_lvstCH4_mean_grid <- mean(wolf_lvstCH4_carbon_grid, na.rm = T)*1000 # convert Mg to kg and take mean of 2010-2013
wolf_lvstCH4_2010_2013_grid <- wolf_lvstCH4_mean_grid * (16.04246/12.0111) #CH4 is 16.04246 g/mol, C in 12.0111 g/mol = 75% Carbon

gosat_lvstCH4 <- rast('../../GOSAT_CH4_ag/GlobalInv_GOSAT_posterior_annual_Livestock_v1.nc') 
ext(gosat_lvstCH4) <- ext(-180, 180, -90, 90)
gosat_lvstCH4_mean <- mean(terra::subset(gosat_lvstCH4, 2:5), na.rm = T) #take mean of 2010-2013
gosat_lvstCH4_2010_2013_tmp <- gosat_lvstCH4_mean * 31557600 # Convert seconds to annual
gosat_lvstCH4_2010_2013 <- terra::subset(gosat_lvstCH4, 1) * gosat_lvstCH4_2010_2013_tmp

global(wolf_lvstCH4_2010_2013_grid, sum, na.rm = T)*1e-9 # 2010 = 116 Tg (118 in Wolf et al. results)
global(gosat_lvstCH4_2010_2013, sum, na.rm = T)*1e-9 #123.5 Tg ... 100 Tg is resampling to 0.05 deg


### Own calc vs GOSAT for rice
riceCH4_kg_year <- rast('cropping/base_total_rice_CH4.tiff')
gosat_riceCH4 <- rast('../../GOSAT_CH4_ag/GlobalInv_GOSAT_posterior_monthly_Rice_v1.nc') 
ext(gosat_riceCH4) <- ext(-180, 180, -90, 90)
gosat_riceCH4_mean <- mean(terra::subset(gosat_riceCH4, 2:13), na.rm = T) #take mean of 2010 monthly
#gosat_riceCH4_mean <- resample(gosat_riceCH4_mean, riceCH4_kg_year, method = "ngb")
gosat_riceCH4_mean <- gosat_riceCH4_mean * 31557600 # Convert seconds to annual
#tmpArea <- terra::cellSize(riceCH4_kg_year, unit = "m") # area of gridcells
#gosat_riceCH4_2010 <- gosat_riceCH4_mean * tmpArea
gosat_riceCH4_2010 <- terra::subset(gosat_riceCH4, 1) * gosat_riceCH4_mean 

global(riceCH4_kg_year, sum , na.rm = T)
global(gosat_riceCH4_2010, sum , na.rm = T)

IPCC$own_rice <- extract(riceCH4_kg_year, IPCC, fun = sum, na.rm = T)[,2]
IPCC$gosat_rice <- extract(gosat_riceCH4_2010, IPCC, fun = sum, na.rm = T)[,2]

plot(IPCC$own_rice ~ IPCC$gosat_rice)
