library(terra)

##Emissions m2
wolf_lvstCH4_carbon_m2 <- rast("Wolf_Global_Livestock_Gases_Carbon_Flux_Per_Area_2000-2013.nc4")
wolf_lvstCH4_carbon_m2 <- subset(wolf_lvstCH4_carbon_m2, c("totCH4_C_10", "totCH4_C_11", "totCH4_C_12", "totCH4_C_13"))
wolf_lvstCH4_mean_m2 <- mean(wolf_lvstCH4_carbon_m2, na.rm = T)/1000 # convert g to kg and take mean of 2010-2013
wolf_lvstCH4_mean_m2 <- wolf_lvstCH4_mean_m2 * (44.0095/12.0111) #CH4 is 16.04246 g/mol, C is 12.0111 g/mol = 75% Carbon
tmpArea <- terra::cellSize(wolf_lvstCH4_mean_m2, unit = "m") # area of 0.05 deg gridcells
wolf_lvstCH4_2010_2013_m2 <- wolf_lvstCH4_mean_m2 * tmpArea

##Emissions gridcell
wolf_lvstCH4_carbon_grid <- rast("Wolf_Global_Livestock_Gases_Carbon_Flux_Per_Gridcell_2000-2013.nc4", "totCH4_C") #Comprised of enteric fermentation (ef) and manure management (mm)
wolf_lvstCH4_carbon_grid <- subset(wolf_lvstCH4_carbon_grid, c("totCH4_C_10", "totCH4_C_11", "totCH4_C_12", "totCH4_C_13"))
wolf_lvstCH4_mean_grid <- mean(wolf_lvstCH4_carbon_grid, na.rm = T)*1000 # convert Mg to kg and take mean of 2010-2013
wolf_lvstCH4_2010_2013_grid <- wolf_lvstCH4_mean_grid * (44.0095/12.0111) #CH4 is 16.04246 g/mol, C is 12.0111 g/mol = 75% Carbon

plot(wolf_lvstCH4_2010_2013_m2 - wolf_lvstCH4_2010_2013_grid)
