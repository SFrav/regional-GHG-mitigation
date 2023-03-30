#Geoprocessing for estimating regional GHG mitigation potential for agriculture
 
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

The model is available as a monolythic script named 'Model_GHG_regional.R'. The gridded data inputs required for this model are not included in this repository

### Extended abstract

The most precise radiative forcing forecasts to-date make it clear that rapid climate change mitigation and adaptation is needed across agricultural systems globally.
Greenhouse gas emissions and mitigation potentials were calculated using global gridded datasets. Estimates of mitigation potential for inhibiting methanogenesis in enteric fermentation were updated using the most recent in vivo experimental data on feed additives. Manure management mitigation potential was calculated using geographically relevant interventions, including storage method, retention time and use of anaerobic digestion. Mitigation potential from improved manure management was calculated as a fraction of CH<sub>4</sub> emissions from existing estimates. Mitigation potential for methanogenisis inhibition and improved manure management were only estimated for ‘intensive systems’.  
Agroforestry GHG mitigation potential was taken from existing estimates. Rice CH<sub>4</sub> and N<sub>2</sub>O emissions were calculated for each cropping period. Cropland emissions were modelled for 14 specific crops (rice, maize, wheat, cotton and soybean) and three more general categories representing the remainder of arable land. 
Cropland soil mitigation potential was assessed for nitrification inhibitors, crop residue return, cover cropping and a change to no-till practices. 
Emissions were calculated by gas and aggregated to CO<sub>2</sub>-e100 using IPCC Sixth Assessment Report (AR6) conversion factors. The model was coded in R (R Development Core Team) using the Terra package.
Our global baseline estimate for CH<sub>4</sub> ranged between 21 and 53 Tg. Our estimates of N<sub>2</sub>O emissions from croplands ranged between 2.3 and 4.8 Tg. 


### Acknowledgement

This research was supported via a TRAIN@ED fellowship funded by the European Union’s Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement No 801215 and the University of Edinburgh Data-Driven Innovation programme, part of the Edinburgh and South East Scotland City Region Deal.
