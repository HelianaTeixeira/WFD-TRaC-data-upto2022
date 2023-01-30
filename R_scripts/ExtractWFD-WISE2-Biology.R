# Extract tables from EEA WISE SOE database
# version:SOE Waterbase WISE2 Biology 
# author: Heliana Teixeira
# date: 19.09.2022

#2nd dataset: Extracting biology data from the EEA State of Environment (WISE_SOE) Waterbase WISE2 Biology
#version:  latest WISE SOE Biology EQR Data By WaterBody from 
#source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE2_BiologyEQRData
#contains: Annually aggregated biological ecological quality ratio (EQR) data from rivers, lakes, transitional and coastal waters, by monitoring site.

#load packages
library(here)
library(tidyverse)
library(RSQLite)
library(openxlsx)

#extract 2nd dataset
#get the SE WQ data for TRaC
WISE2_Biology_AggrWB <- read.csv(file = here("Databases","waterbase_t_wise2_biologyeqrdata.csv"),sep = ";") #all water categories


#Notes: Water body types
#For monitoring sites and water bodies where a water body type has already been reported to the WFD [surfaceWaterBodyTypeCode], 
#the same water body type must be used in WISE-2 reporting (since 2021). 
#For the WISE-2 reporting in 2022, the allowed set of water body types will still be limited to those reported to WFD under the 2nd RBMP.
#For subsequent WISE-2 reportings, this set will be expanded with the additional water body types reported under the 3rd RBMP