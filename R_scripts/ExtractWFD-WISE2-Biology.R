# Extract tables from EEA WISE SOE database
# version:SOE Waterbase WISE2 Biology 
# author: Heliana Teixeira
# date: 19.09.2022

#Description of dataset
#2nd dataset: Extracting biology data from the EEA State of Environment (WISE_SOE) Waterbase WISE2 Biology
#version:  latest WISE SOE Biology EQR Data By WaterBody from 
#source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE2_BiologyEQRData
#contains: Annually aggregated biological ecological quality ratio (EQR) data from rivers, lakes, transitional and coastal waters, by monitoring site.

#Notes: Water body types
#For monitoring sites and water bodies where a water body type has already been reported to the WFD [surfaceWaterBodyTypeCode], 
#the same water body type must be used in WISE-2 reporting (since 2021). 
#For the WISE-2 reporting in 2022, the allowed set of water body types will still be limited to those reported to WFD under the 2nd RBMP.
#For subsequent WISE-2 reportings, this set will be expanded with the additional water body types reported under the 3rd RBMP

#load packages
library(here)
library(tidyverse)
library(RSQLite)
library(openxlsx)

#extract 2nd dataset
#get the biological data for TRaC
WISE2_Biology_AggrWB <- read.csv(file = here("Databases","waterbase_t_wise2_biologyeqrdata.csv"),sep = ";") #all water categories
#check water categories
levels(as.factor(WISE2_Biology_AggrWB$parameterWaterBodyCategory)) #"CW" "LW" "RW" "TW"
#select TRaC data only
WISE2_Biology_AggrWB.TRaC <- WISE2_Biology_AggrWB %>% 
  filter(parameterWaterBodyCategory == "CW" | parameterWaterBodyCategory == "TW")

#some checks:
levels(as.factor(WISE2_Biology_AggrWB.TRaC$countryCode))
#"BE" "BG" "ES" "IT" "LT" "LV" "NO" "PL" "SE" "UK" - data for only 10 countries

levels(as.factor(WISE2_Biology_AggrWB.TRaC$phenomenonTimeReferenceYear))
#"2008" "2009" "2010" "2011" "2012" "2013" "2014" "2016" "2017" "2018" "2019" "2020" 

WISE2_Biology_AggrWB.TRaC %>% 
  group_by(parameterWaterBodyCategory,observedPropertyDeterminandLabel,countryCode,phenomenonTimeReferenceYear)%>%
  count()%>%
  print(n=91)

WISE2_Biology_AggrWB.TRaC %>% 
  group_by(parameterWaterBodyCategory,phenomenonTimeReferenceYear,observedPropertyDeterminandLabel,countryCode)%>%
  count()%>%
  print(n=91)

#LEFT HERE
#will extract annual data from 2016 onwards to complement info extracted from WISE4
#select 2016 onwards data only
WISE2_Biology_AggrWB.TRaC.recent <- WISE2_Biology_AggrWB.TRaC %>% 
  filter(phenomenonTimeReferenceYear > 2015)
