# Extract tables from EEA WISE SOE database
# version:SOE Waterbase WISE2 Biology 
# author: Heliana Teixeira
# date created: 19.09.2022
# date modified: 05.11.2025


###Description of source data----
#Extracting biology data from the EEA State of Environment (WISE_SOE) Waterbase WISE2 Biology
#https://discodata.eea.europa.eu #Discodata platform

#by monitoring site from
#version: "latest" WISE SOE Biology EQR Data available in 2025
#source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE2_BiologyEQRData
#which contains: Annually aggregated biological ecological quality ratio (EQR) data from rivers, lakes, transitional and coastal waters, by monitoring site.

#by WaterBody from
#version: "latest" WISE SOE Biology EQR Data available in 2025
#source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE2_BiologyEQRDataByWaterBody
#which contains: Annually aggregated biological ecological quality ratio (EQR) data from rivers, lakes, transitional and coastal waters, by water body.

#and related spatial data info from 
#source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_S_WISE_SpatialObject_DerivedData
#which contains: List of spatial object identifiers present in the WISE SOE dataset tables. selected information reported through WFD or WISE5 spatial data reporting.

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

### Get biological data for TRaC from WISE2 ----

#Disaggregated, by monitoring site 
WISE2_Biology_Site <- read.csv(file = here("Databases","waterbase_t_wise2_biologyeqrdata-4Nov2025.csv"),sep = ";") #all water categories
#check water categories
levels(as.factor(WISE2_Biology_Site$parameterWaterBodyCategory)) #"CW" "LW" "RW" "TW"
#select TRaC data only
WISE2_Biology_Site.TRaC <- WISE2_Biology_Site %>% 
  filter(parameterWaterBodyCategory == "CW" | parameterWaterBodyCategory == "TW")
#some checks:
levels(as.factor(WISE2_Biology_Site.TRaC$countryCode))
#"BE" "BG" "EL" "ES" "IT" "LT" "LV" "NL" "NO" "PL" "PT" "SE" "UK" - data for 13 countries
levels(as.factor(WISE2_Biology_Site.TRaC$phenomenonTimeReferenceYear))
#"2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019" "2020" "2021" "2022" "2023"

#some exploration
WISE2_Biology_Site.TRaC %>% 
  group_by(parameterWaterBodyCategory,observedPropertyDeterminandLabel,phenomenonTimeReferenceYear,countryCode)%>%
  count() %>% print(n=207)

#Aggregated by WB
WISE2_Biology_AggrWB <- read.csv(file = here("Databases","waterbase_t_wise2_biologyeqrdatabywaterbody-4Nov2025.csv"),sep = ";") #all water categories
#check water categories
levels(as.factor(WISE2_Biology_AggrWB$parameterWaterBodyCategory)) #"CW" "LW" "RW" "TW"
#select TRaC data only
WISE2_Biology_AggrWB.TRaC <- WISE2_Biology_AggrWB %>% 
  filter(parameterWaterBodyCategory == "CW" | parameterWaterBodyCategory == "TW")
#some checks:
levels(as.factor(WISE2_Biology_AggrWB.TRaC$countryCode))
#"EE" "ES" "IE" "IT" "LT" "LV" "MT" "PL" "PT" "SI" - 4 additional countries if WB aggregated, countries:"EE" "IE" "MT" "SI"
levels(as.factor(WISE2_Biology_AggrWB.TRaC$phenomenonTimeReferenceYear))
#"2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019" "2020" "2021" "2022" "2023"

#some exploration 
WISE2_Biology_AggrWB.TRaC %>% 
  group_by(parameterWaterBodyCategory,observedPropertyDeterminandLabel,phenomenonTimeReferenceYear,countryCode)%>%
  count() %>% print(n=194)

### Get the spatial information data (SitesBio)----
WISE_SOE_Spatial <- read.csv(file = here("Databases","waterbase_s_wise_spatialobject_deriveddata-4Nov2025.csv"),sep = ";") #all water categories

spatial_dat <- WISE_SOE_Spatial %>% 
  select(countryCode,thematicIdIdentifier,thematicIdIdentifierScheme,
         monitoringSiteIdentifier,monitoringSiteIdentifierScheme,monitoringSiteName,
         waterBodyIdentifier,waterBodyIdentifierScheme,waterBodyName,
         specialisedZoneType,surfaceWaterBodyTypeCode,lat,lon)

spatial_dat <- spatial_dat %>%
  filter(specialisedZoneType == "coastalWaterBody" | 
           specialisedZoneType == "transitionalWaterBody") %>% filter(!is.na(monitoringSiteIdentifier) & !is.na(waterBodyIdentifier)) 

spatial_dat %>% count(monitoringSiteIdentifier=="") # n=636 missing monitoring site identifier == ""

## select spatial info to data to join
spatial_dat_shrt <-spatial_dat %>% 
  select(monitoringSiteIdentifier,thematicIdIdentifier,lat,lon,waterBodyIdentifier,waterBodyName,countryCode)
#note: left Water Category out, see if useful later on and re-add

#1st check for duplicates in spatial data table
spatial_dat_shrt%>% group_by(monitoringSiteIdentifier)%>%
  count()%>%
  filter(n>1)%>%
  print(n=74) #74 duplicates & 636 missing monitoring site identifier == ""

#some "monitoringSiteIdentifier" belong to different water bodies - keep all info for now
#e.g. IT19TWLV02 belongs to 2 water bodies in Italy

# first concatenate "monitoringSiteIdentifier" & "waterBodyIdentifier" to identify unique rows
spatial_dat_shrt <- spatial_dat_shrt %>%
  mutate(monitoringSite_WB_ID = paste0(monitoringSiteIdentifier,"_",waterBodyIdentifier))

#check for duplicates this way
spatial_dat_shrt%>% group_by(monitoringSite_WB_ID)%>%
  count()%>%
  filter(n>1)%>%
  print(n=60) #60 duplicates remain

#remove duplicates rows that have exact same info
spatial_dat_shrt <- spatial_dat_shrt %>%
  distinct() #n=4360 rows now

#re-check for duplicates 
spatial_dat_shrt%>% group_by(monitoringSite_WB_ID)%>%
  count()%>%
  filter(n>1)%>%
  print(n=11) # coordinates are slightly different for same site-WB duplicates - keep all info for now 

#check for missing spatial coordinates
spatial_dat_shrt%>%filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count()%>%
  print() #n=24 missing latitudes

### Select TW Site data and summarise ----
WISE2_Biology_Site.TW <- WISE2_Biology_Site.TRaC %>%
  filter(parameterWaterBodyCategory == "TW")

#left_join spatial data info
WISE2_Biology_Site.TWspatial<- left_join(WISE2_Biology_Site.TW,spatial_dat_shrt,
                                         by=c("monitoringSiteIdentifier"), suffix = c("", ".y"), relationship = "many-to-many") %>% 
  select(-countryCode.y) # n=1895 increases by 37 entries 

#E.g., some sites in more than 1 water body
  # site IT19TWLM01 in 2 WB (5*2 =10) 
      # IT19TW01131401 - LAGO VERDE
      # IT19TW01131301 - LAGO MARINELLO

#check for missing spatial coordinates
WISE2_Biology_Site.TWspatial %>% filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count()
    # monitoringSiteIdentifier       n
    # 5 UKEA_BIOSYS_NE_155650        2 no lat in spatial_dat_shrt
    # 6 UKEA_BIOSYS_SO_43800         1 no lat in spatial_dat_shrt
    # 7 UKEA_BIOSYS_SW_9318          3 no lat in spatial_dat_shrt

#NOT NEEDED:
# WISE2_Biology_Site.TW  <- WISE2_Biology_Site.TW  %>% mutate(monitoringSiteIdentifier=
#                                     case_when(monitoringSiteIdentifier == "IT05EC_Ve-8" ~ "IT05EC_VE-8",
#                                               monitoringSiteIdentifier == "IT05ENC4_Ve-6" ~ "IT05ENC4_VE-6",
#                                               monitoringSiteIdentifier == "IT05PNC1_Ve-1"~"IT05PNC1_VE-1",
#                                               TRUE ~ as.character(monitoringSiteIdentifier)))
# #re-join
# WISE2_Biology_Site.TWspatial<- left_join(WISE2_Biology_Site.TW,spatial_dat_shrt,by=c("monitoringSiteIdentifier"),suffix = c("", ".y")) %>% # n stays the same OK
#   select(-countryCode.y)

WISE2_Biology_Site.TWspatial$Created <- Sys.Date()
WISE2_Biology_Site.TWspatial <- WISE2_Biology_Site.TWspatial %>% ungroup()
saveRDS(WISE2_Biology_Site.TWspatial,file = here("Data","dat_BQE_TW.rds"))

### Select CW data and summarise---- 
WISE2_Biology_Site.CW <- WISE2_Biology_Site.TRaC %>%
  filter(parameterWaterBodyCategory == "CW")

#left_join spatial data info
WISE2_Biology_Site.CWspatial<- left_join(WISE2_Biology_Site.CW,spatial_dat_shrt,
                                         by=c("monitoringSiteIdentifier"),suffix = c("", ".y"), relationship = "many-to-many") %>%
  select(-countryCode.y)  #n = 3672 increases by 28 new rows after leftJoin
#E.g., some sites in more than 1 water body
  # site IT15-FS011 in 2 WB (6*2 = 12)

#check for missing spatial coordinates
WISE2_Biology_Site.CWspatial %>% filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count()
    # monitoringSiteIdentifier    n
    # 1 IT07MA00928               1 no match to samples in spatial dataset

WISE2_Biology_Site.CWspatial$Created <- Sys.Date()
WISE2_Biology_Site.CWspatial <- WISE2_Biology_Site.CWspatial %>% ungroup()
saveRDS(WISE2_Biology_Site.CWspatial,file = here("Data","dat_BQE_CW.rds"))

#Select TRaC Waterbody data and summarise ----
# in order to get data for 4 countries ("EE" "IE" "MT" "SI") with no disaggregated data
WISE2_Biology_AggrWB.TRaC2 <- WISE2_Biology_AggrWB.TRaC %>% 
  filter(countryCode == "EE" | countryCode == "IE" | countryCode == "MT" | countryCode == "SI")

#some checks:
levels(as.factor(WISE2_Biology_AggrWB.TRaC2$countryCode))
#"EE" "IE" "MT""SI"

levels(as.factor(WISE2_Biology_AggrWB.TRaC2$phenomenonTimeReferenceYear))
#"2013" "2014" "2015" "2016" "2017" "2018" "2019" "2020" "2021" "2022" "2023"

#left_join spatial data info
spatial_dat_shrt.aggrWB <- spatial_dat_shrt %>% select (waterBodyIdentifier,waterBodyName,countryCode) %>% distinct()
WISE2_Biology_AggrWB.TRaC2spatial<- left_join(WISE2_Biology_AggrWB.TRaC2,spatial_dat_shrt.aggrWB,by=c("waterBodyIdentifier"),suffix = c("", ".y")) # n stays the same OK

#check country code matches
WISE2_Biology_AggrWB.TRaC2spatial %>%
    filter(countryCode != countryCode.y) #all good to drop repetead column

WISE2_Biology_AggrWB.TRaC2spatial <- WISE2_Biology_AggrWB.TRaC2spatial %>%
  select(-countryCode.y)

WISE2_Biology_AggrWB.TRaC2spatial$Created <- Sys.Date()
WISE2_Biology_AggrWB.TRaC2spatial <- WISE2_Biology_AggrWB.TRaC2spatial %>% ungroup()
saveRDS(WISE2_Biology_AggrWB.TRaC2spatial,file = here("Data","dat_BQE_TRaC_aggrWB_EE-IE-MT-SI.rds"))

#NOT USED:
#to extract annual data from 2016 onwards to complement info extracted from WISE4
#select 2016 onwards data only
# WISE2_Biology_AggrWB.TRaC.recent <- WISE2_Biology_AggrWB.TRaC %>% 
#   filter(phenomenonTimeReferenceYear > 2015)
