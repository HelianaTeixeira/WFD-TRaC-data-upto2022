# Extract tables from EEA WISE SOE database
# version:SOE Waterbase WISE2 Biology 
# author: Heliana Teixeira
# date: 19.09.2022


###Description of source data----
#Extracting biology data from the EEA State of Environment (WISE_SOE) Waterbase WISE2 Biology

#by monitoring site from
#version: "latest" WISE SOE Biology EQR Data available in 2022
#source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE2_BiologyEQRData
#which contains: Annually aggregated biological ecological quality ratio (EQR) data from rivers, lakes, transitional and coastal waters, by monitoring site.

#by WaterBody from
#version: "latest" WISE SOE Biology EQR Data available in 2022
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
WISE2_Biology_Site <- read.csv(file = here("Databases","waterbase_t_wise2_biologyeqrdata.csv"),sep = ";") #all water categories
#check water categories
levels(as.factor(WISE2_Biology_Site$parameterWaterBodyCategory)) #"CW" "LW" "RW" "TW"
#select TRaC data only
WISE2_Biology_Site.TRaC <- WISE2_Biology_Site %>% 
  filter(parameterWaterBodyCategory == "CW" | parameterWaterBodyCategory == "TW")
#some checks:
levels(as.factor(WISE2_Biology_Site.TRaC$countryCode))
#"BE" "BG" "ES" "IT" "LT" "LV" "NO" "PL" "SE" "UK" - data for 10 countries
levels(as.factor(WISE2_Biology_Site.TRaC$phenomenonTimeReferenceYear))
##"2008" "2009" "2010" "2011" "2012" "2013" "2014" "2016" "2017" "2018" "2019" "2020"

#some exploration
WISE2_Biology_Site.TRaC %>% 
  group_by(parameterWaterBodyCategory,observedPropertyDeterminandLabel,phenomenonTimeReferenceYear,countryCode)%>%
  count() %>% print(n=91)

#Aggregated by WB
WISE2_Biology_AggrWB <- read.csv(file = here("Databases","waterbase_t_wise2_biologyeqrdatabywaterbody.csv"),sep = ";") #all water categories
#check water categories
levels(as.factor(WISE2_Biology_AggrWB$parameterWaterBodyCategory)) #"CW" "LW" "RW" "TW"
#select TRaC data only
WISE2_Biology_AggrWB.TRaC <- WISE2_Biology_AggrWB %>% 
  filter(parameterWaterBodyCategory == "CW" | parameterWaterBodyCategory == "TW")
#some checks:
levels(as.factor(WISE2_Biology_AggrWB.TRaC$countryCode))
#"EE" "ES" "IE" "IT" "LT" "LV" "PL" "SI" - 3 additional countries if WB aggregated, countries:"EE" "IE" "SI"
levels(as.factor(WISE2_Biology_AggrWB.TRaC$phenomenonTimeReferenceYear))
#"2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019" "2020"

#some exploration 
WISE2_Biology_AggrWB.TRaC %>% 
  group_by(parameterWaterBodyCategory,observedPropertyDeterminandLabel,phenomenonTimeReferenceYear,countryCode)%>%
  count() %>% print(n=133)

### Get the spatial information data (SitesBio)----
WISE_SOE_Spatial <- read.csv(file = here("Databases","waterbase_s_wise_spatialobject_deriveddata.csv"),sep = ";") #all water categories

spatial_dat <- WISE_SOE_Spatial %>% 
  select(countryCode,thematicIdIdentifier,thematicIdIdentifierScheme,
         monitoringSiteIdentifier,monitoringSiteIdentifierScheme,monitoringSiteName,
         waterBodyIdentifier,waterBodyIdentifierScheme,waterBodyName,
         specialisedZoneType,surfaceWaterBodyTypeCode,lat,lon)

spatial_dat <- spatial_dat %>%
  filter(specialisedZoneType == "coastalWaterBody" | 
           specialisedZoneType == "transitionalWaterBody") %>% filter(!is.na(monitoringSiteIdentifier) & !is.na(waterBodyIdentifier)) 

spatial_dat %>% count(monitoringSiteIdentifier=="") #n=558 missing monitoring site identifier

## select spatial info to data to join
spatial_dat_shrt <-spatial_dat %>% 
  select(monitoringSiteIdentifier,thematicIdIdentifier,lat,lon,waterBodyIdentifier,waterBodyName,countryCode)
#note: left Water Category out, see if useful later on and re-add

#1st check for duplicates in spatial data table
spatial_dat_shrt%>% group_by(monitoringSiteIdentifier)%>%
  count()%>%
  filter(n>1)%>%
  print(n=23) #22 duplicates & 558 missing monitoring site identifier == ""

#remove n= 22 duplicates that have exact same info
spatial_dat_shrt <- spatial_dat_shrt %>%
  distinct() #n=2829

#check for missing spatial coordinates
spatial_dat_shrt%>%filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count()%>%
  print(n=26)


### Select TW Site data and summarise ----
WISE2_Biology_Site.TW <- WISE2_Biology_Site.TRaC %>%
  filter(parameterWaterBodyCategory == "TW")

#left_join spatial data info
WISE2_Biology_Site.TWspatial<- left_join(WISE2_Biology_Site.TW,spatial_dat_shrt,by=c("monitoringSiteIdentifier"),suffix = c("", ".y")) %>% # n stays the same OK
  select(-countryCode.y)

#check for missing spatial coordinates
WISE2_Biology_Site.TWspatial %>% filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count()
    # monitoringSiteIdentifier       n
    # 1 BG60000976                   1 no lat in spatial_dat_shrt
    # 2 IT05EC_Ve-8                  2 error in name - correct name mismatch to get lat/lon in left join to IT05EC_VE-8 
    # 3 IT05ENC4_Ve-6                2 error in name - correct name mismatch to get lat/lon in left join to IT05ENC4_VE-6
    # 4 IT05PNC1_Ve-1                2 error in name - correct name mismatch to get lat/lon in left join to IT05PNC1_VE-1
    # 5 UKEA_BIOSYS_NE_155650        2 no lat in spatial_dat_shrt
    # 6 UKEA_BIOSYS_SO_43800         1 no lat in spatial_dat_shrt
    # 7 UKEA_BIOSYS_SW_9318          3 no lat in spatial_dat_shrt

WISE2_Biology_Site.TW  <- WISE2_Biology_Site.TW  %>% mutate(monitoringSiteIdentifier=
                                    case_when(monitoringSiteIdentifier == "IT05EC_Ve-8" ~ "IT05EC_VE-8",
                                              monitoringSiteIdentifier == "IT05ENC4_Ve-6" ~ "IT05ENC4_VE-6",
                                              monitoringSiteIdentifier == "IT05PNC1_Ve-1"~"IT05PNC1_VE-1",
                                              TRUE ~ as.character(monitoringSiteIdentifier)))
#re-join
WISE2_Biology_Site.TWspatial<- left_join(WISE2_Biology_Site.TW,spatial_dat_shrt,by=c("monitoringSiteIdentifier"),suffix = c("", ".y")) %>% # n stays the same OK
  select(-countryCode.y)

WISE2_Biology_Site.TWspatial$Created <- Sys.Date()
WISE2_Biology_Site.TWspatial <- WISE2_Biology_Site.TWspatial %>% ungroup()
saveRDS(WISE2_Biology_Site.TWspatial,file = here("Data","dat_BQE_TW.rds"))

### Select CW data and summarise---- 
WISE2_Biology_Site.CW <- WISE2_Biology_Site.TRaC %>%
  filter(parameterWaterBodyCategory == "CW")

#left_join spatial data info
WISE2_Biology_Site.CWspatial<- left_join(WISE2_Biology_Site.CW,spatial_dat_shrt,by=c("monitoringSiteIdentifier"),suffix = c("", ".y")) %>% # n stays the same OK
  select(-countryCode.y)

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
# in order to get data for 3 countries ("EE" "IE" "SI") with no disaggregated data)
WISE2_Biology_AggrWB.TRaC2 <- WISE2_Biology_AggrWB.TRaC %>% 
  filter(countryCode == "EE" | countryCode == "IE" | countryCode == "SI")

#some checks:
levels(as.factor(WISE2_Biology_AggrWB.TRaC2$countryCode))
#"EE" "IE" "SI"

levels(as.factor(WISE2_Biology_AggrWB.TRaC2$phenomenonTimeReferenceYear))
#"2013" "2014" "2015" "2016" "2017" "2018" "2019" "2020"

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
saveRDS(WISE2_Biology_AggrWB.TRaC2spatial,file = here("Data","dat_BQE_TRaC_aggrWB_EE-IE-SI.rds"))

#NOT USED:
#to extract annual data from 2016 onwards to complement info extracted from WISE4
#select 2016 onwards data only
# WISE2_Biology_AggrWB.TRaC.recent <- WISE2_Biology_AggrWB.TRaC %>% 
#   filter(phenomenonTimeReferenceYear > 2015)
