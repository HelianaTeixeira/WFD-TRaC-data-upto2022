# Extract tables from EEA WISE SOE SQLite database
# version: Waterbase WISE6 WQ ICM v2024
# author: Heliana Teixeira
# date created: 14.12.2022
# date modified: 05.11.2025

### Description----
#This script assembles a set of TRAC Water Quality (WQ) summary metrics for an appropriate set of supporting elements (SE). 
#Data was taken from the EEA state of the environment (SoE) WISE 6 datadase.
#First the Spatial data is extracted, then the SE data.

### Data Source----
# Waterbase - Water Quality ICM, 2024; available online from 2nd July 2025
# temporal range 1899-2024
# downloaded from https://discodata.eea.europa.e (WISE_SOE / latest)
# Waterbase is the generic name given to the EEA's databases on the status and quality of Europe's 
# rivers, lakes, groundwater bodies and transitional, coastal and marine waters, on the quantity of Europe's water resources, 
# and on the emissions to surface waters from point and diffuse sources of pollution.
# The dataset contains time series of nutrients, organic matter, hazardous substances, pesticides and other chemicals 
# in rivers, lakes, groundwater, transitional, coastal and marine waters. 
# A list of spatial object identifiers with selected attributes, reported through WFD and WISE Spatial data reporting, is added to dataset as spatial reference. 
# The data has been compiled and processed by EEA. Please refer to the metadata for additional information.

# The database is split into several datasets: 
# ds1: DisaggregatedData
# source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE6_DisaggregatedData 

# ds2: AggregatedData
# source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE6_AggregatedData 

# ds3: AggregatedDataByWaterBody
# source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE6_AggregatedDataByWaterBody 

# ds4: SpatialObject_DerivedData
# source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_S_WISE_SpatialObject_DerivedData
# contains: List of spatial object identifiers present in the WISE SOE dataset tables. 

# Data is reported by EEA member countries as individual samples from monitoring sites in the DisaggregatedData table 
# or as annual aggregates of samples from monitoring sites in the AggregatedData table. 
# Therefore data found in one table is not found in the other, and visa versa. 
# Data in the the AggregatedDataByWaterBody is mostly historical (HT: and currently only has GW information. discarded for this work).

# For more details see the 'Discodata user guide' in the Documents section.

### Setup ----
#load packages
if (!require("duckdb")) install.packages("duckdb", dependencies = TRUE)
library(here)
library(tidyverse)
library(duckdb)

### WISE6 Spatial data ----
WISE6_Spatial <- read.csv(file = here("Databases","waterbase_s_wise_spatialobject_deriveddata-4Nov2025.csv"),sep = ";") #all water categories

names(WISE6_Spatial)
# [1] "countryCode"                    "thematicIdIdentifier"           "thematicIdIdentifierScheme"    
# [4] "monitoringSiteIdentifier"       "monitoringSiteIdentifierScheme" "monitoringSiteName"            
# [7] "waterBodyIdentifier"            "waterBodyIdentifierScheme"      "waterBodyName"                 
# [10] "specialisedZoneType"            "naturalAWBHMWB"                 "reservoir"                     
# [13] "surfaceWaterBodyTypeCode"       "subUnitIdentifier"              "subUnitIdentifierScheme"       
# [16] "subUnitName"                    "rbdIdentifier"                  "rbdIdentifierScheme"           
# [19] "rbdName"                        "confidentialityStatus"          "lon"                           
# [22] "lat"                            "statusCode"      

unique(WISE6_Spatial$specialisedZoneType)
# [1] ""                          "riverBasinDistrictSubUnit" "groundWaterBody"          
# [4] "riverWaterBody"            "lakeWaterBody"             "coastalWaterBody"         
# [7] "transitionalWaterBody"     "territorialWaters"   

#filter by specialisedZoneType retain only TRaC categories
WISE6_Spatial_TRaC <- WISE6_Spatial %>% 
  filter(specialisedZoneType %in% c("coastalWaterBody", "transitionalWaterBody"))

unique(WISE6_Spatial_TRaC$statusCode)
#[1] "superseded" "stable" "retired" "deprecated"

# Fields of interest:
# [1] "countryCode"  
# [4] "monitoringSiteIdentifier"
# [7] "waterBodyIdentifier"
# [9] "waterBodyName"                 
# [13] "surfaceWaterBodyTypeCode"             
# [21] "lat"                           
# [22] "lon" 

# Get spatial information lat lon
WISE6_Spatial_TRaC <- WISE6_Spatial_TRaC %>%
  select(!c(reservoir,confidentialityStatus))

unique(WISE6_Spatial_TRaC$waterBodyIdentifierScheme)

## select spatial info to data to join
WISE6_Spatial_TRaC_shrt <-WISE6_Spatial_TRaC %>% 
  select(countryCode,
         monitoringSiteIdentifier,
         thematicIdIdentifier,
         thematicIdIdentifierScheme,
         lat,
         lon,
         waterBodyIdentifier,
         waterBodyName,
         naturalAWBHMWB,
         surfaceWaterBodyTypeCode)

#1st check for duplicates in spatial data table
WISE6_Spatial_TRaC_shrt %>% group_by(monitoringSiteIdentifier) %>%
  count()%>%
  filter(n>1)%>%
  print(n=74) #74 duplicates
  #there are 636 entries with no monitoringSiteIdentifier

#remove duplicates that have exact same info
WISE6_Spatial_TRaC_shrt <- WISE6_Spatial_TRaC_shrt %>%
  distinct()

#check for missing spatial coordinates
WISE6_Spatial_TRaC_shrt %>% filter(is.na(lat)) %>%
  group_by(monitoringSiteIdentifier)%>%
  count() %>%
  print(n=24)
# monitoringSiteIdentifier      n
# 1 BG60000976                   1
# 2 ES040ESPF000400127           1
# 3 ESLELEA-R05                  1
# 4 ESORALT-R01                  1
# 5 ESORSGO-R01                  1
# 6 FRFR05026000                 1
# 7 FRFR05029800                 1
# 8 FRFR05076000                 1
# 9 FRFR05200200                 1
# 10 FRFR05238500                 1
# 11 IT15-VES8                    1
# 12 PT03F03                      1
# 13 PT03F04                      1
# 14 RO142945010                  1
# 15 UKEA_BIOSYS_NE_155650        1
# 16 UKEA_BIOSYS_SO_43800         1
# 17 UKEA_BIOSYS_SW_9318          1
# 18 UKEA_WIMS_SO_F0002151        1
# 19 UKEA_WIMS_SW_81930101        1
# 20 UKEA_WIMS_SW_E1008300        1
# 21 UKEA_WIMS_TH_PCRR0025        1
# 22 UKEA_WIMS_TH_PRGR0030        1
# 23 UKEA_WIMS_TH_PRGR0081        1
# 24 UKEA_WIMS_TH_PTHR0107        1

WISE6_Spatial_TRaC_shrt<- WISE6_Spatial_TRaC_shrt %>% ungroup()
WISE6_Spatial_TRaC_shrt$Created <- Sys.Date()
saveRDS(WISE6_Spatial_TRaC_shrt,file = here("Data","dat_W6_SOE_TRaC_Spatial_short_b.rds"))

### WISE6 SE DisaggregatedData ----
# Query the file without loading it fully (for big files)
con2 <- dbConnect(duckdb())

## explore fields in data
head <- dbGetQuery(con2, "
  SELECT *
  FROM read_csv_auto(
    '~/Documents/GitHub/WFD-TRaC-data-upto2022/Databases/waterbase_t_wise6_disaggregateddata.csv',
    sample_size=10000
  )
  LIMIT 5
")

#extract TraC data to a df
df <- dbGetQuery(con2,"
  SELECT parameterWaterBodyCategory, 
  FROM read_csv_auto(
    '~/Documents/GitHub/WFD-TRaC-data-upto2022/Databases/waterbase_t_wise6_disaggregateddata.csv')
")

#inspect water category labels
levels(as.factor(df$parameterWaterBodyCategory))
#[1] "CW"  "GW"  "LW"  "RW"  "TeW" "TW" 

# select only TRaC data
df <- dbGetQuery(con2, "
  SELECT *
  FROM read_csv_auto(
    '~/Documents/GitHub/WFD-TRaC-data-upto2022/Databases/waterbase_t_wise6_disaggregateddata.csv'
  )
  WHERE parameterWaterBodyCategory IN ('CW','TW')
")

#run dbDisconnect() to disconnect from database
dbDisconnect(con2)

### Inspect SE parameters data
names(df)
# [1] "countryCode"                        "monitoringSiteIdentifier"          
# [3] "monitoringSiteIdentifierScheme"     "parameterWaterBodyCategory"        
# [5] "observedPropertyDeterminandCode"    "observedPropertyDeterminandLabel"  
# [7] "procedureAnalysedMatrix"            "resultUom"                         
# [9] "phenomenonTimeSamplingDate"         "phenomenonTimeReferenceYear"       
# [11] "sampleIdentifier"                   "resultObservedValue"               
# [13] "resultQualityObservedValueBelowLOQ" "procedureLOQValue"                 
# [15] "parameterSampleDepth"               "parameterSedimentDepthSampled"     
# [17] "parameterSpecies"                   "resultMoisture"                    
# [19] "resultFat"                          "resultExtractableLipid"            
# [21] "resultLipid"                        "resultObservationStatus"           
# [23] "Remarks"                            "metadata_versionId"                
# [25] "metadata_beginLifeSpanVersion"      "metadata_statusCode"               
# [27] "metadata_observationStatus"         "metadata_statements"               
# [29] "UID"   

levels(factor(df$observedPropertyDeterminandCode)) 
levels(factor(df$observedPropertyDeterminandLabel))

parameters <-df %>% 
  select(observedPropertyDeterminandCode,observedPropertyDeterminandLabel)

parameters$observedPropertyDeterminandLabel <- factor(parameters$observedPropertyDeterminandLabel)
parameters$observedPropertyDeterminandCode <- factor(parameters$observedPropertyDeterminandCode)

summary_codes <- parameters %>% 
  group_by(observedPropertyDeterminandLabel,observedPropertyDeterminandCode) %>%
  count()

#parameters of interest:
  #Total suspended solids EEA_31-02-7
  #Total phosphorus CAS_7723-14-0
  #Total oxidised nitrogen EEA_3161-02-2
  #Total nitrogen EEA_31615-01-7
  #Total inorganic nitrogen EEA_3161-05-5
  #Secchi depth EEA_3111-01-1
  #Salinity EEA_3141-01-3
  #Phosphate CAS_14265-44-2
  #pH EEA_3152-01-0
  #Oxygen saturation EEA_3131-01-9
  #Nitrate CAS_14797-55-8
  #Non-ionised ammonia EEA_31613-01-1
  #Dissolved oxygen EEA_3132-01-2
  #Chloride CAS_16887-00-6
  #BOD5 EEA_3133-01-5
  #Ammonium CAS_14798-03-9
  #Ammonia CAS_7664-41-7
  #Alkalinity EEA_3153-02-4
  #Chlorophyll a EEA_3164-01-0
  #Total nitrogen to total phosphorus ratio EEA_3164-07-6
  #Particulate organic nitrogen EEA_3161-04-4
  # BOD7 EEA_3133-02-6
  # Water temperature EEA_3121-01-5

#and, in addition to those by Geoff for FW, also:
  # Turbidity EEA_3112-01-4
  # Dissolved organic carbon (DOC) EEA_3133-05-9
  # Total organic carbon (TOC) EEA_3133-06-0
  # Nitrite CAS_14797-65-0
  # Total dissolved solids EEA_31-03-8
  # Total organic nitrogen EEA_3161-03-3

#dropped:
# Carbonate CAS_3812-32-6
# Calcium CAS_7440-70-2

#Create a list of determinand codes of interest
detUsed <- c("EEA_3152-01-0","EEA_3164-01-0","EEA_3111-01-1","CAS_14797-55-8",
             "CAS_14798-03-9","CAS_7723-14-0","EEA_31-02-7","EEA_3133-01-5",
             "EEA_3131-01-9","CAS_14265-44-2","CAS_16887-00-6","EEA_3132-01-2",
             "EEA_31615-01-7","EEA_31613-01-1","EEA_3161-05-5","EEA_3141-01-3",
             "EEA_3161-02-2","CAS_7664-41-7","EEA_3153-02-4",
             "EEA_3112-01-4","EEA_3133-05-9","EEA_3133-06-0",
             "EEA_31-03-8","EEA_3161-03-3","CAS_14797-65-0",
             "EEA_3164-07-6","EEA_3161-04-4","EEA_3133-02-6","EEA_3121-01-5")


#add seasons
df<- df %>%
  mutate(phenomenonTimeSamplingMonth = as.numeric(substr(phenomenonTimeSamplingDate,6,7))) %>%
  mutate(Season = case_when(
    phenomenonTimeSamplingMonth %in% c(12,1,2) ~ "Winter",
    phenomenonTimeSamplingMonth %in% c(3,4,5) ~ "Spring",
    phenomenonTimeSamplingMonth %in% c(6,7,8) ~ "Summer",
    phenomenonTimeSamplingMonth %in% c(9,10,11) ~ "Autumn",
    TRUE ~ NA_character_
  ))

# define season names expected
canonical_seasons <- c("Winter", "Spring", "Summer", "Autumn")

#Then, below select the TW and CW data and these codes and aggregate the data using a mean.

### Select TW disaggregated data and summarise----
#summarise TW data
dat_WQ_TW <- df %>%
  select(
    monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,
         observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,
         resultUom,Season,phenomenonTimeReferenceYear,parameterSampleDepth,sampleIdentifier,resultObservedValue) %>% 
  filter(
    parameterWaterBodyCategory == "TW" & observedPropertyDeterminandCode %in% detUsed) 

dat_WQ_TW <- dat_WQ_TW %>% 
  #compute flag per site/year/variable *without* Season in the grouping
  group_by(
    monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,
    observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,
    resultUom,phenomenonTimeReferenceYear,parameterSampleDepth) %>%
  # Check if all 4 seasons are present
  mutate(has_all_seasons = n_distinct(Season,na.rm = TRUE) == 4) %>%
  ungroup() 

dat_WQ_TW <- dat_WQ_TW %>%
  # Replace Season with "Annual" only for those sites/years with all 4 seasons
  mutate(Season = ifelse(has_all_seasons, "Annual", Season)) %>%
  select(-has_all_seasons) %>%
  # Re-summarise (group again with updated Season)
  group_by(
    monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,
    observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,
    resultUom,Season,phenomenonTimeReferenceYear,parameterSampleDepth) %>%
  summarise(
    resultMeanValue = mean(resultObservedValue, na.rm=TRUE),
    resultStdValue = sd(resultObservedValue, na.rm=TRUE), # for data dispersion
    resultMedianValue = median(resultObservedValue, na.rm=TRUE),
    resultMinimumValue = min(resultObservedValue, na.rm=TRUE),
    resultMaximumValue = max(resultObservedValue, na.rm=TRUE),
    resultNumberOfSamples = n(), 
    .groups = "drop"
    ) %>% 
  mutate(metadata_versionId = "Waterbase_v2024_WISE6_DisaggregatedData")

#left_join SE & spatial data info
dat_WQ_TW <- dat_WQ_TW %>%
  mutate(thematicIdIdentifierScheme = monitoringSiteIdentifierScheme)
  
dat_WQ_TW.Spatial <- left_join(dat_WQ_TW,WISE6_Spatial_TRaC_shrt,
                             by=c("monitoringSiteIdentifier", "thematicIdIdentifierScheme")) # n stays the same OK

#check for missing spatial coordinates
dat_WQ_TW.Spatial %>% filter(is.na(lat)) %>%
  group_by(countryCode) %>%
  count()

dat_WQ_TW.Spatial %>% filter(is.na(lat)) %>%
  group_by(monitoringSiteIdentifier) %>%
  count()

#Save TW data
dat_WQ_TW.Spatial$Created <- Sys.Date()
dat_WQ_TW.Spatial <- dat_WQ_TW.Spatial %>% ungroup()
saveRDS(dat_WQ_TW.Spatial,file = here("Data","dat_WQ_TW.rds"))

### Select CW disaggregated data and summarise---- 
dat_WQ_CW <- df %>%
  select(
    monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,
    observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,
    resultUom,Season,phenomenonTimeReferenceYear,parameterSampleDepth,sampleIdentifier,resultObservedValue) %>% 
  filter(
    parameterWaterBodyCategory == "CW" & observedPropertyDeterminandCode %in% detUsed) 

dat_WQ_CW <- dat_WQ_CW%>%
  #compute flag per site/year/variable *without* Season in the grouping
  group_by(
    monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,
    observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,
    resultUom,phenomenonTimeReferenceYear,parameterSampleDepth) %>%
  # Check if all 4 seasons are present
  mutate(has_all_seasons = n_distinct(Season,na.rm = TRUE) == 4) %>%
  ungroup() 

dat_WQ_CW <- dat_WQ_CW %>%
  # Replace Season with "Annual" only for those sites/years with all 4 seasons
  mutate(Season = ifelse(has_all_seasons, "Annual", Season)) %>%
  select(-has_all_seasons) %>%
  # Re-summarise (group again with updated Season)
  group_by(
    monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,
    observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,
    resultUom,Season,phenomenonTimeReferenceYear,parameterSampleDepth) %>%
  summarise(
    resultMeanValue = mean(resultObservedValue, na.rm=TRUE),
    resultStdValue = sd(resultObservedValue, na.rm=TRUE), #for data dispersion
    resultMedianValue = median(resultObservedValue, na.rm=TRUE),
    resultMinimumValue = min(resultObservedValue, na.rm=TRUE),
    resultMaximumValue = max(resultObservedValue, na.rm=TRUE),
    resultNumberOfSamples = n(),
    .groups = "drop"
    ) %>% 
  mutate(metadata_versionId = "Waterbase_v2024_WISE6_DisaggregatedData")

#left_join SE & spatial data info
dat_WQ_CW <- dat_WQ_CW  %>%
  mutate(thematicIdIdentifierScheme = monitoringSiteIdentifierScheme)

dat_WQ_CW.Spatial <- left_join(dat_WQ_CW ,WISE6_Spatial_TRaC_shrt,
                               by=c("monitoringSiteIdentifier", "thematicIdIdentifierScheme")) # n stays the same OK


#check for missing spatial coordinates
dat_WQ_CW.Spatial  %>% filter(is.na(lat)) %>%
  group_by(countryCode) %>%
  count()

dat_WQ_CW.Spatial %>% filter(is.na(lat)) %>%
  group_by(monitoringSiteIdentifier) %>%
  count()

#Save CW data
dat_WQ_CW.Spatial $Created <- Sys.Date()
dat_WQ_CW.Spatial  <- dat_WQ_CW.Spatial  %>% ungroup()
saveRDS(dat_WQ_CW.Spatial ,file = here("Data","dat_WQ_CW.rds"))

### explore generated TW & CW datasets for checking ----
library(readr)
test <- read_rds(here("Data","dat_WQ_CW.rds")) 
test %>%
  filter(observedPropertyDeterminandLabel == "Secchi depth")%>%
  group_by(countryCode)%>%
  count()

test2 <- read_rds(here("Data","dat_WQ_TW.rds")) 
test2 %>%
  filter(observedPropertyDeterminandLabel == "Secchi depth")%>%
  group_by(countryCode)%>%
  count()


#check for SD in CW but now only for year corresponding to WFD 2nd cycle 2016
test %>%
  filter(observedPropertyDeterminandLabel == "Secchi depth")%>%
  filter(phenomenonTimeReferenceYear >2009 & phenomenonTimeReferenceYear <2016) %>% 
  group_by(countryCode)%>%
  count()
  
#check for Oxygen in TW but now only for year corresponding to WFD 2nd cycle 2016
test2 %>%
  filter(observedPropertyDeterminandLabel == "Oxygen saturation")%>% #Oxygen saturation; Dissolved oxygen
  filter(phenomenonTimeReferenceYear >2009 & phenomenonTimeReferenceYear <2016) %>% 
  group_by(countryCode)%>%
  count()

test2 %>%
  filter(observedPropertyDeterminandLabel == "Dissolved oxygen")%>% 
  filter(phenomenonTimeReferenceYear >2009 & phenomenonTimeReferenceYear <2016) %>% 
  group_by(countryCode)%>%
  count()

### get summary of years available per Water Category ----
CW <- readRDS(here("Data","dat_WQ_CW.rds"))
summary(as.factor(CW$phenomenonTimeReferenceYear))
#2000  2001  2002  2003  2004  2007  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021  2022  2023 
#2     3     2     2     2     3    83   379   300     9    15  8286  8161  5956  6305 12877  7384  9260  6495  7862  45656 


TW <- readRDS(here("Data","dat_WQ_TW.rds"))
summary(as.factor(TW$phenomenonTimeReferenceYear))
#2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 
#10    8   49   17   16   14   12   13   15  193  571  190  177  419 1822 1914 1578 3392 4451 4731 4569 3135 4763 3166    2 




### Create a list of dets, write to Excel for reference----
TW.dets.summary<-dat_WQ_TW.Spatial%>%group_by(observedPropertyDeterminandLabel,observedPropertyDeterminandCode,resultUom)%>%
  count()%>%
  arrange(desc(n))%>%
  print(n=32)

CW.dets.summary<-dat_WQ_CW.Spatial%>%group_by(observedPropertyDeterminandLabel,observedPropertyDeterminandCode,resultUom)%>%
  count()%>%
  arrange(desc(n))%>%
  print(n=32)

library(openxlsx)
write.xlsx(TW.dets.summary,here("Data","DetList-TW.xlsx"))
write.xlsx(CW.dets.summary,here("Data","DetList-CW.xlsx"))

## Select TRaC aggregated data by monitoring site ---- 
WISE6_WQ_AggSite <- read.csv(file = here("Databases","waterbase_t_wise6_aggregateddata.csv"),sep = ";") #all water categories
#check water categories
levels(as.factor(WISE6_WQ_AggSite$parameterWaterBodyCategory)) #"CW" "GW" "LW" "RW" "TW"

#select TRaC data only
WISE6_WQ_AggSite.TRaC <- WISE6_WQ_AggSite %>% 
  filter(parameterWaterBodyCategory == "CW" | parameterWaterBodyCategory == "TW")
#some checks:
levels(as.factor(WISE6_WQ_AggSite.TRaC$parameterWaterBodyCategory))
#"CW" "TW"
levels(as.factor(WISE6_WQ_AggSite.TRaC$countryCode))
# "BG" "DE" "ES" "FR" "HR" "IS" "IT" "LV" "MT" "NL" "NO" "RO" "UK"; data for n=13 countries
levels(as.factor(WISE6_WQ_AggSite.TRaC$phenomenonTimeReferenceYear))
# [1] "1971" "1972" "1973" "1974" "1975" "1976" "1977" "1978" "1979" "1980" "1981" "1982"
# [13] "1983" "1984" "1985" "1986" "1987" "1988" "1989" "1990" "1991" "1992" "1993" "1994"
# [25] "1995" "1996" "1997" "1998" "1999" "2000" "2001" "2002" "2003" "2004" "2005" "2006"
# [37] "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018"
# [49] "2019" "2020" "2021" "2022" "2023"

rm(WISE6_WQ_AggSite) #free memory
gc()
#search e.g. for transparency data
WISE6_WQ_AggSite.TRaC %>%
  filter(observedPropertyDeterminandLabel == "Secchi depth")%>%
  group_by(countryCode)%>%
  count()
# countryCode     n
# IT             15
# NL             12

#left_join SE & spatial data info
WISE6_Spatial_TRaC_shrt<-readRDS(here("Data","dat_W6_SOE_TRaC_Spatial_short_b.rds"))
#TRaCAggSite join spatial data
WISE6_WQ_AggSite.TRaC <- WISE6_WQ_AggSite.TRaC  %>%
  mutate(thematicIdIdentifierScheme = monitoringSiteIdentifierScheme)


WISE6_WQ_AggSite.TRaC.Spatial <- left_join(WISE6_WQ_AggSite.TRaC,WISE6_Spatial_TRaC_shrt,
                               by=c("monitoringSiteIdentifier", "thematicIdIdentifierScheme")) # n stays the same OK

WISE6_WQ_AggSite.TRaC.Spatial$Created <- Sys.Date()
#Save TW data
WISE6_WQ_AggSite.TW.Spatial <- WISE6_WQ_AggSite.TRaC.Spatial %>%
  filter(parameterWaterBodyCategory == "TW")

saveRDS(WISE6_WQ_AggSite.TW.Spatial ,file = here("Data", "dat_WQ_TWagg.rds"))

#Save CW data
WISE6_WQ_AggSite.CW.Spatial <- WISE6_WQ_AggSite.TRaC.Spatial %>%
  filter(parameterWaterBodyCategory == "CW")

saveRDS(WISE6_WQ_AggSite.CW.Spatial ,file = here("Data", "dat_WQ_CWagg.rds"))


## Select TRaC aggregated data by waterbody ---- 
WISE6_WQ_AggWB <- read.csv(file = here("Databases","waterbase_t_wise6_aggregateddatabywaterbody.csv"),sep = ";") #all water categories
#check water categories
levels(as.factor(WISE6_WQ_AggWB$parameterWaterBodyCategory)) #ONLY GW !! No TRaC data available - discarded.
