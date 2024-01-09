# Extract tables from EEA WISE SOE SQLite database
# version: Waterbase WISE6 WQ ICM v2021
# author: Heliana Teixeira
# date created: 14.12.2022

#Description
#This script assembles a set of TRAC WQ summary metrics for an appropriate set of supporting elements, 
#taken from the EEA state of the environment WISE 6 data set 
#downloaded from https://www.eea.europa.eu/data-and-maps/data/waterbase-water-quality-icm-2
#version 2021 available online from 11th May 2022.
#disaggregated data

#load packages
library(here)
library(tidyverse)
library(RSQLite)

### Mapping the original database table(s) into a list of separate data frames----
## connect to db
con2 <- dbConnect(drv=RSQLite::SQLite(), here("Databases", "Waterbase_v2021_1_WISE6_DisaggregatedData.sqlite")) #edit file name: dbname="?.sqlite"

## inspect all tables it contains
dbListTables(con2) #list tables' name
    # "S_WISE6_SpatialObject_DerivedData" 
    # "T_WISE6_DisaggregatedData" 

## explore fields in tables
dbListFields(con2,"S_WISE6_SpatialObject_DerivedData")
#of interest:
# [1] "countryCode"  
# [4] "monitoringSiteIdentifier"
# [7] "waterBodyIdentifier"          
# [8] "waterBodyIdentifierScheme"     
# [9] "waterBodyName"                 
# [13] "surfaceWaterBodyTypeCode"             
# [21] "lat"                           
# [22] "lon" 

dbListFields(con2,"T_WISE6_DisaggregatedData")
# [1] "monitoringSiteIdentifier"           "monitoringSiteIdentifierScheme"    
# [3] "parameterWaterBodyCategory"         "observedPropertyDeterminandCode"   
# [5] "observedPropertyDeterminandLabel"   "procedureAnalysedMatrix"           
# [7] "resultUom"                          "phenomenonTimeSamplingDate"        
# [9] "sampleIdentifier"                   "resultObservedValue"               
# [11] "resultQualityObservedValueBelowLOQ" "procedureLOQValue"                 
# [13] "parameterSampleDepth"               "parameterSedimentDepthSampled"     
# [15] "parameterSpecies"                   "resultMoisture"                    
# [17] "resultFat"                          "resultExtractableLipid"            
# [19] "resultLipid"                        "resultObservationStatus"           
# [21] "Remarks"                            "metadata_versionId"                
# [23] "metadata_beginLifeSpanVersion"      "metadata_statusCode"               
# [25] "metadata_observationStatus"         "metadata_statements"               
# [27] "UID"

##then extract dataframe for each table
library(purrr)
tables <- dbListTables(con2)
lDataFrames <- map(tables, ~{
  dbGetQuery(conn=con2, statement=paste("SELECT * FROM '", .x, "'", sep=""))
})

#run dbDisconnect() to disconnect from a database
dbDisconnect(con2)

### Inspect tables ----
#Table 1 Spatial data
str(lDataFrames[[1]])
tail(lDataFrames[[1]])

levels(factor(lDataFrames[[1]]$"specialisedZoneType"))
# [1] "coastalWaterBody"      "groundWaterBody"       "lakeWaterBody"         "riverWaterBody"       
# [5] "territorialWaters"     "transitionalWaterBody"

lDataFrames[[1]] %>% 
  filter(specialisedZoneType == "coastalWaterBody" | 
         specialisedZoneType == "transitionalWaterBody")%>%
  count() #n=2850

#Table 2 SE parameters data
str(lDataFrames[[2]])
tail(lDataFrames[[2]])

levels(factor(lDataFrames[[2]]$parameterWaterBodyCategory))
# "CW"  "GW"  "LW"  "RW"  "TeW" "TW" 

lDataFrames[[2]] %>% 
  filter(parameterWaterBodyCategory == "CW" | 
           parameterWaterBodyCategory == "TW")%>%
  count() #n=1388291

### Get spatial information lat lon ----
names(lDataFrames[[1]])

spatial_dat <- lDataFrames[[1]] %>% 
  select(countryCode,thematicIdIdentifier,thematicIdIdentifierScheme,
         monitoringSiteIdentifier,monitoringSiteIdentifierScheme,monitoringSiteName,
         waterBodyIdentifier,waterBodyIdentifierScheme,waterBodyName,
         specialisedZoneType,surfaceWaterBodyTypeCode,lat,lon)

spatial_dat <- spatial_dat %>%
  filter(specialisedZoneType == "coastalWaterBody" | 
           specialisedZoneType == "transitionalWaterBody")

## select spatial info to data to join
spatial_dat_shrt <-spatial_dat %>% select(monitoringSiteIdentifier,thematicIdIdentifier,lat,lon,waterBodyIdentifier,waterBodyName,countryCode)

#1st check for duplicates in spatial data table
spatial_dat_shrt%>% group_by(monitoringSiteIdentifier)%>%
  count()%>%
  filter(n>1)%>%
  print(n=23) #22 duplicates

#remove n= 22 duplicates that have exact same info
spatial_dat_shrt <- spatial_dat_shrt %>%
  distinct() #n=2828

#check for missing spatial coordinates
spatial_dat_shrt%>%filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count()%>%
  print(n=26)
# monitoringSiteIdentifier      n
# 1 BG60000976                   1
# 2 ES040ESPF000400127           1
# 3 ESBIBID-R01                  1
# 4 ESLELEA-R05                  1
# 5 ESOKOKA-R03                  1
# 6 ESOKOLA-R01                  1
# 7 ESORALT-R01                  1
# 8 ESORORI-R01                  1
# 9 ESORSGO-R01                  1
# 10 IT09-MAT-P077                1
# 11 IT09-MAT-P084                1
# 12 IT09-MAT-P087                1
# 13 IT09-MAT-P104                1
# 14 IT09-MAT-P210                1
# 15 IT09-MAT-P212                1
# 16 IT15-VES8                    1
# 17 UKEA_BIOSYS_NE_155650        1
# 18 UKEA_BIOSYS_SO_43800         1
# 19 UKEA_BIOSYS_SW_9318          1
# 20 UKEA_WIMS_SO_F0002151        1
# 21 UKEA_WIMS_SW_81930101        1
# 22 UKEA_WIMS_SW_E1008300        1
# 23 UKEA_WIMS_TH_PCRR0025        1
# 24 UKEA_WIMS_TH_PRGR0030        1
# 25 UKEA_WIMS_TH_PRGR0081        1
# 26 UKEA_WIMS_TH_PTHR0107        1

### Get SE parameters data ----
names(lDataFrames[[2]])
levels(factor(lDataFrames[[2]]$observedPropertyDeterminandCode)) 
levels(factor(lDataFrames[[2]]$observedPropertyDeterminandLabel))

parameters<-lDataFrames[[2]] %>% select(observedPropertyDeterminandCode,observedPropertyDeterminandLabel)
parameters$observedPropertyDeterminandLabel <- factor(parameters$observedPropertyDeterminandLabel)
parameters$observedPropertyDeterminandCode <- factor(parameters$observedPropertyDeterminandCode)

summary_codes<-parameters%>%group_by(observedPropertyDeterminandLabel,observedPropertyDeterminandCode)%>%
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
#and in addition to those by Geoff for FW:
  # Turbidity EEA_3112-01-4
  # Dissolved organic carbon (DOC) EEA_3133-05-9
  # Total organic carbon (TOC) EEA_3133-06-0
  # Nitrite CAS_14797-65-0
  # Total dissolved solids EEA_31-03-8
  # Total organic nitrogen EEA_3161-03-3

#dropped:
# Particulate organic nitrogen EEA_3161-04-4
# BOD7 EEA_3133-02-6
# Carbonate CAS_3812-32-6
# Calcium CAS_7440-70-2
# Water temperature EEA_3121-01-5
# Total nitrogen to total phosphorus ratio EEA_3164-07-6
# Nitrate to orthophosphate ratio EEA_3164-08-7

#Create a list of determinand codes of interest
detUsed <- c("EEA_3152-01-0","EEA_3164-01-0","EEA_3111-01-1","CAS_14797-55-8",
             "CAS_14798-03-9","CAS_7723-14-0","EEA_31-02-7","EEA_3133-01-5",
             "EEA_3131-01-9","CAS_14265-44-2","CAS_16887-00-6","EEA_3132-01-2",
             "EEA_31615-01-7","EEA_31613-01-1","EEA_3161-05-5","EEA_3141-01-3",
             "EEA_3161-02-2","CAS_7664-41-7","EEA_3153-02-4",
             "EEA_3112-01-4","EEA_3133-05-9","EEA_3133-06-0",
             "EEA_31-03-8","EEA_3161-03-3","CAS_14797-65-0")

#Then, below select the TW and CW data and these codes and aggregate the data using a mean.

### Select TW disagregated data and summarise----
dat_WQ_TW<- lDataFrames[[2]] %>%
  select(monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,resultUom,phenomenonTimeSamplingDate,parameterSampleDepth,sampleIdentifier,resultObservedValue) %>% 
  filter(parameterWaterBodyCategory == "TW" & observedPropertyDeterminandCode %in% detUsed) %>% 
  mutate(phenomenonTimeReferenceYear = as.numeric(substr(phenomenonTimeSamplingDate,1,4))) %>% 
  group_by(monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,resultUom,phenomenonTimeReferenceYear,parameterSampleDepth) %>%
  summarise(resultMeanValue = mean(resultObservedValue, na.rm=TRUE),
            resultMinimumValue = min(resultObservedValue, na.rm=TRUE),
            resultMaximumValue = max(resultObservedValue, na.rm=TRUE),
            resultNumberOfSamples = n()) %>% 
  mutate(metadata_versionId = "Waterbase_v2021_1_WISE6_DisaggregatedData.sqlite")

#left_join spatial data info
dat_WQ_TW.test2 <- left_join(dat_WQ_TW,spatial_dat_shrt,by=c("monitoringSiteIdentifier")) # n stays the same OK

#check for missing spatial coordinates
dat_WQ_TW.test2%>%filter(is.na(lat))%>%
  group_by(countryCode)%>%
  count()
# countryCode      n
# 1 BG              2
# 2 ES              9
# 3 IT             10
# 4 UK            478
# 5 NA             33

dat_WQ_TW.test2%>%filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count()%>%
  print(n=16)
# monitoringSiteIdentifier       n
# 1 BG60000976                   2 no lat in spatial_dat_shrt
# 2 ES040ESPF000400127           9 no lat
# 3 IT05EC_Ve-8                 11 error in name
# 4 IT05ENC4_Ve-6               11 error in name
# 5 IT05PNC1_Ve-1               11 error in name
# 6 IT09-MAT-P077                3 no lat
# 7 IT09-MAT-P084                3 no lat
# 8 IT09-MAT-P210                2 no lat
# 9 IT09-MAT-P212                2 no lat
# 10 UKEA_WIMS_SO_F0002151       64 no lat
# 11 UKEA_WIMS_SW_81930101       64 no lat
# 12 UKEA_WIMS_SW_E1008300       65 no lat
# 13 UKEA_WIMS_TH_PCRR0025       59 no lat
# 14 UKEA_WIMS_TH_PRGR0030       67 no lat
# 15 UKEA_WIMS_TH_PRGR0081       68 no lat
# 16 UKEA_WIMS_TH_PTHR0107       91 no lat


#correct samples name mismatch to get lat/lon in left join
# italian: correct names in from dat_WQ_TW
# 3 IT05EC_Ve-8                 11
# 4 IT05ENC4_Ve-6               11
# 5 IT05PNC1_Ve-1               11
#to:
# 1 IT05EC_VE-8 
# 2 IT05ENC4_VE-6  
# 3 IT05PNC1_VE-1 

dat_WQ_TW <- dat_WQ_TW %>% mutate(monitoringSiteIdentifier=
                       case_when(monitoringSiteIdentifier == "IT05EC_Ve-8" ~ "IT05EC_VE-8",
                                 monitoringSiteIdentifier == "IT05ENC4_Ve-6" ~ "IT05ENC4_VE-6",
                                 monitoringSiteIdentifier == "IT05PNC1_Ve-1"~"IT05PNC1_VE-1",
                                 TRUE ~ as.character(monitoringSiteIdentifier)))

#re-check
dat_WQ_TW%>%
  group_by(monitoringSiteIdentifier)%>%
  count() #n=593 OK

#left_join spatial data info
dat_WQ_TW.spatial <- left_join(dat_WQ_TW,spatial_dat_shrt,by=c("monitoringSiteIdentifier")) # n stays the same OK

#re-check
dat_WQ_TW.spatial%>%filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count() #n=13 OK

#in case monitoring site id not available check if ok to use thematicIdIdentifier
#dat_WQ_TW.test3 <- left_join(dat_WQ_TW,spatial_dat_shrt,by=c("monitoringSiteIdentifier", is.na("monitoringSiteIdentifier")=="thematicIdIdentifier" ))

dat_WQ_TW.spatial$Created <- Sys.Date()
dat_WQ_TW.spatial <- dat_WQ_TW.spatial %>% ungroup()
saveRDS(dat_WQ_TW.spatial,file = here("Data","dat_WQ_TW.rds"))

### Select CW disaggregated data and summarise---- 
dat_WQ_CW <- lDataFrames[[2]] %>%
  select(monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,resultUom,phenomenonTimeSamplingDate,parameterSampleDepth,sampleIdentifier,resultObservedValue) %>% 
  filter(parameterWaterBodyCategory == "CW" & observedPropertyDeterminandCode %in% detUsed) %>% 
  mutate(phenomenonTimeReferenceYear = as.numeric(substr(phenomenonTimeSamplingDate,1,4))) %>% 
  group_by(monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,resultUom,phenomenonTimeReferenceYear,parameterSampleDepth) %>%
  summarise(resultMeanValue = mean(resultObservedValue, na.rm=TRUE),
            resultMinimumValue = min(resultObservedValue, na.rm=TRUE),
            resultMaximumValue = max(resultObservedValue, na.rm=TRUE),
            resultNumberOfSamples = n()) %>% 
  mutate(metadata_versionId = "Waterbase_v2021_1_WISE6_DisaggregatedData.sqlite")

#search e.g. for transparency data
library(readr)
test <- read_rds(here("Data","dat_WQ_CW.rds")) 
test %>%
  filter(observedPropertyDeterminandLabel == "Secchi depth")%>%
  group_by(countryCode)%>%
  count()

#CW
# countryCode     n
# 1 EE             25
# 2 ES             16
# 3 IT           1801
# 4 LT              1
# 5 MT             77
# 6 NO             87
# 7 PT             63
# 8 NA              1 #na's

#TW
#countryCode     n
# BE              3
# BG             16
# ES             36
# IT           1057
# LT             67
# PT            185

#check for SD in CW but now only for year corresponding to WFD 2nd cycle 2016
test %>%
  filter(observedPropertyDeterminandLabel == "Secchi depth")%>%
  filter(phenomenonTimeReferenceYear >2009 & phenomenonTimeReferenceYear <2016) %>% 
  group_by(countryCode)%>%
  count()
  
#Correct!!!
# countryCode      n
# 1 IT            506
# 2 LT              1
# 3 PT             44

test2 <- read_rds(here("Data","dat_WQ_TW.rds")) 
#check for Oxygen in TW but now only for year corresponding to WFD 2nd cycle 2016
test2 %>%
  filter(observedPropertyDeterminandLabel == "Oxygen saturation")%>% #Oxygen saturation; Dissolved oxygen
  filter(phenomenonTimeReferenceYear >2009 & phenomenonTimeReferenceYear <2016) %>% 
  group_by(countryCode)%>%
  count()

#DO in TW in 2nd cycle years
# countryCode     n
# 1 BE              9
# 2 BG             17
# 3 HR              5
# 4 IT            182
# 5 LT             29
# 6 PT            105
# 7 UK             26

#O saturation in TW in 2nd cycle years
# countryCode     n
# 1 BE              9
# 2 BG             14
# 3 FR             15
# 4 HR              5
# 5 IT            279
# 6 LT             29
# 7 PT             53
# 8 UK             26

#left_join spatial data info
dat_WQ_CW.spatial<- left_join(dat_WQ_CW,spatial_dat_shrt,by=c("monitoringSiteIdentifier")) # n stays the same OK

#check for missing spatial coordinates
dat_WQ_CW.spatial%>%filter(is.na(lat))%>%
  group_by(countryCode)%>%
  count()
# countryCode     n
# 1 IT              8
# 2 NA             16

dat_WQ_CW.spatial%>%filter(is.na(lat))%>%
  group_by(monitoringSiteIdentifier)%>%
  count()
# monitoringSiteIdentifier       n
# 1 IT07MA00971                 16 no lat in spatial_dat_shrt
# 2 IT09-MAT-P087                3 no lat in spatial_dat_shrt
# 3 IT09-MAT-P104                2 no lat in spatial_dat_shrt
# 4 IT15-VES8                    3 no lat in spatial_dat_shrt

#in case monitoring site id not available check if ok to use thematicIdIdentifier
#dat_WQ_CW.test3 <- left_join(dat_WQ_CW,spatial_dat_shrt,by=c("monitoringSiteIdentifier", is.na("monitoringSiteIdentifier")=="thematicIdIdentifier" ))

dat_WQ_CW.spatial$Created <- Sys.Date()
dat_WQ_CW.spatial <- dat_WQ_CW.spatial %>% ungroup()
saveRDS(dat_WQ_CW.spatial,file = here("Data","dat_WQ_CW.rds"))

### Create a list of dets, write to Excel for reference----

TW.dets.summary<-dat_WQ_TW.spatial%>%group_by(observedPropertyDeterminandLabel,observedPropertyDeterminandCode,resultUom)%>%
  count()%>%
  arrange(desc(n))%>%
  print(n=24)

CW.dets.summary<-dat_WQ_CW.spatial%>%group_by(observedPropertyDeterminandLabel,observedPropertyDeterminandCode,resultUom)%>%
  count()%>%
  arrange(desc(n))%>%
  print(n=23)

library(openxlsx)
write.xlsx(TW.dets.summary,here("Data","DetList-TW.xlsx"))
write.xlsx(CW.dets.summary,here("Data","DetList-CW.xlsx"))

### get summary of years available per Water Category ----
dat_WQ_CW <- readRDS(here("Data","dat_WQ_CW.rds"))
summary(as.factor(dat_WQ_CW$phenomenonTimeReferenceYear))
# 2000  2001  2002  2003  2004  2007  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019  2020  2021 
#   2     3     2     2     2     8    72   336   265     5     6   7236  7180  5201  5632  10970 6514  7828  1156 

dat_WQ_TW <- readRDS(here("Data","dat_WQ_TW.rds"))
summary(as.factor(dat_WQ_TW$phenomenonTimeReferenceYear))
# 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 
#  9    8   42   14   13   11   10   21   12   167  498  152  116  334  1599 1672 1370 3071 3838 4125 3303    6 

## Select CW aggregated data by monitoring site and summarise---- 
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
# "BG" "DE" "ES" "FR" "HR" "IT" "LV" "MT" "NL" "NO" "RO" "UK" ; data for n=12 countries
levels(as.factor(WISE6_WQ_AggSite.TRaC$phenomenonTimeReferenceYear))
# [1] "1971" "1972" "1973" "1974" "1975" "1976" "1977" "1978" "1979" "1980" "1981" "1982"
# [13] "1983" "1984" "1985" "1986" "1987" "1988" "1989" "1990" "1991" "1992" "1993" "1994"
# [25] "1995" "1996" "1997" "1998" "1999" "2000" "2001" "2002" "2003" "2004" "2005" "2006"
# [37] "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018"
# [49] "2019" "2020"

#search e.g. for transparency data
WISE6_WQ_AggSite.TRaC %>%
  filter(observedPropertyDeterminandLabel == "Secchi depth")%>%
  group_by(countryCode)%>%
  count()
# countryCode     n
# IT             15

#aggregated by WB
## Select CW aggregated data by waterbody and summarise---- 
WISE6_WQ_AggWB <- read.csv(file = here("Databases","waterbase_t_wise6_aggregateddatabywaterbody.csv"),sep = ";") #all water categories
#check water categories
levels(as.factor(WISE6_WQ_AggWB$parameterWaterBodyCategory)) #ONLY GW !!