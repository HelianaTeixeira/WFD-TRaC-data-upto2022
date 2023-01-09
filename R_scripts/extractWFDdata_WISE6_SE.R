# Extract tables from EEA WISE SQLite database
# version: WISE6 WQ ICM
# author: Heliana Teixeira
# date: 14.12.2022

#load packages
library(here)
library(tidyverse)
library(RSQLite)

#### Mapping the original database table(s) into a list of separate data frames----
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

#select the TW and CW data and these codes and aggregate the data using a mean.

#Select TW data and summarise
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

dat_WQ_TW$Created <- Sys.Date()
dat_WQ_TW <- dat_WQ_TW %>% ungroup()
saveRDS(dat_WQ_TW,file = here("Data","dat_WQ_TW.rds"))


#Select CW data and summarise
dat_WQ_CW<- lDataFrames[[2]] %>%
  select(monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,resultUom,phenomenonTimeSamplingDate,parameterSampleDepth,sampleIdentifier,resultObservedValue) %>% 
  filter(parameterWaterBodyCategory == "CW" & observedPropertyDeterminandCode %in% detUsed) %>% 
  mutate(phenomenonTimeReferenceYear = as.numeric(substr(phenomenonTimeSamplingDate,1,4))) %>% 
  group_by(monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,resultUom,phenomenonTimeReferenceYear,parameterSampleDepth) %>%
  summarise(resultMeanValue = mean(resultObservedValue, na.rm=TRUE),
            resultMinimumValue = min(resultObservedValue, na.rm=TRUE),
            resultMaximumValue = max(resultObservedValue, na.rm=TRUE),
            resultNumberOfSamples = n()) %>% 
  mutate(metadata_versionId = "Waterbase_v2021_1_WISE6_DisaggregatedData.sqlite")

dat_WQ_CW$Created <- Sys.Date()
dat_WQ_CW <- dat_WQ_CW %>% ungroup()
saveRDS(dat_WQ_CW,file = here("Data","dat_WQ_CW.rds"))

#Create a list of dets, write to Excel for reference
library(openxlsx)
write.xlsx(summary_codes,here("Data","DetList.xlsx"))
