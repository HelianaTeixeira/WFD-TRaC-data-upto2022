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
tables <- dbListTables(con2) 

library(purrr)
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
parameters<-lDataFrames[[2]] %>% select(observedPropertyDeterminandCode,observedPropertyDeterminandLabel)%>%
  group_by()

CAS_14797-55-8                                   Nitrate "CAS_14797-55-8"
CAS_14797-65-0                                   Nitrite
CAS_14798-03-9                                  Ammonium "CAS_14798-03-9"
CAS_16887-00-6                                  Chloride "CAS_16887-00-6"
EEA_3132-01-2                          Dissolved oxygen "EEA_3132-01-2"
EEA_3152-01-0                                        pH "EEA_3152-01-0"
CAS_7723-14-0                          Total phosphorus "CAS_7723-14-0"
EEA_31-03-8                    Total dissolved solids



"EEA_3164-01-0","EEA_3111-01-1","EEA_31-02-7","EEA_3133-01-5",
"EEA_3131-01-9","CAS_14265-44-2",
"EEA_31615-01-7","EEA_31613-01-1","EEA_3161-05-5","EEA_3141-01-3",
"EEA_3161-02-2","CAS_7664-41-7","EEA_3153-02-4



levels(factor(lDataFrames[[2]]$observedPropertyDeterminandCode)) 
levels(factor(lDataFrames[[2]]$observedPropertyDeterminandLabel))

#Create a list of determinand codes of interest
detUsed <- c("EEA_3152-01-0","EEA_3164-01-0","EEA_3111-01-1","CAS_14797-55-8",
             "CAS_14798-03-9","CAS_7723-14-0","EEA_31-02-7","EEA_3133-01-5",
             "EEA_3131-01-9","CAS_14265-44-2","CAS_16887-00-6","EEA_3132-01-2",
             "EEA_31615-01-7","EEA_31613-01-1","EEA_3161-05-5","EEA_3141-01-3",
             "EEA_3161-02-2","CAS_7664-41-7","EEA_3153-02-4")

#select the TW and CW data and these codes and aggregate the data using a mean.

#Select TW data and summarise
dat_WQ_TW<- DisAggData %>%
  select(monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,resultUom,phenomenonTimeSamplingDate,parameterSampleDepth,sampleIdentifier,resultObservedValue) %>% 
  filter(parameterWaterBodyCategory == "TW" & observedPropertyDeterminandCode %in% detUsed) %>% 
  mutate(phenomenonTimeReferenceYear = as.numeric(substr(phenomenonTimeSamplingDate,1,4))) %>% 
  group_by(monitoringSiteIdentifier,monitoringSiteIdentifierScheme,parameterWaterBodyCategory,observedPropertyDeterminandCode,observedPropertyDeterminandLabel,procedureAnalysedMatrix,resultUom,phenomenonTimeReferenceYear,parameterSampleDepth) %>%
  summarise(resultMeanValue = mean(resultObservedValue, na.rm=TRUE),
            resultMinimumValue = min(resultObservedValue, na.rm=TRUE),
            resultMaximumValue = max(resultObservedValue, na.rm=TRUE),
            resultNumberOfSamples = n()) %>% 
  mutate(metadata_versionId = "Waterbase_v2020_1_WISE6_DisaggregatedData.sqlite") %>% 
  collect()

#Select CW data and summarise
