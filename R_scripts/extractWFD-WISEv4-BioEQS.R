# Extract tables from EEA WISE WFD SQLite database
# version: WISE SOW v01_r04 
# author: Heliana Teixeira
# date created: 19.09.2022

#Description of dataset
#1st dataset: Extracting biology data from the EEA Water Framework Directive Database (WISE_WFD)
#version: WISE SOW v01_r04 or wise-wfd-database_v01_r04 , last modified 01 Oct 2021
#source: https://www.eea.europa.eu/data-and-maps/data/wise-wfd-4 
#contains: data from 1st (2010) and 2nd (2016) River Basin Management Plans (EQS only - categorical).

#note for future: 
# The WISE-4 data flow is no longer active. 
# It has been superseded by WISE-6 for Water quality, and by WISE-2 for Biology reporting. 

#load packages
library(here)
library(tidyverse)
library(RSQLite)
library(openxlsx)

#extract 1st dataset
#### Mapping the original database tables into a list of separate data frames----
## connect to db
con1 <- dbConnect(drv=RSQLite::SQLite(), here("Databases", "WISE_SOW.sqlite")) #edit file name: dbname="WISE_SOW.sqlite"

## inspect all tables it contains
dbListTables(con1) #list tables' name

## Explore tables of interest
# tables 1 to 14 on ground water - ignore
# tables 15 to 31 on surface water:
  # [15] "SOW_SWB_FailingRBSP"                              "SOW_SWB_FailingRBSPOther"                        
  # [17] "SOW_SWB_QE_qeEcologicalExemptionType"             "SOW_SWB_QualityElement"                          
  # [19] "SOW_SWB_SWB_hmwbPhysicalAlteration"               "SOW_SWB_SWB_hmwbWaterUse"                        
  # [21] "SOW_SWB_SWB_surfaceWaterBodyIntercalibrationType" "SOW_SWB_SWB_swSignificantImpactType"             
  # [23] "SOW_SWB_SWB_swSignificantPressureType"            "SOW_SWB_SWE_swEcologicalExemptionPressure"       
  # [25] "SOW_SWB_SWEcologicalExemptionType"                "SOW_SWB_SWP_SWC_swChemicalExemptionPressure"     
  # [27] "SOW_SWB_SWP_SWChemicalExemptionType"              "SOW_SWB_SWPrioritySubstance"                     
  # [29] "SOW_SWB_SurfaceWaterBody"                         "SOW_SWB_swSignificantImpactOther"                
  # [31] "SOW_SWB_swSignificantPressureOther"      
#Tables of interest to our work
  # [18] "SOW_SWB_QualityElement" #table used by Geoff to extract the FW data
  # [21] "SOW_SWB_SWB_surfaceWaterBodyIntercalibrationType"
  # [29] "SOW_SWB_SurfaceWaterBody"
#potential interest
  # [22] "SOW_SWB_SWB_swSignificantImpactType" 
  # [23] "SOW_SWB_SWB_swSignificantPressureType"
  # [30] "SOW_SWB_swSignificantImpactOther"                
  # [31] "SOW_SWB_swSignificantPressureOther" 

## explore fields in tables
dbListFields(con1,"SOW_SWB_QualityElement")
#of interest:
  #"cYear" 
  #"surfaceWaterBodyCategory"
  #"qeCode"
  #"qeMonitoringPeriod"
  #"euSurfaceWaterBodyCode"
  #"naturalAWBHMWB"

dbListFields(con1,"SOW_SWB_SWB_surfaceWaterBodyIntercalibrationType")
#of interest:
  #"euSurfaceWaterBodyCode"  
  #"surfaceWaterBodyTypeCode"
  #"surfaceWaterBodyIntercalibrationTypeCode"
  #"surfaceWaterBodyIntercalibrationType"

dbListFields(con1,"SOW_SWB_SWB_swSignificantImpactType")
#of interest:
  #"cYear"
  #"euSurfaceWaterBodyCode"
  #"swSignificantImpactType"

dbListFields(con1,"SOW_SWB_SWB_swSignificantPressureType")
#of interest:
  #"euSurfaceWaterBodyCode"  
  #"cYear"
  #"swSignificantPressureType"
  #"swSignificantPressureTypeGroup"

dbListFields(con1,"SOW_SWB_swSignificantImpactOther")
#of interest:
  # "euSurfaceWaterBodyCode"  
  # "cYear"
  # "swSignificantImpactType"
  # "swSignificantImpactOther"

dbListFields(con1,"SOW_SWB_swSignificantPressureOther")
#of interest:
  # "euSurfaceWaterBodyCode"
  # "cYear"
  # "swSignificantPressureType"
  # "swSignificantPressureOther"

##then extract dataframe for each table
tables <- dbListTables(con1) 
tables <- tables[tables != "sqlite_sequence"] # exclude sqlite_sequence (contains table information)
# #Alternative 1
# lDataFrames <- vector("list", length=length(tables))
# 
# ## create a data.frame for each table
# for (i in seq(along=tables)) {
#   lDataFrames[[i]] <- dbGetQuery(conn=con1, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
# }

#Alternative 2
library(purrr)
lDataFrames <- map(tables, ~{
  dbGetQuery(conn=con1, statement=paste("SELECT * FROM '", .x, "'", sep=""))
})

#run dbDisconnect() to disconnect from a database
dbDisconnect(con1)

### Inspect tables ----
str(lDataFrames[[18]]) # "SOW_SWB_QualityElement" 
# 'data.frame':	4188366 obs. of  20 variables:
# $ id                                : int  1 2 3 4 5 6 7 8 9 10 ...
# $ cYear                             : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
# $ countryCode                       : chr  "SE" "SE" "SE" "SE" ...
# $ euRBDCode                         : chr  "SE3" "SE3" "SE3" "SE3" ...
# $ euSubUnitCode                     : chr  "SE3" "SE3" "SE3" "SE3" ...
# $ euSurfaceWaterBodyCode            : chr  "SE667135-161090" "SE667135-161090" "SE667135-161090" "SE667135-161090" ...
# $ surfaceWaterBodyCategory          : chr  "LW" "LW" "LW" "LW" ...
# $ naturalAWBHMWB                    : chr  "Natural water body" "Natural water body" "Natural water body" "Natural water body" ...
# $ surfaceWaterBodyTypeCode          : chr  "S4SSYY" "S4SSYY" "S4SSYY" "S4SSYY" ...
# $ swEcologicalStatusOrPotentialValue: chr  "3" "3" "3" "3" ...
# $ swChemicalStatusValue             : chr  "3" "3" "3" "3" ...
# $ qeCode                            : chr  "QE1-2-3 - Macrophytes" "QE1-3 - Benthic invertebrates" "QE2 - Hydromorphological quality elements" "QE1-2-2 - Angiosperms" ...
# $ qeStatusOrPotentialValue          : chr  "Unknown" "Unknown" "Unknown" "Unknown" ...
# $ qeMonitoringResults               : chr  "Not in WFD2010" "Not in WFD2010" "Not in WFD2010" "Not in WFD2010" ...
# $ qeMonitoringPeriod                : chr  "Not in WFD2010" "Not in WFD2010" "Not in WFD2010" "Not in WFD2010" ...
# $ qeStatusOrPotentialChange         : chr  "Not in WFD2010" "Not in WFD2010" "Not in WFD2010" "Not in WFD2010" ...
# $ qeStatusOrPotentialComparability  : chr  "Not in WFD2010" "Not in WFD2010" "Not in WFD2010" "Not in WFD2010" ...
# $ cArea                             : num  3.27 3.27 3.27 3.27 3.27 ...
# $ cLength                           : num  NA NA NA NA NA ...
# $ wiseEvolutionType                 : chr  "change" "change" "change" "change" ...

levels(factor(lDataFrames[[18]]$qeMonitoringPeriod)) #take into account monitoring period 

str(lDataFrames[[21]]) # "SOW_SWB_SWB_surfaceWaterBodyIntercalibrationType"
#this table has the codes to link WB to typology

# 'data.frame':	184486 obs. of  16 variables:
#  $ id                                      : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ cYear                                   : int  2016 2016 2016 2016 2016 2016 2016 2016 2016 2016 ...
#  $ countryCode                             : chr  "FI" "FI" "FI" "FI" ...
#  $ euRBDCode                               : chr  "FIVHA1" "FIVHA1" "FIVHA1" "FIVHA1" ...
#  $ euSubUnitCode                           : chr  "FIVHA1" "FIVHA1" "FIVHA1" "FIVHA1" ...
#  $ euSurfaceWaterBodyCode                  : chr  "FI04_152_1_002_001" "FI04_152_1_017_001" "FI04_153_1_022_001" "FI04_153_1_030_001" ...
#  $ surfaceWaterBodyCategory                : chr  "LW" "LW" "LW" "LW" ...
#  $ naturalAWBHMWB                          : chr  "Natural water body" "Natural water body" "Natural water body" "Natural water body" ...
#  $ surfaceWaterBodyTypeCode                : chr  "Vh" "Vh" "Lm" "Ph" ...
#  $ swEcologicalStatusOrPotentialValue      : chr  "2" "2" "4" "2" ...
#  $ swChemicalStatusValue                   : chr  "3" "3" "2" "3" ...
#  $ surfaceWaterBodyIntercalibrationTypeCode: chr  "LW-L-N2a" "LW-L-N2a" "inapplicable" "LW-L-N-M 102" ...
#  $ surfaceWaterBodyIntercalibrationType    : chr  "LW-L-N2a - Northern, lowland, shallow, low alkalinity, clear" "LW-L-N2a - Northern, lowland, shallow, low alkalinity, clear" "Inapplicable" "LW-L-N-M 102 - Northern low alkalinity, humic" ...
#  $ cArea                                   : num  1.463 1.75 0.045 1.658 1.454 ...
#  $ cLength                                 : num  NA NA NA NA NA 0.453 NA NA NA NA ...
#  $ wiseEvolutionType                       : chr  "noChange" "noChange" "noChange" "noChange" ...

str(lDataFrames[[29]]) #"SOW_SWB_SurfaceWaterBody" 

str(lDataFrames[[22]]) #potential interest "SOW_SWB_SWB_swSignificantImpactType"
  #explore impact type for TraC WC
  W4_TraC_ImpactType <- lDataFrames[[22]]%>%
    filter(surfaceWaterBodyCategory == c("TW","CW")) %>%
    group_by(surfaceWaterBodyCategory,swSignificantImpactType)%>%
    summarise(count=n_distinct(id))%>%
    print(n = 29)
  
  write.xlsx(W4_TraC_ImpactType,here("Data","W4_TraC_ImpactType.xlsx"))

str(lDataFrames[[23]]) #potential interest "SOW_SWB_SWB_swSignificantPressureType"
    #explore pressure type for TraC WC
    W4_TraC_PressureType <- lDataFrames[[23]]%>%
      filter(surfaceWaterBodyCategory == c("TW","CW")) %>%
      group_by(surfaceWaterBodyCategory,swSignificantPressureType)%>%
      summarise(count=n_distinct(id))%>%
      print(n = 110)
    
    W4_TraC_PressureTypeGroup <- lDataFrames[[23]]%>%
      filter(surfaceWaterBodyCategory == c("TW","CW")) %>%
      group_by(surfaceWaterBodyCategory,swSignificantPressureTypeGroup)%>%
      summarise(count=n_distinct(id))%>%
      print(n = 23)

    write.xlsx(W4_TraC_PressureType,here("Data","W4_TraC_PressureType.xlsx"))
    write.xlsx(W4_TraC_PressureTypeGroup,here("Data","W4_TraC_PressureTypeGroup.xlsx"))

str(lDataFrames[[30]]) #potential interest "SOW_SWB_swSignificantImpactOther"
str(lDataFrames[[31]]) #potential interest "SOW_SWB_swSignificantPressureOther"

#### Get WFD data set ----
### Extract table of Biological Quality Element classification results for TRaC Waters
## 1st check Water Categories
unique(lDataFrames[[18]]$surfaceWaterBodyCategory)
# "LW","RW","CW","TW","Unpopulated"

  #identify "Unpopulated" records
  lDataFrames[[18]] %>%
    filter(surfaceWaterBodyCategory == "Unpopulated") %>%
    select(euSurfaceWaterBodyCode)%>%
    unique()
    #"UKGI6901" "UKGI6902" "UKGI6903"
 
   lDataFrames[[18]] %>%
    filter(surfaceWaterBodyCategory == "Unpopulated") %>%
    select(euSurfaceWaterBodyCode)%>%
    count() 
    # n=57
  
  #track Water Category for UK "Unpopulated" records from WB table 29
lDataFrames[[29]]%>%
  filter(euSurfaceWaterBodyCode == "UKGI6901" |
           euSurfaceWaterBodyCode == "UKGI6902" |
           euSurfaceWaterBodyCode == "UKGI6903")%>%
  select (euSurfaceWaterBodyCode,surfaceWaterBodyCategory)
  #"Unpopulated" recodrs are all UK CW data - so keep them in
  

str(lDataFrames[[18]]) #'data.frame':	4188366 obs. of  20 variables

## 2nd select TraC waters data
WDF_TRaC <- lDataFrames[[18]]%>%
  filter(surfaceWaterBodyCategory == "CW" |
           surfaceWaterBodyCategory == "TW" |
           surfaceWaterBodyCategory == "Unpopulated") 

## 3rd select the biological data
#list the available quality element codes
levels(factor(WDF_TRaC$qeCode))

#get biological quality elements (BQE start with QE1)
WDF_TRaC_BQE <-WDF_TRaC%>%
  filter(grepl("QE1",qeCode)) %>%   # get biological quality elements            
  mutate(qeStatusOrPotentialValue=as.integer(qeStatusOrPotentialValue)) %>% # convert class to integer
  separate(qeMonitoringPeriod,c("StartYear","EndYear")) %>%  # create start and end year values
  mutate(StartYear=as.numeric(StartYear),EndYear=as.numeric(EndYear)) %>% 
  filter(!is.na(qeStatusOrPotentialValue))  # remove missing classifications

WDF_TRaC_BQE %>%
  group_by(cYear)%>%
  count()
# 5567 with 2010 data only
# 7523 with 2016 data only
# 13090 observations in total with both 2010 and 2016 data (from 1st and 2nd RBMPs)

#confirm WC
levels(factor(WDF_TRaC_BQE$surfaceWaterBodyCategory)) #CW TW

#checking also SE data in WISE4 (start with QE3; not extracted) 
WDF_TRaC_SE <-WDF_TRaC %>%
  filter(grepl("QE3",qeCode)) # check available SE quality elements   

    levels(as.factor(WDF_TRaC_SE$qeCode))
    # [1] "QE3-1 - General parameters"              "QE3-1-1 - Transparency conditions"      
    # [3] "QE3-1-2 - Thermal conditions"            "QE3-1-3 - Oxygenation conditions"       
    # [5] "QE3-1-4 - Salinity conditions"           "QE3-1-5 - Acidification status"         
    # [7] "QE3-1-6-1 - Nitrogen conditions"         "QE3-1-6-2 - Phosphorus conditions"      
    # [9] "QE3-3 - River Basin Specific Pollutants"
    
    WDF_TRaC_SE %>%
      group_by(cYear)%>%
      count()
    # cYear   n
    # 2010   8446
    # 2016   52320
    # 60766 observations in total with both 2010 and 2016 data (from 1st and 2nd RBMPs)

### Get waterbody WB information----
SWB <- lDataFrames[[29]] %>% #"SOW_SWB_SurfaceWaterBody" 
  select(cYear,countryCode,euSurfaceWaterBodyCode,surfaceWaterBodyCategory,surfaceWaterBodyName)
      #excluded var not applicable to TraC waters: broaderType,broaderTypeCode,broadType,broadTypeCode
  
levels(factor(SWB$surfaceWaterBodyCategory)) #[1] "CW" , "LW" ,"RW", "TeW" , "TW", "Unpopulated"

#check WC of unpopulated
SWB %>%
  filter(surfaceWaterBodyCategory == "Unpopulated") %>%
  select(euSurfaceWaterBodyCode)
# euSurfaceWaterBodyCode
# 1 UKGI6901
# 2 UKGI6903
# 3 UKGI6902 #n= 3 the same UK WB previously identified as CW (see above)

#correct WC spellings before merge
SWB <- SWB %>%
  mutate(surfaceWaterBodyCategory = case_when(surfaceWaterBodyCategory == "Unpopulated" ~ "CW",
            TRUE ~ as.character(surfaceWaterBodyCategory)))

#check corrections to WC
levels(factor(SWB$surfaceWaterBodyCategory)) #"CW" "LW" "RW" "TeW" "TW"

SWB %>%
  group_by(surfaceWaterBodyCategory)%>%
  count() #unique  8708 CW & 2055 TW

SWB %>%
  filter(surfaceWaterBodyCategory == "TW") %>% #CW
  select(surfaceWaterBodyCategory,surfaceWaterBodyName) %>% 
  unique()

## merge BQE (n=13090, 21 var) with WB national info
dat_WDF_TRaC_BQE <- left_join(WDF_TRaC_BQE,SWB,by = c("cYear","countryCode","euSurfaceWaterBodyCode","surfaceWaterBodyCategory"))

## merge SE (n=60766, 20 var) with WB national info
dat_WDF_TRaC_SE <- left_join(WDF_TRaC_SE,SWB,by = c("cYear","countryCode","euSurfaceWaterBodyCode","surfaceWaterBodyCategory"))

### Get WB intercalibration Typology information----
SWB_IC <- lDataFrames[[21]] %>% #"SOW_SWB_SWB_surfaceWaterBodyIntercalibrationType" 
  select(cYear,countryCode,euSurfaceWaterBodyCode,surfaceWaterBodyCategory,
         surfaceWaterBodyIntercalibrationTypeCode,surfaceWaterBodyIntercalibrationType)

levels(factor(SWB_IC$surfaceWaterBodyCategory)) #[1] "CW" , "LW" ,"RW",  "TW", "Unpopulated"

#check WC of unpopulated
SWB_IC %>%
  filter(surfaceWaterBodyCategory == "Unpopulated") %>%
  select(euSurfaceWaterBodyCode)
# euSurfaceWaterBodyCode
# 1 UKGI6901
# 2 UKGI6903
# 3 UKGI6902 #n= 3 the same UK WB previously identified as CW (see above)

SWB_IC %>%
  filter(surfaceWaterBodyCategory == "Unpopulated") %>%
  count() #n = 3 assumed CW

#correct WC spellings before merge
SWB_IC <- SWB_IC %>%
  mutate(surfaceWaterBodyCategory = case_when(surfaceWaterBodyCategory == "Unpopulated" ~ "CW",
                                              TRUE ~ as.character(surfaceWaterBodyCategory)))

#check corrections to WC
levels(factor(SWB_IC$surfaceWaterBodyCategory)) #"CW" "LW" "RW" "TW"

dat_WDF_TRaC_BQE%>%
  select(id)%>%
  unique()%>%
  count() # 13090 unique ids

dat_WDF_TRaC_SE%>%
  select(id)%>%
  unique()%>%
  count() # 60766 unique ids

## merge BQE (n=13090, 22 var) with IC Type info
dat_WDF_TRaC_BQE_type <- left_join(dat_WDF_TRaC_BQE,SWB_IC,by=c("cYear","countryCode","euSurfaceWaterBodyCode","surfaceWaterBodyCategory"))
# final obs in dataset (n=13095, 24 var)

#check for the 5 duplicates
dat_WDF_TRaC_BQE_type %>%
  group_by(id) %>%
  filter(n()>1) %>%
  summarize(n=n())

# merge creates 5 duplicates:
#     id        n
# 1 1806855     2
# 2 1873913     2
# 3 1897280     2
# 4 1961857     2
# 5 1962447     2

#checking - are Polish data - one WB attributed to two common IC types simultaneously
dat_WDF_TRaC_BQE_type %>% filter(id==1806855)
#   surfaceWaterBodyIntercalibrationTypeCode  surfaceWaterBodyIntercalibrationType
# 1                                   CW-BC5  CW-BC5 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 6-12 psu, Exposed, <90 ice days
# 2                                   CW-BC7  CW-BC7 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 8-11 psu, Exposed, <90 ice days

dat_WDF_TRaC_BQE_type %>% filter(id==1873913)
#   surfaceWaterBodyIntercalibrationTypeCode  surfaceWaterBodyIntercalibrationType
# 1                                   CW-BC7  CW-BC7 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 8-11 psu, Exposed, <90 ice days
# 2                                   CW-BC5  CW-BC5 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 6-12 psu, Exposed, <90 ice days

dat_WDF_TRaC_BQE_type %>% filter(id==1897280)
#   surfaceWaterBodyIntercalibrationTypeCode  surfaceWaterBodyIntercalibrationType
# 1                                   CW-BC7  CW-BC7 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 8-11 psu, Exposed, <90 ice days
# 2                                   CW-BC5  CW-BC5 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 6-12 psu, Exposed, <90 ice days

dat_WDF_TRaC_BQE_type %>% filter(id==1961857)
#   surfaceWaterBodyIntercalibrationTypeCode  surfaceWaterBodyIntercalibrationType
# 1                                   CW-BC5  CW-BC5 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 6-12 psu, Exposed, <90 ice days
# 2                                   CW-BC7  CW-BC7 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 8-11 psu, Exposed, <90 ice days

dat_WDF_TRaC_BQE_type %>% filter(id==1962447)
#   surfaceWaterBodyIntercalibrationTypeCode  surfaceWaterBodyIntercalibrationType
# 1                                   CW-BC7  CW-BC7 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 8-11 psu, Exposed, <90 ice days
# 2                                   CW-BC5  CW-BC5 - Baltic Sea, surface water salinity 6-8 psu, bottom water salinity 6-12 psu, Exposed, <90 ice days

dat_WDF_TRaC_BQE_type$Created <- (Sys.Date()) # add date of creation, 25 variables

## merge SE (n=60766, 21 var) with IC Type info
dat_WDF_TRaC_SE_type <- left_join(dat_WDF_TRaC_SE,SWB_IC,by=c("cYear","countryCode","euSurfaceWaterBodyCode","surfaceWaterBodyCategory"))
# final obs in dataset (n=60790, 23 var)

#check for the 24 duplicates
dat_WDF_TRaC_SE_type %>%
  group_by(id) %>%
  filter(n()>1) %>%
  summarize(n=n())

  #duplicates are Polish data as in BQE data, see one example below
  dat_WDF_TRaC_SE_type %>% filter(id==1788981)

dat_WDF_TRaC_SE_type$Created <- (Sys.Date()) # add date of creation, 24 variables
  
#IC Typology count (id inapplicable)
types_count_BQE <- dat_WDF_TRaC_BQE_type%>%
  group_by(surfaceWaterBodyIntercalibrationTypeCode)%>%
  count() %>%
  print(n = 31)#31

types_count_SE <- dat_WDF_TRaC_SE_type%>%
  group_by(surfaceWaterBodyIntercalibrationTypeCode)%>%
  count()%>%
  print(n = 32)#32

write.xlsx(types_count_BQE,here("Data","W4_ICtypesTRaC_withEQS.xlsx"))
write.xlsx(types_count_SE,here("Data","W4_ICtypesTRaC_withSE.xlsx"))

#note on IC types:
# 2098 inapplicable
# 5567 NA - same n as the 2010 assessment - check coincidence!   
dat_WDF_TRaC_BQE_type%>%filter(is.na(surfaceWaterBodyIntercalibrationTypeCode))%>%
  count()
#keep NA's and do not change to "inapplicable" for control of original input

###Generate new datasets ----
#for BQE
dat_W4_TraC_BQE <- dat_WDF_TRaC_BQE_type
dat_W4_TW_BQE <- dat_WDF_TRaC_BQE_type %>% filter(surfaceWaterBodyCategory=="TW")
dat_W4_CW_BQE <- dat_WDF_TRaC_BQE_type %>% filter(surfaceWaterBodyCategory=="CW")
#for SE
dat_W4_TraC_SE <- dat_WDF_TRaC_SE_type

TRaC.BQE.overview<-print(table(dat_W4_TraC_BQE$countryCode,dat_W4_TraC_BQE$surfaceWaterBodyCategory))
TW.BQE.overview<-print(table(dat_W4_TW_BQE$countryCode,dat_W4_TW_BQE$qeCode))
CW.BQE.overview<-print(table(dat_W4_CW_BQE$countryCode,dat_W4_CW_BQE$qeCode))

TRaC.SE.overview <-print(table(dat_W4_TraC_SE$countryCode,dat_W4_TraC_SE$surfaceWaterBodyCategory))
# 24 unpopulated entries from UK to be corrected

#identify "Unpopulated" records in SE
lDataFrames[[18]] %>%
  filter(grepl("QE3",qeCode)) %>%   # get SE     
  filter(surfaceWaterBodyCategory == "Unpopulated") %>%
  select(euSurfaceWaterBodyCode)%>%
  unique()
#"UKGI6901" "UKGI6902" "UKGI6903"

#correct WC attributions in SE selected data
dat_W4_TraC_SE <- dat_W4_TraC_SE %>%
  mutate(surfaceWaterBodyCategory = case_when(surfaceWaterBodyCategory == "Unpopulated" ~ "CW",
                                              TRUE ~ as.character(surfaceWaterBodyCategory)))
#check
TRaC.SE.overview 

TRaC.SE.overview_code <-print(table(dat_W4_TraC_SE$countryCode,dat_W4_TraC_SE$qeCode))

write.xlsx(TRaC.BQE.overview,here("Data","W4TRaC-BQE-overview.xlsx"))
write.xlsx(TW.BQE.overview,here("Data","W4TW-BQE-overview.xlsx"))
write.xlsx(CW.BQE.overview,here("Data","W4CW-BQE-overview.xlsx"))
write.xlsx(TRaC.SE.overview_code,here("Data","W4TRaC-SE-overview.xlsx"))

saveRDS(dat_W4_TraC_BQE,file = here("Data","dat_W4_TraC_BQE.rds"))
saveRDS(dat_W4_TW_BQE,file = here("Data","dat_W4_TW_BQE.rds"))
saveRDS(dat_W4_CW_BQE,file = here("Data","dat_W4_CW_BQE.rds"))
saveRDS(dat_W4_TraC_SE,file = here("Data","dat_W4_TraC_SE.rds"))
