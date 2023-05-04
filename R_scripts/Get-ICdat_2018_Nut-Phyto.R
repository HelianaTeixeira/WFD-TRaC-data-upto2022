#Merge IC datasets per common types

#BC4
### Get the BC4 NUT (TP & TN) & Phyto data (ds5: Latvia and ds6: Estonia) 
ds5 <- read.csv(file = here("Databases","ds5_CWBALBC4.csv"),sep = ";")
ds6 <- read.csv(file = here("Databases","ds6_CWBALBC4.csv"),sep = ";") 
# Drop columns of the dataframe ds5
ds5 <- select (ds5,-c(log.TP,log.TN,NPratio,NPratioclass))
#merge datasets
BC4dat<- full_join(ds5,ds6)
# Export to CSV
write.csv(BC4dat,file=here("Databases","IC_BC4_NutTP-TN_Phyto.csv"))

#BT1
### Get the BT1 NUT (TP & TN) & Phyto data (ds1: Lithuania and ds2: Poland) 
ds1 <- read.csv(file = here("Databases","ds1_TRWBALBT1a.csv"),sep = ";")
ds2 <- read.csv(file = here("Databases","ds2_TRWBALBT1b.csv"),sep = ";") 
# Drop columns of the dataframes
ds1 <- select (ds1,-c("log.TN" ,"log.TP","TNPredEQR_mod1","TNResid_mod1",
                      "TNPredEQR_mod2","TNResid_mod2","TNPredEQR_mod3","TNResid_mod3",
                      "TPPredEQR_mod1","TPResid_mod1", "TPPredEQR_mod2","TPResid_mod2",
                      "TPPredEQR_mod3","TPResid_mod3","NPratio"))

ds2 <- select (ds2,-c("TNPredEQR_mod1","TNResid_mod1","TNPredEQR_mod2",
                      "TNResid_mod2","TNPredEQR_mod3", "TNResid_mod3" ,
                      "TPPredEQR_mod1" ,"TPResid_mod1" , "TPPredEQR_mod2",
                      "TPResid_mod2" ,"TPPredEQR_mod3","TPResid_mod3",
                      "log.TN" ,"log.TP","NPratio"))

#merge datasets
BT1dat<- full_join(ds1,ds2)
# Export to CSV
write.csv(BT1dat,file=here("Databases","IC_BT1_NutTP-TN_Phyto.csv"))

#MED TypeIIA Adriatic
### Get the MEDII NUT (all nut) & Phyto data (ds10: Italy) 
ds10 <- read.csv(file = here("Databases","ds10_CWMEDII.csv"),sep = ";")
# Select columns of the dataframe ds10
ds10 <- select (ds10,c("UNIQUEID","Wcategory","GIG" ,"Type" , "Country","sample_depth",
                       "Region" ,"Site_name","StationCode", "Date","Chla","Ammonia",
                       "Nitrates" ,"Nitrites" ,"Total_Nitrogen","Exclude_N" ,"Ortophosphates",
                       "Total_Phosphorus" ,"Exclude_P",  "EQR_Chla","EQS"))

# Export to CSV
write.csv(ds10,file=here("Databases","IC_TypeIIAAdriatic_Nut_Phyto.csv"))

