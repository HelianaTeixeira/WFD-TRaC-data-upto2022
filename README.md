
# ECOSTAT - Water Quality Data

## WFD Transitional and Coastal waters (TRaC) data up to 2022

This work was developed within the Water Framework Directive (WFD) Common Implementation Strategy, under the ECOSTAT Working Group. This repository contains the files necessary to assemble Biological (BQE) and Supporting (SE) Quality Elements data from the EU MS national reporting obligations for the WFD. The resulting data will be used to evalute current national boundaries to support good ecological status and to derived methods to improve the current Best Practice Guide (BPG) Toolkit to support SE boundaries establishment. The ultimate goal is to update the BPG and help Member States adjusting and/or deriving their nutrients and other supporting elements' boundaries as they see fit.

- author: Heliana Teixeira
- contacts: heliana.teixeira@ua.pt
- date created: 2022.10.20
- updated: 2025.11.12

Assembling Water Quality data from: 
* WISE4 (identifier: eea_wise-wfd_s) WFD Biological Quality Elements (EQS categorical) (WFD 1st (<2010) & 2nd (<2016) cycles; SE info available in WISE4 was also extracted; note: TraC data in this source spans from 2000 to 2018);
* WISE2 (identifier: eea_waterbase-biology_s) EEA Waterbase – Biology (including normalised EQRs) (span: 1990 to 2022);
* WISE6 (identifier: eea_waterbase-water-quality-icm_s) WFD Water Quality Supporting Elements (span: 1899 to 2024).

note1: For the third WFD reporting cycle (cYear 2022), WISE4 has been superseded by WISE-6 for Water quality, and by WISE-2 for Biology.
note2: Access to data sources above was verified by November 2025.

### Links to WFD data sources:

- **WISE4:** Extracting data from 1st (cYear 2010) and 2nd (cYear 2016) River Basin Management Plans; for Biological status classifications (only categorical EQS) at WB level and correspondent SE data,are available. The WISE-4 data flow is no longer active. It has been superseded by WISE-6 for Water quality, and by WISE-2 for Biology reporting.
source: https://www.eea.europa.eu/data-and-maps/data/wise-wfd-4
version: WISE SOW v01_r04 or wise-wfd-database_v01_r04 , last modified 01 Oct 2021

- **WISE2:** Extracting biology data from the EEA State of Environment (WISE_SOE) Waterbase WISE2 Biology from https://discodata.eea.europa.eu Discodata platform
version: "latest" WISE SOE Biology EQR Data available in 2025

  - *by Monitoring site* from:
source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE2_BiologyEQRData
contains: Annually aggregated biological ecological quality ratio (EQR) data from rivers, lakes, transitional and coastal waters, by monitoring site.

  - *by WaterBody* from:
source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE2_BiologyEQRDataByWaterBody
contains: Annually aggregated biological ecological quality ratio (EQR) data from rivers, lakes, transitional and coastal waters, by water body.

- **WISE6:** Extracting a set of TRAC Water Quality (WQ) summary metrics for an appropriate set of supporting elements (SE), from the EEA state of the environment (SoE) WISE 6 datadase. Waterbase - Water Quality ICM, 2024; available online from 2nd July 2025. Temporal range 1899-2024. The dataset contains time series of nutrients, organic matter, hazardous substances, pesticides and other chemicals in rivers, lakes, groundwater, transitional, coastal and marine waters.
version: "latest" WISE SOE Waterbase T available in 2025

  The database is split into different datasets: 
  - ds1: *DisaggregatedData*
  source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE6_DisaggregatedData 

  - ds2: *AggregatedData*
  source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE6_AggregatedData 

  Data is reported by EEA member countries as individual samples from monitoring sites in the DisaggregatedData table or as annual aggregates of samples from monitoring sites in the AggregatedData table. Therefore data found in one table is not found in the other, and visa versa. 

note3: SE Data in AggregatedDataByWaterBody dataset is mostly historical and currently only has GW information, thus discarded for this work.

  - **Spatial data** Extracting also related spatial information from: 
  source: https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_S_WISE_SpatialObject_DerivedData
  contains: List of spatial object identifiers present in the WISE SOE dataset tables. Selected information reported through WFD or WISE5 spatial data reporting.

## Procedure for data extraction
**1st Extract DATA:** the extract *R scripts* in subdirectory R_scripts (WFD-TRaC-data-upto2022/R_scripts/"extract?-?-?.R") were used to extract data from each of the data sources above indicated for transitional (TW) and coastal (CW) water categories, selecting variables of interest and merging information from different tables (e.g. samples' Status classification and respective Typology information). More details in each script, including of corrections performed (scripts require download of the databases or, alternatively, adjustments to the script to access the databases directly online).

* BQE - From WISE2, EQR & EQS aggregated by Site (yearly), except for EE, IE and SI for which only report aggregated data by WB was available at source, so the EQR & EQSaggWB data was also extracted (for all countries and for those 4 countries). Data was extracted for all years available.

  - extractSOE-WISE2-Biology.R (resulting in 4 .rds files: BQE_TW, BQE_CW, BQE_TRaC_aggrWB_all, BQE_TRaC_aggrWB_EE-IE-MT-Sl; stored in Data);

* BQE - From WISE4, only WB aggregated EQS data (i.e., categories, not EQR) was available for the 1st and 2nd reporting cycle years 2010 and 2016.

  - extractWFD-WISEv4-BioEQS.R (resulting in 4 .rds output files: W4_CW_BQE, W4_TW_BQE, W4_TraC_BQE and W4_TraC_SE; stored in Data); 

* SE - From WISE6, this script extracts source Site disaggregated SE data summarised (mean) by Year and Site and the source Site aggregated data by year.
* 
  - extractSOE-WISE6-SE.R (resulting in 4 .rds output files: disaggregated data mean by Site (WQ_TW, WQ_CW) and aggregated by Site (WQ_TWagg, WQ_CWagg); stored in Data).
 
  - W6_SOE_TRaC_Spatial data was also extracted (2 .rds files; stored in Data)

**2nd Prepare SE DATA:** 
This script prepares the previously extracted water quality data (WQ from WISE-6) for selected supporting elements (SE) prior to joining it with the biological classifications (BQE from WISE-4 & WISE-2), for both TRaC water categories (TW & CW). 

* WQ(SE) - From WISE6, this script uses the previously source Site disaggregated SE data summarised (mean) by Year and Site and the source Site aggregated data by year, which are here **merged** (*WQ siteMean_year - xlsx outputs*) then they are aggregated at WB level per year (*WQ aggWBmean_year - xlsx outputs*) and then the WB level data is further aggregated per reporting cyle (*WQ summary_periods - xlsx outputs*).
  
* *Determinants* lists per water category are also extracted (.xlsx and .rds files; stored in Data)
  
**3rd Prepare Biological DATA:** 
This script prepares the previously extracted biological classifications (BQE from WISE-4 & WISE-2) prior to joining it with the water quality data (WQ from WISE-6) for selected supporting elements (SE), for both TRaC water categories (TW & CW).

* BQE - From WISE2, EQR & EQS aggregated by Site (yearly), except for EE, IE and SI for which only report aggregated data by WB was available at source, so the EQR & EQSaggWB data was also extracted (for all countries and for those 4 countries). Data was extracted for all years available.

* BQE - From WISE4, only WB aggregated EQS data (i.e., categories, not EQR) was available for the 1st and 2nd reporting cycle years 2010 and 2016.

**4th COMBINE BIOLOGY & SE DATA:** the script WFD-TRaC-data-upto2022/R_scripts/"CombineTraC_SE-BQE.Rmd" was used to merge the extracted water quality data (WQ from WISE-6) for selected supporting elements (SE, i.e. nutrients and other physico-chemical parameters) to the biological classifications (BQE from WISE-4 (only EQS) & WISE-2 (both EQR and EQS)), for both TRaC water categories (TW & CW), using samples' code. More details within the script, including of corrections performed.

## OUTPUT Datasets: 
Overall, separate datasets were created (subdirectory WFD-TRaC-data-upto2022/DataCreated) for each water category (TW & CW) per WFD reporting cycle: 2010 (1st cycle), 2016 (2nd cycle) and 2022 (3rd cycle):

(datasets need update as they changed format)
1. BQESEdatTW_WFD2010.xlsx - TW 2010 to be added
2. BQESEdatCW_WFD2010.xlsx - CW 2010 to be added
3. BQESEdatTW_WFD2016.xlsx (n=111) for all BQE in all available MS reporting
4. BQESEdatCW_WFD2016.xlsx (n=115) for all BQE in all available MS reporting
5. ...
