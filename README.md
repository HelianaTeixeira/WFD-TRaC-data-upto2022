
# ECOSTAT - Water Quality Data

## WFD-TRaC-data-upto2022
Assembling Water Quality data from: 
* WISE4 (identifier: eea_wise-wfd_s) WFD Biological Quality Elements (EQS) (WFD 1st 2010 & 2nd 2016 cycles; TraC data span 2000 to 2018; note: SE info also available in WISE4);
* WISE2 (identifier: eea_waterbase-biology_s) EEA Waterbase – Biology (normalised EQR) (1990 to 2022);
* WISE6 (identifier: eea_waterbase-water-quality-icm_s) WFD Water Quality Supporting Elements (1899 to 2022).

(Acess to data sources above was verified by December 2023).

This work was developed within the Water Framework Directive (WFD) Common Implementation Strategy, under the ECOSTAT Working Group. This repository contains the files necessary to assemble Biological and Supporting Quality Elements data from the EU MS national reporting obligations for the WFD. The resulting data will be used to evalute current national boundaries to support good ecological status and to derived methods to improve the current Best Practice Guide (BPG) Toolkit. The ultimate goals is to update the BPG and help Member States adjusting and deriving their nutrients and supporting elements' boundaries as they see fit.

- author: Heliana Teixeira
- contacts: heliana.teixeira@ua.pt
- date created: 2022.10.20
- updated: 2023.12.20

links to WFD data sources:
- WISE4: https://www.eea.europa.eu/data-and-maps/data/wise-wfd-4
- WISE2: https://www.eea.europa.eu/data-and-maps/data/waterbase-biology-1 or     https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE2_BiologyEQRData 
- WISE6: https://www.eea.europa.eu/data-and-maps/data/waterbase-water-quality-icm-2 or 
https://discodata.eea.europa.eu/download/WISE_SOE/latest/Waterbase_T_WISE6_DisaggregatedData

note: for the third WFD reporting cycle, WISE4 has been superseded by WISE-6 for Water quality, and by WISE-2 for Biology.

## Procedure for data extraction
1st EXTRACT DATA: the scripts WFD-TRaC-data-upto2022/R_scripts/"extract?-?-?.R" were used to extract data from each of the data sources above indicated for TW and CW water categories, selecting variables of interest and merging information from different tables (e.g. status classification and respective samples typology information). More details in each script, including of corrections performed (scripts require download of the databases or alternative adjustments to script to access the online databases):

  - extractWFD-WISEv4-BioEQS.R
  - extractSOE-WISE2-Biology.R
  - extractSOE-WISE6-SE.R

2nd COMBINE BIOLOGY & SE DATA: the script WFD-TRaC-data-upto2022/R_scripts/"CombineTraC_SE-BQE.Rmd" was used to merge extracted water quality data (WQ from WISE-6) for selected supporting elements (SE, i.e. nutrients and other physico-chemical parameters) to the biological classifications (BQE from WISE-4 & WISE-2), for both TRaC water categories (TW & CW), using samples' code. More details within the script, including of corrections performed.

Overall, separate datasets were created (WFD-TRaC-data-upto2022/DataCreated) reagrding each water category (TW & CW) for each WFD reporting cycle: 2010 (1st cycle), 2016 (2nd cycle) and also for the data already available for the 3rd cycle (latest data available from 2016):
1. BQESEdatTW_WFD2010.xlsx - TW 2010 to be added
2. BQESEdatCW_WFD2010.xlsx - CW 2010 to be added
3. BQESEdatTW_WFD2016.xlsx (n=) for all BQE in all available MS reporting
4. BQESEdatCW_WFD2016.xlsx (n=) for all BQE in all available MS reporting
5. TW 2022 - to be added where corresponding BIO & SE data availble for merging
6. CW 2022 - to be added where corresponding BIO & SE data availble for merging
