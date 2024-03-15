# urban-population-internal-migration
Data handling and analysis for the Centre for Cities 'Escape to the country? How Covid changed London's population' briefing

## Description
This repo contains the scripts and workbooks used to analyse data for our March 2024 release 'Escape to the country? How Covid changed London's population'. This briefing looks at the impact of the Covid-19 pandemic on urban population growth in the UK, with a specific focus on London. 

The briefing uses data published by the Office for National Statistics on total population; internal migration; births and deaths; and average private rents. See links to the data sources below. 

We also use several Centre for Cities Lookups and workbooks edited before use in R. These are uploaded to the repo with names that match the code used in the R scripts and are explained in more detail below. 

## Getting Started
### Dependencies
R, Windows 10 or 11, Microsoft Excel

### External data sources
All data is gathered from the Office for National Statistics. All below links are functional at the time of writing but may be updated by the ONS as they release new data. 

Population Estimates for England and Wales: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales

Internal Migration between local authorities in England and Wales: 
https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/internalmigrationinenglandandwales

Births in England and Wales:
https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/birthsummarytables

Death registrations by local authority and health board: 
https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard 

Birth data popularity (ONS analysis): 
https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/articles/howpopularisyourbirthday/2015-12-18 

Price Index of Private Rents: 
https://www.ons.gov.uk/peoplepopulationandcommunity/housing/articles/redevelopmentofprivaterentalpricesstatisticsimpactanalysisuk/latest#local-authority-data

## Understanding the Data
We use data downloaded directly from the ONS as inputs for our R scripts. To obtain this data, please follow the links above. 

### Input workbooks
Where we do not use raw ONS data in our R scripts, we have uploaded the Excel workbooks in the 'Input workbooks' folder. This includes geography lookups and workbooks where we have sorted data in Excel before analysis in R..

Data on private rents is from the Price Index of Private Rents link above, but we have added a London lookup and filtered out London in Excel prior to analysis in the average_private_rents.R script. 

Data on births and deaths are workbooks compiling separate ONS downloads. There are additional tabs which summarise this data for use in our year-by-year analysis. In these sheets, there is some manual inserting of local authority ONS codes from later geographies and summing of data to fit those geographies. This is to account for when geographies change between years, and data pulls from years either side of the boundary/code change. 

### Output workbooks
The 'Output workbooks' folder contains workbooks used for our final analysis. Most of the data contained in these spreadsheets are outputs from R scripts. 

'internal migration statistics' is the output from the 'internal migration script for stats' R script. Additional analysis is conducted to summarise migration patterns between London (Inner and Outer) and the rest of England and Wales; migration between Inner and Outer London; migration between London and different regions in England & Wales.  

'internal migration local authorities statistics' is the output from the 'internal migration script local authorities for stats' R script. Additional analysis is conducted to summarise migration patterns between different Inner and Outer London local authorities and the rest of England and Wales; and between London and specific South Western and South Eastern local authorities. 

'urban population statistics' is the output from the 'urban population script' R script. Additional analysis is conducted to understand change in population at PUA and urban/non-urban levels. 

'natural increase data' is the output from the 'births deaths natural increase script' R script. Additional analysis is conducted to summarise change in births, deaths and natural increase between 2015 and 2022. 

'average private rents data' is the output from the 'average private rents script' R script. 

'international migration calculations' pastes data from the other scripts and includes calculations for international migration between 2015-2022. Note that this uses slightly different internal migration totals to that used in the rest of the report, as it includes moves between Scotland and Northern Ireland and English and Welsh local authorities. The R scripts and other data for internal migration only look at movement between English and Welsh local authorities. 

## Understanding the Scripts
There are currently six scripts in this folder. External (non-Centre for Cities) users of these scripts will need to:
- download any necessary raw files from the ONS
- download relevant workbooks (including geographies workbooks) from the Input workbooks folder
- adjust the path at the beginning of the scripts to draw from their own computer/cloud data source
- adjust the script at the end to output to an appropriate folder 

### average_private_rents_script 
Processes average private rent data (contained in Input workbooks folder); creates a column to index to January 2020 rents and a further column to index to average UK rents; creates filtered subsets of the data to compare annual rates of change between authorities; plots graph for selected authorities; outputs to the workbook that forms the basis of the 'average private rents data' workbook in the Output workbooks folder. 

### births_deaths_natural_increase_script 
Processes deaths and births data (contained in Input workbooks folder); attaches geographies from lookup; calculates natural increase; summarises births, deaths and natural increase by PUA and by Country. 

### internal_migration_script_for_histograms
Processes raw data from ONS on internal migration estimates (see link above); attaches geographies from lookup; labels moves between different geographies (e.g. London and outside London; Inner London to outside London etc.); summarises moves by PUA and calculates net migration; repeats the process for each year; calculates 2015-2019 averages; exports selected data to an excel for use in visualisations (not included in Output workbooks folder because data replicated elsewhere); generates many histograms and overlays them to create stacked histograms (including more than included in the final report). 

### internal_migration_script_for_stats
Processes raw data from ONS on internal migration estimates (see link above); attaches geographies from lookup; labels moves between different geographies (e.g. London and outside London; Inner London to outside London etc.); creates summary age bands; summarises moves by PUA and calculates net migration; repeats the process for each year; calculates 2015-2019 averages; puts relevant data into summary dataframes which are exported as separate tabs to an Excel workbook which forms the basis of the 'internal migration calculations' workbook. 

### internal_migration_script_localauthorities_for_stats
Processes raw data from ONS on internal migration estimates (see link above); attaches geographies from lookup; lists authorities of interest; labels moves between different geographies (e.g. Tower Hamlets and outside London; London and Bristol etc.); creates summary age bands; summarises moves by selected authorities and calculates net migration; repeats the process for each year; neatens data frames before exporting to workbook which forms the basis of the 'internal migration local authorities statistics' workbook. 

### urban_population_script 
Processes raw data from the Nomis API, attaches geographies from lookup, and summarises total population statistics by PUA and other geographies. Exports to Excel workbook that forms the basis of the 'urban population statistics' workbook. 

## Notes on data used for this work
### Internal migration data
This data uses changes in GP registrations as its primary data source. The data is based on yearly snapshots, so does not capture when people move twice within a year. The ONS uses supplementary information to correct for phenomena such as university leavers remaining registered with their university GP. Overall, we feel confident that the data remains sound throughout the years studied. Some transient moves which wouldn’t ordinarily have resulted in a new GP registration, but did because of the pandemic, may have been captured. However, given that Covid-19 tests and vaccinations were not organised by GP surgeries, we think that the effect, if there is any, is likely to be small. The published data does not include Scottish or Northern Irish local authorities, so we are unable to consider them.

### Natural increase
Deaths data is provided month by month, and we assign data to mid-year to mid-year accordingly. Births data is provided by calendar year. We apportion to match mid-years used for the other data, with an adjustment for variation in birth-month popularity (July to December (50.8 per cent); January to June (49.2 per cent)). 

### Private rental prices
These data are experimental and are due to be replaced in Spring 2024. Nevertheless, we think they are accurate enough to illustrate differences between authorities for our purposes. 

## Usage
The data and outputs in this repo are directly used for the Centre for Cities 'Escape to the Country?' briefing, published March 2024.  

In future, we could easily make minor updates to the scripts and conduct analysis in workbooks again to update these findings for future years. Scripts could also be edited to look in more detail at changes to migration patterns to/from other areas/cities in the UK. 

## Contributions
Any contributions you make are greatly appreciated. Please create an open issue with the tag "enhancement" before opening a pull request.

## Acknowledgements
Thanks to Xuanru Lin for her immense contributions to both raw data processing and R script writing for this project. 
