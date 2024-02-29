# urban-population-internal-migration
Data handling and analysis for the Centre for Cities 'Escape to the Country?' briefing

## Description
This repo contains the scripts and workbooks used to analyse data for our March 2024 release 'Escape to the Country?'. This briefing looks at the impact of the Covid-19 pandemic on urban population growth in the UK, with a specific focus on London. 

The briefing uses data published by the Office for National Statistics on total population; internal migration; births and deaths; and average private rents. See links to the data sources below. 

## Getting Started
### Dependencies
R, Windows 10 or 11, Microsoft Excel

### Data sources
All data is gathered from the Office for National Statistics
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
For the annual update, the price paid data includes two updates - the complete price data for the year previous, and the provisional data for the year current

There are three datasets to download - the provisional price paid data for the current year; and the complete price paid data for the previous year; and the updated postcode registry. For the postcode registry, see the postcode repo.

The postcodes need to be merged with their lookups, and these new lookups then need be merged with each land registry data set for price paid data to identify prices by PUA, for both years.

If the boundaries of PUAs have changed, then the lookups also need to be changed before merging with the postcodes, and the prices for all previous years for those cities need to be changed

Download the previous complete data and the provisional current year price paid data as a csv here: https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads

## Understanding the Scripts
There are currently two scripts in this folder

Price Paid Script - Processes the data for each annual update (and includes an option to generate monthly transactions)

Loop Merge - Runs a loop in Stata which appends all of the results for every year into a single file

Before doing any work on any script, make sure that the Postcode lookup is updated using the script in the Postcode repo.

### Price Paid Script
This script will need to be updated every year to account for cleaning - it is common that the price paid data contains errors (e.g. terraced houses in Sheffield worth a billion pounds).

At the end, there are distinct outputs for both PUAs and CAs. These are combined together into the final data for both complete and provisional data in the workboook.

Likewise, there is also a separate 'ending' for monthly transactions, which is required for City Monitor. In theory, this could in a later update be separated out as a separate script.

Currently in Excel, the final step is for each year, count the number of transactions by PUA; and sum the value of all the transactions. Divide the value of all transactions by the number of transactions to acquire the mean price for PUA. This could in a later update of the script all be done in Stata.

### Loop Merge
This is only for use when the boundaries of a city or a combined authority change and there is a need to correct historic city data.

For this data, download each year's CSVs back to 2001 directly into your repo's folder alongside the script. The script should automatically pick out every csv in the folder and append them into a single .dta file, with which you can work to calculate historic PUA/CA data.

Delete all the annual csvs and the .dta file once you have finished, to avoid clogging up your folders and the repo.

## Caveats
This is price paid data, not advertised price or true market prices at any given time.

Errors do sometimes slip into the raw data and must be checked for and removed in any running of the script. Sorting and looking for unusually expensive properties is typical.

## Usage
The complete and provisional price paid data is important for calculating the average price of dwellings in cities, as well as the affordability ratio, for use in Cities Outlook, City Monitor, the Data Tool, and other uses. The monthly transactions data is used only in City Monitor.

In future, we could merge this data with the Energy Performance Certificates to produce estimates on prices per square metre within cities, including for different types of housing or different parts of cities.

## Roadmap
Run the Loop Merge script to generate prices for the new NEMCA and historic CAs
Update the Price Paid Script to do as many of the calculations within Stata
Separate out monthly transactions into a separate script
Integrate with the EPC data to produce estimates on prices per square metre

## Contributions
Any contributions you make are greatly appreciated. Please create an open issue with the tag "enhancement" before opening a pull request.

## Acknowledgements
Thanks to James Evans for writing the postcode script
