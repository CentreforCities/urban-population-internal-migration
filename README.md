# urban-population-internal-migration
Data handling and analysis for Escape to the Country briefing

Description
House price data for all transactions in England and Wales is released monthly by the Land Registry at the postcode level. This data allows us to calculate both mean house prices for individual cities and combined authorities in England and Wales by year and in total from 2003 for Cities Outlook, the Data Tool, and City Monitor, monthly transactions which is used in City Monitor (and in future, prices by square metre).

The most important use of this repo is the generation ofthe annual average price data in early January, both provisional and complete.

English and Welsh data is downloaded here: https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads

Scottish data is calculated in workbooks in the housing theme folder and uses this data: https://www.ros.gov.uk/data-and-statistics/house-price-statistics

Belfast data is also available; Northern Irish data is not. See: https://www.finance-ni.gov.uk/articles/northern-ireland-house-price-index

Getting Started
Dependencies
STATA, Windows 10, Updated Postcode Lookup

Price paid data: https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads

Understanding the Data
For the annual update, the price paid data includes two updates - the complete price data for the year previous, and the provisional data for the year current

There are three datasets to download - the provisional price paid data for the current year; and the complete price paid data for the previous year; and the updated postcode registry. For the postcode registry, see the postcode repo.

The postcodes need to be merged with their lookups, and these new lookups then need be merged with each land registry data set for price paid data to identify prices by PUA, for both years.

If the boundaries of PUAs have changed, then the lookups also need to be changed before merging with the postcodes, and the prices for all previous years for those cities need to be changed

Download the previous complete data and the provisional current year price paid data as a csv here: https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads

Understanding the Scripts
There are currently two scripts in this folder

Price Paid Script - Processes the data for each annual update (and includes an option to generate monthly transactions)

Loop Merge - Runs a loop in Stata which appends all of the results for every year into a single file

Before doing any work on any script, make sure that the Postcode lookup is updated using the script in the Postcode repo.

Price Paid Script
This script will need to be updated every year to account for cleaning - it is common that the price paid data contains errors (e.g. terraced houses in Sheffield worth a billion pounds).

At the end, there are distinct outputs for both PUAs and CAs. These are combined together into the final data for both complete and provisional data in the workboook.

Likewise, there is also a separate 'ending' for monthly transactions, which is required for City Monitor. In theory, this could in a later update be separated out as a separate script.

Currently in Excel, the final step is for each year, count the number of transactions by PUA; and sum the value of all the transactions. Divide the value of all transactions by the number of transactions to acquire the mean price for PUA. This could in a later update of the script all be done in Stata.

Loop Merge
This is only for use when the boundaries of a city or a combined authority change and there is a need to correct historic city data.

For this data, download each year's CSVs back to 2001 directly into your repo's folder alongside the script. The script should automatically pick out every csv in the folder and append them into a single .dta file, with which you can work to calculate historic PUA/CA data.

Delete all the annual csvs and the .dta file once you have finished, to avoid clogging up your folders and the repo.

Caveats
This is price paid data, not advertised price or true market prices at any given time.

Errors do sometimes slip into the raw data and must be checked for and removed in any running of the script. Sorting and looking for unusually expensive properties is typical.

Usage
The complete and provisional price paid data is important for calculating the average price of dwellings in cities, as well as the affordability ratio, for use in Cities Outlook, City Monitor, the Data Tool, and other uses. The monthly transactions data is used only in City Monitor.

In future, we could merge this data with the Energy Performance Certificates to produce estimates on prices per square metre within cities, including for different types of housing or different parts of cities.

Roadmap
Run the Loop Merge script to generate prices for the new NEMCA and historic CAs
Update the Price Paid Script to do as many of the calculations within Stata
Separate out monthly transactions into a separate script
Integrate with the EPC data to produce estimates on prices per square metre
Contributions
Any contributions you make are greatly appreciated. Please create a open an issue with the tag "enhancement" before opening a pull request.

Acknowledgements
Thanks to James Evans for writing the postcode script
