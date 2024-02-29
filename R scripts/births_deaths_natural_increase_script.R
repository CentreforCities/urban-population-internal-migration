library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx) 
library(ggplot2)
library(tidyr)

#import the geography lookup:
path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Input", collapse = NULL)
setwd(path)
geography <- read_xlsx("LAtoPUA2024.xlsx")
geography <- geography %>%
  rename("Region" = "Region...3") 
geography <- geography[,1:8]  # select columns needed
geography_death <- geography[,1:8]  # select columns needed
geography_birth <- geography[,1:8]  # select columns needed


##DEATH
path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Input/Birth_Death/Death", collapse = NULL)
setwd(path)

# import datasets death 2015-2022 
raw15death <- read_excel("data_deathsummary2014-22.xlsx", sheet = "2015")
raw16death <- read_excel("data_deathsummary2014-22.xlsx", sheet = "2016")
raw17death <- read_excel("data_deathsummary2014-22.xlsx", sheet = "2017")
raw18death <- read_excel("data_deathsummary2014-22.xlsx", sheet = "2018")
raw19death <- read_excel("data_deathsummary2014-22.xlsx", sheet = "2019")
raw20death <- read_excel("data_deathsummary2014-22.xlsx", sheet = "2020")
raw21death <- read_excel("data_deathsummary2014-22.xlsx", sheet = "2021")
raw22death <- read_excel("data_deathsummary2014-22.xlsx", sheet = "2022")


# List of data frames
dfs_death <- list(raw15death, raw16death, raw17death, raw18death, raw19death, raw20death, raw21death, raw22death)
result_list_death <- list()

# Loop through each dataset and calculate sum

for (i in seq_along(dfs_death)) {
  raw <- dfs_death[[i]]
  raw <- na.omit(raw)
  ## Extract the area and code
  raw_name <- raw[, 1]
  # Convert the remaining dataset to numeric
  raw_data<- raw[, 2]
  raw_data<- as.data.frame(lapply(raw_data, as.numeric))
  result <- cbind(raw_name, raw_data)
  # Append result to the result list
  result_list_death[[i]] <- list(death_sum = result)
  
}


# Loop through each element in the result_list_death
for (i in seq_along(result_list_death)) {
  
  # Extract Number of live births from the i-th element of the result_list
  death_sum <- result_list_death[[i]]$death_sum
  
  #merge
  geography_death<- left_join(geography_death, death_sum, by = "LA Code")
  
  # Rename newly added column from death_2015 to death_2022
  new_col_name <- paste0("death_", 2014 + i)
  geography_death <- rename(geography_death, !!new_col_name := Number_of_deaths)
  
}



##BIRTH

path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Input/Birth_Death/Birth", collapse = NULL)
setwd(path)

# import datasets birth 2015-2022 
birth_sum2015 <- read_excel("data_birthsummary2014-22.xlsx", sheet = "2015")
birth_sum2016 <- read_excel("data_birthsummary2014-22.xlsx", sheet = "2016")
birth_sum2017 <- read_excel("data_birthsummary2014-22.xlsx", sheet = "2017")
birth_sum2018 <- read_excel("data_birthsummary2014-22.xlsx", sheet = "2018")
birth_sum2019 <- read_excel("data_birthsummary2014-22.xlsx", sheet = "2019")
birth_sum2020 <- read_excel("data_birthsummary2014-22.xlsx", sheet = "2020")
birth_sum2021 <- read_excel("data_birthsummary2014-22.xlsx", sheet = "2021")
birth_sum2022 <- read_excel("data_birthsummary2014-22.xlsx", sheet = "2022")

# List of data frames
dfs_birth <- list(birth_sum2015,birth_sum2016,birth_sum2017,birth_sum2018,birth_sum2019,birth_sum2020,birth_sum2021,birth_sum2022)
result_list_birth <- list()


# Loop through each dataset and calculate sum

for (i in seq_along(dfs_birth)) {
  raw <- dfs_birth[[i]]
  raw <- na.omit(raw)
  ## Extract the area and code
  raw_name <- raw[, 1]
  # Convert the remaining dataset to numeric
  raw_data<- raw[, 2]
  raw_data<- as.data.frame(lapply(raw_data, as.numeric))
  result <- cbind(raw_name, raw_data)
  # Append result to the result list
  result_list_birth[[i]] <- list(birth_sum = result)
  
}


# Loop through each element in the result_list_birth
for (i in seq_along(result_list_birth)) {
  
  # Extract Number of live births from the i-th element of the result_list
  birth_sum <- result_list_birth[[i]]$birth_sum
  
  #merge
  geography_birth<- left_join(geography_birth, birth_sum, by = "LA Code")
  
  # Rename newly added column from death_2015 to death_2022
  new_col_name <- paste0("birth_", 2014 + i)
  geography_birth <- rename(geography_birth, !!new_col_name := Number_of_live_births)
  
}


#move the last column of a data frame to the second column 
#geography_birth <- geography_birth[, c(1, ncol(geography_birth), 2:(ncol(geography_birth)-1))]
#geography_death <- geography_death[, c(1, ncol(geography_death), 2:(ncol(geography_death)-1))]
colnames(geography_birth)[1] <- "LA_Code"
colnames(geography_death)[1] <- "LA_Code"

# Merge datasets based on the "LA_Code" column
merged_data <- inner_join(geography_birth, geography_death, by = "LA_Code")


# Calculate the differences
# Find columns with the same ending and calculate the differences
differences <- data.frame(LA_Code = merged_data$LA_Code)  # Initialize the differences dataframe with the LA_Code column
for (year in 2015:2022) {
  for (col in colnames(merged_data)) {
    if (grepl(paste0("birth_", year, "$"), col)) {  # Check if the column name ends with "_YYYY" (where YYYY is the current year)
      # Extract the year part from the column name
      year_part <- sub(".*_(\\d{4})", "\\1", col)
      # Construct the corresponding death column name
      corresponding_col <- paste0("death_", year_part)
      # Calculate the difference and store it in the differences dataframe
      new_col_name <- paste0("Diff_", col, "_to_", corresponding_col)
      differences[[new_col_name]] <- merged_data[[col]] - merged_data[[corresponding_col]]
    }
  }
}

#add differences onto merged_data
births_deaths_net <- left_join(merged_data, differences, by = "LA_Code")

#remove duplicate geography & rename geography so no longer has .x
# Identify columns ending with ".y"
columns_to_remove <- grep("\\.y$", names(births_deaths_net))
# Remove columns ending with ".y"
births_deaths_net <- births_deaths_net[, -columns_to_remove]
# Get column names containing ".x"
columns_to_rename <- grep("\\.x$", names(births_deaths_net), value = TRUE)
# Remove ".x" from column names
new_column_names <- sub("\\.x$", "", columns_to_rename)
# Rename columns
names(births_deaths_net)[names(births_deaths_net) %in% columns_to_rename] <- new_column_names

# Remove rows where the value in the "Country" column is either "Scotland" or "Northern Ireland"
births_deaths_net <- births_deaths_net[!(births_deaths_net$Country %in% c("Scotland", "Northern Ireland")), ]

#rename difference columns so just diff_2015 etc 
# Define the new column names
new_column_names <- c("net_2015", paste0("net_", 2016:2022))
# Rename columns 25 through 32
births_deaths_net <- births_deaths_net %>%
  rename_at(vars(25:32), ~ new_column_names)

# Select columns from the 9th column onward and convert their data to numeric
births_deaths_net <- births_deaths_net %>%
  mutate_at(vars(9:ncol(births_deaths_net)), as.numeric)

#create 15-19 averages column

# Create a new column "death_15_19" containing the sum of values in columns "birth_15" through "birth_19"
births_deaths_net$death_15_19 <- rowMeans(births_deaths_net[, paste0("death_", 2015:2019)])
# Create a new column "net_15_19" containing the sum of values in columns "birth_15" through "birth_19"
births_deaths_net$net_15_19 <- rowMeans(births_deaths_net[, paste0("net_", 2015:2019)])



#collapse by PUA
births_PUA <- births_deaths_net %>%
  group_by(PUA) %>%
  summarize(across(starts_with("birth_"), sum, na.rm = TRUE))
# Create a new column "birth_15_19" containing the sum of values in columns "birth_15" through "birth_19"
births_PUA$birth_15_19 <- rowMeans(births_PUA[, paste0("birth_", 2015:2019)])

deaths_PUA <- births_deaths_net %>%
  group_by(PUA) %>%
  summarize(across(starts_with("death_"), sum, na.rm = TRUE))
# Create a new column "death_15_19" containing the sum of values in columns "birth_15" through "birth_19"
deaths_PUA$death_15_19 <- rowMeans(deaths_PUA[, paste0("death_", 2015:2019)])

net_PUA <- births_deaths_net %>%
  group_by(PUA) %>%
  summarize(across(starts_with("net_"), sum, na.rm = TRUE))
# Create a new column "net_15_19" containing the sum of values in columns "birth_15" through "birth_19"
net_PUA$net_15_19 <- rowMeans(net_PUA[, paste0("net_", 2015:2019)])

#collapse by country
births_Country <- births_deaths_net %>%
  group_by(Country) %>%
  summarize(across(starts_with("birth_"), sum, na.rm = TRUE))
# Create a new column "birth_15_19" containing the sum of values in columns "birth_15" through "birth_19"
births_Country$birth_15_19 <- rowMeans(births_Country[, paste0("birth_", 2015:2019)])

deaths_Country <- births_deaths_net %>%
  group_by(Country) %>%
  summarize(across(starts_with("death_"), sum, na.rm = TRUE))
# Create a new column "death_15_19" containing the sum of values in columns "birth_15" through "birth_19"
deaths_Country$death_15_19 <- rowMeans(deaths_Country[, paste0("death_", 2015:2019)])

net_Country <- births_deaths_net %>%
  group_by(Country) %>%
  summarize(across(starts_with("net_"), sum, na.rm = TRUE))
# Create a new column "net_15_19" containing the sum of values in columns "birth_15" through "birth_19"
net_Country$net_15_19 <- rowMeans(net_Country[, paste0("net_", 2015:2019)])


#summarise all
# Create a new column "All" and populate it with the text "E&W" in all rows
births_deaths_net$All <- "E&W"

births_All <- births_deaths_net %>%
  group_by(All) %>%
  summarize(across(starts_with("birth_"), sum, na.rm = TRUE))
# Create a new column "birth_15_19" containing the sum of values in columns "birth_15" through "birth_19"
births_All$birth_15_19 <- rowMeans(births_All[, paste0("birth_", 2015:2019)])

deaths_All <- births_deaths_net %>%
  group_by(All) %>%
  summarize(across(starts_with("death_"), sum, na.rm = TRUE))
# Create a new column "death_15_19" containing the sum of values in columns "birth_15" through "birth_19"
deaths_All$death_15_19 <- rowMeans(deaths_All[, paste0("death_", 2015:2019)])

net_All <- births_deaths_net %>%
  group_by(All) %>%
  summarize(across(starts_with("net_"), sum, na.rm = TRUE))
# Create a new column "net_15_19" containing the sum of values in columns "birth_15" through "birth_19"
net_All$net_15_19 <- rowMeans(net_All[, paste0("net_", 2015:2019)])


### EXPORT age band stats to excel
# Export data as an excel workbook with multiple sheets:
export_to_excel <- function(data_frames, file_path) {
  # Create a New Excel workbook
  workbook <- createWorkbook()
  # Loop through the data frames
  for (i in seq_along(data_frames)) {
    # Get the data frame and sheet name
    df <- data_frames[[i]]
    sheet_name <- names(data_frames)[i]
    # Add the data frame to the workbook as a New sheet
    addWorksheet(workbook, sheetName = sheet_name)
    writeData(workbook, sheet = sheet_name, x = df)
  }
  # Save the Excel file
  saveWorkbook(workbook, file_path, overwrite = TRUE)
}

# GeNerate file path for Excel workbook with date
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "_Int_mig_natural_increase", ".csv")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Output")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(births_deaths_net, births_PUA, deaths_PUA, net_PUA, births_All, deaths_All, net_All, births_Country, deaths_Country, net_Country)
names(df_list_geog) <- c("ROUGH_LA_birth_death", "births_PUA", "deaths_PUA", "net_PUA", "births_All",
                         "deaths_All", "net_All", "births_Country", "deaths_Country", "net_Country")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)