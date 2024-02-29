library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx) 
library(ggplot2)
library(tidyr)

path = paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Input/Rental LA data", collapse = NULL)
setwd(path)

#Import rentals data
rawrent <- read_excel("Average rents, PRS, 2015-2023 by LA EDIT.xlsx", sheet = "London raw")

##cleaning import
# Remove first two rows
rawrent <- rawrent[-c(1), ]
# Set the first row as column names
colnames(rawrent) <- as.character(rawrent[1, ])
rawrent <- rawrent[-1, ]

#fix dates column 
# Create a sequence of dates from January 2015 to October 2023
dates <- seq(as.Date("2015-01-01"), as.Date("2023-10-01"), by = "month")

# Repeat the sequence to match the length of the dataframe
n_rows <- nrow(rawrent)
dates_rep <- rep(dates, length.out = n_rows)

# Add the 'month year' column to the dataframe & make it the first column
rawrent$Month_Year <- format(dates_rep, "%B %Y")
rawrent <- rawrent %>%
  select(Month_Year, everything())
rawrent <- rawrent[,-2]

#convert date column to date
rawrent$Month_Year <- as.Date(paste("01", rawrent$Month_Year), format = "%d %B %Y")

#Select columns from the 7th column onwards and apply the transformation
rawrent <- rawrent %>%
  mutate(across(7:ncol(rawrent), as.numeric))

#Index to rents in January 2020
rawrent <- rawrent %>%
  group_by(`Area name`) %>%
  mutate(Price_Ratio = `Rental price` / `Rental price`[`Month_Year` == as.Date("2020-01-01")])

##look at annual change in year to:
# Filter data for dates from January 1, 2020
jan2020_data <- subset(rawrent, Month_Year == as.Date("2020-01-01"))

# Filter data for dates from January 1, 2021
jan2021_data <- subset(rawrent, Month_Year == as.Date("2021-01-01"))

# Filter data for dates from April 1st 2021
apr2021_data <- subset(rawrent, Month_Year == as.Date("2021-04-01"))

# Filter data for dates from July 1st 2021
jul2021_data <- subset(rawrent, Month_Year == as.Date("2021-07-01")) 

# Filter data for dates from July 1st 2022
jul2022_data <- subset(rawrent, Month_Year == as.Date("2022-07-01"))

# Filter data for dates from July 1st 2023
jul2023_data <- subset(rawrent, Month_Year == as.Date("2023-07-01"))

#plot average rental price on graph for all London LAs 
#Filter data for selected boroughs
selected_LAs <- rawrent %>%
  filter(`Area name` %in% c("Tower Hamlets", "Lewisham", "Bexley", "Haringey"))

# Plot average rents
ggplot(selected_LAs, aes(x = Month_Year)) + 
  geom_line(aes(y = `Rental price`, color = `Area name`), size = 1.5) +
  labs(x = "Year", y = "Average rental price (GBP)", color = "Local authority") +
  scale_y_continuous(limits = c(800, 2400), breaks = seq(800, 2400, by = 200)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Average rental price, selected London Boroughs, January 2015 - October 2023")

# Plot indexed
ggplot(selected_LAs, aes(x = Month_Year)) + 
  geom_line(aes(y = `Price_Ratio`, color = `Area name`), size = 1.5) +
  labs(x = "Year", y = "Average rent, indexed to Jan 2020", color = "Local authority") +
  scale_y_continuous(limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, by = 0.1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Indexed average rent, selected London Boroughs, January 2015 - October 2023")


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

# Generate file path for Excel workbook with date
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "_Rent_change", ".csv")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Output")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(selected_LAs,jan2020_data, jan2021_data, apr2021_data, jul2021_data, jul2022_data, jul2023_data)
names(df_list_geog) <- c("selected_raw","jan20", "jan21", "apr21", "jul21", "jul22", "jul23" )                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)
