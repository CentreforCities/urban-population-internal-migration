library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(stringr)
library(tidyverse)
library(nomisr)



#Get population data from nomis
# ML EDIT - adding all years throughout the code
pop <- nomis_get_data(id="NM_2002_1", geography="TYPE432", 
                      time= c(1991:2022), measures=20100, c_age=0, gender=0)


#Define function to filter nomis population data
get_pop <- function(df, year) {
  var <- paste0("POPULATION", as.character(year))
  result_df <- df %>%
    filter(DATE == year) %>%  
    select(GEOGRAPHY_CODE, OBS_VALUE) %>%  
    rename(.data = ., !!var := OBS_VALUE) %>% 
    rename("LACD" = "GEOGRAPHY_CODE")
  return(result_df)  
}

#Run function to get datasets 
for (i in c(1991:2022)){
  x <- paste("pop", i, sep="_")
  assign(x, get_pop(pop, i))
}

#Replace missing values for 2022 pop estimates by their 2021 values (this applies to Scottish and NI values)
temporary_df <- left_join(pop_2021, pop_2022, by="LACD")
temporary_df[is.na(temporary_df$POPULATION2022), "POPULATION2022"] <- temporary_df[is.na(temporary_df$POPULATION2022), "POPULATION2021"]
pop_2022 <- temporary_df[,c(1,3)]


#Get geo data 
path = paste0(dirname("~"),"\\Centre for Cities\\Centre For Cities POC - Documents\\Research", collapse = NULL)
setwd(path)

geography <- read_xlsx("Cities Outlook\\Cities Outlook 2024\\geo_lookupML.xlsx")


#Clean up geography datageography <- geography[,1:6] # Select only needed columns for geography lookup
geography <- geography %>%
  rename("Region" = "Region...3",
         "LANM" = "LA",
         "LACD" = "LA Code",
         "Urban_non_urban" = "Urban Region") %>%
  select(1:8)

#Merging pop data with geo data
df_list <- list(geography, pop_1991, pop_1992, pop_1993, pop_1994, pop_1995, pop_1996, pop_1997, pop_1998, pop_1999, 
                pop_2000, pop_2001, pop_2002, pop_2003, pop_2004, pop_2005, pop_2006, pop_2007, pop_2008, pop_2009,
                pop_2010, pop_2011,pop_2012, pop_2013, pop_2014, pop_2015, pop_2016, 
                pop_2017, pop_2018, pop_2019, pop_2020, pop_2021, pop_2022)
pop_geo <- reduce(df_list, left_join, by="LACD")

#Replacing colons with NAs but only for PUA col as this is the only column we're interested in here 
pop_geo[pop_geo$PUA==":", "PUA"] <- NA
pop_geo[pop_geo$`Combined Authorities`==":", "Combined Authorities"] <- NA

#Getting columns to sum across
sum_columns <- colnames(pop_geo[9:length(pop_geo)])

#Define function to generate summary 
generate_summary <- function(variable, nas=F){
  if (nas==T){
    filtered_data <- pop_geo[!is.na(pop_geo[[deparse(substitute(variable))]]),]
  } else {
    filtered_data <- pop_geo
  }
  result <- filtered_data %>% group_by({{variable}}) %>%
    summarise(across(sum_columns, sum, na.rm = T)) %>%
    mutate(absolute_change12_22 = POPULATION2022-POPULATION2012,
           percentage_change12_22 = ((POPULATION2022-POPULATION2012)/POPULATION2012)*100)
  return(result)
}

pop_by_pua <- generate_summary(PUA, nas=T)
pop_all <- generate_summary(NA)
pop_by_CA <- generate_summary(`Combined Authorities`, nas=T)
pop_by_region <- generate_summary(`Region`, nas=T) 
pop_by_superregion <- generate_summary(`Super Region`, nas=T) 
pop_by_urban_nonurban <- generate_summary(`Urban_non_urban`, nas=T) 
pop_by_country <- generate_summary(`Country`, nas=T) 

# Find top ten and bottom ten PUAs:
highest_growth <- head(pop_by_pua[order(pop_by_pua$percentage_change12_22, decreasing = TRUE),], 10)
lowest_growth <- head(pop_by_pua[order(pop_by_pua$percentage_change12_22, decreasing = FALSE),], 10)

# export tables:
export_to_excel <- function(data_frames, file_path) {
  # Create a new Excel workbook
  workbook <- createWorkbook()
  # Loop through the data frames
  for (i in seq_along(data_frames)) {
    # Get the data frame and sheet name
    df <- data_frames[[i]]
    sheet_name <- names(data_frames)[i]
    # Add the data frame to the workbook as a new sheet
    addWorksheet(workbook, sheetName = sheet_name)
    writeData(workbook, sheet = sheet_name, x = df)
  }
  # Save the Excel file
  saveWorkbook(workbook, file_path, overwrite = TRUE)
}

date <- format(Sys.Date(), "%Y-%m-%d")
file_name <- paste0(date, "population", ".xlsx")
dir_path <- paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Cities Outlook/Cities Outlook 2024/Chapter 3 Data/Final CSVs for Apend", collapse = NULL)
file_path <- file.path(dir_path, file_name)

# ML EDIT - edited to put all in one excel sheet 
df_list <- list(pop_by_pua, highest_growth, lowest_growth, pop_by_CA, pop_by_region, pop_by_superregion, pop_by_urban_nonurban, pop_by_country, pop_all)
names(df_list) <- c("population_by_PUA", "highest_growth", "lowest_growth", "pop_by_CA", "pop_regions", "pop_by_superregion", "pop_by_urban_nonurban", "pop_by_country", "pop_all")

# Export data frames to Excel
export_to_excel(df_list, file_path)


# Export raw population data to save in inputs folder. Use in future to check raw data used in these calculations (because the data drawn from the nomis API
#might change between running this script)
date_raw <- format(Sys.Date(), "%Y-%m-%d")
file_name_raw <- paste0(date_raw, "population_raw2024", ".xlsx")
dir_path_raw <- paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Cities Outlook/Cities Outlook 2024/Chapter 3 Data/Input data", collapse = NULL)
file_path_raw <- file.path(dir_path_raw, file_name_raw)

# ML EDIT - edited to put all in one excel sheet 
df_list_raw <- list(pop)
names(df_list_raw) <- c("raw population")

# Export data frames to Excel
export_to_excel(df_list_raw, file_path_raw)

#DONE! 
#Could add more code to get data to save into theme data folder - a task for the future!


