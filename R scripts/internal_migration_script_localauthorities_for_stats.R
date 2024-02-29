
library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx) 
library(ggplot2)
library(tidyr)

path = paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Input", collapse = NULL)
setwd(path)

# import datasets 2015-2022 
raw15 <- read_excel("detailedestimates2015on2021and2023las.xlsx", sheet = "2015 on 2021 LAs")
raw16 <- read_excel("detailedestimates2016on2021and2023las.xlsx", sheet = "2016 on 2021 LAs")
raw17 <- read_excel("detailedestimates2017on2021and2023las.xlsx", sheet = "2017 on 2021 LAs")
raw18 <- read_excel("detailedestimates2018on2021and2023las.xlsx", sheet = "2018 on 2021 LAs")
raw19 <- read_excel("detailedestimates2019on2021and2023las.xlsx", sheet = "2019 on 2021 LAs")
raw20 <- read_excel("detailedestimates2020on2021and2023las.xlsx", sheet = "2020 on 2021 LAs")
raw21 <- read_excel("detailedestimates2021on2021and2023las.xlsx", sheet = "2021 on 2021 LAs")
raw22 <- read_excel("detailedestimates2022on2021and2023las.xlsx", sheet = "2022 on 2021 LAs")

# List of data frames
dfs <- list(raw15, raw16, raw17, raw18, raw19, raw20, raw21, raw22)
result_list <- list()

#import the geography lookup:
geography <- read_xlsx("LAtoPUA2024.xlsx")
geography <- geography[,1:8]  # select columns needed

#import Inner/Outer London lookup: 
innerLDN <- read_xlsx("2024_Inner_London_lookup.xlsx")
innerLDN <- select(innerLDN, 1, 3)

#add Inner/Outer London to Geographies, rename columns + label non London as such 
geography <- left_join(geography, innerLDN, by = "LA Code")
geography <- geography %>%
  rename("Region" = "Region...3", "InOutLDN" = "IN or OUT London") 
geography <- geography %>%
  mutate(InOutLDN = if_else(is.na(InOutLDN), "not London", InOutLDN))

#duplicate LA Code column
geography <- cbind(geography[,1], geography)
#rename first column inla and rename second column outla
colnames(geography)[1] <- "inla"
colnames(geography)[2] <- "outla" 

#Specify the list of local authorities you want to keep 
#note the code below treats the SW LAs differently. The London boroughs, it looks for moves between the LA and anywhere NOT in London. 
#For SW it, looks at moves between this LA and London only. 
selected_LA <- c("Kensington and Chelsea", "Westminster", "Hammersmith and Fulham", "Camden", "Richmond upon Thames", "Islington", "Southwark", "Lambeth",
                 "Greenwich", "Tower Hamlets", "Hackney", "Newham", "City of London", "Wandsworth", "Lewisham", "Haringey", "Croydon", "Hounslow",
                 "Watford", "Epping Forest", "Wiltshire", "Cornwall", "Bristol, City of", "Brighton and Hove", "Lewes", "Thanet",
                 "Oxford", "South Oxfordshire", "Seven Oaks", "Winchester", "Barking and Dagenham" = "BND_nonLDF_move",
                 "Barnet" ,       "Bexley" ,   "Brent" ,     "Bromley" ,   "Ealing",   "Enfield" ,   "Harrow" ,   "Havering" ,"Hillingdon" ,
                 "Kingston upon Thames" , "Merton" , "Redbridge" , "Sutton" , "Waltham Forest" ,"Dartford" , "Gravesham","Elmbridge" ,"Epsom and Ewell" ,
                 "Runnymede" ,"Spelthorne" ,   "Woking" ,    "Broxbourne" ,  "Hertsmere" ,"Three Rivers")

create_move_column <- function(data, region, move_column_name) {
  data %>%
    mutate(!!move_column_name :=
             case_when(
               (inLA == !!region & outInOutLDN == "not London") |
                 (outLA == !!region & inInOutLDN == "not London") ~ !!move_column_name,
               TRUE ~ "ignore"
             )
    )
}

london_boroughs <- c(
  "Kensington and Chelsea" = "KC_nonLDN_move",
  "Westminster" = "Westminster_nonLDN_move",
  "Camden"= "Camden_nonLDN_move",
  "Hammersmith and Fulham" = "HF_nonLDN_move",
  "Richmond upon Thames" = "Richmond_nonLDN_move",
  "Southwark" = "Southwark_nonLDN_move",
  "City of London" = "City_nonLDN_move",
  "Wandsworth" = "Wandsworth_nonLDN_move",
  "Newham" = "Newham_nonLDN_move",
  "Lewisham" = "Lewisham_nonLDN_move",
  "Greenwich" = "Greenwich_nonLDN_move",
  "Tower Hamlets" = "TowerHamlets_nonLDN_move",
  "Islington" = "Islington_nonLDN_move",
  "Lambeth" = "Lambeth_nonLDN_move",
  "Hackney" = "Hackney_nonLDN_move",
  "Haringey" = "Haringey_nonLDN_move",
  "Croydon" = "Croydon_nonLDN_move",
  "Watford" = "Watford_nonLDN_move",
  "Epping Forest" = "Epping_Forest_nonLDN_move",
  "Hounslow" = "Hounslow_nonLDN_move", 
  "Barking and Dagenham" = "BND_nonLDF_move",
  "Barnet" = "Barnet_nonLDF_move",
  "Bexley" = "Bexley_nonLDF_move",
  "Brent" = "Brent_nonLDF_move",
  "Bromley" = "Bromley_nonLDF_move",
  "Ealing" = "Ealing_nonLDF_move",
  "Enfield" = "Enfield_nonLDF_move",
  "Harrow" = "Harrow_nonLDF_move",
  "Havering" = "Havering_nonLDF_move",
  "Hillingdon" = "Hillingdon_nonLDF_move",
  "Kingston upon Thames" = "Kingston_nonLDF_move",
  "Merton" = "Merton_nonLDF_move",
  "Redbridge" = "Redbridge_nonLDF_move",
  "Sutton" = "Sutton_nonLDF_move",
  "Waltham Forest" = "WalthamForest_nonLDF_move",
  "Dartford" = "Dartford_nonLDF_move",
  "Gravesham" = "Gravesham_nonLDF_move",
  "Elmbridge" = "Elmbridge_nonLDF_move",
  "Epsom and Ewell" = "Epsom_nonLDF_move",
  "Runnymede" = "Runnymede_nonLDF_move",
  "Spelthorne" = "Spelthorne_nonLDF_move",
  "Woking" = "Woking_nonLDF_move",
  "Broxbourne" = "Broxbourne_nonLDF_move",
  "Hertsmere" = "Hertsmere_nonLDF_move",
  "Three Rivers" = "ThreeRivers_nonLDF_move"
)

# Loop through each dataset and perform operations
for (i in seq_along(dfs)) {
  raw <- dfs[[i]]

  #add in geographies
  raw_geog_in <- geography |> 
    merge(raw, by = "inla") |> 
    mutate(across(everything(), ~ifelse(. == ":", "non_urban", .))) 
  #remove outla.x & rename in columns
  raw_geog_in <- raw_geog_in[, !colnames(raw_geog_in) %in% c("outla.x")]
  raw_geog_in <- raw_geog_in %>%
    rename(inLA = LA, inRegion = Region, inCountry = Country, inPUA = PUA, inCA = "Combined Authorities", inSuperRegion = "Super Region",
           inUrbanRegion = "Urban Region", outla = outla.y, inInOutLDN = InOutLDN)
  
  #add out geographies
  raw_geog_inout <- geography |> 
    merge(raw_geog_in, by = "outla") |>
    mutate(across(everything(), ~ifelse(. == ":", "non_urban", .))) 
  #remove inla.x and rename out columns
  raw_geog_inout <- raw_geog_inout[, !colnames(raw_geog_inout) %in% c("inla.x")]
  raw_geog_inout <- raw_geog_inout %>%
    rename(outLA = LA, outRegion = Region, outCountry = Country, outPUA = PUA, outCA = "Combined Authorities", outSuperRegion = "Super Region",
           outUrbanRegion = "Urban Region", inla = inla.y, outInOutLDN = InOutLDN)
  
  # Apply create_move_column function for each London borough
  for (borough in names(london_boroughs)) {
    raw_geog_inout <- raw_geog_inout %>%
      create_move_column(borough, london_boroughs[borough])
  }
  
  ##understanding moves between London and SE Local authorities (Selected Bristol)
  #create column labelling moves between Hounslow and non-London LAs 
  raw_geog_inout <- raw_geog_inout %>%
    mutate(Winchester_LDN_move = ifelse(inla == "Winchester" & outPUA == "London", "Winchester",
                                     ifelse(inPUA == "London" & outLA == "Winchester", "Winchester", "ignore")))
  
  raw_geog_inout <- raw_geog_inout %>%
    mutate(Thanet_LDN_move = ifelse(inla == "Thanet" & outPUA == "London", "Thanet",
                                     ifelse(inPUA == "London" & outLA == "Thanet", "Thanet", "ignore")))
  
  raw_geog_inout <- raw_geog_inout %>%
    mutate(Oxford_LDN_move = ifelse(inla == "Oxford" & outPUA == "London", "Oxford",
                                     ifelse(inPUA == "London" & outLA == "Oxford", "Oxford", "ignore")))
  
  raw_geog_inout <- raw_geog_inout %>%
    mutate(Lewes_LDN_move = ifelse(inla == "Lewes" & outPUA == "London", "Lewes",
                                    ifelse(inPUA == "London" & outLA == "Lewes", "Lewes", "ignore")))
  
  raw_geog_inout <- raw_geog_inout %>%
    mutate(Brighton_LDN_move = ifelse(inla == "Brighton and Hove" & outPUA == "London", "Brighton and Hove",
                                    ifelse(inPUA == "London" & outLA == "Brighton and Hove", "Brighton and Hove", "ignore")))
  
  raw_geog_inout <- raw_geog_inout %>%
    mutate(SevenOaks_LDN_move = ifelse(inla == "Seven Oaks" & outPUA == "London", "Seven Oaks",
                                    ifelse(inPUA == "London" & outLA == "Seven Oaks", "Seven Oaks", "ignore")))
  
  raw_geog_inout <- raw_geog_inout %>%
    mutate(SouthOxfordshire_LDN_move = ifelse(inla == "South Oxfordshire" & outPUA == "London", "South Oxfordshire",
                                    ifelse(inPUA == "London" & outLA == "South Oxfordshire", "South Oxfordshire", "ignore")))
  
  
  #add age band categories
  raw_geog_inout$Band_0_5 <- raw_geog_inout$Age_0 + raw_geog_inout$Age_1 + raw_geog_inout$Age_2 + raw_geog_inout$Age_3 + raw_geog_inout$Age_4 + raw_geog_inout$Age_5
  raw_geog_inout$Band_18_19 <- raw_geog_inout$Age_18 + raw_geog_inout$Age_19  
  raw_geog_inout$Band_22_25 <- raw_geog_inout$Age_22 + raw_geog_inout$Age_23 + raw_geog_inout$Age_24 + raw_geog_inout$Age_25
  raw_geog_inout$Band_30_35 <- raw_geog_inout$Age_30 + raw_geog_inout$Age_31 + raw_geog_inout$Age_32 + raw_geog_inout$Age_33 + raw_geog_inout$Age_34 + raw_geog_inout$Age_34 
  raw_geog_inout$Band_36_45 <- raw_geog_inout$Age_36 + raw_geog_inout$Age_37 + raw_geog_inout$Age_38 + raw_geog_inout$Age_39 + raw_geog_inout$Age_40 + raw_geog_inout$Age_41 +
    raw_geog_inout$Age_42 + raw_geog_inout$Age_43 + raw_geog_inout$Age_44 + raw_geog_inout$Age_45 
  raw_geog_inout$Band_56_65 <- raw_geog_inout$Age_56 + raw_geog_inout$Age_57 + raw_geog_inout$Age_58 + raw_geog_inout$Age_59 + raw_geog_inout$Age_60 + raw_geog_inout$Age_61 +
    raw_geog_inout$Age_62 + raw_geog_inout$Age_63 + raw_geog_inout$Age_64 + raw_geog_inout$Age_65 
  
  #create all ages summary column
  allages <- raw_geog_inout %>%
    select(contains("Age"))
  
  raw_geog_inout$allages <- rowSums(allages)
  
  # Replace "ignore" with NA
  raw_geog_inout[raw_geog_inout == "ignore"] <- NA
  
  # Use coalesce to select the first non-NA value
  raw_geog_inout$selectedLA_nonLDN_move <- coalesce(
    raw_geog_inout$KC_nonLDN_move,
    raw_geog_inout$Westminster_nonLDN_move,
    raw_geog_inout$Camden_nonLDN_move,
    raw_geog_inout$HF_nonLDN_move,
    raw_geog_inout$Richmond_nonLDN_move,
    raw_geog_inout$Southwark_nonLDN_move,
    raw_geog_inout$City_nonLDN_move,
    raw_geog_inout$Wandsworth_nonLDN_move,
    raw_geog_inout$Newham_nonLDN_move,
    raw_geog_inout$Lewisham_nonLDN_move,
    raw_geog_inout$Greenwich_nonLDN_move,
    raw_geog_inout$TowerHamlets_nonLDN_move,
    raw_geog_inout$Islington_nonLDN_move,
    raw_geog_inout$Lambeth_nonLDN_move,
    raw_geog_inout$Hackney_nonLDN_move,
    raw_geog_inout$Haringey_nonLDN_move,
    raw_geog_inout$Croydon_nonLDN_move,
    raw_geog_inout$Watford_nonLDN_move,
    raw_geog_inout$Epping_Forest_nonLDN_move,
    raw_geog_inout$Hounslow_nonLDN_move, 
    raw_geog_inout$BND_nonLDF_move,
    raw_geog_inout$Barnet_nonLDF_move,
    raw_geog_inout$Bexley_nonLDF_move,
    raw_geog_inout$Brent_nonLDF_move,
    raw_geog_inout$Bromley_nonLDF_move,
    raw_geog_inout$Ealing_nonLDF_move,
    raw_geog_inout$Enfield_nonLDF_move,
    raw_geog_inout$Harrow_nonLDF_move,
    raw_geog_inout$Havering_nonLDF_move,
    raw_geog_inout$Hillingdon_nonLDF_move,
    raw_geog_inout$Kingston_nonLDF_move,
    raw_geog_inout$Merton_nonLDF_move,
    raw_geog_inout$Redbridge_nonLDF_move,
    raw_geog_inout$Sutton_nonLDF_move,
    raw_geog_inout$WalthamForest_nonLDF_move,
    raw_geog_inout$Dartford_nonLDF_move,
    raw_geog_inout$Gravesham_nonLDF_move,
    raw_geog_inout$Elmbridge_nonLDF_move,
    raw_geog_inout$Epsom_nonLDF_move,
    raw_geog_inout$Runnymede_nonLDF_move,
    raw_geog_inout$Spelthorne_nonLDF_move,
    raw_geog_inout$Woking_nonLDF_move,
    raw_geog_inout$Broxbourne_nonLDF_move,
    raw_geog_inout$Hertsmere_nonLDF_move,
    raw_geog_inout$ThreeRivers_nonLDF_move,
    raw_geog_inout$Bristol_LDN_move,
    raw_geog_inout$Cornwall_LDN_move,
    raw_geog_inout$Wiltshire_LDN_move,
    raw_geog_inout$Thanet_LDN_move,
    raw_geog_inout$Winchester_LDN_move,
    raw_geog_inout$Oxford_LDN_move,
    raw_geog_inout$SouthOxfordshire_LDN_move,
    raw_geog_inout$Lewes_LDN_move,
    raw_geog_inout$Brighton_LDN_move,
    raw_geog_inout$SevenOaks_LDN_move
  )
  
  # Replace NA with "ignore" in selectedLA_nonLDN_move column
  raw_geog_inout <- raw_geog_inout %>%
    mutate(selectedLA_nonLDN_move = ifelse(is.na(selectedLA_nonLDN_move), "ignore", selectedLA_nonLDN_move))
  
  #collapse by selected LA
  sum_in_selectLA <- raw_geog_inout %>%
    group_by(inLA) %>%
    filter(selectedLA_nonLDN_move != "ignore") %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  age_bands_in <- sum_in_selectLA %>%
    select(1, (ncol(sum_in_selectLA) - 6):ncol(sum_in_selectLA))
  #name columns
  colnames(age_bands_in) <- c("LA", "0_5_in","18_19_in", "22_25_in", "30_35_in", "36_45_in","56_65_in", "allages_in")
  
  sum_out_selectLA <- raw_geog_inout %>%
    group_by(outLA) %>%
    filter(selectedLA_nonLDN_move != "ignore") %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  age_bands_out <- sum_out_selectLA %>%
    select(1, (ncol(sum_out_selectLA) - 6):ncol(sum_out_selectLA)) 
  #name columns
  colnames(age_bands_out) <- c("LA", "0_5_out", "18_19_out", "22_25_out", "30_35_out","36_45_out","56_65_out", "allages_out")
  
  #join bands in and bands out
  age_bands_inout <- left_join(age_bands_in, age_bands_out, by = "LA")
  
  # Subtracting columns and creating new net columns
  age_bands_inout$net0_5 <- age_bands_inout$"0_5_in" - age_bands_inout$"0_5_out"
  age_bands_inout$net18_19 <- age_bands_inout$"18_19_in" - age_bands_inout$"18_19_out"
  age_bands_inout$net22_25 <- age_bands_inout$"22_25_in" - age_bands_inout$"22_25_out"
  age_bands_inout$net30_35 <- age_bands_inout$"30_35_in" - age_bands_inout$"30_35_out"
  age_bands_inout$net36_45 <- age_bands_inout$"36_45_in" - age_bands_inout$"36_45_out"
  age_bands_inout$net56_65 <- age_bands_inout$"56_65_in" - age_bands_inout$"56_65_out"
  age_bands_inout$total <- age_bands_inout$allages_in - age_bands_inout$allages_out
  
  #select LAs we care about only
  age_bands_inout <- age_bands_inout[age_bands_inout$LA %in% selected_LA, ]
  
  # Convert tibble to data frame to make averaging later better
  age_bands_inout <- as.data.frame(age_bands_inout)
  # Set the first column as row names
  rownames(age_bands_inout) <- age_bands_inout[, 1]
  # Remove the first column after setting it as row names
  age_bands_inout <- age_bands_inout[, -1]
  
  # Append data_inlondon to the result list
  result_list[[i]] <- list(age_bands_inout = age_bands_inout)
  
}

# Loop through each element in the result_list
for (i in seq_along(result_list)) {
  
  # Extract data_inlondon and data_outlondon from the i-th element of the result_list
  age_bands_inout <- result_list[[i]]$age_bands_inout
  
  # Assign them to separate datasets with meaningful names
  assign(paste0("age_bands_inout_", 2014+i), age_bands_inout)
  
}
  
### I HAVE INSERTED THE MAKE DATAFRAME NOT A TIBLLE THING, SO WE CAN DO THE AVERAGE, IN THE LOOP ABOVE. 

##CREATING A 15-19 AVERAGE DATASET
average_2015_2019 <- (as.matrix(age_bands_inout_2015) + as.matrix(age_bands_inout_2016) + as.matrix(age_bands_inout_2017) + as.matrix(age_bands_inout_2018)
                      + as.matrix(age_bands_inout_2019)) / 5

average_2015_2019 <- as.data.frame(average_2015_2019)


# get rownames back into column 1 and select only the 5 target authorities.
#repeat for relevant years 
age_bands_inout_2022$LA <- rownames(age_bands_inout_2022)
age_bands_inout_2022 <- age_bands_inout_2022[, c("LA", setdiff(names(age_bands_inout_2022), "LA"))]
rownames(age_bands_inout_2022) <- NULL
age_bands_inout_2022 <- age_bands_inout_2022[age_bands_inout_2022$LA %in% selected_LA, ]

age_bands_inout_2021$LA <- rownames(age_bands_inout_2021)
age_bands_inout_2021 <- age_bands_inout_2021[, c("LA", setdiff(names(age_bands_inout_2021), "LA"))]
rownames(age_bands_inout_2021) <- NULL
age_bands_inout_2021 <- age_bands_inout_2021[age_bands_inout_2021$LA %in% selected_LA, ]

age_bands_inout_2020$LA <- rownames(age_bands_inout_2020)
age_bands_inout_2020 <- age_bands_inout_2020[, c("LA", setdiff(names(age_bands_inout_2020), "LA"))]
rownames(age_bands_inout_2020) <- NULL
age_bands_inout_2020 <- age_bands_inout_2020[age_bands_inout_2020$LA %in% selected_LA, ]

average_2015_2019$LA <- rownames(average_2015_2019)
average_2015_2019 <- average_2015_2019[, c("LA", setdiff(names(average_2015_2019), "LA"))]
rownames(average_2015_2019) <- NULL
average_2015_2019 <- average_2015_2019[average_2015_2019$LA %in% selected_LA, ]


### EXPORT age band stats to excel
# Export data as an excel workbook with multiple sheets:
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

# Generate file path for Excel workbook with date
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "_Int_mig_age_bands_selectedLAs", ".csv")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Output")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(average_2015_2019, age_bands_inout_2022, age_bands_inout_2021, age_bands_inout_2020)
names(df_list_geog) <- c("2015_2019", "2022", "2021", "2020")

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)
  
  
  
