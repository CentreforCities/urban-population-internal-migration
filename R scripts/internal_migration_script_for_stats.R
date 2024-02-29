
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
geography <- geography[,1:8]  # select columns Needed

#import Inner/Outer London lookup: 
InnerLDN <- read_xlsx("2024_Inner_London_lookup.xlsx")
InnerLDN <- select(InnerLDN, 1, 3)

#add Inner/Outer London to Geographies, rename columns + label non London as such 
geography <- left_join(geography, InnerLDN, by = "LA Code")
geography <- geography %>%
  rename("Region" = "Region...3", "InOutLDN" = "IN or OUT London") 
geography <- geography %>%
  mutate(InOutLDN = if_else(is.na(InOutLDN), "not London", InOutLDN))

#duplicate LA Code column
geography <- cbind(geography[,1], geography)
#rename first column inla and rename second column outla
colnames(geography)[1] <- "inla"
colnames(geography)[2] <- "outla" 

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
  
  #create column labelling intraPUA moves 
  raw_geog_inout <- raw_geog_inout %>%
    mutate(insidePUA = ifelse(inPUA == outPUA & inPUA == "non_urban", "non_urban_both",
                              ifelse(inPUA == outPUA, "intraPUAmove", "outPUAmove")))
  #create column labelling intraRegion moves
  raw_geog_inout <- raw_geog_inout %>%
    mutate(insideRegion = ifelse(inRegion == outRegion, "intraRegionmove", "betweenRegion"))
  #create column labelling intraSuperRegion moves 
  raw_geog_inout <- raw_geog_inout %>%
    mutate(insideSR = ifelse(inSuperRegion == outSuperRegion, "intraSRmove", "betweenSR"))
  #create column labelling intraInnerLDN moves 
  raw_geog_inout <- raw_geog_inout %>%
    mutate(insideInnerLondon = ifelse(inInOutLDN == outInOutLDN & inInOutLDN == "not London", "not London both",
                                      ifelse(inInOutLDN == outInOutLDN & inInOutLDN == "Outer London", "intraOuterLDN",
                                             ifelse(inInOutLDN == outInOutLDN, "intraInnerLDN", "move between InnerLDN and elsewhere")))) 
  #create column labelling moves between Inner London and nonLondon LAs
  raw_geog_inout <- raw_geog_inout %>%
    mutate(InnerLondonMove = ifelse(inInOutLDN == outInOutLDN & inInOutLDN == "not London", "not London both",
                                    ifelse(inInOutLDN == outInOutLDN & inInOutLDN == "Outer London", "Outer London both",
                                           ifelse(inInOutLDN == "Outer London" & outInOutLDN == "Inner London", "Inner to Outer London",
                                                  ifelse(inInOutLDN == "Inner London" & outInOutLDN == "Outer London", "Outer to Inner London",
                                                         ifelse(inInOutLDN == "Outer London" & outInOutLDN == "not London", "non to Outer London",
                                                                ifelse(inInOutLDN == "not London" & outInOutLDN == "Outer London", "Outer to non London",
                                                                       ifelse(inInOutLDN == outInOutLDN, "intraInnerLDNmove", "move between Inner and non London"))))))))
  #create column labelling moves between Outer London and nonLondon LAs
  raw_geog_inout <- raw_geog_inout %>%
    mutate(OuterLondonMove = ifelse(inInOutLDN == outInOutLDN & inInOutLDN == "not London", "not London both",
                                    ifelse(inInOutLDN == outInOutLDN & inInOutLDN == "Outer London", "Outer London both",
                                           ifelse(inInOutLDN == "Outer London" & outInOutLDN == "Inner London", "Inner to Outer London",
                                                  ifelse(inInOutLDN == "Inner London" & outInOutLDN == "Outer London", "Outer to Inner London",
                                                         ifelse(inInOutLDN == "Inner London" & outInOutLDN == "not London", "non to Inner London",
                                                                ifelse(inInOutLDN == "not London" & outInOutLDN == "Inner London", "Inner to non London",
                                                                       ifelse(inInOutLDN == outInOutLDN, "intraOuterLDNmove", "move between Outer and non London"))))))))
  #create column labelling moves between Inner and Outer London LAs
  raw_geog_inout <- raw_geog_inout %>%
    mutate(BetweenInOutLondonMove = ifelse(inInOutLDN == outInOutLDN & inInOutLDN == "not London", "ignore",
                                           ifelse(inInOutLDN == "Inner London" & outInOutLDN == "Outer London", "Between Inner and Outer London",
                                                  ifelse(inInOutLDN == "Outer London" & outInOutLDN == "Inner London", "Between Inner and Outer London", "ignore"
                                                  ))))
  ## Between London and Regions
  #create column labelling moves between London and South East
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_SouthEast = ifelse(outPUA == inPUA & inPUA == "London", "ignore",
                                           ifelse(outPUA == "London" & inRegion == "South East", "London to South East",
                                                  ifelse(outRegion == "South East" & inPUA == "London", "South East to London", "ignore"))))
  #create column labelling moves between London and South West
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_SouthWest = ifelse(outPUA == "London" & inRegion == "South West", "London to South West",
                                    ifelse(outRegion == "South West" & inPUA == "London", "South West to London", "ignore")))
  #create column labelling moves between London and East
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_East = ifelse(outPUA == "London" & inRegion == "East", "London to East",
                                     ifelse(outRegion == "East" & inPUA == "London", "East to London", "ignore")))
  #create column labelling moves between London and North East
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_NorthEast = ifelse(outPUA == "London" & inRegion == "North East", "London to North East",
                                     ifelse(outRegion == "North East" & inPUA == "London", "North East to London", "ignore")))
  #create column labelling moves between London and North West
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_NorthWest = ifelse(outPUA == "London" & inRegion == "North West", "London to North West",
                                     ifelse(outRegion == "North West" & inPUA == "London", "North West to London", "ignore")))
  #create column labelling moves between London and East Midlands
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_EastMidlands = ifelse(outPUA == "London" & inRegion == "East Midlands", "London to East Midlands",
                                     ifelse(outRegion == "East Midlands" & inPUA == "London", "East Midlands to London", "ignore")))
  #create column labelling moves between London and West Midlands
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_WestMidlands = ifelse(outPUA == "London" & inRegion == "West Midlands", "London to West Midlands",
                                        ifelse(outRegion == "West Midlands" & inPUA == "London", "West Midlands to London", "ignore")))
  #create column labelling moves between London and Yorkshire & Humber
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_YorkHum = ifelse(outPUA == "London" & inRegion == "Yorkshire & Humber", "London to Yorkshire & Humber",
                                        ifelse(outRegion == "Yorkshire & Humber" & inPUA == "London", "Yorkshire & Humber to London", "ignore")))
  #create column labelling moves between London and Wales
  raw_geog_inout <- raw_geog_inout %>%
    mutate(London_Wales = ifelse(outPUA == "London" & inRegion == "Wales", "London to Wales",
                                     ifelse(outRegion == "Wales" & inPUA == "London", "Wales to London", "ignore")))
  
  
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
  
  #collapse by in-PUA, excluding intraPUA moves
  sum_in_PUA <- raw_geog_inout %>%
    group_by(inPUA) %>%
    filter(insidePUA == "outPUAmove") %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  age_bands_in <- sum_in_PUA %>%
    select(1, (ncol(sum_in_PUA) - 6):ncol(sum_in_PUA))
  #name columns
  colnames(age_bands_in) <- c("PUA", "0_5_in","18_19_in", "22_25_in", "30_35_in", "36_45_in", "56_65_in","allages_in")
  
  sum_out_PUA <- raw_geog_inout %>%
    group_by(outPUA) %>%
    filter(insidePUA == "outPUAmove") %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  age_bands_out <- sum_out_PUA %>%
    select(1, (ncol(sum_out_PUA) - 6):ncol(sum_out_PUA)) 
  #name columns
  colnames(age_bands_out) <- c("PUA", "0_5_out", "18_19_out", "22_25_out", "30_35_out", "36_45_out", "56_65_out","allages_out")
  
  
  ### Inner London to outside London PUA analysis
  sum_in_InnerLDN <- raw_geog_inout %>%
    filter(InnerLondonMove == "move between Inner and non London") %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  Inner_age_bands_in <- sum_in_InnerLDN %>%
    select(1, (ncol(sum_in_InnerLDN) - 6):ncol(sum_in_InnerLDN))
  #name columns
  colnames(Inner_age_bands_in) <- c("PUA", "Inner_0_5_in", "Inner_18_19_in", "Inner_22_25_in", "Inner_30_35_in", "Inner_36_45_in", "Inner_56_65_in", "Inner_allages_in")
  
  sum_out_InnerLDN <- raw_geog_inout %>%
    filter(InnerLondonMove == "move between Inner and non London") %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  Inner_age_bands_out <- sum_out_InnerLDN %>%
    select(1, (ncol(sum_out_InnerLDN) - 6):ncol(sum_out_InnerLDN))
  #name columns
  colnames(Inner_age_bands_out) <- c("PUA", "Inner_0_5_out", "Inner_18_19_out", "Inner_22_25_out", "Inner_30_35_out", "Inner_36_45_out", "Inner_56_65_out","Inner_allages_out")
  
  
  ### Outer London to outside London PUA analysis
  sum_in_OuterLDN <- raw_geog_inout %>%
    filter(OuterLondonMove == "move between Outer and non London") %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  outer_age_bands_in <- sum_in_OuterLDN %>%
    select(1, (ncol(sum_in_OuterLDN) - 6):ncol(sum_in_OuterLDN))
  #name columns
  colnames(outer_age_bands_in) <- c("PUA","outer_0_5_in", "outer_18_19_in", "outer_22_25_in", "outer_30_35_in", "outer_36_45_in", "outer_56_65_in","outer_allages_in")
  
  sum_out_OuterLDN <- raw_geog_inout %>%
    filter(OuterLondonMove == "move between Outer and non London") %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  outer_age_bands_out <- sum_out_OuterLDN %>%
    select(1, (ncol(sum_out_OuterLDN) - 6):ncol(sum_out_OuterLDN))
  #name columns
  colnames(outer_age_bands_out) <- c("PUA","outer_0_5_out", "outer_18_19_out", "outer_22_25_out", "outer_30_35_out", "outer_36_45_out","outer_56_65_out", "outer_allages_out")
  
  
  ### Inner London to Outer London PUA analysis
  sum_in_InOutLDN <- raw_geog_inout %>%
    filter(BetweenInOutLondonMove == "Between Inner and Outer London") %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  inout_age_bands_in <- sum_in_InOutLDN %>%
    select(1, (ncol(sum_in_InOutLDN) - 6):ncol(sum_in_InOutLDN))
  #name columns
  colnames(inout_age_bands_in) <- c("PUA", "inout_0_5_in","inout_18_19_in", "inout_22_25_in", "inout_30_35_in", "inout_36_45_in","inout_56_65_in", "inout_allages_in")
  
  sum_out_InOutLDN <- raw_geog_inout %>%
    filter(BetweenInOutLondonMove == "Between Inner and Outer London") %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  inout_age_bands_out <- sum_out_InOutLDN %>%
    select(1, (ncol(sum_out_InOutLDN) - 6):ncol(sum_out_InOutLDN))
  #name columns
  colnames(inout_age_bands_out) <- c("PUA","inout_0_5_out","inout_18_19_out", "inout_22_25_out", "inout_30_35_out", "inout_36_45_out", "inout_56_65_out", "inout_allages_out")
  
  
  ### London PUA to South East analysis
  sum_in_SELDN <- raw_geog_inout %>%
    filter(London_SouthEast %in% c("London to South East", "South East to London")) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  SE_age_bands_in <- sum_in_SELDN %>%
    select(1, (ncol(sum_in_SELDN) - 6):ncol(sum_in_SELDN))
  #name columns
  colnames(SE_age_bands_in) <- c("PUA", "SE_0_5_in","SE_18_19_in", "SE_22_25_in", "SE_30_35_in", "SE_36_45_in","SE_56_65_in", "SE_allages_in")
  
  sum_out_SELDN <- raw_geog_inout %>%
    filter(London_SouthEast %in% c("London to South East", "South East to London")) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #select bands only
  SE_age_bands_out <- sum_out_SELDN %>%
    select(1, (ncol(sum_out_SELDN) - 6):ncol(sum_out_SELDN))
  #name columns
  colnames(SE_age_bands_out) <- c("PUA","SE_0_5_out","SE_18_19_out", "SE_22_25_out", "SE_30_35_out", "SE_36_45_out", "SE_56_65_out", "SE_allages_out")
  
  ### London PUA to South West analysis
  sum_in_SWLDN <- raw_geog_inout %>%
    filter(London_SouthWest %in% c("London to South West", "South West to London")) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #SWlect bands only
  SW_age_bands_in <- sum_in_SWLDN %>%
    select(1, (ncol(sum_in_SWLDN) - 6):ncol(sum_in_SWLDN))
  #name columns
  colnames(SW_age_bands_in) <- c("PUA", "SW_0_5_in","SW_18_19_in", "SW_22_25_in", "SW_30_35_in", "SW_36_45_in","SW_56_65_in", "SW_allages_in")
  
  sum_out_SWLDN <- raw_geog_inout %>%
    filter(London_SouthWest %in% c("London to South West", "South West to London")) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #SWlect bands only
  SW_age_bands_out <- sum_out_SWLDN %>%
    select(1, (ncol(sum_out_SWLDN) - 6):ncol(sum_out_SWLDN))
  #name columns
  colnames(SW_age_bands_out) <- c("PUA","SW_0_5_out","SW_18_19_out", "SW_22_25_out", "SW_30_35_out", "SW_36_45_out", "SW_56_65_out", "SW_allages_out")
  
  ### London PUA to East analysis
  sum_in_EastLDN <- raw_geog_inout %>%
    filter(London_East %in% c("London to East", "East to London")) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #Eastlect bands only
  East_age_bands_in <- sum_in_EastLDN %>%
    select(1, (ncol(sum_in_EastLDN) - 6):ncol(sum_in_EastLDN))
  #name columns
  colnames(East_age_bands_in) <- c("PUA", "East_0_5_in","East_18_19_in", "East_22_25_in", "East_30_35_in", "East_36_45_in","East_56_65_in", "East_allages_in")
  
  sum_out_EastLDN <- raw_geog_inout %>%
  filter(London_East %in% c("London to East", "East to London")) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #Eastlect bands only
  East_age_bands_out <- sum_out_EastLDN %>%
    select(1, (ncol(sum_out_EastLDN) - 6):ncol(sum_out_EastLDN))
  #name columns
  colnames(East_age_bands_out) <- c("PUA","East_0_5_out","East_18_19_out", "East_22_25_out", "East_30_35_out", "East_36_45_out", "East_56_65_out", "East_allages_out")
  
  ### London PUA to North West analysis
  sum_in_NWLDN <- raw_geog_inout %>%
    filter(
      London_NorthWest %in% c("London to North West", "North West to London")
    ) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #NWlect bands only
  NW_age_bands_in <- sum_in_NWLDN %>%
    select(1, (ncol(sum_in_NWLDN) - 6):ncol(sum_in_NWLDN))
  #name columns
  colnames(NW_age_bands_in) <- c("PUA", "NW_0_5_in","NW_18_19_in", "NW_22_25_in", "NW_30_35_in", "NW_36_45_in","NW_56_65_in", "NW_allages_in")
  
  sum_out_NWLDN <- raw_geog_inout %>%
    filter(
      London_NorthWest %in% c("London to North West", "North West to London")
    ) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #NWlect bands only
  NW_age_bands_out <- sum_out_NWLDN %>%
    select(1, (ncol(sum_out_NWLDN) - 6):ncol(sum_out_NWLDN))
  #name columns
  colnames(NW_age_bands_out) <- c("PUA","NW_0_5_out","NW_18_19_out", "NW_22_25_out", "NW_30_35_out", "NW_36_45_out", "NW_56_65_out", "NW_allages_out")
  
  ### London PUA to North East analysis
  sum_in_NELDN <- raw_geog_inout %>%
    filter(
      London_NorthEast %in% c("London to North East", "North East to London")
    ) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #NElect bands only
  NE_age_bands_in <- sum_in_NELDN %>%
    select(1, (ncol(sum_in_NELDN) - 6):ncol(sum_in_NELDN))
  #name columns
  colnames(NE_age_bands_in) <- c("PUA", "NE_0_5_in","NE_18_19_in", "NE_22_25_in", "NE_30_35_in", "NE_36_45_in","NE_56_65_in", "NE_allages_in")
  
  sum_out_NELDN <- raw_geog_inout %>%
    filter(
      London_NorthEast %in% c("London to North East", "North East to London")
    ) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #NElect bands only
  NE_age_bands_out <- sum_out_NELDN %>%
    select(1, (ncol(sum_out_NELDN) - 6):ncol(sum_out_NELDN))
  #name columns
  colnames(NE_age_bands_out) <- c("PUA","NE_0_5_out","NE_18_19_out", "NE_22_25_out", "NE_30_35_out", "NE_36_45_out", "NE_56_65_out", "NE_allages_out")
  
  ### London PUA to West Midlands analysis
  sum_in_WMidsLDN <- raw_geog_inout %>%
    filter(
      London_WestMidlands %in% c("London to West Midlands", "West Midlands to London")
    ) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #WMidslect bands only
  WMids_age_bands_in <- sum_in_WMidsLDN %>%
    select(1, (ncol(sum_in_WMidsLDN) - 6):ncol(sum_in_WMidsLDN))
  #name columns
  colnames(WMids_age_bands_in) <- c("PUA", "WMids_0_5_in","WMids_18_19_in", "WMids_22_25_in", "WMids_30_35_in", "WMids_36_45_in","WMids_56_65_in", "WMids_allages_in")
  
  sum_out_WMidsLDN <- raw_geog_inout %>%
    filter(
      London_WestMidlands %in% c("London to West Midlands", "West Midlands to London")
    ) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #WMidslect bands only
  WMids_age_bands_out <- sum_out_WMidsLDN %>%
    select(1, (ncol(sum_out_WMidsLDN) - 6):ncol(sum_out_WMidsLDN))
  #name columns
  colnames(WMids_age_bands_out) <- c("PUA","WMids_0_5_out","WMids_18_19_out", "WMids_22_25_out", "WMids_30_35_out", "WMids_36_45_out", "WMids_56_65_out", "WMids_allages_out")
  
  ### London PUA to East Midlands analysis
  sum_in_EMidsLDN <- raw_geog_inout %>%
    filter(
      London_EastMidlands %in% c("London to East Midlands", "East Midlands to London")
    ) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #EMidslect bands only
  EMids_age_bands_in <- sum_in_EMidsLDN %>%
    select(1, (ncol(sum_in_EMidsLDN) - 6):ncol(sum_in_EMidsLDN))
  #name columns
  colnames(EMids_age_bands_in) <- c("PUA", "EMids_0_5_in","EMids_18_19_in", "EMids_22_25_in", "EMids_30_35_in", "EMids_36_45_in","EMids_56_65_in", "EMids_allages_in")
  
  sum_out_EMidsLDN <- raw_geog_inout %>%
    filter(
      London_EastMidlands %in% c("London to East Midlands", "East Midlands to London")
    ) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #EMidslect bands only
  EMids_age_bands_out <- sum_out_EMidsLDN %>%
    select(1, (ncol(sum_out_EMidsLDN) - 6):ncol(sum_out_EMidsLDN))
  #name columns
  colnames(EMids_age_bands_out) <- c("PUA","EMids_0_5_out","EMids_18_19_out", "EMids_22_25_out", "EMids_30_35_out", "EMids_36_45_out", "EMids_56_65_out", "EMids_allages_out")
  
  ### London PUA to Yorkshire & Humber analysis
  sum_in_YHLDN <- raw_geog_inout %>%
    filter(
      London_YorkHum %in% c("London to Yorkshire & Humber", "Yorkshire & Humber to London")
    ) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #YHlect bands only
  YH_age_bands_in <- sum_in_YHLDN %>%
    select(1, (ncol(sum_in_YHLDN) - 6):ncol(sum_in_YHLDN))
  #name columns
  colnames(YH_age_bands_in) <- c("PUA", "YH_0_5_in","YH_18_19_in", "YH_22_25_in", "YH_30_35_in", "YH_36_45_in","YH_56_65_in", "YH_allages_in")
  
  sum_out_YHLDN <- raw_geog_inout %>%
    filter(
      London_YorkHum %in% c("London to Yorkshire & Humber", "Yorkshire & Humber to London")
    ) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #YHlect bands only
  YH_age_bands_out <- sum_out_YHLDN %>%
    select(1, (ncol(sum_out_YHLDN) - 6):ncol(sum_out_YHLDN))
  #name columns
  colnames(YH_age_bands_out) <- c("PUA","YH_0_5_out","YH_18_19_out", "YH_22_25_out", "YH_30_35_out", "YH_36_45_out", "YH_56_65_out", "YH_allages_out")
  
  ### London PUA to Wales analysis
  sum_in_WalesLDN <- raw_geog_inout %>%
    filter(
      London_Wales %in% c("London to Wales", "Wales to London")
    ) %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #Waleslect bands only
  Wales_age_bands_in <- sum_in_WalesLDN %>%
    select(1, (ncol(sum_in_WalesLDN) - 6):ncol(sum_in_WalesLDN))
  #name columns
  colnames(Wales_age_bands_in) <- c("PUA", "Wales_0_5_in","Wales_18_19_in", "Wales_22_25_in", "Wales_30_35_in", "Wales_36_45_in","Wales_56_65_in", "Wales_allages_in")
  
  sum_out_WalesLDN <- raw_geog_inout %>%
  filter(
    London_Wales %in% c("London to Wales", "Wales to London")
  ) %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with(c("Age", "Band", "all")), sum))
  #Waleslect bands only
  Wales_age_bands_out <- sum_out_WalesLDN %>%
    select(1, (ncol(sum_out_WalesLDN) - 6):ncol(sum_out_WalesLDN))
  #name columns
  colnames(Wales_age_bands_out) <- c("PUA","Wales_0_5_out","Wales_18_19_out", "Wales_22_25_out", "Wales_30_35_out", "Wales_36_45_out", "Wales_56_65_out", "Wales_allages_out")
  
  #join bands in and bands out
  age_bands_inout <- left_join(age_bands_in, age_bands_out, by = "PUA")
  Inner_age_bands_inout <- left_join(Inner_age_bands_in, Inner_age_bands_out, by = "PUA")
  outer_age_bands_inout <- left_join(outer_age_bands_in, outer_age_bands_out, by = "PUA")
  inout_age_bands_inout <- left_join(inout_age_bands_in, inout_age_bands_out, by = "PUA")
  SE_age_bands_SE <- left_join(SE_age_bands_in, SE_age_bands_out, by = "PUA")
  SW_age_bands_SW <- left_join(SW_age_bands_in, SW_age_bands_out, by = "PUA")
  East_age_bands_East <- left_join(East_age_bands_in, East_age_bands_out, by = "PUA")
  NW_age_bands_NW <- left_join(NW_age_bands_in, NW_age_bands_out, by = "PUA")
  NE_age_bands_NE <- left_join(NE_age_bands_in, NE_age_bands_out, by = "PUA")
  EMids_age_bands_EMids <- left_join(EMids_age_bands_in, EMids_age_bands_out, by = "PUA")
  WMids_age_bands_WMids <- left_join(WMids_age_bands_in, WMids_age_bands_out, by = "PUA")
  YH_age_bands_YH <- left_join(YH_age_bands_in, YH_age_bands_out, by = "PUA")
  Wales_age_bands_Wales <- left_join(Wales_age_bands_in, Wales_age_bands_out, by = "PUA")
  
  # creating Net columns
  age_bands_inout$Net0_5 <- age_bands_inout$"0_5_in" - age_bands_inout$"0_5_out"
  age_bands_inout$Net18_19 <- age_bands_inout$"18_19_in" - age_bands_inout$"18_19_out"
  age_bands_inout$Net22_25 <- age_bands_inout$"22_25_in" - age_bands_inout$"22_25_out"
  age_bands_inout$Net30_35 <- age_bands_inout$"30_35_in" - age_bands_inout$"30_35_out"
  age_bands_inout$Net36_45 <- age_bands_inout$"36_45_in" - age_bands_inout$"36_45_out"
  age_bands_inout$Net56_65 <- age_bands_inout$"56_65_in" - age_bands_inout$"56_65_out"
  age_bands_inout$total <- age_bands_inout$allages_in - age_bands_inout$allages_out
  
  Inner_age_bands_inout$Net0_5 <- Inner_age_bands_inout$"Inner_0_5_in" - Inner_age_bands_inout$"Inner_0_5_out"
  Inner_age_bands_inout$Net18_19 <- Inner_age_bands_inout$"Inner_18_19_in" - Inner_age_bands_inout$"Inner_18_19_out"
  Inner_age_bands_inout$Net22_25 <- Inner_age_bands_inout$"Inner_22_25_in" - Inner_age_bands_inout$"Inner_22_25_out"
  Inner_age_bands_inout$Net30_35 <- Inner_age_bands_inout$"Inner_30_35_in" - Inner_age_bands_inout$"Inner_30_35_out"
  Inner_age_bands_inout$Net36_45 <- Inner_age_bands_inout$"Inner_36_45_in" - Inner_age_bands_inout$"Inner_36_45_out"
  Inner_age_bands_inout$Net56_65 <- Inner_age_bands_inout$"Inner_56_65_in" - Inner_age_bands_inout$"Inner_56_65_out"
  Inner_age_bands_inout$total <- Inner_age_bands_inout$Inner_allages_in - Inner_age_bands_inout$Inner_allages_out
  
  outer_age_bands_inout$Net0_5 <- outer_age_bands_inout$"outer_0_5_in" - outer_age_bands_inout$"outer_0_5_out"
  outer_age_bands_inout$Net18_19 <- outer_age_bands_inout$"outer_18_19_in" - outer_age_bands_inout$"outer_18_19_out"
  outer_age_bands_inout$Net22_25 <- outer_age_bands_inout$"outer_22_25_in" - outer_age_bands_inout$"outer_22_25_out"
  outer_age_bands_inout$Net30_35 <- outer_age_bands_inout$"outer_30_35_in" - outer_age_bands_inout$"outer_30_35_out"
  outer_age_bands_inout$Net36_45 <- outer_age_bands_inout$"outer_36_45_in" - outer_age_bands_inout$"outer_36_45_out"
  outer_age_bands_inout$Net56_65 <- outer_age_bands_inout$"outer_56_65_in" - outer_age_bands_inout$"outer_56_65_out"
  outer_age_bands_inout$total <- outer_age_bands_inout$outer_allages_in - outer_age_bands_inout$outer_allages_out
  
  inout_age_bands_inout$Net0_5 <- inout_age_bands_inout$"inout_0_5_in" - inout_age_bands_inout$"inout_0_5_out"
  inout_age_bands_inout$Net18_19 <- inout_age_bands_inout$"inout_18_19_in" - inout_age_bands_inout$"inout_18_19_out"
  inout_age_bands_inout$Net22_25 <- inout_age_bands_inout$"inout_22_25_in" - inout_age_bands_inout$"inout_22_25_out"
  inout_age_bands_inout$Net30_35 <- inout_age_bands_inout$"inout_30_35_in" - inout_age_bands_inout$"inout_30_35_out"
  inout_age_bands_inout$Net36_45 <- inout_age_bands_inout$"inout_36_45_in" - inout_age_bands_inout$"inout_36_45_out"
  inout_age_bands_inout$Net56_65 <- inout_age_bands_inout$"inout_56_65_in" - inout_age_bands_inout$"inout_56_65_out"
  inout_age_bands_inout$total <- inout_age_bands_inout$inout_allages_in - inout_age_bands_inout$inout_allages_out
  
  SE_age_bands_SE$Net0_5 <- SE_age_bands_SE$"SE_0_5_in" - SE_age_bands_SE$"SE_0_5_out"
  SE_age_bands_SE$Net18_19 <- SE_age_bands_SE$"SE_18_19_in" - SE_age_bands_SE$"SE_18_19_out"
  SE_age_bands_SE$Net22_25 <- SE_age_bands_SE$"SE_22_25_in" - SE_age_bands_SE$"SE_22_25_out"
  SE_age_bands_SE$Net30_35 <- SE_age_bands_SE$"SE_30_35_in" - SE_age_bands_SE$"SE_30_35_out"
  SE_age_bands_SE$Net36_45 <- SE_age_bands_SE$"SE_36_45_in" - SE_age_bands_SE$"SE_36_45_out"
  SE_age_bands_SE$Net56_65 <- SE_age_bands_SE$"SE_56_65_in" - SE_age_bands_SE$"SE_56_65_out"
  SE_age_bands_SE$total <- SE_age_bands_SE$SE_allages_in - SE_age_bands_SE$SE_allages_out
  
  SW_age_bands_SW$Net0_5 <- SW_age_bands_SW$"SW_0_5_in" - SW_age_bands_SW$"SW_0_5_out"
  SW_age_bands_SW$Net18_19 <- SW_age_bands_SW$"SW_18_19_in" - SW_age_bands_SW$"SW_18_19_out"
  SW_age_bands_SW$Net22_25 <- SW_age_bands_SW$"SW_22_25_in" - SW_age_bands_SW$"SW_22_25_out"
  SW_age_bands_SW$Net30_35 <- SW_age_bands_SW$"SW_30_35_in" - SW_age_bands_SW$"SW_30_35_out"
  SW_age_bands_SW$Net36_45 <- SW_age_bands_SW$"SW_36_45_in" - SW_age_bands_SW$"SW_36_45_out"
  SW_age_bands_SW$Net56_65 <- SW_age_bands_SW$"SW_56_65_in" - SW_age_bands_SW$"SW_56_65_out"
  SW_age_bands_SW$total <- SW_age_bands_SW$SW_allages_in - SW_age_bands_SW$SW_allages_out
  
  East_age_bands_East$Net0_5 <- East_age_bands_East$"East_0_5_in" - East_age_bands_East$"East_0_5_out"
  East_age_bands_East$Net18_19 <- East_age_bands_East$"East_18_19_in" - East_age_bands_East$"East_18_19_out"
  East_age_bands_East$Net22_25 <- East_age_bands_East$"East_22_25_in" - East_age_bands_East$"East_22_25_out"
  East_age_bands_East$Net30_35 <- East_age_bands_East$"East_30_35_in" - East_age_bands_East$"East_30_35_out"
  East_age_bands_East$Net36_45 <- East_age_bands_East$"East_36_45_in" - East_age_bands_East$"East_36_45_out"
  East_age_bands_East$Net56_65 <- East_age_bands_East$"East_56_65_in" - East_age_bands_East$"East_56_65_out"
  East_age_bands_East$total <- East_age_bands_East$East_allages_in - East_age_bands_East$East_allages_out
  
  NE_age_bands_NE$Net0_5 <- NE_age_bands_NE$"NE_0_5_in" - NE_age_bands_NE$"NE_0_5_out"
  NE_age_bands_NE$Net18_19 <- NE_age_bands_NE$"NE_18_19_in" - NE_age_bands_NE$"NE_18_19_out"
  NE_age_bands_NE$Net22_25 <- NE_age_bands_NE$"NE_22_25_in" - NE_age_bands_NE$"NE_22_25_out"
  NE_age_bands_NE$Net30_35 <- NE_age_bands_NE$"NE_30_35_in" - NE_age_bands_NE$"NE_30_35_out"
  NE_age_bands_NE$Net36_45 <- NE_age_bands_NE$"NE_36_45_in" - NE_age_bands_NE$"NE_36_45_out"
  NE_age_bands_NE$Net56_65 <- NE_age_bands_NE$"NE_56_65_in" - NE_age_bands_NE$"NE_56_65_out"
  NE_age_bands_NE$total <- NE_age_bands_NE$NE_allages_in - NE_age_bands_NE$NE_allages_out
  
  NW_age_bands_NW$Net0_5 <- NW_age_bands_NW$"NW_0_5_in" - NW_age_bands_NW$"NW_0_5_out"
  NW_age_bands_NW$Net18_19 <- NW_age_bands_NW$"NW_18_19_in" - NW_age_bands_NW$"NW_18_19_out"
  NW_age_bands_NW$Net22_25 <- NW_age_bands_NW$"NW_22_25_in" - NW_age_bands_NW$"NW_22_25_out"
  NW_age_bands_NW$Net30_35 <- NW_age_bands_NW$"NW_30_35_in" - NW_age_bands_NW$"NW_30_35_out"
  NW_age_bands_NW$Net36_45 <- NW_age_bands_NW$"NW_36_45_in" - NW_age_bands_NW$"NW_36_45_out"
  NW_age_bands_NW$Net56_65 <- NW_age_bands_NW$"NW_56_65_in" - NW_age_bands_NW$"NW_56_65_out"
  NW_age_bands_NW$total <- NW_age_bands_NW$NW_allages_in - NW_age_bands_NW$NW_allages_out
  
  WMids_age_bands_WMids$Net0_5 <- WMids_age_bands_WMids$"WMids_0_5_in" - WMids_age_bands_WMids$"WMids_0_5_out"
  WMids_age_bands_WMids$Net18_19 <- WMids_age_bands_WMids$"WMids_18_19_in" - WMids_age_bands_WMids$"WMids_18_19_out"
  WMids_age_bands_WMids$Net22_25 <- WMids_age_bands_WMids$"WMids_22_25_in" - WMids_age_bands_WMids$"WMids_22_25_out"
  WMids_age_bands_WMids$Net30_35 <- WMids_age_bands_WMids$"WMids_30_35_in" - WMids_age_bands_WMids$"WMids_30_35_out"
  WMids_age_bands_WMids$Net36_45 <- WMids_age_bands_WMids$"WMids_36_45_in" - WMids_age_bands_WMids$"WMids_36_45_out"
  WMids_age_bands_WMids$Net56_65 <- WMids_age_bands_WMids$"WMids_56_65_in" - WMids_age_bands_WMids$"WMids_56_65_out"
  WMids_age_bands_WMids$total <- WMids_age_bands_WMids$WMids_allages_in - WMids_age_bands_WMids$WMids_allages_out
  
  EMids_age_bands_EMids$Net0_5 <- EMids_age_bands_EMids$"EMids_0_5_in" - EMids_age_bands_EMids$"EMids_0_5_out"
  EMids_age_bands_EMids$Net18_19 <- EMids_age_bands_EMids$"EMids_18_19_in" - EMids_age_bands_EMids$"EMids_18_19_out"
  EMids_age_bands_EMids$Net22_25 <- EMids_age_bands_EMids$"EMids_22_25_in" - EMids_age_bands_EMids$"EMids_22_25_out"
  EMids_age_bands_EMids$Net30_35 <- EMids_age_bands_EMids$"EMids_30_35_in" - EMids_age_bands_EMids$"EMids_30_35_out"
  EMids_age_bands_EMids$Net36_45 <- EMids_age_bands_EMids$"EMids_36_45_in" - EMids_age_bands_EMids$"EMids_36_45_out"
  EMids_age_bands_EMids$Net56_65 <- EMids_age_bands_EMids$"EMids_56_65_in" - EMids_age_bands_EMids$"EMids_56_65_out"
  EMids_age_bands_EMids$total <- EMids_age_bands_EMids$EMids_allages_in - EMids_age_bands_EMids$EMids_allages_out
  
  YH_age_bands_YH$Net0_5 <- YH_age_bands_YH$"YH_0_5_in" - YH_age_bands_YH$"YH_0_5_out"
  YH_age_bands_YH$Net18_19 <- YH_age_bands_YH$"YH_18_19_in" - YH_age_bands_YH$"YH_18_19_out"
  YH_age_bands_YH$Net22_25 <- YH_age_bands_YH$"YH_22_25_in" - YH_age_bands_YH$"YH_22_25_out"
  YH_age_bands_YH$Net30_35 <- YH_age_bands_YH$"YH_30_35_in" - YH_age_bands_YH$"YH_30_35_out"
  YH_age_bands_YH$Net36_45 <- YH_age_bands_YH$"YH_36_45_in" - YH_age_bands_YH$"YH_36_45_out"
  YH_age_bands_YH$Net56_65 <- YH_age_bands_YH$"YH_56_65_in" - YH_age_bands_YH$"YH_56_65_out"
  YH_age_bands_YH$total <- YH_age_bands_YH$YH_allages_in - YH_age_bands_YH$YH_allages_out
  
  Wales_age_bands_Wales$Net0_5 <- Wales_age_bands_Wales$"Wales_0_5_in" - Wales_age_bands_Wales$"Wales_0_5_out"
  Wales_age_bands_Wales$Net18_19 <- Wales_age_bands_Wales$"Wales_18_19_in" - Wales_age_bands_Wales$"Wales_18_19_out"
  Wales_age_bands_Wales$Net22_25 <- Wales_age_bands_Wales$"Wales_22_25_in" - Wales_age_bands_Wales$"Wales_22_25_out"
  Wales_age_bands_Wales$Net30_35 <- Wales_age_bands_Wales$"Wales_30_35_in" - Wales_age_bands_Wales$"Wales_30_35_out"
  Wales_age_bands_Wales$Net36_45 <- Wales_age_bands_Wales$"Wales_36_45_in" - Wales_age_bands_Wales$"Wales_36_45_out"
  Wales_age_bands_Wales$Net56_65 <- Wales_age_bands_Wales$"Wales_56_65_in" - Wales_age_bands_Wales$"Wales_56_65_out"
  Wales_age_bands_Wales$total <- Wales_age_bands_Wales$Wales_allages_in - Wales_age_bands_Wales$Wales_allages_out
  
  # Convert tibble to data frame to make averaging later better
  age_bands_inout <- as.data.frame(age_bands_inout)
  Inner_age_bands_inout <- as.data.frame(Inner_age_bands_inout)
  outer_age_bands_inout <- as.data.frame(outer_age_bands_inout)
  inout_age_bands_inout <- as.data.frame(inout_age_bands_inout)
  SE_age_bands_SE <- as.data.frame(SE_age_bands_SE)
  SW_age_bands_SW <- as.data.frame(SW_age_bands_SW)
  NE_age_bands_NE <- as.data.frame(NE_age_bands_NE)
  NW_age_bands_NW <- as.data.frame(NW_age_bands_NW)
  WMids_age_bands_WMids <- as.data.frame(WMids_age_bands_WMids)
  EMids_age_bands_EMids <- as.data.frame(EMids_age_bands_EMids)
  East_age_bands_East <- as.data.frame(East_age_bands_East)
  YH_age_bands_YH <- as.data.frame(YH_age_bands_YH)
  Wales_age_bands_Wales <- as.data.frame(Wales_age_bands_Wales)
  # Set the first column as row names
  rownames(age_bands_inout) <- age_bands_inout[, 1]
  rownames(Inner_age_bands_inout) <- Inner_age_bands_inout[, 1]
  rownames(outer_age_bands_inout) <- outer_age_bands_inout[, 1]
  rownames(inout_age_bands_inout) <- inout_age_bands_inout[, 1]
  rownames(East_age_bands_East) <- East_age_bands_East[, 1]
  rownames(SE_age_bands_SE) <- SE_age_bands_SE[, 1]
  rownames(NE_age_bands_NE) <- NE_age_bands_NE[, 1]
  rownames(NW_age_bands_NW) <- NW_age_bands_NW[, 1]
  rownames(WMids_age_bands_WMids) <- WMids_age_bands_WMids[, 1]
  rownames(EMids_age_bands_EMids) <- EMids_age_bands_EMids[, 1]
  rownames(WMids_age_bands_WMids) <- WMids_age_bands_WMids[, 1]
  rownames(YH_age_bands_YH) <- YH_age_bands_YH[, 1]
  rownames(Wales_age_bands_Wales) <- Wales_age_bands_Wales[, 1]
  # Remove the first column after setting it as row names
  age_bands_inout <- age_bands_inout[, -1]
  Inner_age_bands_inout <- Inner_age_bands_inout[, -1]
  outer_age_bands_inout <- outer_age_bands_inout[, -1]
  inout_age_bands_inout <- inout_age_bands_inout[, -1]
  SE_age_bands_SE <- SE_age_bands_SE[, -1]
  SW_age_bands_SW <- SW_age_bands_SW[, -1]
  NE_age_bands_NE <- NE_age_bands_NE[, -1]
  NW_age_bands_NW <- NW_age_bands_NW[, -1]
  WMids_age_bands_WMids <- WMids_age_bands_WMids[, -1]
  EMids_age_bands_EMids <- EMids_age_bands_EMids[, -1]
  WMids_age_bands_WMids <- WMids_age_bands_WMids[, -1]
  YH_age_bands_YH <- YH_age_bands_YH[, -1]
  Wales_age_bands_Wales <- Wales_age_bands_Wales[, -1]
  East_age_bands_East <- East_age_bands_East[, -1]
  
  # Append data_inlondon to the result list
  result_list[[i]] <- list(age_bands_inout = age_bands_inout, Inner_age_bands_inout = Inner_age_bands_inout,
                           outer_age_bands_inout = outer_age_bands_inout, inout_age_bands_inout = inout_age_bands_inout,
                           SE_age_bands_SE = SE_age_bands_SE, SW_age_bands_SW = SW_age_bands_SW, NE_age_bands_NE = NE_age_bands_NE, 
                           NW_age_bands_NW = NW_age_bands_NW, WMids_age_bands_WMids = WMids_age_bands_WMids, EMids_age_bands_EMids = EMids_age_bands_EMids,
                           East_age_bands_East = East_age_bands_East, YH_age_bands_YH = YH_age_bands_YH, Wales_age_bands_Wales = Wales_age_bands_Wales)
}

# Loop through each element in the result_list
for (i in seq_along(result_list)) {
  
  # Extract data_inlondon and data_outlondon from the i-th element of the result_list
  age_bands_inout <- result_list[[i]]$age_bands_inout
  Inner_age_bands_inout <- result_list[[i]]$Inner_age_bands_inout
  outer_age_bands_inout <- result_list[[i]]$outer_age_bands_inout
  inout_age_bands_inout <- result_list[[i]]$inout_age_bands_inout
  SE_age_bands_SE <- result_list[[i]]$SE_age_bands_SE
  SW_age_bands_SW <- result_list[[i]]$SW_age_bands_SW
  NE_age_bands_NE <- result_list[[i]]$NE_age_bands_NE
  NW_age_bands_NW <- result_list[[i]]$NW_age_bands_NW
  EMids_age_bands_EMids <- result_list[[i]]$EMids_age_bands_EMids
  WMids_age_bands_WMids <- result_list[[i]]$WMids_age_bands_WMids
  East_age_bands_East <- result_list[[i]]$East_age_bands_East
  YH_age_bands_YH <- result_list[[i]]$YH_age_bands_YH
  Wales_age_bands_Wales <- result_list[[i]]$Wales_age_bands_Wales

  # Assign them to separate datasets with meaningful names
  assign(paste0("age_bands_inout_", 2014+i), age_bands_inout)
  assign(paste0("Inner_age_bands_inout_", 2014+i), Inner_age_bands_inout)
  assign(paste0("outer_age_bands_inout_", 2014+i), outer_age_bands_inout)
  assign(paste0("inout_age_bands_inout_", 2014+i), inout_age_bands_inout)
  assign(paste0("SE_age_bands_SE_", 2014+i), SE_age_bands_SE)
  assign(paste0("SW_age_bands_SW_", 2014+i), SW_age_bands_SW)
  assign(paste0("NE_age_bands_NE_", 2014+i), NE_age_bands_NE)
  assign(paste0("NW_age_bands_NW_", 2014+i), NW_age_bands_NW)
  assign(paste0("EMids_age_bands_EMids_", 2014+i), EMids_age_bands_EMids)
  assign(paste0("WMids_age_bands_WMids_", 2014+i), WMids_age_bands_WMids)
  assign(paste0("East_age_bands_East_", 2014+i), East_age_bands_East)
  assign(paste0("YH_age_bands_YH_", 2014+i), YH_age_bands_YH)
  assign(paste0("Wales_age_bands_Wales_", 2014+i), Wales_age_bands_Wales)
}

### I HAVE INSERTED THE MAKE DATAFRAME NOT A TIBLLE THING, SO WE CAN DO THE AVERAGE, IN THE LOOP ABOVE. 

##CREATING A 15-19 AVERAGE DATASET
average_2015_2019 <- (as.matrix(age_bands_inout_2015) + as.matrix(age_bands_inout_2016) + as.matrix(age_bands_inout_2017) + as.matrix(age_bands_inout_2018)
                      + as.matrix(age_bands_inout_2019)) / 5
Inner_average_2015_2019 <- (as.matrix(Inner_age_bands_inout_2015) + as.matrix(Inner_age_bands_inout_2016) + as.matrix(Inner_age_bands_inout_2017) + as.matrix(Inner_age_bands_inout_2018)
                      + as.matrix(Inner_age_bands_inout_2019)) / 5
outer_average_2015_2019 <- (as.matrix(outer_age_bands_inout_2015) + as.matrix(outer_age_bands_inout_2016) + as.matrix(outer_age_bands_inout_2017) + as.matrix(outer_age_bands_inout_2018)
                      + as.matrix(outer_age_bands_inout_2019)) / 5
inout_average_2015_2019 <- (as.matrix(inout_age_bands_inout_2015) + as.matrix(inout_age_bands_inout_2016) + as.matrix(inout_age_bands_inout_2017) + as.matrix(inout_age_bands_inout_2018)
                      + as.matrix(inout_age_bands_inout_2019)) / 5
SW_average_2015_2019 <- (as.matrix(SW_age_bands_SW_2015) + as.matrix(SW_age_bands_SW_2016) + as.matrix(SW_age_bands_SW_2017) + as.matrix(SW_age_bands_SW_2018)
                            + as.matrix(SW_age_bands_SW_2019)) / 5
SE_average_2015_2019 <- (as.matrix(SE_age_bands_SE_2015) + as.matrix(SE_age_bands_SE_2016) + as.matrix(SE_age_bands_SE_2017) + as.matrix(SE_age_bands_SE_2018)
                            + as.matrix(SE_age_bands_SE_2019)) / 5
NE_average_2015_2019 <- (as.matrix(NE_age_bands_NE_2015) + as.matrix(NE_age_bands_NE_2016) + as.matrix(NE_age_bands_NE_2017) + as.matrix(NE_age_bands_NE_2018)
                            + as.matrix(NE_age_bands_NE_2019)) / 5
NW_average_2015_2019 <- (as.matrix(NW_age_bands_NW_2015) + as.matrix(NW_age_bands_NW_2016) + as.matrix(NW_age_bands_NW_2017) + as.matrix(NW_age_bands_NW_2018)
                            + as.matrix(NW_age_bands_NW_2019)) / 5
WMids_average_2015_2019 <- (as.matrix(WMids_age_bands_WMids_2015) + as.matrix(WMids_age_bands_WMids_2016) + as.matrix(WMids_age_bands_WMids_2017) + as.matrix(WMids_age_bands_WMids_2018)
                            + as.matrix(WMids_age_bands_WMids_2019)) / 5
EMids_average_2015_2019 <- (as.matrix(EMids_age_bands_EMids_2015) + as.matrix(EMids_age_bands_EMids_2016) + as.matrix(EMids_age_bands_EMids_2017) + as.matrix(EMids_age_bands_EMids_2018)
                            + as.matrix(EMids_age_bands_EMids_2019)) / 5
YH_average_2015_2019 <- (as.matrix(YH_age_bands_YH_2015) + as.matrix(YH_age_bands_YH_2016) + as.matrix(YH_age_bands_YH_2017) + as.matrix(YH_age_bands_YH_2018)
                            + as.matrix(YH_age_bands_YH_2019)) / 5
Wales_average_2015_2019 <- (as.matrix(Wales_age_bands_Wales_2015) + as.matrix(Wales_age_bands_Wales_2016) + as.matrix(Wales_age_bands_Wales_2017) + as.matrix(Wales_age_bands_Wales_2018)
                            + as.matrix(Wales_age_bands_Wales_2019)) / 5
East_average_2015_2019 <- (as.matrix(East_age_bands_East_2015) + as.matrix(East_age_bands_East_2016) + as.matrix(East_age_bands_East_2017) + as.matrix(East_age_bands_East_2018)
                            + as.matrix(East_age_bands_East_2019)) / 5

average_2015_2019 <- as.data.frame(average_2015_2019)
Inner_average_2015_2019 <- as.data.frame(Inner_average_2015_2019)
outer_average_2015_2019 <- as.data.frame(outer_average_2015_2019)
inout_average_2015_2019 <- as.data.frame(inout_average_2015_2019)
YH_average_2015_2019 <- as.data.frame(YH_average_2015_2019)
SE_average_2015_2019 <- as.data.frame(SE_average_2015_2019)
SW_average_2015_2019 <- as.data.frame(SW_average_2015_2019)
NW_average_2015_2019 <- as.data.frame(NW_average_2015_2019)
NE_average_2015_2019 <- as.data.frame(NE_average_2015_2019)
EMids_average_2015_2019 <- as.data.frame(EMids_average_2015_2019)
WMids_average_2015_2019 <- as.data.frame(WMids_average_2015_2019)
Wales_average_2015_2019 <- as.data.frame(Wales_average_2015_2019)
East_average_2015_2019 <- as.data.frame(East_average_2015_2019)

#make row named PUAs the first column again - done only for exported dataframes (can do more!)
age_bands_inout_2022$PUA <- rownames(age_bands_inout_2022)
age_bands_inout_2022 <- age_bands_inout_2022[, c("PUA", setdiff(names(age_bands_inout_2022), "PUA"))]
rownames(age_bands_inout_2022) <- NULL

age_bands_inout_2021$PUA <- rownames(age_bands_inout_2021)
age_bands_inout_2021 <- age_bands_inout_2021[, c("PUA", setdiff(names(age_bands_inout_2021), "PUA"))]
rownames(age_bands_inout_2021) <- NULL

age_bands_inout_2020$PUA <- rownames(age_bands_inout_2020)
age_bands_inout_2020 <- age_bands_inout_2020[, c("PUA", setdiff(names(age_bands_inout_2020), "PUA"))]
rownames(age_bands_inout_2020) <- NULL

age_bands_inout_2019$PUA <- rownames(age_bands_inout_2019)
age_bands_inout_2019 <- age_bands_inout_2019[, c("PUA", setdiff(names(age_bands_inout_2019), "PUA"))]
rownames(age_bands_inout_2019) <- NULL

age_bands_inout_2018$PUA <- rownames(age_bands_inout_2018)
age_bands_inout_2018 <- age_bands_inout_2018[, c("PUA", setdiff(names(age_bands_inout_2018), "PUA"))]
rownames(age_bands_inout_2018) <- NULL

age_bands_inout_2017$PUA <- rownames(age_bands_inout_2017)
age_bands_inout_2017 <- age_bands_inout_2017[, c("PUA", setdiff(names(age_bands_inout_2017), "PUA"))]
rownames(age_bands_inout_2017) <- NULL

age_bands_inout_2016$PUA <- rownames(age_bands_inout_2016)
age_bands_inout_2016 <- age_bands_inout_2016[, c("PUA", setdiff(names(age_bands_inout_2016), "PUA"))]
rownames(age_bands_inout_2016) <- NULL

age_bands_inout_2015$PUA <- rownames(age_bands_inout_2015)
age_bands_inout_2015 <- age_bands_inout_2015[, c("PUA", setdiff(names(age_bands_inout_2015), "PUA"))]
rownames(age_bands_inout_2015) <- NULL

average_2015_2019$PUA <- rownames(average_2015_2019)
average_2015_2019 <- average_2015_2019[, c("PUA", setdiff(names(average_2015_2019), "PUA"))]
rownames(average_2015_2019) <- NULL


Inner_age_bands_inout_2022$PUA <- rownames(Inner_age_bands_inout_2022)
Inner_age_bands_inout_2022 <- Inner_age_bands_inout_2022[, c("PUA", setdiff(names(Inner_age_bands_inout_2022), "PUA"))]
rownames(Inner_age_bands_inout_2022) <- NULL

Inner_age_bands_inout_2021$PUA <- rownames(Inner_age_bands_inout_2021)
Inner_age_bands_inout_2021 <- Inner_age_bands_inout_2021[, c("PUA", setdiff(names(Inner_age_bands_inout_2021), "PUA"))]
rownames(Inner_age_bands_inout_2021) <- NULL

Inner_age_bands_inout_2020$PUA <- rownames(Inner_age_bands_inout_2020)
Inner_age_bands_inout_2020 <- Inner_age_bands_inout_2020[, c("PUA", setdiff(names(Inner_age_bands_inout_2020), "PUA"))]
rownames(Inner_age_bands_inout_2020) <- NULL

Inner_average_2015_2019$PUA <- rownames(Inner_average_2015_2019)
Inner_average_2015_2019 <- Inner_average_2015_2019[, c("PUA", setdiff(names(Inner_average_2015_2019), "PUA"))]
rownames(Inner_average_2015_2019) <- NULL


outer_age_bands_inout_2022$PUA <- rownames(outer_age_bands_inout_2022)
outer_age_bands_inout_2022 <- outer_age_bands_inout_2022[, c("PUA", setdiff(names(outer_age_bands_inout_2022), "PUA"))]
rownames(outer_age_bands_inout_2022) <- NULL

outer_age_bands_inout_2021$PUA <- rownames(outer_age_bands_inout_2021)
outer_age_bands_inout_2021 <- outer_age_bands_inout_2021[, c("PUA", setdiff(names(outer_age_bands_inout_2021), "PUA"))]
rownames(outer_age_bands_inout_2021) <- NULL

outer_age_bands_inout_2020$PUA <- rownames(outer_age_bands_inout_2020)
outer_age_bands_inout_2020 <- outer_age_bands_inout_2020[, c("PUA", setdiff(names(outer_age_bands_inout_2020), "PUA"))]
rownames(outer_age_bands_inout_2020) <- NULL

outer_average_2015_2019$PUA <- rownames(outer_average_2015_2019)
outer_average_2015_2019 <- outer_average_2015_2019[, c("PUA", setdiff(names(outer_average_2015_2019), "PUA"))]
rownames(outer_average_2015_2019) <- NULL


inout_age_bands_inout_2022$PUA <- rownames(inout_age_bands_inout_2022)
inout_age_bands_inout_2022 <- inout_age_bands_inout_2022[, c("PUA", setdiff(names(inout_age_bands_inout_2022), "PUA"))]
rownames(inout_age_bands_inout_2022) <- NULL

inout_age_bands_inout_2021$PUA <- rownames(inout_age_bands_inout_2021)
inout_age_bands_inout_2021 <- inout_age_bands_inout_2021[, c("PUA", setdiff(names(inout_age_bands_inout_2021), "PUA"))]
rownames(inout_age_bands_inout_2021) <- NULL

inout_age_bands_inout_2020$PUA <- rownames(inout_age_bands_inout_2020)
inout_age_bands_inout_2020 <- inout_age_bands_inout_2020[, c("PUA", setdiff(names(inout_age_bands_inout_2020), "PUA"))]
rownames(inout_age_bands_inout_2020) <- NULL

inout_average_2015_2019$PUA <- rownames(inout_average_2015_2019)
inout_average_2015_2019 <- inout_average_2015_2019[, c("PUA", setdiff(names(inout_average_2015_2019), "PUA"))]
rownames(inout_average_2015_2019) <- NULL

East_age_bands_East_2022$PUA <- rownames(East_age_bands_East_2022)
East_age_bands_East_2022 <- East_age_bands_East_2022[, c("PUA", setdiff(names(East_age_bands_East_2022), "PUA"))]
rownames(East_age_bands_East_2022) <- NULL

East_age_bands_East_2021$PUA <- rownames(East_age_bands_East_2021)
East_age_bands_East_2021 <- East_age_bands_East_2021[, c("PUA", setdiff(names(East_age_bands_East_2021), "PUA"))]
rownames(East_age_bands_East_2021) <- NULL

East_age_bands_East_2020$PUA <- rownames(East_age_bands_East_2020)
East_age_bands_East_2020 <- East_age_bands_East_2020[, c("PUA", setdiff(names(East_age_bands_East_2020), "PUA"))]
rownames(East_age_bands_East_2020) <- NULL

East_average_2015_2019$PUA <- rownames(East_average_2015_2019)
East_average_2015_2019 <- East_average_2015_2019[, c("PUA", setdiff(names(East_average_2015_2019), "PUA"))]
rownames(East_average_2015_2019) <- NULL

SW_age_bands_SW_2022$PUA <- rownames(SW_age_bands_SW_2022)
SW_age_bands_SW_2022 <- SW_age_bands_SW_2022[, c("PUA", setdiff(names(SW_age_bands_SW_2022), "PUA"))]
rownames(SW_age_bands_SW_2022) <- NULL

SW_age_bands_SW_2021$PUA <- rownames(SW_age_bands_SW_2021)
SW_age_bands_SW_2021 <- SW_age_bands_SW_2021[, c("PUA", setdiff(names(SW_age_bands_SW_2021), "PUA"))]
rownames(SW_age_bands_SW_2021) <- NULL

SW_age_bands_SW_2020$PUA <- rownames(SW_age_bands_SW_2020)
SW_age_bands_SW_2020 <- SW_age_bands_SW_2020[, c("PUA", setdiff(names(SW_age_bands_SW_2020), "PUA"))]
rownames(SW_age_bands_SW_2020) <- NULL

SW_average_2015_2019$PUA <- rownames(SW_average_2015_2019)
SW_average_2015_2019 <- SW_average_2015_2019[, c("PUA", setdiff(names(SW_average_2015_2019), "PUA"))]
rownames(SW_average_2015_2019) <- NULL

SE_age_bands_SE_2022$PUA <- rownames(SE_age_bands_SE_2022)
SE_age_bands_SE_2022 <- SE_age_bands_SE_2022[, c("PUA", setdiff(names(SE_age_bands_SE_2022), "PUA"))]
rownames(SE_age_bands_SE_2022) <- NULL

SE_age_bands_SE_2021$PUA <- rownames(SE_age_bands_SE_2021)
SE_age_bands_SE_2021 <- SE_age_bands_SE_2021[, c("PUA", setdiff(names(SE_age_bands_SE_2021), "PUA"))]
rownames(SE_age_bands_SE_2021) <- NULL

SE_age_bands_SE_2020$PUA <- rownames(SE_age_bands_SE_2020)
SE_age_bands_SE_2020 <- SE_age_bands_SE_2020[, c("PUA", setdiff(names(SE_age_bands_SE_2020), "PUA"))]
rownames(SE_age_bands_SE_2020) <- NULL

SE_average_2015_2019$PUA <- rownames(SE_average_2015_2019)
SE_average_2015_2019 <- SE_average_2015_2019[, c("PUA", setdiff(names(SE_average_2015_2019), "PUA"))]
rownames(SE_average_2015_2019) <- NULL

EMids_age_bands_EMids_2022$PUA <- rownames(EMids_age_bands_EMids_2022)
EMids_age_bands_EMids_2022 <- EMids_age_bands_EMids_2022[, c("PUA", setdiff(names(EMids_age_bands_EMids_2022), "PUA"))]
rownames(EMids_age_bands_EMids_2022) <- NULL

EMids_age_bands_EMids_2021$PUA <- rownames(EMids_age_bands_EMids_2021)
EMids_age_bands_EMids_2021 <- EMids_age_bands_EMids_2021[, c("PUA", setdiff(names(EMids_age_bands_EMids_2021), "PUA"))]
rownames(EMids_age_bands_EMids_2021) <- NULL

EMids_age_bands_EMids_2020$PUA <- rownames(EMids_age_bands_EMids_2020)
EMids_age_bands_EMids_2020 <- EMids_age_bands_EMids_2020[, c("PUA", setdiff(names(EMids_age_bands_EMids_2020), "PUA"))]
rownames(EMids_age_bands_EMids_2020) <- NULL

EMids_average_2015_2019$PUA <- rownames(EMids_average_2015_2019)
EMids_average_2015_2019 <- EMids_average_2015_2019[, c("PUA", setdiff(names(EMids_average_2015_2019), "PUA"))]
rownames(EMids_average_2015_2019) <- NULL

WMids_age_bands_WMids_2022$PUA <- rownames(WMids_age_bands_WMids_2022)
WMids_age_bands_WMids_2022 <- WMids_age_bands_WMids_2022[, c("PUA", setdiff(names(WMids_age_bands_WMids_2022), "PUA"))]
rownames(WMids_age_bands_WMids_2022) <- NULL

WMids_age_bands_WMids_2021$PUA <- rownames(WMids_age_bands_WMids_2021)
WMids_age_bands_WMids_2021 <- WMids_age_bands_WMids_2021[, c("PUA", setdiff(names(WMids_age_bands_WMids_2021), "PUA"))]
rownames(WMids_age_bands_WMids_2021) <- NULL

WMids_age_bands_WMids_2020$PUA <- rownames(WMids_age_bands_WMids_2020)
WMids_age_bands_WMids_2020 <- WMids_age_bands_WMids_2020[, c("PUA", setdiff(names(WMids_age_bands_WMids_2020), "PUA"))]
rownames(WMids_age_bands_WMids_2020) <- NULL

WMids_average_2015_2019$PUA <- rownames(WMids_average_2015_2019)
WMids_average_2015_2019 <- WMids_average_2015_2019[, c("PUA", setdiff(names(WMids_average_2015_2019), "PUA"))]
rownames(WMids_average_2015_2019) <- NULL

NW_age_bands_NW_2022$PUA <- rownames(NW_age_bands_NW_2022)
NW_age_bands_NW_2022 <- NW_age_bands_NW_2022[, c("PUA", setdiff(names(NW_age_bands_NW_2022), "PUA"))]
rownames(NW_age_bands_NW_2022) <- NULL

NW_age_bands_NW_2021$PUA <- rownames(NW_age_bands_NW_2021)
NW_age_bands_NW_2021 <- NW_age_bands_NW_2021[, c("PUA", setdiff(names(NW_age_bands_NW_2021), "PUA"))]
rownames(NW_age_bands_NW_2021) <- NULL

NW_age_bands_NW_2020$PUA <- rownames(NW_age_bands_NW_2020)
NW_age_bands_NW_2020 <- NW_age_bands_NW_2020[, c("PUA", setdiff(names(NW_age_bands_NW_2020), "PUA"))]
rownames(NW_age_bands_NW_2020) <- NULL

NW_average_2015_2019$PUA <- rownames(NW_average_2015_2019)
NW_average_2015_2019 <- NW_average_2015_2019[, c("PUA", setdiff(names(NW_average_2015_2019), "PUA"))]
rownames(NW_average_2015_2019) <- NULL

NE_age_bands_NE_2022$PUA <- rownames(NE_age_bands_NE_2022)
NE_age_bands_NE_2022 <- NE_age_bands_NE_2022[, c("PUA", setdiff(names(NE_age_bands_NE_2022), "PUA"))]
rownames(NE_age_bands_NE_2022) <- NULL

NE_age_bands_NE_2021$PUA <- rownames(NE_age_bands_NE_2021)
NE_age_bands_NE_2021 <- NE_age_bands_NE_2021[, c("PUA", setdiff(names(NE_age_bands_NE_2021), "PUA"))]
rownames(NE_age_bands_NE_2021) <- NULL

NE_age_bands_NE_2020$PUA <- rownames(NE_age_bands_NE_2020)
NE_age_bands_NE_2020 <- NE_age_bands_NE_2020[, c("PUA", setdiff(names(NE_age_bands_NE_2020), "PUA"))]
rownames(NE_age_bands_NE_2020) <- NULL

NE_average_2015_2019$PUA <- rownames(NE_average_2015_2019)
NE_average_2015_2019 <- NE_average_2015_2019[, c("PUA", setdiff(names(NE_average_2015_2019), "PUA"))]
rownames(NE_average_2015_2019) <- NULL

Wales_age_bands_Wales_2022$PUA <- rownames(Wales_age_bands_Wales_2022)
Wales_age_bands_Wales_2022 <- Wales_age_bands_Wales_2022[, c("PUA", setdiff(names(Wales_age_bands_Wales_2022), "PUA"))]
rownames(Wales_age_bands_Wales_2022) <- NULL

Wales_age_bands_Wales_2021$PUA <- rownames(Wales_age_bands_Wales_2021)
Wales_age_bands_Wales_2021 <- Wales_age_bands_Wales_2021[, c("PUA", setdiff(names(Wales_age_bands_Wales_2021), "PUA"))]
rownames(Wales_age_bands_Wales_2021) <- NULL

Wales_age_bands_Wales_2020$PUA <- rownames(Wales_age_bands_Wales_2020)
Wales_age_bands_Wales_2020 <- Wales_age_bands_Wales_2020[, c("PUA", setdiff(names(Wales_age_bands_Wales_2020), "PUA"))]
rownames(Wales_age_bands_Wales_2020) <- NULL

Wales_average_2015_2019$PUA <- rownames(Wales_average_2015_2019)
Wales_average_2015_2019 <- Wales_average_2015_2019[, c("PUA", setdiff(names(Wales_average_2015_2019), "PUA"))]
rownames(Wales_average_2015_2019) <- NULL

YH_age_bands_YH_2022$PUA <- rownames(YH_age_bands_YH_2022)
YH_age_bands_YH_2022 <- YH_age_bands_YH_2022[, c("PUA", setdiff(names(YH_age_bands_YH_2022), "PUA"))]
rownames(YH_age_bands_YH_2022) <- NULL

YH_age_bands_YH_2021$PUA <- rownames(YH_age_bands_YH_2021)
YH_age_bands_YH_2021 <- YH_age_bands_YH_2021[, c("PUA", setdiff(names(YH_age_bands_YH_2021), "PUA"))]
rownames(YH_age_bands_YH_2021) <- NULL

YH_age_bands_YH_2020$PUA <- rownames(YH_age_bands_YH_2020)
YH_age_bands_YH_2020 <- YH_age_bands_YH_2020[, c("PUA", setdiff(names(YH_age_bands_YH_2020), "PUA"))]
rownames(YH_age_bands_YH_2020) <- NULL

YH_average_2015_2019$PUA <- rownames(YH_average_2015_2019)
YH_average_2015_2019 <- YH_average_2015_2019[, c("PUA", setdiff(names(YH_average_2015_2019), "PUA"))]
rownames(YH_average_2015_2019) <- NULL


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
file_name_geog <- paste0(date, "_Int_mig_age_bands", ".csv")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Output")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(average_2015_2019, age_bands_inout_2022, age_bands_inout_2021, age_bands_inout_2020, age_bands_inout_2019,
                     age_bands_inout_2018, age_bands_inout_2017, age_bands_inout_2016, age_bands_inout_2015,
                     Inner_average_2015_2019, Inner_age_bands_inout_2022, Inner_age_bands_inout_2021, Inner_age_bands_inout_2020, 
                     outer_average_2015_2019, outer_age_bands_inout_2022, outer_age_bands_inout_2021, outer_age_bands_inout_2020, 
                     inout_average_2015_2019, inout_age_bands_inout_2022, inout_age_bands_inout_2021, inout_age_bands_inout_2020,
                     East_average_2015_2019, East_age_bands_East_2022, East_age_bands_East_2021, East_age_bands_East_2020,
                     SE_average_2015_2019, SE_age_bands_SE_2022, SE_age_bands_SE_2021, SE_age_bands_SE_2020,
                     SW_average_2015_2019, SW_age_bands_SW_2022, SW_age_bands_SW_2021, SW_age_bands_SW_2020,
                     NE_average_2015_2019, NE_age_bands_NE_2022, NE_age_bands_NE_2021, NE_age_bands_NE_2020,
                     NW_average_2015_2019, NW_age_bands_NW_2022, NW_age_bands_NW_2021, NW_age_bands_NW_2020,
                     WMids_average_2015_2019, WMids_age_bands_WMids_2022, WMids_age_bands_WMids_2021, WMids_age_bands_WMids_2020,
                     EMids_average_2015_2019, EMids_age_bands_EMids_2022, EMids_age_bands_EMids_2021, EMids_age_bands_EMids_2020,
                     Wales_average_2015_2019, Wales_age_bands_Wales_2022, Wales_age_bands_Wales_2021, Wales_age_bands_Wales_2020,
                     YH_average_2015_2019, YH_age_bands_YH_2022, YH_age_bands_YH_2021, YH_age_bands_YH_2020)
names(df_list_geog) <- c("2015_2019", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015",
                         "Inner_2015_2019", "Inner_2022", "Inner_2021", "Inner_2020",
                         "outer_2015_2019", "outer_2022", "outer_2021", "outer_2020",
                         "inout_2015_2019", "inout_2022", "inout_2021", "inout_2020",
                         "East_2015_2019", "East_2022", "East_2021", "East_2020",
                         "SE_2015_2019", "SE_2022", "SE_2021", "SE_2020",
                         "SW_2015_2019", "SW_2022", "SW_2021", "SW_2020",
                         "NE_2015_2019", "NE_2022", "NE_2021", "NE_2020",
                         "NW_2015_2019", "NW_2022", "NW_2021", "NW_2020",
                         "WMids_2015_2019", "WMids_2022", "WMids_2021", "WMids_2020",
                         "EMids_2015_2019", "EMids_2022", "EMids_2021", "EMids_2020",
                         "Wales_2015_2019", "Wales_2022", "Wales_2021", "Wales_2020",
                         "YH_2015_2019", "YH_2022", "YH_2021", "YH_2020")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)

