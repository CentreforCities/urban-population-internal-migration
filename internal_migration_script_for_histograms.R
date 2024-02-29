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
  
  
  
  #collapse by in-PUA, excluding intraPUA moves
  sum_in_PUA <- raw_geog_inout %>%
    group_by(inPUA) %>%
    filter(insidePUA == "outPUAmove") %>%
    summarize(across(starts_with("Age"), sum))
  
  sum_out_PUA <- raw_geog_inout %>%
    group_by(outPUA) %>%
    filter(insidePUA == "outPUAmove") %>%
    summarize(across(starts_with("Age"), sum))
  
  # extract London & calculate net
  data_inlondon <- subset(sum_in_PUA, inPUA == "London")
  data_inlondon <- data_inlondon[,-1]
  data_outlondon <- subset(sum_out_PUA, outPUA == "London")
  data_outlondon <- data_outlondon[,-1]
  Net_london <- data_inlondon - data_outlondon
  
  ### Inner London to outside London PUA analysis
  sum_in_InnerLDN <- raw_geog_inout %>%
    filter(InnerLondonMove == "move between Inner and non London") %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with("Age"), sum))
  
  sum_out_InnerLDN <- raw_geog_inout %>%
    filter(InnerLondonMove == "move between Inner and non London") %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with("Age"), sum))
  
  # extract Inner London & calculate net
  data_inInnerlondon <- subset(sum_in_InnerLDN, inInOutLDN == "Inner London")
  data_inInnerlondon <- data_inInnerlondon[,-1]
  data_outInnerlondon <- subset(sum_out_InnerLDN, outInOutLDN == "Inner London")
  data_outInnerlondon <- data_outInnerlondon[,-1]
  Net_InnerLDN_to_outsideLDN <- data_inInnerlondon - data_outInnerlondon
  
  ### Outer London to outside London PUA analysis
  sum_in_OuterLDN <- raw_geog_inout %>%
    filter(OuterLondonMove == "move between Outer and non London") %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with("Age"), sum))
  
  sum_out_OuterLDN <- raw_geog_inout %>%
    filter(OuterLondonMove == "move between Outer and non London") %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with("Age"), sum))
  
  # extract Outer London & calculate net
  data_inOuterlondon <- subset(sum_in_OuterLDN, inInOutLDN == "Outer London")
  data_inOuterlondon <- data_inOuterlondon[,-1]
  data_outOuterlondon <- subset(sum_out_OuterLDN, outInOutLDN == "Outer London")
  data_outOuterlondon <- data_outOuterlondon[,-1]
  Net_OuterLDN_to_outsideLDN <- data_inOuterlondon - data_outOuterlondon
  
  ### Inner London to Outer London PUA analysis
  sum_in_InOutLDN <- raw_geog_inout %>%
    filter(BetweenInOutLondonMove == "Between Inner and Outer London") %>%
    group_by(inInOutLDN) %>%
    summarize(across(starts_with("Age"), sum))
  
  sum_out_InOutLDN <- raw_geog_inout %>%
    filter(BetweenInOutLondonMove == "Between Inner and Outer London") %>%
    group_by(outInOutLDN) %>%
    summarize(across(starts_with("Age"), sum))
  
  # extract Outer London & calculate net
  data_inInOutlondon <- subset(sum_in_InOutLDN, inInOutLDN == "Inner London")
  data_inInOutlondon <- data_inInOutlondon[,-1]
  data_outInOutlondon <- subset(sum_out_InOutLDN, outInOutLDN == "Inner London")
  data_outInOutlondon <- data_outInOutlondon[,-1]
  Net_InOutLDN_to_outsideLDN <- data_inInOutlondon - data_outInOutlondon
  
  # Append data_inlondon to the result list
  result_list[[i]] <- list(data_inlondon = data_inlondon, data_outlondon = data_outlondon, 
                           data_inInnerlondon = data_inInnerlondon, data_outInnerlondon = data_outInnerlondon,
                           data_inOuterlondon = data_inOuterlondon, data_outOuterlondon = data_outOuterlondon,
                           data_inInOutlondon = data_inInOutlondon, data_outInOutlondon = data_outInOutlondon,
                           Net_london = Net_london, Net_InnerLDN_to_outsideLDN = Net_InnerLDN_to_outsideLDN, 
                           Net_OuterLDN_to_outsideLDN = Net_OuterLDN_to_outsideLDN, Net_InOutLDN_to_outsideLDN = Net_InOutLDN_to_outsideLDN)
}

# Loop through each element in the result_list
for (i in seq_along(result_list)) {
  
  # Extract data_inlondon and data_outlondon from the i-th element of the result_list
  data_inlondon <- result_list[[i]]$data_inlondon
  data_outlondon <- result_list[[i]]$data_outlondon
  data_inInnerlondon <- result_list[[i]]$data_inInnerlondon
  data_outInnerlondon <- result_list[[i]]$data_outInnerlondon
  data_inOuterlondon <- result_list[[i]]$data_inOuterlondon
  data_outOuterlondon <- result_list[[i]]$data_outOuterlondon
  data_inInOutlondon <- result_list[[i]]$data_inInOutlondon
  data_outInOutlondon <- result_list[[i]]$data_outInOutlondon
  Net_london <- result_list[[i]]$Net_london
  Net_InnerLDN_to_outsideLDN <- result_list[[i]]$Net_InnerLDN_to_outsideLDN
  Net_OuterLDN_to_outsideLDN <- result_list[[i]]$Net_OuterLDN_to_outsideLDN
  Net_InOutLDN <- result_list[[i]]$Net_InOutLDN
  
  # Assign them to separate datasets with meaningful names
  assign(paste0("data_inlondon_", 2014+i), data_inlondon)
  assign(paste0("data_outlondon_", 2014+i), data_outlondon)
  assign(paste0("data_inInnerlondon_", 2014+i), data_inInnerlondon)
  assign(paste0("data_outInnerlondon_", 2014+i), data_outInnerlondon)
  assign(paste0("data_inOuterlondon_", 2014+i), data_inOuterlondon)
  assign(paste0("data_outOuterlondon_", 2014+i), data_outOuterlondon)
  assign(paste0("data_inInOutlondon_", 2014+i), data_inInOutlondon)
  assign(paste0("data_outInOutlondon_", 2014+i), data_outInOutlondon)
  assign(paste0("Net_london_", 2014+i), Net_london)
  assign(paste0("Net_InnerLDN_to_outsideLDN", 2014+i), Net_InnerLDN_to_outsideLDN)
  assign(paste0("Net_OuterLDN_to_outsideLDN", 2014+i), Net_OuterLDN_to_outsideLDN)
  assign(paste0("Net_InOutLDN", 2014+i), Net_InOutLDN)
}


##CREATING A 15-19 AVERAGE DATASET
#calculate 15_19 averages
### THIS COULD BE WRITTEN MORE NEATLY! 
# Step 1: Identify datasets starting with "data_inlondon_"
inlondon_1519 <- ls(pattern = "^data_inlondon_(2015|2016|2017|2018|2019)")
outlondon_1519 <- ls(pattern = "^data_outlondon_(2015|2016|2017|2018|2019)")
#net_london_1519 <- ls(pattern = "^Net_london_(2015|2016|2017|2018|2019)")
inInnerlondon_1519 <- ls(pattern = "^data_inInnerlondon_(2015|2016|2017|2018|2019)")
outInnerlondon_1519 <- ls(pattern = "^data_outInnerlondon_(2015|2016|2017|2018|2019)")
#net_innerLDN_to_outsideLDN_1519 <- ls(pattern = "^Net_london_(2015|2016|2017|2018|2019)")
inOuterlondon_1519 <- ls(pattern = "^data_inOuterlondon_(2015|2016|2017|2018|2019)")
outOuterlondon_1519 <- ls(pattern = "^data_outOuterlondon_(2015|2016|2017|2018|2019)")

inInOutlondon_1519 <- ls(pattern = "^data_inInOutlondon_(2015|2016|2017|2018|2019)")
outInOutlondon_1519 <- ls(pattern = "^data_outInOutlondon_(2015|2016|2017|2018|2019)")

# Step 2: Extract variables from each dataset and combine them
variables_list_inLondon1519 <- lapply(inlondon_1519, function(dataset) {
  get(dataset)  })
variables_list_outLondon1519 <- lapply(outlondon_1519, function(dataset) {
  get(dataset)  })
variables_list_inInnerLondon1519 <- lapply(inInnerlondon_1519, function(dataset) {
  get(dataset)  })
variables_list_outInnerLondon1519 <- lapply(outInnerlondon_1519, function(dataset) {
  get(dataset)  })
variables_list_inOuterLondon1519 <- lapply(inOuterlondon_1519, function(dataset) {
  get(dataset)  })
variables_list_outOuterLondon1519 <- lapply(outOuterlondon_1519, function(dataset) {
  get(dataset)  })

variables_list_inInOutLondon1519 <- lapply(inInOutlondon_1519, function(dataset) {
  get(dataset)  })
variables_list_outInOutLondon1519 <- lapply(outInOutlondon_1519, function(dataset) {
  get(dataset)  })

# Step 3: Combine corresponding variables from all datasets
combined_inLondon_1519 <- bind_rows(variables_list_inLondon1519, .id = "Dataset")
combined_outLondon_1519 <- bind_rows(variables_list_outLondon1519, .id = "Dataset")
combined_inInnerLondon_1519 <- bind_rows(variables_list_inInnerLondon1519, .id = "Dataset")
combined_outInnerLondon_1519 <- bind_rows(variables_list_outInnerLondon1519, .id = "Dataset")
combined_inOuterLondon_1519 <- bind_rows(variables_list_inOuterLondon1519, .id = "Dataset")
combined_outOuterLondon_1519 <- bind_rows(variables_list_outOuterLondon1519, .id = "Dataset")
combined_inInOutLondon_1519 <- bind_rows(variables_list_inInOutLondon1519, .id = "Dataset")
combined_outInOutLondon_1519 <- bind_rows(variables_list_outInOutLondon1519, .id = "Dataset")

# Step 4: Calculate the average of each variable
average15_19_inLondon <- combined_inLondon_1519 %>%
  summarize(across(starts_with("Age"), mean))
average15_19_outLondon <- combined_outLondon_1519 %>%
  summarize(across(starts_with("Age"), mean)) 
average15_19_netLondon <- average15_19_inLondon - average15_19_outLondon

average15_19_inInnerLondon <- combined_inInnerLondon_1519 %>%
  summarize(across(starts_with("Age"), mean))
average15_19_outInnerLondon <- combined_outInnerLondon_1519 %>%
  summarize(across(starts_with("Age"), mean))
average15_19_netInnerLondon <-average15_19_inInnerLondon - average15_19_outInnerLondon

average15_19_inOuterLondon <- combined_inOuterLondon_1519 %>%
  summarize(across(starts_with("Age"), mean))
average15_19_outOuterLondon <- combined_outOuterLondon_1519 %>%
  summarize(across(starts_with("Age"), mean))
average15_19_netOuterLondon <-average15_19_inOuterLondon - average15_19_outOuterLondon

average15_19_inInOutLondon <- combined_inInOutLondon_1519 %>%
  summarize(across(starts_with("Age"), mean))
average15_19_outInOutLondon <- combined_outInOutLondon_1519 %>%
  summarize(across(starts_with("Age"), mean))
average15_19_netInOutLondon <-average15_19_inInOutLondon - average15_19_outInOutLondon


#EXPORT TO EXCEL data needed for visualisations 
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
file_name_geog <- paste0(date, "_for_visualisations", ".csv")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Social and Demographics/Escape to the Country/Data/Output")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(average15_19_netLondon, Net_london_2021,Net_InnerLDN_to_outsideLDN2021,Net_OuterLDN_to_outsideLDN2021, average15_19_netOuterLondon,
                     average15_19_netInnerLondon,average15_19_inInOutLondon, average15_19_outInOutLondon, data_inInOutlondon_2021,data_outInOutlondon_2021)
names(df_list_geog) <- c("pre_covid_net", "2021_net","2021_Inner_net", "2021_Outer_net","pre_covid_outer_net","pre_covid_inner_net",
                         "pre_covid_inout_in","pre_covid_inout_out", "2021_inout_in", "2021_inout_out")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)


## DRAW HISTOGRAMS
years <- 2015:2022

##Yearly for LONDON PUA
# Iterate over each year
for (year in years) {
  # Draw IN histogram
  long_inlondon <- gather(get(paste0("data_inlondon_", year)), key = "Variable", value = "Pop")
  long_inlondon <- long_inlondon[-1, ]
  long_inlondon <- long_inlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inlondon$Age <- as.factor(long_inlondon$Age)
  long_inlondon$Pop <- as.numeric(long_inlondon$Pop)
  histogram_inlondon <- ggplot(long_inlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving into london in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])

  # Draw OUT histogram
  long_outlondon <- gather(get(paste0("data_outlondon_", year)), key = "Variable", value = "Pop")
  long_outlondon <- long_outlondon[-1, ]
  long_outlondon <- long_outlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outlondon$Age <- as.factor(long_outlondon$Age)
  long_outlondon$Pop <- as.numeric(long_outlondon$Pop)
  histogram_outlondon <- ggplot(long_outlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving out of london in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)])
  
  # Stack the histograms
  combined_histogram <- ggplot() +
    geom_col(data = histogram_inlondon$data, aes(x = Age, y=Pop, fill = "In London"), position = "identity", alpha = 0.8) +
    labs(title = paste("Population Movement in and out of London in", year),
         x = "Age", y = "Pop") +
    scale_fill_manual(values = c("In London" = "grey", "Out London" = "green"),
                      name = "Population",
                      labels = c("In London", "Out London")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "In London") + # Facet for histogram_inlondon
    geom_col(data = histogram_outlondon$data, aes(x = Age, y=Pop,fill = "Out London"), position = "identity", alpha = 0.5) +
    labs(x = "Age") +
    theme(legend.position = "top") +
    facet_grid(. ~ "Out London") # Facet for histogram_outlondon
  
  print(combined_histogram) 
  
  # Draw NET histogram
  long_london <- gather(get(paste0("Net_london_", year)), key = "Variable", value = "Pop")
  long_london <- long_london %>%
    mutate(Age = seq(0, length.out = nrow(long_london), by = 1)) %>%
    select(Age, everything()) 
  
  long_london$Age <- as.factor(long_london$Age)
  long_london$Pop <- as.numeric(long_london$Pop)
  net_histogram <- ggplot(long_london, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Net Pop flow, London PUA", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_london$Age)[seq(1, length(levels(long_london$Age)), by = 5)]) 
  
  print(net_histogram)
}

##Yearly for Inner London
# Iterate over each year
for (year in years) {
  # Draw IN histogram
  long_inInnerlondon <- gather(get(paste0("data_inInnerlondon_", year)), key = "Variable", value = "Pop")
  long_inInnerlondon <- long_inInnerlondon[-1, ]
  long_inInnerlondon <- long_inInnerlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inInnerlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inInnerlondon$Age <- as.factor(long_inInnerlondon$Age)
  long_inInnerlondon$Pop <- as.numeric(long_inInnerlondon$Pop)
  histogram_inInnerlondon <- ggplot(long_inInnerlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving into Inner London", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inInnerlondon$Age)[seq(1, length(levels(long_inInnerlondon$Age)), by = 5)])
  
  # Draw OUT histogram
  long_outInnerlondon <- gather(get(paste0("data_outInnerlondon_", year)), key = "Variable", value = "Pop")
  long_outInnerlondon <- long_outInnerlondon[-1, ]
  long_outInnerlondon <- long_outInnerlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outInnerlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outInnerlondon$Age <- as.factor(long_outInnerlondon$Age)
  long_outInnerlondon$Pop <- as.numeric(long_outInnerlondon$Pop)
  histogram_outInnerlondon <- ggplot(long_outInnerlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving out of Inner london to outside London in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outInnerlondon$Age)[seq(1, length(levels(long_outInnerlondon$Age)), by = 5)])
  
  # Stack the histograms
  combined_Innerhistogram <- ggplot() +
    geom_col(data = histogram_inInnerlondon$data, aes(x = Age, y=Pop, fill = "In Inner London"), position = "identity", alpha = 0.8) +
    labs(title = paste("Population Movement between Inner London and outside London in", year),
         x = "Age", y = "Pop") +
    scale_fill_manual(values = c("In Inner London" = "grey", "Out Inner London" = "green"),
                      name = "Population",
                      labels = c("In Inner London", "Out Inner London")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outInnerlondon$Age)[seq(1, length(levels(long_outInnerlondon$Age)), by = 5)]) +
    facet_grid(. ~ "In Inner London") + # Facet for histogram_inlondon
    geom_col(data = histogram_outInnerlondon$data, aes(x = Age, y=Pop,fill = "Out Inner London"), position = "identity", alpha = 0.5) +
    labs(x = "Age") +
    theme(legend.position = "top") +
    facet_grid(. ~ "Out Inner London") # Facet for histogram_outlondon
  
  print(combined_Innerhistogram) 
  
  # Draw NET histogram
  long_Innerlondon <- gather(get(paste0("Net_InnerLDN_to_outsideLDN", year)), key = "Variable", value = "Pop")
  long_Innerlondon <- long_Innerlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_Innerlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_Innerlondon$Age <- as.factor(long_Innerlondon$Age)
  long_Innerlondon$Pop <- as.numeric(long_Innerlondon$Pop)
  net_inner_histogram <- ggplot(long_Innerlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Net Pop flow between Inner London and outside London", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_Innerlondon$Age)[seq(1, length(levels(long_Innerlondon$Age)), by = 5)]) 
  
  print(net_inner_histogram)
}

##Yearly for Outer London
# Iterate over each year
for (year in years) {
  # Draw IN histogram
  long_inOuterlondon <- gather(get(paste0("data_inOuterlondon_", year)), key = "Variable", value = "Pop")
  long_inOuterlondon <- long_inOuterlondon[-1, ]
  long_inOuterlondon <- long_inOuterlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inOuterlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inOuterlondon$Age <- as.factor(long_inOuterlondon$Age)
  long_inOuterlondon$Pop <- as.numeric(long_inOuterlondon$Pop)
  histogram_inOuterlondon <- ggplot(long_inOuterlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving into Outer London", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inOuterlondon$Age)[seq(1, length(levels(long_inOuterlondon$Age)), by = 5)])
  
  # Draw OUT histogram
  long_outOuterlondon <- gather(get(paste0("data_outOuterlondon_", year)), key = "Variable", value = "Pop")
  long_outOuterlondon <- long_outOuterlondon[-1, ]
  long_outOuterlondon <- long_outOuterlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outOuterlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outOuterlondon$Age <- as.factor(long_outOuterlondon$Age)
  long_outOuterlondon$Pop <- as.numeric(long_outOuterlondon$Pop)
  histogram_outOuterlondon <- ggplot(long_outOuterlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving out of Outer london to outside London in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outOuterlondon$Age)[seq(1, length(levels(long_outOuterlondon$Age)), by = 5)])
  
  # Stack the histograms
  combined_Outerhistogram <- ggplot() +
    geom_col(data = histogram_inOuterlondon$data, aes(x = Age, y=Pop, fill = "In Outer London"), position = "identity", alpha = 0.8) +
    labs(title = paste("Population Movement between Outer London and outside London in", year),
         x = "Age", y = "Pop") +
    scale_fill_manual(values = c("In Outer London" = "grey", "Out Outer London" = "green"),
                      name = "Population",
                      labels = c("In Outer London", "Out Outer London")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outOuterlondon$Age)[seq(1, length(levels(long_outOuterlondon$Age)), by = 5)]) +
    facet_grid(. ~ "In Outer London") + # Facet for histogram_inlondon
    geom_col(data = histogram_outOuterlondon$data, aes(x = Age, y=Pop,fill = "Out Outer London"), position = "identity", alpha = 0.5) +
    labs(x = "Age") +
    theme(legend.position = "top") +
    facet_grid(. ~ "Out Outer London") # Facet for histogram_outlondon
  
  print(combined_Outerhistogram) 
  
  # Draw NET histogram
  long_Outerlondon <- gather(get(paste0("Net_OuterLDN_to_outsideLDN", year)), key = "Variable", value = "Pop")
  long_Outerlondon <- long_Outerlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_Outerlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_Outerlondon$Age <- as.factor(long_Outerlondon$Age)
  long_Outerlondon$Pop <- as.numeric(long_Outerlondon$Pop)
  net_Outer_histogram <- ggplot(long_Outerlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Net Pop flow between Outer London and outside London", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_Outerlondon$Age)[seq(1, length(levels(long_Outerlondon$Age)), by = 5)]) 
  
  print(net_Outer_histogram)
}

##Yearly for Inner to Outer London
# Iterate over each year
for (year in years) {
  # Draw IN histogram
  long_inInOutlondon <- gather(get(paste0("data_inInOutlondon_", year)), key = "Variable", value = "Pop")
  long_inInOutlondon <- long_inInOutlondon[-1, ]
  long_inInOutlondon <- long_inInOutlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inInOutlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inInOutlondon$Age <- as.factor(long_inInOutlondon$Age)
  long_inInOutlondon$Pop <- as.numeric(long_inInOutlondon$Pop)
  histogram_inInOutlondon <- ggplot(long_inInOutlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving outer to inner London", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inInOutlondon$Age)[seq(1, length(levels(long_inInOutlondon$Age)), by = 5)])
  
  # Draw OUT histogram
  long_outInOutlondon <- gather(get(paste0("data_outInOutlondon_", year)), key = "Variable", value = "Pop")
  long_outInOutlondon <- long_outInOutlondon[-1, ]
  long_outInOutlondon <- long_outInOutlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outInOutlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outInOutlondon$Age <- as.factor(long_outInOutlondon$Age)
  long_outInOutlondon$Pop <- as.numeric(long_outInOutlondon$Pop)
  histogram_outInOutlondon <- ggplot(long_outInOutlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving inner to outer london", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outInOutlondon$Age)[seq(1, length(levels(long_outInOutlondon$Age)), by = 5)])
  
  # Stack the histograms
  combined_InOuthistogram <- ggplot() +
    geom_col(data = histogram_inInOutlondon$data, aes(x = Age, y=Pop, fill = "Outer to Inner London"), position = "identity", alpha = 0.8) +
    labs(title = paste("Population Movement between Outer London and Inner London", year),
         x = "Age", y = "Pop") +
    scale_fill_manual(values = c("Outer to Inner London" = "grey", "Inner to Outer London" = "green"),
                      name = "Population",
                      labels = c("Outer to Inner London", "Inner to Outer London")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outInOutlondon$Age)[seq(1, length(levels(long_outInOutlondon$Age)), by = 5)]) +
    facet_grid(. ~ "Outer to Inner London") + # Facet for histogram_inlondon
    geom_col(data = histogram_outInOutlondon$data, aes(x = Age, y=Pop,fill = "Inner to Outer London"), position = "identity", alpha = 0.5) +
    labs(x = "Age") +
    theme(legend.position = "top") +
    facet_grid(. ~ "Inner to Outer London") # Facet for histogram_outlondon
  
  print(combined_InOuthistogram) 
  
  # Draw NET histogram
  long_InOutlondon <- gather(get(paste0("Net_InOutLDN", year)), key = "Variable", value = "Pop")
  long_InOutlondon <- long_InOutlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_InOutlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_InOutlondon$Age <- as.factor(long_InOutlondon$Age)
  long_InOutlondon$Pop <- as.numeric(long_InOutlondon$Pop)
  net_InOut_histogram <- ggplot(long_InOutlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Net Pop flow between Inner and Outer London", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_InOutlondon$Age)[seq(1, length(levels(long_InOutlondon$Age)), by = 5)]) 
  
  print(net_InOut_histogram)
}


##for 15-19 average, LONDON PUA
#draw histogram_inlondon
long_inlondon <- gather(average15_19_inLondon, key = "Variable", value = "Pop")
long_inlondon <- long_inlondon[-1, ]
long_inlondon <- long_inlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_inlondon), by = 1)) %>%
  select(Age, everything()) 

long_inlondon$Age <- as.factor(long_inlondon$Age)
long_inlondon$Pop <- as.numeric(long_inlondon$Pop)
histogram_in1519london <- ggplot(long_inlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "pop moving into london in 2022",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])

#draw histogram_outlondon
long_outlondon <- gather(average15_19_outLondon, key = "Variable", value = "Pop")
long_outlondon <- long_outlondon[-1, ]
long_outlondon <- long_outlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_outlondon), by = 1)) %>%
  select(Age, everything()) 

long_outlondon$Age <- as.factor(long_outlondon$Age)
long_outlondon$Pop <- as.numeric(long_outlondon$Pop)
histogram_out1519london <- ggplot(long_outlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "pop moving out of london in 2022",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)])

# Stack the histograms
combined_1519histogram <- ggplot() +
  geom_col(data = histogram_in1519london$data, aes(x = Age, y=Pop, fill = "In London"), position = "identity", alpha = 0.8) +
  labs(title = "Population Movement in and out of London in 2015-19 Average",
       y = "Pop") +
  scale_fill_manual(values = c("In London" = "grey", "Out London" = "green"),
                    name = "Population",
                    labels = c("In London", "Out London")) +  # Specify legend labels
  theme_minimal() +
  scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
  facet_grid(. ~ "In London") + # Facet for histogram_inlondon
  geom_col(data = histogram_out1519london$data, aes(x = Age, y=Pop,fill = "Out London"), position = "identity", alpha = 0.5) +
  labs(x = "Age") +
  theme(legend.position = "top") +
  facet_grid(. ~ "Out London") # Facet for histogram_outlondon

print(combined_1519histogram) 

#draw net 1519 london
long_netlondon <- gather(average15_19_netLondon, key = "Variable", value = "Pop")
long_netlondon <- long_netlondon[-1, ]
long_netlondon <- long_netlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_netlondon), by = 1)) %>%
  select(Age, everything()) 

long_netlondon$Age <- as.factor(long_netlondon$Age)
long_netlondon$Pop <- as.numeric(long_netlondon$Pop)
histogram_out1519london <- ggplot(long_netlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Net population flows, London 2015-2019",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_netlondon$Age)[seq(1, length(levels(long_netlondon$Age)), by = 5)])

print(histogram_out1519london)

##for 15-19 average, Inner London to Outside London PUA
#draw histogram_inlondon
long_inlondon <- gather(average15_19_inInnerLondon, key = "Variable", value = "Pop")
long_inlondon <- long_inlondon[-1, ]
long_inlondon <- long_inlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_inlondon), by = 1)) %>%
  select(Age, everything()) 

long_inlondon$Age <- as.factor(long_inlondon$Age)
long_inlondon$Pop <- as.numeric(long_inlondon$Pop)
histogram_inInner1519london <- ggplot(long_inlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pop moving into Inner london from outside London, 2015-2019 average per year",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])

#draw histogram_outlondon
long_outlondon <- gather(average15_19_outInnerLondon, key = "Variable", value = "Pop")
long_outlondon <- long_outlondon[-1, ]
long_outlondon <- long_outlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_outlondon), by = 1)) %>%
  select(Age, everything()) 

long_outlondon$Age <- as.factor(long_outlondon$Age)
long_outlondon$Pop <- as.numeric(long_outlondon$Pop)
histogram_outInner1519london <- ggplot(long_outlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pop moving to outside London from Inner London, 2015-19 yearly average",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)])

# Stack the histograms
combined_1519Innerhistogram <- ggplot() +
  geom_col(data = histogram_inInner1519london$data, aes(x = Age, y=Pop, fill = "In London"), position = "identity", alpha = 0.8) +
  labs(title = "Population Movement between Inner London and outside London in 2015-19 Average",
       y = "Pop") +
  scale_fill_manual(values = c("Into Inner London" = "grey", "Out of Inner London" = "green"),
                    name = "Population",
                    labels = c("Into Inner", "Out of Inner London")) +  # Specify legend labels
  theme_minimal() +
  scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
  facet_grid(. ~ "Into Inner London") + # Facet for histogram_inlondon
  geom_col(data = histogram_outInner1519london$data, aes(x = Age, y=Pop,fill = "Out of Inner London"), position = "identity", alpha = 0.5) +
  labs(x = "Age") +
  theme(legend.position = "top") +
  facet_grid(. ~ "Out of Inner London") # Facet for histogram_outlondon

print(combined_1519Innerhistogram) 

#draw net 1519 inner to outside london
long_netlondon <- gather(average15_19_netInnerLondon, key = "Variable", value = "Pop")
long_netlondon <- long_netlondon[-1, ]
long_netlondon <- long_netlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_netlondon), by = 1)) %>%
  select(Age, everything()) 

long_netlondon$Age <- as.factor(long_netlondon$Age)
long_netlondon$Pop <- as.numeric(long_netlondon$Pop)
histogram_Innernet1519london <- ggplot(long_netlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Net population flows, Inner London to Outside London 2015-2019",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_netlondon$Age)[seq(1, length(levels(long_netlondon$Age)), by = 5)])

print(histogram_Innernet1519london)


##for 15-19 average, Outer London to Outside London PUA
#draw histogram_inlondon
long_inlondon <- gather(average15_19_inOuterLondon, key = "Variable", value = "Pop")
long_inlondon <- long_inlondon[-1, ]
long_inlondon <- long_inlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_inlondon), by = 1)) %>%
  select(Age, everything()) 

long_inlondon$Age <- as.factor(long_inlondon$Age)
long_inlondon$Pop <- as.numeric(long_inlondon$Pop)
histogram_inOuter1519london <- ggplot(long_inlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pop moving into Outer london from outside London, 2015-2019 average per year",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])

#draw histogram_outlondon
long_outlondon <- gather(average15_19_outOuterLondon, key = "Variable", value = "Pop")
long_outlondon <- long_outlondon[-1, ]
long_outlondon <- long_outlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_outlondon), by = 1)) %>%
  select(Age, everything()) 

long_outlondon$Age <- as.factor(long_outlondon$Age)
long_outlondon$Pop <- as.numeric(long_outlondon$Pop)
histogram_outOuter1519london <- ggplot(long_outlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pop moving to outside London from Outer London, 2015-19 yearly average",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)])

# Stack the histograms
combined_1519Outerhistogram <- ggplot() +
  geom_col(data = histogram_inOuter1519london$data, aes(x = Age, y=Pop, fill = "In London"), position = "identity", alpha = 0.8) +
  labs(title = "Population Movement between Outer London and outside London in 2015-19 Average",
       y = "Pop") +
  scale_fill_manual(values = c("Into Outer London" = "grey", "Out of Outer London" = "green"),
                    name = "Population",
                    labels = c("Into Outer", "Out of Outer London")) +  # Specify legend labels
  theme_minimal() +
  scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
  facet_grid(. ~ "Into Outer London") + # Facet for histogram_inlondon
  geom_col(data = histogram_outOuter1519london$data, aes(x = Age, y=Pop,fill = "Out of Outer London"), position = "identity", alpha = 0.5) +
  labs(x = "Age") +
  theme(legend.position = "top") +
  facet_grid(. ~ "Out of Outer London") # Facet for histogram_outlondon

print(combined_1519Outerhistogram) 

#draw net 1519 outer to outside london
long_netlondon <- gather(average15_19_netOuterLondon, key = "Variable", value = "Pop")
long_netlondon <- long_netlondon[-1, ]
long_netlondon <- long_netlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_netlondon), by = 1)) %>%
  select(Age, everything()) 

long_netlondon$Age <- as.factor(long_netlondon$Age)
long_netlondon$Pop <- as.numeric(long_netlondon$Pop)
histogram_Outernet1519london <- ggplot(long_netlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Net population flows, Outer London to Outside London 2015-2019",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_netlondon$Age)[seq(1, length(levels(long_netlondon$Age)), by = 5)])

print(histogram_Outernet1519london)


##for 15-19 average, Between Inner and Outer London
#draw histogram_inlondon
long_inlondon <- gather(average15_19_inInOutLondon, key = "Variable", value = "Pop")
long_inlondon <- long_inlondon[-1, ]
long_inlondon <- long_inlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_inlondon), by = 1)) %>%
  select(Age, everything()) 

long_inlondon$Age <- as.factor(long_inlondon$Age)
long_inlondon$Pop <- as.numeric(long_inlondon$Pop)
histogram_inInOut1519london <- ggplot(long_inlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pop moving into Inner London from Outer London, 2015-2019 average per year",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])

print(histogram_inInOut1519london) 

#draw histogram_outlondon
long_outlondon <- gather(average15_19_outInOutLondon, key = "Variable", value = "Pop")
long_outlondon <- long_outlondon[-1, ]
long_outlondon <- long_outlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_outlondon), by = 1)) %>%
  select(Age, everything()) 

long_outlondon$Age <- as.factor(long_outlondon$Age)
long_outlondon$Pop <- as.numeric(long_outlondon$Pop)
histogram_outInOut1519london <- ggplot(long_outlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pop moving into Outer London from Inner London, 2015-2019 average per year",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)])

print(histogram_outInOut1519london) 

# Stack the histograms
combined_1519InOuthistogram <- ggplot() +
  geom_col(data = histogram_inInOut1519london$data, aes(x = Age, y=Pop, fill = "Outer to Inner London"), position = "identity", alpha = 0.8) +
  labs(title = "Population Movement between Inner and Outer London in 2015-19 Average",
       y = "Pop") +
  scale_fill_manual(values = c("Outer to Inner London" = "grey", "Inner to Outer London" = "green"),
                    name = "Population",
                    labels = c("Outer to Inner London", "Inner to Outer London")) +  # Specify legend labels
  theme_minimal() +
  scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
  facet_grid(. ~ "Outer to Inner London") + # Facet for histogram_inlondon
  geom_col(data = histogram_outInOut1519london$data, aes(x = Age, y=Pop,fill = "Inner to Outer London"), position = "identity", alpha = 0.5) +
  labs(x = "Age") +
  theme(legend.position = "top") +
  facet_grid(. ~ "Inner to Outer London") # Facet for histogram_outlondon

print(combined_1519InOuthistogram) 

#draw net 1519 inner to outer london
long_netlondon <- gather(average15_19_netInOutLondon, key = "Variable", value = "Pop")
long_netlondon <- long_netlondon[-1, ]
long_netlondon <- long_netlondon %>%
  mutate(Age = seq(0, length.out = nrow(long_netlondon), by = 1)) %>%
  select(Age, everything()) 

long_netlondon$Age <- as.factor(long_netlondon$Age)
long_netlondon$Pop <- as.numeric(long_netlondon$Pop)
histogram_InOutnet1519london <- ggplot(long_netlondon, aes(x = Age, y = Pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Net population flows between Inner and Outer London, 2015-2019",
       x = "Age", y = "Pop") +
  theme_minimal()  +
  scale_x_discrete(breaks = levels(long_netlondon$Age)[seq(1, length(levels(long_netlondon$Age)), by = 5)])

print(histogram_InOutnet1519london)


### Stack London 2015-19 on 2020-21 net graphs
  # Draw IN histogram
  long_inlondon <- gather(get(paste0("data_inlondon_2021")), key = "Variable", value = "Pop")
  long_inlondon <- long_inlondon[-1, ]
  long_inlondon <- long_inlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inlondon$Age <- as.factor(long_inlondon$Age)
  long_inlondon$Pop <- as.numeric(long_inlondon$Pop)
  histogram_in21london <- ggplot(long_inlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving into london in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])
  
  # Draw OUT histogram
  long_outlondon <- gather(get(paste0("data_outlondon_2021")), key = "Variable", value = "Pop")
  long_outlondon <- long_outlondon[-1, ]
  long_outlondon <- long_outlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outlondon$Age <- as.factor(long_outlondon$Age)
  long_outlondon$Pop <- as.numeric(long_outlondon$Pop)
  histogram_out21london <- ggplot(long_outlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving out of london in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)])
  
  # Stack the histograms
  combined_histogram21 <- ggplot() +
    geom_col(data = histogram_in21london$data, aes(x = Age, y=Pop, fill = "In London"), position = "identity", alpha = 0.8) +
    labs(title = paste("Population Movement in and out of London in 2021"),
         x = "Age", y = "Pop") +
    scale_fill_manual(values = c("In London" = "grey", "Out London" = "green"),
                      name = "Population",
                      labels = c("In London", "Out London")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "In London") + # Facet for histogram_inlondon
    geom_col(data = histogram_out21london$data, aes(x = Age, y=Pop,fill = "Out London"), position = "identity", alpha = 0.5) +
    labs(x = "Age") +
    theme(legend.position = "top") +
    facet_grid(. ~ "Out London") # Facet for histogram_outlondon
  
  print(combined_histogram21) 
  
  # Draw NET histogram
  long_london <- gather(get(paste0("Net_london_2021")), key = "Variable", value = "Pop")
  long_london <- long_london %>%
    mutate(Age = seq(0, length.out = nrow(long_london), by = 1)) %>%
    select(Age, everything()) 
  
  long_london$Age <- as.factor(long_london$Age)
  long_london$Pop <- as.numeric(long_london$Pop)
  net_histogram21 <- ggplot(long_london, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Net Pop flow, London PUA, 2020-21"),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_london$Age)[seq(1, length(levels(long_london$Age)), by = 5)]) 
  
  print(net_histogram21)
  
  # Stack 2015-19 average and 2021 net histograms
  combined_151921nethistogram <- ggplot() +
    geom_col(data = histogram_out1519london$data, aes(x = as.numeric(Age) + 1, y=Pop, fill = "2015-19 Average Net"), position = "identity", alpha = 0.8) +
    labs(title = "Net population movement, 2015-19 yearly average & 2020-21",
         x = "Age",
         y = "Pop") +
    scale_fill_manual(values = c("2015-19 Average Net" = "orange", "2020-21 Net" = "blue"),
                      name = "Population",
                      labels = c("2015-19 Average Net", "2020-21 Net")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "2015-19 Average Net") + # Facet for histogram_inlondon
    geom_col(data = net_histogram21$data, aes(x = Age, y=Pop,fill = "2020-21 Net"), position = "identity", alpha = 0.5) +
    theme(legend.position = "top") +
    facet_grid(. ~ "2020-21 Net") # Facet for histogram_outlondon
  
  print(combined_151921nethistogram)
  
  
##stack inner net 2015-19 on 2020-21
  # Draw IN histogram
  long_inInnerlondon <- gather(get(paste0("data_inInnerlondon_2021")), key = "Variable", value = "Pop")
  long_inInnerlondon <- long_inInnerlondon[-1, ]
  long_inInnerlondon <- long_inInnerlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inInnerlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inInnerlondon$Age <- as.factor(long_inInnerlondon$Age)
  long_inInnerlondon$Pop <- as.numeric(long_inInnerlondon$Pop)
  histogram_inInnerlondon <- ggplot(long_inInnerlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving into Inner London"),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inInnerlondon$Age)[seq(1, length(levels(long_inInnerlondon$Age)), by = 5)])
  
  # Draw OUT histogram
  long_outInnerlondon <- gather(get(paste0("data_outInnerlondon_2021")), key = "Variable", value = "Pop")
  long_outInnerlondon <- long_outInnerlondon[-1, ]
  long_outInnerlondon <- long_outInnerlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outInnerlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outInnerlondon$Age <- as.factor(long_outInnerlondon$Age)
  long_outInnerlondon$Pop <- as.numeric(long_outInnerlondon$Pop)
  histogram_outInnerlondon <- ggplot(long_outInnerlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving out of Inner london to outside London in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outInnerlondon$Age)[seq(1, length(levels(long_outInnerlondon$Age)), by = 5)])
  
  # Stack the histograms
  combined_Innerhistogram21 <- ggplot() +
    geom_col(data = histogram_inInnerlondon$data, aes(x = Age, y=Pop, fill = "In Inner London"), position = "identity", alpha = 0.8) +
    labs(title = paste("Population Movement between Inner London and outside London in"),
         x = "Age", y = "Pop") +
    scale_fill_manual(values = c("In Inner London" = "grey", "Out Inner London" = "green"),
                      name = "Population",
                      labels = c("In Inner London", "Out Inner London")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outInnerlondon$Age)[seq(1, length(levels(long_outInnerlondon$Age)), by = 5)]) +
    facet_grid(. ~ "In Inner London") + # Facet for histogram_inlondon
    geom_col(data = histogram_outInnerlondon$data, aes(x = Age, y=Pop,fill = "Out Inner London"), position = "identity", alpha = 0.5) +
    labs(x = "Age") +
    theme(legend.position = "top") +
    facet_grid(. ~ "Out Inner London") # Facet for histogram_outlondon
  
  print(combined_Innerhistogram21) 
  
  # Draw NET histogram
  long_Innerlondon <- gather(get(paste0("Net_InnerLDN_to_outsideLDN2021")), key = "Variable", value = "Pop")
  long_Innerlondon <- long_Innerlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_Innerlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_Innerlondon$Age <- as.factor(long_Innerlondon$Age)
  long_Innerlondon$Pop <- as.numeric(long_Innerlondon$Pop)
  net_inner_histogram21 <- ggplot(long_Innerlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Net Pop flow between Inner London and outside London"),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_Innerlondon$Age)[seq(1, length(levels(long_Innerlondon$Age)), by = 5)]) 
  
  print(net_inner_histogram21)
  
  # Stack 2015-19 average and 2021 net histograms
  combined_151921nethistogram <- ggplot() +
    geom_col(data = histogram_Innernet1519london$data, aes(x = as.numeric(Age) + 1, y=Pop, fill = "2015-19 Average Net"), position = "identity", alpha = 0.8) +
    labs(title = "Net population movement Inner London to outside London, 2015-19 yearly average & 2020-21",
         x = "Age",
         y = "Pop") +
    scale_fill_manual(values = c("2015-19 Average Net" = "orange", "2020-21 Net" = "blue"),
                      name = "Population",
                      labels = c("2015-19 Average Net", "2020-21 Net")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "2015-19 Average Net") + # Facet for histogram_inlondon
    geom_col(data = net_inner_histogram21$data, aes(x = Age, y=Pop,fill = "2020-21 Net"), position = "identity", alpha = 0.5) +
    theme(legend.position = "top") +
    facet_grid(. ~ "2020-21 Net") # Facet for histogram_outlondon
  
  print(combined_151921nethistogram)
  
  
##Stack 2015-19 on 2020-21 outer london to outside london
  # Draw IN histogram
  long_inOuterlondon <- gather(get(paste0("data_inOuterlondon_2021")), key = "Variable", value = "Pop")
  long_inOuterlondon <- long_inOuterlondon[-1, ]
  long_inOuterlondon <- long_inOuterlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inOuterlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inOuterlondon$Age <- as.factor(long_inOuterlondon$Age)
  long_inOuterlondon$Pop <- as.numeric(long_inOuterlondon$Pop)
  histogram_inOuterlondon <- ggplot(long_inOuterlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving into Outer London"),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inOuterlondon$Age)[seq(1, length(levels(long_inOuterlondon$Age)), by = 5)])
  
  # Draw OUT histogram
  long_outOuterlondon <- gather(get(paste0("data_outOuterlondon_2021")), key = "Variable", value = "Pop")
  long_outOuterlondon <- long_outOuterlondon[-1, ]
  long_outOuterlondon <- long_outOuterlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outOuterlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outOuterlondon$Age <- as.factor(long_outOuterlondon$Age)
  long_outOuterlondon$Pop <- as.numeric(long_outOuterlondon$Pop)
  histogram_outOuterlondon <- ggplot(long_outOuterlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving out of Outer london to outside London in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outOuterlondon$Age)[seq(1, length(levels(long_outOuterlondon$Age)), by = 5)])
  
  # Stack the histograms
  combined_Outerhistogram21 <- ggplot() +
    geom_col(data = histogram_inOuterlondon$data, aes(x = Age, y=Pop, fill = "In Outer London"), position = "identity", alpha = 0.8) +
    labs(title = paste("Population Movement between Outer London and outside London in"),
         x = "Age", y = "Pop") +
    scale_fill_manual(values = c("In Outer London" = "grey", "Out Outer London" = "green"),
                      name = "Population",
                      labels = c("In Outer London", "Out Outer London")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outOuterlondon$Age)[seq(1, length(levels(long_outOuterlondon$Age)), by = 5)]) +
    facet_grid(. ~ "In Outer London") + # Facet for histogram_inlondon
    geom_col(data = histogram_outOuterlondon$data, aes(x = Age, y=Pop,fill = "Out Outer London"), position = "identity", alpha = 0.5) +
    labs(x = "Age") +
    theme(legend.position = "top") +
    facet_grid(. ~ "Out Outer London") # Facet for histogram_outlondon
  
  print(combined_Outerhistogram21) 
  
  # Draw NET histogram
  long_Outerlondon <- gather(get(paste0("Net_OuterLDN_to_outsideLDN2021")), key = "Variable", value = "Pop")
  long_Outerlondon <- long_Outerlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_Outerlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_Outerlondon$Age <- as.factor(long_Outerlondon$Age)
  long_Outerlondon$Pop <- as.numeric(long_Outerlondon$Pop)
  net_Outer_histogram21 <- ggplot(long_Outerlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Net Pop flow between Outer London and outside London"),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_Outerlondon$Age)[seq(1, length(levels(long_Outerlondon$Age)), by = 5)]) 
  
  print(net_Outer_histogram21)
  
  # Stack 2015-19 average and 2021 net histograms
  combined_151921nethistogram21 <- ggplot() +
    geom_col(data = histogram_Outernet1519london$data, aes(x = as.numeric(Age) + 1, y=Pop, fill = "2015-19 Average Net"), position = "identity", alpha = 0.8) +
    labs(title = "Net population movement Outer London to outside London, 2015-19 yearly average & 2020-21",
         x = "Age",
         y = "Pop") +
    scale_fill_manual(values = c("2015-19 Average Net" = "orange", "2020-21 Net" = "blue"),
                      name = "Population",
                      labels = c("2015-19 Average Net", "2020-21 Net")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "2015-19 Average Net") + # Facet for histogram_inlondon
    geom_col(data = net_Outer_histogram21$data, aes(x = Age, y=Pop,fill = "2020-21 Net"), position = "identity", alpha = 0.5) +
    theme(legend.position = "top") +
    facet_grid(. ~ "2020-21 Net") # Facet for histogram_outlondon
  
  print(combined_151921nethistogram21)
  
  
  
  ## Stack London 2015-19 on 2020-21 net Inner to Outer graphs
  # Draw IN histogram
  long_inlondon <- gather(get(paste0("data_inlondon_2021")), key = "Variable", value = "Pop")
  long_inlondon <- long_inlondon[-1, ]
  long_inlondon <- long_inlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inlondon$Age <- as.factor(long_inlondon$Age)
  long_inlondon$Pop <- as.numeric(long_inlondon$Pop)
  histogram_in21london <- ggplot(long_inlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving into london in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])
  
  # Draw OUT histogram
  long_outlondon <- gather(get(paste0("data_outlondon_2021")), key = "Variable", value = "Pop")
  long_outlondon <- long_outlondon[-1, ]
  long_outlondon <- long_outlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outlondon$Age <- as.factor(long_outlondon$Age)
  long_outlondon$Pop <- as.numeric(long_outlondon$Pop)
  histogram_out21london <- ggplot(long_outlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving out of london in", year),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)])
  
  # Stack the histograms
  combined_histogram21 <- ggplot() +
    geom_col(data = histogram_in21london$data, aes(x = Age, y=Pop, fill = "In London"), position = "identity", alpha = 0.8) +
    labs(title = paste("Population Movement in and out of London in 2021"),
         x = "Age", y = "Pop") +
    scale_fill_manual(values = c("In London" = "grey", "Out London" = "green"),
                      name = "Population",
                      labels = c("In London", "Out London")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "In London") + # Facet for histogram_inlondon
    geom_col(data = histogram_out21london$data, aes(x = Age, y=Pop,fill = "Out London"), position = "identity", alpha = 0.5) +
    labs(x = "Age") +
    theme(legend.position = "top") +
    facet_grid(. ~ "Out London") # Facet for histogram_outlondon
  
  print(combined_histogram21) 
  
  # Draw NET histogram
  
  long_london <- gather(get(paste0("Net_london_2021")), key = "Variable", value = "Pop")
  long_london <- long_london %>%
    mutate(Age = seq(0, length.out = nrow(long_london), by = 1)) %>%
    select(Age, everything()) 
  
  long_london$Age <- as.factor(long_london$Age)
  long_london$Pop <- as.numeric(long_london$Pop)
  net_histogram21 <- ggplot(long_london, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Net Pop flow, London PUA, 2020-21"),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_london$Age)[seq(1, length(levels(long_london$Age)), by = 5)]) 
  
  print(net_histogram21)
  
  # Stack 2015-19 average and 2021 net histograms
  combined_151921nethistogram <- ggplot() +
    geom_col(data = histogram_out1519london$data, aes(x = as.numeric(Age) + 1, y=Pop, fill = "2015-19 Average Net"), position = "identity", alpha = 0.8) +
    labs(title = "Net population movement, 2015-19 yearly average & 2020-21",
         x = "Age",
         y = "Pop") +
    scale_fill_manual(values = c("2015-19 Average Net" = "orange", "2020-21 Net" = "blue"),
                      name = "Population",
                      labels = c("2015-19 Average Net", "2020-21 Net")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "2015-19 Average Net") + # Facet for histogram_inlondon
    geom_col(data = net_histogram21$data, aes(x = Age, y=Pop,fill = "2020-21 Net"), position = "identity", alpha = 0.5) +
    theme(legend.position = "top") +
    facet_grid(. ~ "2020-21 Net") # Facet for histogram_outlondon
  
  print(combined_151921nethistogram)
  
  
  ##Stack average and 2020-21 Outer to Inner and Inner to Outer histograms.
  #draw histogram_inlondon
  long_inlondon <- gather(average15_19_inInOutLondon, key = "Variable", value = "Pop")
  long_inlondon <- long_inlondon[-1, ]
  long_inlondon <- long_inlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inlondon$Age <- as.factor(long_inlondon$Age)
  long_inlondon$Pop <- as.numeric(long_inlondon$Pop)
  histogram_inInOut1519london <- ggplot(long_inlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Pop moving into Inner London from Outer London, 2015-2019 average per year",
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])

  # Draw IN histogram
  long_inInOutlondon <- gather(get(paste0("data_inInOutlondon_2021")), key = "Variable", value = "Pop")
  long_inInOutlondon <- long_inInOutlondon[-1, ]
  long_inInOutlondon <- long_inInOutlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_inInOutlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_inInOutlondon$Age <- as.factor(long_inInOutlondon$Age)
  long_inInOutlondon$Pop <- as.numeric(long_inInOutlondon$Pop)
  histogram_inInOutlondon <- ggplot(long_inInOutlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving outer to inner London 2021"),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inInOutlondon$Age)[seq(1, length(levels(long_inInOutlondon$Age)), by = 5)])
  
  # Stack 2015-19 average and 2021 net histograms for IN 
  combined_151921INhistogram <- ggplot() +
    geom_col(data = histogram_inInOut1519london$data, aes(x = as.numeric(Age) + 1, y=Pop, fill = "2014-19 Average"), position = "identity", alpha = 0.8) +
    labs(title = "Population movement from Outer to Inner London, 2014-19 yearly average & 2020-21",
         x = "Age",
         y = "Pop") +
    scale_fill_manual(values = c("2014-19 Average" = "black", "2020-21" = "darkgreen"),
                      name = "Population",
                      labels = c("2014-19 Average", "2020-21")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "2014-19 Average") + # Facet for histogram_inlondon
    geom_col(data = histogram_inInOutlondon$data, aes(x = Age, y=Pop,fill = "2020-21"), position = "identity", alpha = 0.5) +
    theme(legend.position = "top") +
    facet_grid(. ~ "2020-21") # Facet for histogram_outlondon
  
  print(combined_151921INhistogram)
  
  
  #draw histogram_outlondon
  long_outlondon <- gather(average15_19_outInOutLondon, key = "Variable", value = "Pop")
  long_outlondon <- long_outlondon[-1, ]
  long_outlondon <- long_outlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outlondon$Age <- as.factor(long_outlondon$Age)
  long_outlondon$Pop <- as.numeric(long_outlondon$Pop)
  histogram_outInOut1519london <- ggplot(long_outlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Pop moving into Outer London from Inner London, 2015-2019 average per year",
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_inlondon$Age)[seq(1, length(levels(long_inlondon$Age)), by = 5)])
  

  # Draw OUT histogram
  long_outInOutlondon <- gather(get(paste0("data_outInOutlondon_2021")), key = "Variable", value = "Pop")
  long_outInOutlondon <- long_outInOutlondon[-1, ]
  long_outInOutlondon <- long_outInOutlondon %>%
    mutate(Age = seq(0, length.out = nrow(long_outInOutlondon), by = 1)) %>%
    select(Age, everything()) 
  
  long_outInOutlondon$Age <- as.factor(long_outInOutlondon$Age)
  long_outInOutlondon$Pop <- as.numeric(long_outInOutlondon$Pop)
  histogram_outInOutlondon <- ggplot(long_outInOutlondon, aes(x = Age, y = Pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("pop moving inner to outer london 2021"),
         x = "Age", y = "Pop") +
    theme_minimal()  +
    scale_x_discrete(breaks = levels(long_outInOutlondon$Age)[seq(1, length(levels(long_outInOutlondon$Age)), by = 5)])
  
  # Stack 2015-19 average and 2021 net histograms for OUT
  combined_151921OUThistogram <- ggplot() +
    geom_col(data = histogram_outInOut1519london$data, aes(x = as.numeric(Age) + 1, y=Pop, fill = "2014-19 Average"), position = "identity", alpha = 0.8) +
    labs(title = "Population movement from Inner to Outer London, 2014-19 yearly average & 2020-21",
         x = "Age",
         y = "Pop") +
    scale_fill_manual(values = c("2014-19 Average" = "black", "2020-21" = "purple"),
                      name = "Population",
                      labels = c("2014-19 Average", "2020-21")) +  # Specify legend labels
    theme_minimal() +
    scale_x_discrete(breaks = levels(long_outlondon$Age)[seq(1, length(levels(long_outlondon$Age)), by = 5)]) +
    facet_grid(. ~ "2014-19 Average") + # Facet for histogram_inlondon
    geom_col(data = histogram_outInOutlondon$data, aes(x = Age, y=Pop,fill = "2020-21"), position = "identity", alpha = 0.5) +
    theme(legend.position = "top") +
    facet_grid(. ~ "2020-21") # Facet for histogram_outlondon
  
  print(combined_151921OUThistogram)
  
  
  
  
 