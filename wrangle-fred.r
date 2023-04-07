
library(tidyverse)


# Map state names to USGS area codes and state abbreviations 
state_codes = list(
  AZ = "04",
  CA = "06",
  CO = "08",
  NV = "32",
  OR = "41",
  UT = "49",
  WA = "53")

state_names = list(
  AZ = "Arizona",
  CA = "California",
  CO = "Colorado",
  NV = "Nevada",
  OR = "Oregon",
  UT = "Utah",
  WA = "Washington")


# Import my project functions and variables
source("library.r")


# Import and wrangle FRED U.S. Census population data
# source: https://fred.stlouisfed.org/release/tables?rid=118&eid=259194
# Units are thousands of people
df_pop = read.csv(paste0(dataDir, "/FRED_Population.csv"))

# "YYYY-MM-DD" --> YYYY
df_pop[["year"]] = as.numeric(gsub('-\\d\\d-\\d\\d', '', df_pop$time))

# Pivot so states are columns (as well as population)
df_pop = pivot_longer(
  df_pop,
  cols=names(state_codes),
  names_to="state_abbrv",
  values_to="population")

# Add USGS state code column
df_pop[["USGS_state_area_code"]] = df_pop[["state_abbrv"]]
for (i in 1:length(state_codes)) {
  df_pop[["USGS_state_area_code"]] = gsub(
    names(state_codes)[i], state_codes[i], 
    df_pop[["USGS_state_area_code"]])
}

# Add state name column
df_pop[["state_name"]] = df_pop[["state_abbrv"]]
for (i in 1:length(state_names)) {
  df_pop[["state_name"]] = gsub(
    names(state_names)[i], state_names[i], 
    df_pop[["state_name"]])
}

# Thousands of people --> people
df_pop[["population"]] =
  df_pop[["population"]] * 1000

