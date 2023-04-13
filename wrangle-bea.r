
library(tidyverse)
library(zoo)


# Import my project functions and variables
source("library.r")


# Wrangle BEA GDP data
path = paste(
  dataDir, 
  "SAGDP (bea - annual GDP by state)/SAGDP2N_CO_1997_2021.csv",
  sep='/')
df_naics = read.csv(path, sep = ',')
df_naics = head(df_naics, nrow(df_naics)-4)  # drop metadata/notes (last 4 lines of csv)

path = paste(
  dataDir, 
  "SAGDP (bea - annual GDP by state)/SAGDP2S_CO_1963_1997.csv",
  sep='/')
df_sic = read.csv(path, sep = ',')
df_sic = head(df_sic, nrow(df_sic)-4)  # drop metadata/notes (last 4 lines of csv)

# Subset and combine BEA GDP data
df_sic_subset = filter(df_sic, Description == "All industry total ") |>
  pivot_longer(
    cols=-c("GeoFIPS", "GeoName", "Region", "TableName", "LineCode", "IndustryClassification", "Description", "Unit"),
    names_to="year",
    values_to="gdpMillionsCurrentDollars",
    values_transform = as.numeric)
df_sic_subset$year = gsub('[A-z]', '', df_sic_subset$year) |> as.numeric()  # convert years col: "X2000" --> 2000

df_naics_subset = filter(df_naics, Description == "All industry total ") |>
  pivot_longer(
    cols=-c("GeoFIPS", "GeoName", "Region", "TableName", "LineCode", "IndustryClassification", "Description", "Unit"),
    names_to="year",
    values_to="gdpMillionsCurrentDollars",
    values_transform = as.numeric)
df_naics_subset$year = gsub('[A-z]', '', df_naics_subset$year) |> as.numeric()  # convert years col: "X2000" --> 2000

df_gdp_co = bind_rows(df_naics_subset, df_sic_subset)
df_gdp_co = arrange(df_gdp_co, year)  # sort by year, ascending

# Convert GDP data to 5 year rolling (moving?) average
df_gdp_co_5year = df_gdp_co
df_gdp_co_5year$gdpMillionsCurrentDollars = zoo::rollmean(
  df_gdp_co$gdpMillionsCurrentDollars, k = 5, fill = NA, align = "center")

# Millions of dollars --> dollars
df_gdp_co_5year[["gdpCurrentDollars"]] = df_gdp_co_5year$gdpMillionsCurrentDollars * 1000000

