
library(tidyverse)
library(readxl)


# Import my project functions and variables
source("library.r")


# Import USDA Cash Crop data
path = paste(
  dataDir, 
  "VA_State_US_total (value added by US ag, including net farm income).xlsx",
  sep='/')

# Read in USDA Cash Crop data xlsx file, sheet by sheet
sheet_names = c("United States", "Colorado")
sheets = lapply(sheet_names, function(s) {
  # Trim
  x = read_excel(path, sheet = s, skip = 2, col_names = TRUE)  # drop header
  x = x[-1,]  # drop first row (I believe it just defines notes units
  x = x[1:(nrow(x)-5),]  # remove footnotes
  x = drop_empty(x)  # remove rows/cols with only NA values

  # Make rows columns and columns rows (so columns are categories and rows are
  # years)
  x = transpose_df(x)
  x[["year"]] = x[[1]]
  x = x[,-1]

  # "number" --> number
  x[] = lapply(x, as.numeric)

  return(x)
})
names(sheets) = sheet_names

# Subset USDA Cash Crop data
df_usda_co = sheets[["Colorado"]] |>
  select(
    "Value of Ag. Sector Production (1,000 $/yr)" = 
      "Value of agricultural sector production",
    "Net Farm Income (1,000 $/yr)" = "Net farm income",
    "year")

# Convert USDA Cash Crop data to 5 year rolling (moving?) average
df_usda_co_5year = df_usda_co |>
  filter(year >= 1963)

df_usda_co_5year[["Value of Ag. Sector Production (1,000 $/yr)"]] =
  zoo::rollmean(
    df_usda_co_5year[["Value of Ag. Sector Production (1,000 $/yr)"]],
    k = 5, fill = NA, align = "center")

df_usda_co_5year[["Net Farm Income (1,000 $/yr)"]] =
  zoo::rollmean(
    df_usda_co_5year[["Net Farm Income (1,000 $/yr)"]],
    k = 5, fill = NA, align = "center")

