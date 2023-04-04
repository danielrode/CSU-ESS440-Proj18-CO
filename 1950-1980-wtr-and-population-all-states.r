
# Map state names to USGS area codes
state_codes = list(
  AZ = "04",
  CA = "06",
  CO = "08",
  NV = "32",
  OR = "41",
  UT = "49",
  WA = "53")


# Import my project functions and variables
source("library.r")


# Import USGS water-use data
source("wrangle-usgs.r")


# Import and wrangle FRED U.S. population data
# source: https://fred.stlouisfed.org/release/tables?rid=118&eid=259194
# Units are thousands of people
df_pop = read.csv(paste0(dataDir, "/FRED_Population.csv"))

# "YYYY-MM-DD" --> YYYY
df_pop[["year"]] = as.numeric(gsub('-\\d\\d-\\d\\d','',df_pop$time))


# Join USGS water data and FRED population data
df = full_join(df_wtr_old, df_pop, by = "year")


# Format and structure data
states = c(
  "AZ",
  "CA",
  "CO",
  "NV",
  "OR",
  "UT",
  "WA")

pop_only_years = c(1985, 1990, 1995, 2000, 2005, 2010, 2015)
df_state_tallys_nested = list()
for (s in states) {
  df_state_tallys_nested[[s]] = df |>
    filter(Area == state_codes[[s]] | year %in% pop_only_years) |>
    select(year, withdrawalSum, pop = s)

  # Thousands of people --> people
  df_state_tallys_nested[[s]]$pop =
    df_state_tallys_nested[[s]]$pop * 1000

  df_state_tallys_nested[[s]][["state"]] = s
}

# Flatten nested dataframes into one
df_state_tallys = df_state_tallys_nested[[1]]
for (i in 2:length(df_state_tallys_nested)) {
  df_state_tallys = full_join(df_state_tallys, df_state_tallys_nested[[i]])
}

# Pivot so columns are years and rows are withdrawalSum and population
df_state_tallys = df_state_tallys |>
  pivot_longer(c(withdrawalSum, pop),
    names_to="type", values_to="value") |>
  pivot_wider(names_from="year")

df_state_tallys$type = gsub(
  "withdrawalSum", "Freshwater withdrawals (gal/day)", df_state_tallys$type)
df_state_tallys$type = gsub("pop", "Population", df_state_tallys$type)


# Save data to CSV
write.csv(df_state_tallys, file="export.csv")

