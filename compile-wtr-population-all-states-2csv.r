

library(tidyverse)


# Import my project functions and variables
source("library.r")


# Import USGS water-use data
source("wrangle-usgs.r")  # defines `df_wtr_old`


# Import FRED U.S. Census population data
source("wrangle-fred.r")  # defines `df_pop`


# Join USGS water data and FRED population data
df_wtr_old = df_wtr_old |> rename("USGS_state_area_code" = "Area")
df = full_join(df_wtr_old, df_pop, by = c("year", "USGS_state_area_code"))


# Format and structure data

# Filter data
states = unique(df_pop[["state_name"]])
years = seq(from = 1950, to = 2015, by = 5)
df = df |>
  select(year, state_name, population, withdrawalSum) |>
  filter(year %in% years) |>
  filter(state_name %in% states)

# Pivot so columns are years and rows are withdrawalSum and population
df = df |>
  pivot_longer(
    c(withdrawalSum, population),
    names_to="type", values_to="value") |>
  pivot_wider(names_from="year")

# Rename value types so they are more readable
df$type = gsub("withdrawalSum", "Freshwater withdrawals (gal/day)", df$type)
df$type = gsub("population", "Population", df$type)


# Save population and water use state tally data to CSV
write.csv(df, file="export.csv")

