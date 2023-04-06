

# README

# TODO regarding n<30, see
# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module6-RandomError/PH717-Module6-RandomError11.html

# END


# TODO document https://www.bea.gov/cautionary-note-about-annual-gdp-state-discontinuity in final report
# TODO scrape figures for report https://www.usgs.gov/mission-areas/water-resources/science/trends-water-use



library(tidyverse)
library(plotly)
library(htmlwidgets)
library(readxl)
library(trend)


# Import my project functions and variables
source("library.r")


# Import and subset USGS water use data
source("wrangle-usgs.r")


# Functions
report = function(df, x, y) {
  # Test whether x is related to y, and print results
  # Note: x and y must be strings (names of the columns rather than the column
  # objects themselves).

  # Test for correlation, and print test's p-value
  correlation = cor.test(df[[x]], df[[y]])
  cat(paste(
    paste0("`", x, "`"), "<-->", paste0("`", y, "`"),
    "correlation p-value:", correlation$p.value),
    "\n")

  # Test for correlation, and print test's r^2
  formula = as.formula(paste0('`', x, '`~`', y, '`'))
  model = lm(formula, data = df)  # linear regression
  cat(paste(
    paste0("`", x, "`"), "<-->", paste0("`", y, "`"),
    "correlation adjusted r^2:", summary(model)$adj.r.squared),
    "\n")

  # Test for trend in x and y (independantly), and print results
  for (i in c(x, y)) {
    result = mk.test(df[[i]])
    cat(paste(
      paste0("`", i, "`"),
      "Mann-Kendall trend test statistic:", result$statistic),
      "\n")
    cat(paste(
      paste0("`", i, "`"),
      "Mann-Kendall trend test p-value:", result$p.value),
      "\n")
  }
}

plot_wateruse_vs = function(df, y2, y2_label) {
  plot = 
    plot_ly(df) |>  # , width = 1200) |>
      # Plot water use
      add_trace(
        x = ~year,
        y = ~withdrawalSum,
        type = "bar",
        marker = list(color = "lightskyblue"),
        width = 3,
        # `name` sets legend label for this item
        name = "Water Withdrawals")
  
  # Plot other variables
  for (i in y2) {
    plot = add_trace(
      plot,
      x = df[["year"]],
      y = df[[i]],
      type = "scatter", mode = "lines",
      line = list(width = 5),
      yaxis = "y2",
      name = i)
  }

  # Configure aesthetic
  plot = layout(
    plot,
    font = list(size=18),
    xaxis = list(
      title = "Year"
    ),
    yaxis = list(
      title = "Water Withdrawals (gal/day)"
    ),
    yaxis2 = list(
      title = y2_label,
      overlaying = "y",
      side = "right"
    ),
    legend = list(x = 1.2))  # fix legend position to prevent y2-axis overlap

  # Show plot
  tmp_plot_path = paste0(tmp_dir_path, "/plotly_ccvswateruse_plot.html")
  saveWidget(plot, file = tmp_plot_path)  # save plot
  system(paste("firefox", tmp_plot_path))  # show plot
}

plot_x_vs_y = function(df, x, y) {
  # Calculate linear best fit line
  formula = as.formula(paste0('`', y, '`~`', x, '`'))
  model = lm(formula, data = df)  # linear regression

  # Draw plot
  plot =
    plot_ly(df) |>
      add_trace(
        x = ~df[[x]],
        y = ~df[[y]],
        type = "scatter", mode = "markers") |>
      add_lines(
        x = ~df[[x]],
        y = fitted(model),
        line = list(dash="solid", width = 1.1, color="red")
      ) |>
      layout(
        font = list(size=24),
        showlegend = F,
        xaxis = list(
          title = x
        ),
        yaxis = list(
          title = y
        )
      )
  
  # Show plot
  saveWidget(plot, file = "./tmp_plot2.html")  # save plot
  system(paste("firefox", "./tmp_plot2.html"))  # show plot
}


# Import BEA GDP data
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

df_gdp = bind_rows(df_naics_subset, df_sic_subset)
df_gdp = arrange(df_gdp, year)  # sort by year, ascending

# Convert GDP data to 5 year rolling (moving?) average
df_gdp_5year = df_gdp
df_gdp_5year$gdpMillionsCurrentDollars = zoo::rollmean(
  df_gdp$gdpMillionsCurrentDollars, k = 5, fill = NA, align = "left")

# Millions of dollars --> dollars
df_gdp_5year[["gdpCurrentDollars"]] = df_gdp_5year$gdpMillionsCurrentDollars * 1000000


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
    "Value of agricultural sector production",
    "Net farm income",
    "year")

# Thousands of dollars --> dollars
# I assume this is the multiplication factor because of the second row of the
# xlsx file just repeating "$1000"
# TODO this should be done inside the lapply function above for all columns
# (not just these subset ones)
df_usda_co["Value of agricultural sector production"] = 
  df_usda_co["Value of agricultural sector production"] * 1000
df_usda_co["Net farm income"] = df_usda_co["Net farm income"] * 1000

# Convert USDA Cash Crop data to 5 year rolling (moving?) average
df_usda_co_5year = df_usda_co |>
  filter(year >= 1963)

df_usda_co_5year[["Value of agricultural sector production"]] = 
  zoo::rollmean(
    df_usda_co_5year[["Value of agricultural sector production"]],
    k = 5, fill = NA, align = "left")

df_usda_co_5year[["Net farm income"]] =
  zoo::rollmean(
    df_usda_co_5year[["Net farm income"]],
    k = 5, fill = NA, align = "left")


# Combine water use, GDP, and cash crop data for plotting
df = inner_join(df_gdp_5year, df_wtr_sum, by = "year")
df = inner_join(df, df_usda_co_5year, by = "year")


# Statistical analysis
pairs = list(
  c("gdpCurrentDollars", "Value of agricultural sector production"),
  c("gdpCurrentDollars", "Net farm income"),
  c("withdrawalSum", "Value of agricultural sector production"),
  c("withdrawalSum", "Net farm income"))

for (i in 1:length(pairs)) {
  a = pairs[[i]][1]
  b = pairs[[i]][2]
  report(df, a, b)
}


# Plot - Visually inspect relationship between water use and cash crops
cols = list(
  "Value of agricultural sector production",
  "Net farm income")
plot_wateruse_vs(
  df,
  cols,
  y2_label = "$ (USD)")


# Plot - Visually inspect relationship between GDP and cash crops
plot = 
  plot_ly(df) |>  # , width = 1200) |>
    add_trace(
      x = ~df[["year"]],
      y = ~gdpCurrentDollars,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      name = "GDP") |>
    add_trace(
      x = ~year,
      y = ~`Value of agricultural sector production`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      name = "Value of Ag Sector Prod.") |>
    add_trace(
      x = ~year,
      y = ~`Net farm income`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      name = "Net Farm Income") |>
    layout(
      font = list(size=18),
      xaxis = list(
        title = "Year"
      ),
      yaxis = list(
        title = "$ (USD)"
      ),
      legend = list(x = 1.2)  # fix legend position to prevent y2-axis overlap
    )

tmp_plot_path = paste0(tmp_dir_path, "/plotly_gdpvscc_plot.html")
saveWidget(plot, file = tmp_plot_path)  # save plot
system(paste("firefox", tmp_plot_path))  # show plot

