

# README

# TODO regarding n<30, see
# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module6-RandomError/PH717-Module6-RandomError11.html

# END


# TODO document https://www.bea.gov/cautionary-note-about-annual-gdp-state-discontinuity in final report
# TODO scrape figures for report https://www.usgs.gov/mission-areas/water-resources/science/trends-water-use



library(tidyverse)
library(plotly)
library(htmlwidgets)
library(trend)


# Import my project functions and variables
source("library.r")


# Variables
plot_water_use_bar_color = "rgb(91,155,213)"
# plot_water_use_bar_color = "lightskyblue"
plot_population_line_color = "rgb(233,63,51)"


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
        y = ~`Water Withdrawals (million gallons/day)`,
        type = "bar",
        marker = list(color = plot_water_use_bar_color),
        width = 3,
        # `name` sets legend label for this item
        name = "Water Withdrawals")
  
  # Plot other variables
  for (i in y2) {
    # Set line color based on category ID
    category_color = "black"
    if (i == "GDP (million $/yr)") {
      category_color = "rgb(112,173,71)"
    } else if (i == "Value of Ag. Sector Production (1,000 $/yr)") {
      category_color = "rgb(190,144,37)"
    } else if (i == "Population (ten thousands)") {
      category_color = plot_population_line_color
    }

    plot = add_trace(
      plot,
      x = df[["year"]],
      y = df[[i]],
      type = "scatter", mode = "lines",
      line = list(width = 5, color = category_color),
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
      tickformat=',d',  # full number: "14k" --> "14,000"
      title = "Water Withdrawals (million gallons/day)"
    ),
    yaxis2 = list(
      tickformat=',d',  # full number: "14k" --> "14,000"
      title = y2_label,
      overlaying = "y",
      side = "right"
    ),
    margin = list(r = 200),  # prevent y2-axis labels from getting cut off
    legend = list(
      orientation = "h",  # show entries horizontally
      xanchor = "center",  # use center of legend as anchor
      x = 0.5,  # put legend in center of x-axis
      y = -0.16)  # fix legend position to prevent x-axis overlap
    )

  # Show plot
  uniq_path_part = gsub("[^A-z]", "_", y2)
  tmp_plot_path = paste0(tmp_dir_path, "/plotly-wtr-", uniq_path_part, ".html")
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
          tickformat=',d',  # full number: "14k" --> "14,000"
          title = x
        ),
        yaxis = list(
          tickformat=',d',  # full number: "14k" --> "14,000"
          title = y
        )
      )

  # Show plot
  uniq_path_part = gsub("[^A-z]", "_", paste0(x, y))
  tmp_plot_path = paste0(tmp_dir_path, "/plotly-", uniq_path_part, ".html")
  saveWidget(plot, file = tmp_plot_path)  # save plot
  system(paste("firefox", tmp_plot_path))  # show plot
}


# Import BEA GDP data
source("wrangle-bea.r")  # defines `df_gdp_co_5year`


# Import USDA cash crop data
source("wrangle-usda.r")  # defines `df_usda_co_5year`


# Import Census (FRED) population data
source("wrangle-fred.r")  # defines `df_pop`
df_pop_co = filter(df_pop, state_name == "Colorado")
df_pop_co_5year = df_pop_co
df_pop_co_5year$population = zoo::rollmean(
  df_pop_co$population, k = 5, fill = NA, align = "center")


# Import and subset Our World in Data global population and global freshwater
# use data
df_pop_global = read.csv("data/global/population.csv") |>
  as_tibble() |>
  filter(Entity == "World" & Year >= 1901 & Year <= 2010)
df_wtr_fresh_global = 
  read.csv("data/global/global-freshwater-use-over-the-long-run.csv") |>
  as_tibble() |>
  filter(Entity == "World")


# Import PRISM precipitation data
source("wrangle-prism.r")  # defines `df_precip_co`
df_precip_co_5year = df_precip_co
df_precip_co_5year$precip_mm = zoo::rollmean(
  df_precip_co$precip_mm, k = 5, fill = NA, align = "center")

# Redefine `select` so it refers to dplyr::select and not the select imported
# by `terra` package (which is imported by `wrangle-prism.r`)
select = dplyr::select


# Join data for plotting:
# - USGS water use
# - Census (FRED) population
# - BEA GDP
# - USDA cash crop
# - PRISM precipitation
# (Data is 5 year rolling average where applicable.)
df = inner_join(
  select(df_gdp_co_5year,
    "year",
    "gdpCurrentDollars"),
  select(df_wtr_sum_co,
    "year",
    "Water Withdrawals (million gallons/day)" = "withdrawalSum"),
  by = "year") |>
  mutate(
    "GDP (million $/yr)" = `gdpCurrentDollars` / 1000000
  )

df = inner_join(df,
  select(df_usda_co_5year,
    "year",
    "Value of Ag. Sector Production (1,000 $/yr)",
    "Net Farm Income (1,000 $/yr)"),
  by = "year")

df = inner_join(df,
  select(df_pop_co_5year,
    "year",
    "Population" = "population"),
  by = "year") |>
  mutate(
    "Population (ten thousands)" = Population / 10000
  )

df = inner_join(df,
  select(df_precip_co_5year,
    "year",
    "Precipitation (mm)" = "precip_mm"), 
  by = "year")


# Join Our World in Data global population and freshwater use data for 
# plotting
df_global = inner_join(
  select(df_pop_global,
    "year" = "Year",
    "Population" = "Population..historical.estimates."),
  select(df_wtr_fresh_global,
    "year" = "Year",
    "Freshwater Use (m^3)" = "Freshwater.use"),
  by = "year"
  ) |>
  mutate(
    "Freshwater Use (million gallons/day)" = 
      `Freshwater Use (m^3)` * 264.2 / 1000000 / 365,
    "Population (ten thousands)" = Population / 10000
  )


# Analyze Colorado population vs water use
cat("Examining relationship between Colorado population and water use...\n")

col = "Population (ten thousands)"
report(df, "Water Withdrawals (million gallons/day)", col)

# Plot - Visualize relationship between population and water use
plot_wateruse_vs(df,
  list(col),
  y2_label = col)

plot_x_vs_y(df, col, "Water Withdrawals (million gallons/day)")


# Analyze global population vs water use
cat("Examining relationship between global population and water use...\n")

report(df_global, "Freshwater Use (million gallons/day)", "Population")

# Plot - Visualize relationship between global population vs global freshwater
# use
plot_x_vs_y(df_global, 
  "Population (ten thousands)", "Freshwater Use (million gallons/day)")

plot = 
  plot_ly(df_global) |>
    add_trace(
      x = ~year,
      y = ~`Freshwater Use (million gallons/day)`,
      type = "bar",
      marker = list(color = plot_water_use_bar_color),
      name = "Freshwater Use") |>
    add_trace(
      x = ~year,
      y = ~`Population (ten thousands)`,
      type = "scatter", mode = "lines",
      line = list(width = 5, color = plot_population_line_color),
      yaxis = "y2",
      name = "Population") |>
    layout(
      font = list(size=18),
      xaxis = list(
        title = "Year"
      ),
      yaxis = list(
        tickformat=',d',  # full number: "14k" --> "14,000"
        title = "Freshwater Use (million gallons/day)"
      ),
      yaxis2 = list(
        tickformat=',d',  # full number: "14k" --> "14,000"
        title = "Population (ten thousands)",
        overlaying = "y",
        side = "right"
      ),
      margin = list(r = 150),  # prevent y2-axis labels from getting cut off
      legend = list(
        orientation = "h",  # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 0.5,  # put legend in center of x-axis
        y = -0.16)  # fix legend position to prevent x-axis overlap
    )

# Show plot
tmp_plot_path = paste0(tmp_dir_path, "/plotly-global-pop-wtr.html")
saveWidget(plot, file = tmp_plot_path)  # save plot
system(paste("firefox", tmp_plot_path))  # show plot


# Analyze Colorado GDP vs water use
cat("Examining relationship between Colorado GDP and water use...\n")

col = "GDP (million $/yr)"
report(df, "Water Withdrawals (million gallons/day)", col)

# Plot - Visualize relationship between Colorado GDP and water use
plot_wateruse_vs(df,
  list(col),
  y2_label = col)

plot_x_vs_y(df, col, "Water Withdrawals (million gallons/day)")


# Analyze Colorado cash crop metrics vs water use
cat("Examining relationship between Colorado cash crop and water use...\n")
pairs = list(
  c("GDP (million $/yr)", "Value of Ag. Sector Production (1,000 $/yr)"),
  c("GDP (million $/yr)", "Net Farm Income (1,000 $/yr)"),
  c("Water Withdrawals (million gallons/day)", 
    "Value of Ag. Sector Production (1,000 $/yr)"),
  c("Water Withdrawals (million gallons/day)",
    "Net Farm Income (1,000 $/yr)"))

for (i in 1:length(pairs)) {
  a = pairs[[i]][1]
  b = pairs[[i]][2]
  report(df, a, b)
}


# Plot - Visually inspect relationship between Colorado cash crops and water
# use
col = "Value of Ag. Sector Production (1,000 $/yr)"
plot_wateruse_vs(
  df,
  col,
  y2_label = col)

plot_x_vs_y(df, col, "Water Withdrawals (million gallons/day)")

# col = "Net Farm Income (1,000 $/yr)"
# plot_wateruse_vs(
#   df,
#   col,
#   y2_label = col)


# Plot - Visually inspect relationship between Colorado GDP and cash crops
plot = 
  plot_ly(df) |>  # , width = 1200) |>
    add_trace(
      x = ~df[["year"]],
      y = ~`GDP (million $/yr)`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      name = "GDP") |>
    add_trace(
      x = ~year,
      y = ~`Value of Ag. Sector Production (1,000 $/yr)`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      name = "Value of Ag Sector Prod.") |>
    add_trace(
      x = ~year,
      y = ~`Net Farm Income (1,000 $/yr)`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      name = "Net Farm Income (1,000 $/yr)") |>
    layout(
      font = list(size=18),
      xaxis = list(
        title = "Year"
      ),
      yaxis = list(
        tickformat=',d',  # full number: "14k" --> "14,000"
        title = "$ (USD)"
      ),
      legend = list(
        orientation = "h",  # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 0.5,  # put legend in center of x-axis
        y = -0.16)  # fix legend position to prevent x-axis overlap
    )

tmp_plot_path = paste0(tmp_dir_path, "/plotly_gdpvscc_plot.html")
saveWidget(plot, file = tmp_plot_path)  # save plot
system(paste("firefox", tmp_plot_path))  # show plot


# Analyze Colorado precipitation vs water use
col = "Precipitation (mm)"
cat(
  "Examining relationship between Colorado precipitation and water use...\n")

df_reduced = head(df, nrow(df) - 1)  # exclude 2015 (outlier)
report(df_reduced, "Water Withdrawals (million gallons/day)", col)

# Plot - Visualize relationship between precipitation and water use
plot_wateruse_vs(df,
  list(col),
  y2_label = col)

plot_x_vs_y(df, col, "Water Withdrawals (million gallons/day)")

