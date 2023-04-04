

# README

# TODO regarding n<30, see
# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module6-RandomError/PH717-Module6-RandomError11.html

# END



library(tidyverse)
library(plotly)
library(htmlwidgets)
library(trend)


# Variables
HOME = Sys.glob('~')
dataDir = paste(HOME, "downloads/ess440/data", sep="/")


# Funcitons
drop_letters = function(s) {
  s = gsub('[A-z]', '', s)
  s = as.numeric(s)
  return(s)
}

rproc = function(df, rows, tf) {
  # rproc = row processor: Run a function on each row of a dataframe.
  # tf = transformation function (performs operations on given array)
  for (r in rows) {
    df[r,] = tf(df[r,])
  }
  
  return(df)
}

cproc = function(df, cols, tf) {
  # cproc = column processor: Run a function on each column of a dataframe.
  for (c in cols) {
    df[,c] = tf(df[,c])
  }

  return(df)
}


# Import USGS data
path = paste(dataDir, "USGS Water Use Data for Colorado state-wide.csv", sep='/')
df = read.csv(path, sep = '\t', comment.char = "#", skip=2)

# Drop '2s' row (what the hell is it for?)
df = df[-1,]

# Subset data
cols = c(
  # Freshwater withdrawals:
  "Public Supply self-supplied groundwater withdrawals, fresh, in Mgal/d",
  "Public Supply total self-supplied withdrawals, fresh, in Mgal/d",
  "Public Supply deliveries to domestic, in Mgal/d",
  "Public Supply deliveries to commercial, in Mgal/d",
  "Public Supply deliveries to industrial, in Mgal/d",
  "Public Supply deliveries to thermoelectric, in Mgal/d",
  "Domestic total self-supplied withdrawals plus deliveries, in Mgal/d",
  # "Domestic consumptive use, fresh, in Mgal/d",
  "Industrial total self-supplied withdrawals, fresh, in Mgal/d",
  "Industrial deliveries from public supply, in Mgal/d",
  # "Industrial consumptive use, fresh, in Mgal/d",
  "Total Thermoelectric Power self-supplied groundwater withdrawals, fresh, in Mgal/d",
  "Total Thermoelectric Power self-supplied surface-water withdrawals, fresh, in Mgal/d",
  # "Total Thermoelectric Power consumptive use, fresh, in Mgal/d",
  "Mining total self-supplied withdrawals, fresh, in Mgal/d",
  # "Mining consumptive use, fresh, in Mgal/d",
  "Livestock total self-supplied withdrawals, fresh, in Mgal/d",
  "Livestock (Stock) total self-supplied withdrawals, fresh, in Mgal/d",
  # "Livestock (Stock) consumptive use, fresh, in Mgal/d",
  "Livestock (Animal Specialties) total self-supplied withdrawals, fresh, in Mgal/d",
  "Aquaculture total self-supplied withdrawals, fresh, in Mgal/d",
  "Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d",
  # "Irrigation, Total consumptive use, fresh, in Mgal/d",
  "Hydroelectric Power offstream surface-water withdrawals, fresh, in Mgal/d"
  # I believe these are included under Total Thermoelectric Power, TODO verify that:
  #   Fossil-fuel Thermoelectric Power total self-supplied withdrawals, fresh, in Mgal/d
  #   Fossil-fuel Thermoelectric Power deliveries from public supply, in Mgal/d
  #   Fossil-fuel Thermoelectric Power consumptive use, fresh, in Mgal/d
  #   Nuclear Thermoelectric Power total self-supplied withdrawals, fresh, in Mgal/d
  #
  # Commercial, hydroelectric power, and wastewater treatment were not
  # estimated since 2000, so they are excluded from this analysis.
  # 
  # I excluded consumptive water use columns, because I believe these are
  # included in other columns
  #
  # I did not consider hydroelectric in-stream use because the water was not
  # withdrawn.
  #
  # TODO consider evaluating these columns:
  #   Total Population total population of area, in thousands 
  #   Irrigation, Total surface irrigation, in thousand acres 
  #   Irrigation, Total total irrigation, in thousand acres 
)

cols = gsub('[^A-z]', '.', cols)  # make cols labels match acutal col names

# Convert "-" --> NA; "number" --> number
df = cproc(df, cols, as.numeric)
df[["population"]] = as.numeric(
  df[["Total.Population.total.population.of.area..in.thousands"]])*1000

# Sum annual freshwater withdrawals
df[["withdrawalSum"]] = df |>
  select(all_of(c(cols))) |>
  rowSums(na.rm=T)
df$withdrawalSum = df$withdrawalSum*1000000  # convert column units to gal/day


# Statistical analysis
cor.test(df$withdrawalSum, df$population)

model = lm(withdrawalSum~population, data = df)  # linear regression
summary(model)

# Look for trends
mk.test(df$population)
mk.test(df$withdrawalSum)


# Plot - Visually inspect relationship between population and water use
plot = 
  plot_ly(
    df,
    x = ~year,
    y = ~withdrawalSum,
    type = "scatter", mode = "lines",
    name = "Water Withdrawals") |>

    add_trace(
      x = ~year,
      y = ~population,
      mode = "lines", yaxis = "y2",
      name = "Population") |>

    layout(
      font = list(size=18),
      xaxis = list(
        title = "Year"
      ),
      yaxis = list(
        title = "Water Withdrawals (gal/day)"
      ),
      yaxis2 = list(
        title = "Population",
        overlaying = "y",
        side = "right"
      ),
      width = 1200,
      legend = list(x = 1.2)  # fix legend position to prevent y2-axis overlap
    )

saveWidget(plot, file = "./tmp_plot.html")  # save plot
system(paste("firefox", "./tmp_plot.html"))  # show plot

plot = 
  plot_ly(df) |>

    add_trace(
      x = ~population,
      y = ~withdrawalSum
    ) |>

    add_lines(
      x = ~population,
      y = fitted(model),
      line = list(dash="solid", width = 1.1, color="red")
    ) |>

    layout(
      font = list(size=24),
      showlegend = F,
      xaxis = list(
        title = "Population"
      ),
      yaxis = list(
        title = "Water Withdrawals (gal/day)"
      )
    )

saveWidget(plot, file = "./tmp_plot2.html")  # save plot
system(paste("firefox", "./tmp_plot2.html"))  # show plot

