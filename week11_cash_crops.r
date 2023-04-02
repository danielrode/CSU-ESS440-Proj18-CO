

# README

# TODO regarding n<30, see
# https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module6-RandomError/PH717-Module6-RandomError11.html

# END


# TODO document https://www.bea.gov/cautionary-note-about-annual-gdp-state-discontinuity in final report
# TODO scrape figures for report https://www.usgs.gov/mission-areas/water-resources/science/trends-water-use
# TODO seperate out water use data import code into `wrangle-usgs.r`



# TODO, remove library imports for unused packages
library(stringr)
library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(dplyr)
library(mlbench)
library(ggcorrplot)
library(plotly)
library(htmlwidgets)
library(tibble)
library(trend)
library(tidyr)
library(zoo)
library(readxl)
library(tidyxl)


# Variables
HOME = Sys.glob('~')
dataDir = paste(HOME, "downloads/ess440/data", sep="/")
tmp_dir_path = system("mktemp -d", intern=T)


# Funcitons
transpose_df = function(df) {
  df = as_tibble(cbind(names_col = names(df), t(df)))
  
  # Set column names from first row
  names(df) = make.unique(unlist(df[1,]))
  df = df[-1,]  # delete first row (since it is now labels)

  return(df)
}

drop_empty = function(df) {
  # Drop rows and columns that only contain NA values
  df = df[,colSums(is.na(df)) != nrow(df)]  # drop columns
  df = df[rowSums(is.na(df)) != ncol(df),]  # drop rows

  return(df)
}

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


# Import and subset USGS water use data
cols = c(
  # TODO figure out why 1995 water use is off the most from https://labs.waterdata.usgs.gov/visualizations/water-use/index.html
  "Aquaculture total self-supplied withdrawals, fresh, in Mgal/d",
  "Domestic total self-supplied withdrawals, fresh, in Mgal/d",
  # "Hydroelectric Power offstream surface-water withdrawals, fresh, in Mgal/d",
  "Industrial total self-supplied withdrawals, fresh, in Mgal/d",
  "Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d",
  "Livestock (Animal Specialties) total self-supplied withdrawals, fresh, in Mgal/d",
  "Livestock (Stock) total self-supplied withdrawals, fresh, in Mgal/d",
  "Livestock total self-supplied withdrawals, fresh, in Mgal/d",
  "Mining total self-supplied withdrawals, fresh, in Mgal/d",
  "Public Supply deliveries to commercial, in Mgal/d",
  "Public Supply deliveries to domestic, in Mgal/d",
  "Public Supply deliveries to industrial, in Mgal/d",
  "Public Supply deliveries to thermoelectric, in Mgal/d",
  "Public Supply total self-supplied withdrawals, fresh, in Mgal/d",
  "Total Thermoelectric Power self-supplied groundwater withdrawals, fresh, in Mgal/d",
  "Total Thermoelectric Power self-supplied surface-water withdrawals, fresh, in Mgal/d"
)

path = paste(dataDir, "USGS Water Use Data for Colorado state-wide.csv", sep='/')
df_wtr = read.csv(path, sep = '\t', comment.char = "#", skip=2)

cols = gsub('[^A-z]', '.', cols)  # make col labels match acutal col names
df_wtr = df_wtr[-1,]  # drop '2s' row (what the hell is it for?)
df_wtr = cproc(df_wtr, cols, as.numeric)  # convert "-" --> NA; "number" --> number

# Convert year column from strings to numbers
df_wtr$year = as.numeric(df_wtr$year)

# Sum annual freshwater withdrawals
df_wtr[["withdrawalSum"]] = df_wtr |>
  select(all_of(c(cols))) |>
  rowSums(na.rm=T)
df_wtr$withdrawalSum = df_wtr$withdrawalSum*1000000  # convert column units to gal/day


# Import older USGS water use data (1950-1980)
cols = c(
  # For definitions, see original data readme ("metadata") files and
  # https://www.usgs.gov/mission-areas/water-resources/science/water-use-terminology
  # To see how categories changed over the years,
  # https://www.usgs.gov/mission-areas/water-resources/science/changes-water-use-categories
  #
  # TP = total popoulation
  # PS = public supply
  # DO = domestic
  # OI = other industrial
  # PT = total thermoelectric power
  # LS = livestock (stock)
  # IR = irrigation
  # HY = hydroelectric power
  # INPT = other industrial and thermoelectric
  # "DO-SSCUsFr;self-supplied consumptive use, fresh, in Mgal/d",
  "DO-WGWFr;self-supplied groundwater withdrawals, fresh, in Mgal/d",
  "DO-WSWFr;self-supplied surface-water withdrawals, fresh, in Mgal/d",
  # "HY-InUse;instream water use, in Mgal/d",
  # "INPT-RecWW;reclaimed wastewater, in Mgal/d",  # NOTE: Industrial and Thermoelectric were combined in 1955
  "INPT-WGWFr;self-supplied groundwater withdrawals, fresh, in Mgal/d",
  # "INPT-WGWSa;self-supplied groundwater withdrawals, saline, in Mgal/d",
  "INPT-WSWFr;self-supplied surface-water withdrawals, fresh, in Mgal/d",
  # "INPT-WSWSa;self-supplied surface-water withdrawals, saline, in Mgal/d",  # NOTE: Industrial and Thermoelectric were combined in 1950
  # "IR-CLoss;conveyance loss, in Mgal/d",
  # "IR-CUsFr;consumptive use, fresh, in Mgal/d",
  # "IR-DelFmTot;total water delivered to farms, in Mgal/d",
  # "IR-IrTot;total irrigation, in thousand acres",
  # "IR-RecWW;reclaimed wastewater, in Mgal/d",
  "IR-WFrTo; total self-supplied withdrawals, fresh, in Mgal/d  (Total Delivered to farms + Conveyance Losses) [see ReadMe tab]",
  "IR-WGWFr;self-supplied groundwater withdrawals, fresh, in Mgal/d",
  "IR-WSWFr;self-supplied surface-water withdrawals, fresh, in Mgal/d",
  # "LS-CUsFr;consumptive use, fresh, in Mgal/d",
  "LS-WGWFr;self-supplied groundwater withdrawals, fresh, in Mgal/d",
  "LS-WSWFr;self-supplied surface-water withdrawals, fresh, in Mgal/d",
  # "OI-RecWW;reclaimed wastewater, in Mgal/d",
  # "OI-SSCUTot;total consumptive use, in Mgal/d",
  # "OI-SSCUsFr;total consumptive use fresh, in Mgal/d",
  # "OI-SSCUsFr;total consumptive use, in Mgal/d",
  # "OI-SSCUsSa",
  # "OI-SSCUsSa;total consumptive use saline, in Mgal/d",
  "OI-WGWFr;self-supplied groundwater withdrawals, fresh, in Mgal/d",
  # "OI-WGWSa;self-supplied groundwater withdrawals, saline, in Mgal/d",
  "OI-WSWFr;self-supplied surface-water withdrawals, fresh, in Mgal/d",
  # "OI-WSWSa;self-supplied surface-water withdrawals, saline, in Mgal/d",
  # "PS-CUFr;consumptive use, fresh, in Mgal/d",
  "PS-DelCI;deliveries to commercial and other industrial, in Mgal/d",
  "PS-DelDP;deliveries to domestic and public use and losses, in Mgal/d",
  # "PS-GWPop;population served by groundwater, in thousands",
  # "PS-SWPop;population served by surface water, in thousands",
  "PS-WGWFr;self-supplied groundwater withdrawals, fresh, in Mgal/d",
  "PS-WSWFr;self-supplied surface-water withdrawals, fresh, in Mgal/d",
  # "PT-SSCUTot;total consumptive use, in Mgal/d",
  # "PT-SSCUsFr",  # Total Thermoelectric Power, consumptive use, fresh, in Mgal/d
  # "PT-SSCUsFr;self-supplied surface-water withdrawals, saline, in Mgal/d",
  # "PT-SSCUsSa",  # Total Thermoelectric Power, consumptive use, saline, in Mgal/d
  # "PT-SSCUsSa;total consumptive use, in Mgal/d",
  "PT-WGWFr;self-supplied groundwater withdrawals, fresh, in Mgal/d",
  # "PT-WGWSa;self-supplied groundwater withdrawals, saline, in Mgal/d",
  "PT-WSWFr;self-supplied surface-water withdrawals, fresh, in Mgal/d"
  # "PT-WSWSa;self-supplied surface-water withdrawals, saline, in Mgal/d",
  # "TP-TotPop;total population of area, in thousands",
)

dataDirUSGSOld = paste(
  dataDir,
  "Export.1950-1980-United-States-Compilation-metadata",
  sep = '/')

import_old_usgs_xlsx = function(path) {
  # Extract dataset year from file name
  year = strsplit(path, "/") |> 
    unlist() |>
    tail(n=1) |>
    strsplit("\\.") |>
    unlist()
  year = (
    strsplit(year[2], "-") |>
      unlist() |>
      as.numeric())[1] |> unlist()

  # Read in xlsx file, sheet by sheet
  cell_comments = tidyxl::xlsx_cells(path)
  sheet_names = excel_sheets(path)
  sheets = lapply(sheet_names, function(s) {
    x = read_excel(path, sheet = s, skip = 3, col_names = TRUE)

    # Replace cryptic column labels with descriptive names (from cell 
    # comments)
    current_col_labels = colnames(x)
    descriptive_col_labels = filter(cell_comments, row==4 & sheet==s)$comment

    # Use descritive column label (from execl comment/annotation), unless 
    # their isn't one, then use original column label
    updated_col_labels = ifelse(
      is.na(descriptive_col_labels), 
      current_col_labels, 
      paste(current_col_labels, descriptive_col_labels, sep=";"))
    updated_col_labels = trimws(updated_col_labels)
    updated_col_labels = gsub("\\.$", "", updated_col_labels)
    names(x) = updated_col_labels

    # Delete any note rows appended to end of sheet
    mark = which(startsWith(x$Area, "Notes: ")) - 1
    if (length(mark) != 0) {
      x = head(x, n = mark)
      mark = length(zoo::na.trim(x$Area))
      x = head(x, n = mark)
    }

    # Add column with state names (from state code cell comments)
    current_col_values = x$Area
    updated_col_values = filter(cell_comments, col==1 & sheet==s)$comment
    # Strip leading/trailing NA values from list
    updated_col_values = zoo::na.trim(updated_col_values)
    updated_col_values = trimws(updated_col_values)
    updated_col_values = gsub("\\.$", "", updated_col_values)
    x[["state"]] = updated_col_values

    return(x)
  })

  # Verify that all state name columns are identical (to be assured there was
  # not a state code to state name conversion mismatch)
  for (s in sheets) {
    if (! identical(s$state, sheets[[1]]$state)) {
      print("error: State code --> name conversion failed: mismatch found")
      stop()
    }
  }

  # Unpack sheets object and join all columns into one dataframe
  df = sheets[[1]]
  for (s in sheets[-1]) {
    df = inner_join(df, s, by = "Area")
  }

  # Add year column
  df[["year"]] = year

  return(df)
}

# Check for Rdata file
rdata_path = paste0(dataDirUSGSOld, "/cache.Rdata")
if (file.exists(rdata_path)) {
  # Import data from Rdata file
  load(rdata_path)
} else {
  # Import data from original files

  # Run import function on all old USGS data xlsx files
  path_list = Sys.glob(
    paste0(dataDirUSGSOld, "/Export.*-United-States-Compilation.xlsx"))
  df_list_old_usgs = lapply(path_list, import_old_usgs_xlsx)

  # Flatten nested dataframes into one
  df_wtr_old = df_list_old_usgs[[1]]
  for (i in 2:length(df_list_old_usgs)) {
    df_wtr_old = full_join(df_wtr_old, df_list_old_usgs[[i]])
  }

  # Sum annual freshwater withdrawals
  df_wtr_old[["withdrawalSum"]] = df_wtr_old |>
    select(all_of(c(cols))) |>
    rowSums(na.rm=T)
  # Convert column units to gal/day
  df_wtr_old$withdrawalSum = df_wtr_old$withdrawalSum*1000000

  # Save to Rdata file
  save(df_wtr_old, file = rdata_path)
}


# Subset and merge old and new USGS water use data
df_wtr_sum = bind_rows(
  filter(df_wtr_old, Area == "08") |>  # USGS code 08 = Colorado
    select(year, withdrawalSum),
  select(df_wtr, year, withdrawalSum))
df_wtr_sum = arrange(df_wtr_sum, year)  # sort by year, ascending


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
  print(a)
  print(b)

  correlation = cor.test(df[[a]], df[[b]])
  print(correlation)

  formula = as.formula(paste0('`', a, '`~`', b, '`'))
  model = lm(formula, data = df)  # linear regression
  print(summary(model))
}


# Look for trends
# mk.test(df$gdpCurrentDollars)
# mk.test(df$withdrawalSum)


# Plot - Visually inspect relationship between GDP and cash crops
plot = 
  plot_ly(df) |>  # , width = 1200) |>
    add_trace(
      x = ~year,
      y = ~gdpCurrentDollars,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      yaxis = "y2",
      name = "GDP") |>
    add_trace(
      x = ~year,
      y = ~`Value of agricultural sector production`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      yaxis = "y2",
      name = "Value of Ag Sector Prod.") |>
    add_trace(
      x = ~year,
      y = ~`Net farm income`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      yaxis = "y2",
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


# Plot - Visually inspect relationship between water use and cash crops
plot = 
  plot_ly(df) |>  # , width = 1200) |>
    add_trace(
      x = ~year,
      y = ~withdrawalSum,
      type = "bar",
      marker = list(color = "lightskyblue"),
      width = 3,
      # `name` sets legend label for this item
      name = "Water Withdrawals") |>
    add_trace(
      x = ~year,
      y = ~`Value of agricultural sector production`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      yaxis = "y2",
      name = "Value of Ag Sector Prod.") |>
    add_trace(
      x = ~year,
      y = ~`Net farm income`,
      type = "scatter", mode = "lines",
      line = list(width = 5),
      yaxis = "y2",
      name = "Net Farm Income") |>
    layout(
      font = list(size=18),
      xaxis = list(
        title = "Year"
      ),
      yaxis = list(
        title = "Water Withdrawals (gal/day)"
      ),
      yaxis2 = list(
        title = "$ (USD)",
        overlaying = "y",
        side = "right"
      ),
      legend = list(x = 1.2)  # fix legend position to prevent y2-axis overlap
    )

tmp_plot_path = paste0(tmp_dir_path, "/plotly_ccvswateruse_plot.html")
saveWidget(plot, file = tmp_plot_path)  # save plot
system(paste("firefox", tmp_plot_path))  # show plot


# plot =
#   plot_ly(df) |>
#     add_trace(
#       x = ~gdpCurrentDollars,
#       y = ~withdrawalSum
#     ) |>
#     add_lines(
#       x = ~gdpCurrentDollars,
#       y = fitted(model),
#       line = list(dash="solid", width = 1.1, color="red")
#     ) |>
#     layout(
#       font = list(size=24),
#       showlegend = F,
#       xaxis = list(
#         title = "GDP (USD)"
#       ),
#       yaxis = list(
#         title = "Water Withdrawals (gal/day)"
#       )
#     )

# saveWidget(plot, file = "./tmp_plot2.html")  # save plot
# system(paste("firefox", "./tmp_plot2.html"))  # show plot

