
# Import and subset PRISM precipitation data
#############################################


library(raster)
library(prism)
library(sf)
library(tigris)
library(terra)
library(tidyverse)

# Redefine `select` so it refers to dplyr::select and not the select imported
# by `terra` package
select = dplyr::select


# Variables
year_range = 1953:2017  # PRISM data years of interest

# The random number at the end is to reduce chances of path collision
tmp_cache_path = "/tmp/prism_precip_data_12152.RData"


# Define main function
retrieve_data_from_prism = function() {

  # Download PRISM data
  cat("Downloading PRISM data...\n")
  prism_set_dl_dir("/home/daniel/downloads/r_prism_data")
  get_prism_annual(type = 'ppt', years = year_range, keepZip = F)


  # Retrieve Colorado state boundary

  # Use first PRISM data file as CRS reference (all PRISM files downloaded 
  # above should have the same CRS)
  prism_crs = prism_archive_subset("ppt", "annual", years = year_range[1]) |>
    pd_stack() |>
    terra::crs()

  CO_boundary =
    tigris::counties(state = "Colorado", cb = TRUE) |>
    st_as_sf() |>
    st_transform(prism_crs)


  # Calculate Colorado's mean annual precipitation for given year range
  CO_mean_precip = year_range |> lapply(function(y) {
    # Get file ID for one year of PRISM data
    prism_ID = prism_archive_subset("ppt", "annual", years = y)

    # Use file ID to query spatial data
    prism_spatial = pd_stack(prism_ID)

    # Crop PRISM raster to Colorado boundary
    prism_cropped = terra::crop(prism_spatial, CO_boundary)

    # Convert data to tibble/table, extract values, calculate mean
    precip_values = prism_cropped |> 
      raster::rasterToPoints() |> as_tibble()
    precip_values = precip_values[[prism_ID]]

    return(c(year = y, precip_mm = mean(precip_values)))
  }) |> bind_rows()
  # Precip values are in millimeters (see "What units are the data values in?"
  # section at https://prism.oregonstate.edu/FAQ/)

  return(CO_mean_precip)
}


# Attempt to load data from cache
if (file.exists(tmp_cache_path)) {
  load(tmp_cache_path) 
} else {
  # If unable to retrieve data from cache, retrieve it from source
  df_precip_co = retrieve_data_from_prism()
}


# Save data to cache
save(df_precip_co, file = tmp_cache_path)

