
# Functions and variables used throughout this project
#######################################################


library(tidyverse)


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
