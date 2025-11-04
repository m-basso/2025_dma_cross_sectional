#General cleaning functions 

# Function to clean character columns (tolower, replace "not recorded" with NA)----------------

tidyCh_col <- function(x) {
  x <- iconv(x, to = "UTF-8", sub = "")
  x <- tolower(x)
  x <- gsub("not recorded", NA, x)
  x <- trimws(x)
  return(x)
}

# Function to apply tidyCh_col to the entire dataframe ----------------------------------------

tidyCh_df <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- tidyCh_col(df[[col]])
    }
  }
  return(df)
}

# Function to remove columns by name ----------------------------------------------------------

remove_columns <- function(df, cols_to_remove) {
  df <- df[, !(names(df) %in% cols_to_remove)]
  return(df)
}

# Function to make columns numeric -----------------------------------------------------------

make_numeric <- function(df, cols_num) {
  df[cols_num] <- lapply(df[cols_num], function(x) {
    # Convert to numeric, suppress warnings for NA introduction
    suppressWarnings(as.numeric(x))
  })
  return(df)
}

# Function to factorize specific columns ------------------------------------------------------

factorize_columns <- function(df, cols_fact) {
  df[cols_fact] <- lapply(df[cols_fact], as.factor)
  return(df)
}

# Function to convert from ug to mg -----------------------------------------------------------

convert_ug_to_mg <- function(data, columns_to_convert) {
  data[, columns_to_convert] <- apply(data[, columns_to_convert], 2, function(x) x / 1000)
  return(data)   #without this I also need to select columns before the <- 
                 #with this no need to specify, the function will return all my dataset with other non selected columns
}

# Function to add _g and _mg to column names --------------------------------------------------
add_suffix <- function(data, columns_to_modify, suffix) {
  # Check which columns are being modified
  matched_columns <- colnames(data)[colnames(data) %in% columns_to_modify]
  print(paste("Modifying columns:", paste(matched_columns, collapse = ", ")))
  
  # Apply the suffix to matched columns
  colnames(data)[colnames(data) %in% columns_to_modify] <- paste0(matched_columns, "_", suffix)
  return(data)
}


