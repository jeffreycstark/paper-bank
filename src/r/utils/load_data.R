#' Load data from project data directory
#' 
#' @param filename Character. Name of the file to load
#' @param data_type Character. Type of data: "raw", "interim", or "processed"
#' @return Data frame or tibble
#' @export
#' @examples
#' df <- load_data("survey_data.rds", "processed")
load_data <- function(filename, data_type = "processed") {
  
  require(here)
  require(tidyverse)
  
  # Construct file path
  data_path <- here::here("data", data_type, filename)
  
  # Check if file exists
  if (!file.exists(data_path)) {
    stop("Data file not found: ", data_path)
  }
  
  # Detect file type and load accordingly
  ext <- tools::file_ext(filename)
  
  data <- switch(
    tolower(ext),
    "csv" = readr::read_csv(data_path, show_col_types = FALSE),
    "tsv" = readr::read_tsv(data_path, show_col_types = FALSE),
    "rds" = readRDS(data_path),
    "rda" = load(data_path),
    "rdata" = load(data_path),
    "dta" = haven::read_dta(data_path),
    "sav" = haven::read_sav(data_path),
    "xlsx" = readxl::read_excel(data_path),
    "xls" = readxl::read_excel(data_path),
    stop("Unsupported file type: ", ext)
  )
  
  message("Loaded data from: ", data_path)
  return(data)
}

#' Save data to project data directory
#' 
#' @param data Data frame or tibble to save
#' @param filename Character. Name of the file to save
#' @param data_type Character. Type of data: "interim" or "processed"
#' @export
save_data <- function(data, filename, data_type = "processed") {
  
  require(here)
  require(tidyverse)
  
  # Construct file path
  data_path <- here::here("data", data_type, filename)
  
  # Create directory if it doesn't exist
  dir.create(dirname(data_path), recursive = TRUE, showWarnings = FALSE)
  
  # Detect file type and save accordingly
  ext <- tools::file_ext(filename)
  
  switch(
    tolower(ext),
    "csv" = readr::write_csv(data, data_path),
    "tsv" = readr::write_tsv(data, data_path),
    "rds" = saveRDS(data, data_path),
    "rdata" = save(data, file = data_path),
    stop("Unsupported file type: ", ext)
  )
  
  message("Data saved to: ", data_path)
  invisible(data_path)
}
