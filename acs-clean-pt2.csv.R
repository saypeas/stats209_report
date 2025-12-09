library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Path to your files
data_file <- "/Users/viyan/Downloads/thesis/data/raw/nhgis0002_csv/nhgis0002_ts_nominal_county.csv"
codebook_file <- "/Users/viyan/Downloads/thesis/data/raw/nhgis0002_csv/nhgis0002_ts_nominal_county_codebook.txt"
output_dir <- "/Users/viyan/Downloads/thesis/data/raw/nhgis0002_csv"

# 1. Function to extract variable descriptions with context
extract_var_descriptions <- function(path) {
  lines <- readLines(path)
  
  # Initialize variables to track context
  var_mapping <- c()
  current_table_name <- NULL
  current_nhgis_code <- NULL
  
  # Process each line
  for (i in 1:length(lines)) {
    line <- lines[i]
    
    # Look for table names (usually formatted as "1. Table Name")
    if (grepl("^[0-9]+\\. ", line)) {
      current_table_name <- str_match(line, "^[0-9]+\\. (.+)$")[1,2]
    }
    
    # Look for NHGIS codes
    nhgis_match <- str_match(line, "NHGIS code:\\s+([A-Z0-9]+)")
    if (!is.na(nhgis_match[1,1])) {
      current_nhgis_code <- nhgis_match[1,2]
    }
    
    # Extract variable codes and descriptions
    # Adjusted to match your specific time series format which may differ
    var_match <- str_match(line, "^\\s+([A-Z0-9]+):\\s+(.+)$")
    if (!is.na(var_match[1,1])) {
      var_code <- var_match[1,2]  # The variable code
      var_desc <- var_match[1,3]  # The description
      
      # Create a unique name using NHGIS code and description
      if (!is.null(current_nhgis_code)) {
        unique_desc <- paste0(current_nhgis_code, "_", var_desc)
        var_mapping[var_code] <- unique_desc
      } else {
        var_mapping[var_code] <- var_desc
      }
    }
  }
  
  # Print some info about the processing
  cat("Processed", path, "\n")
  cat("  Found", length(var_mapping), "variables\n")
  
  return(var_mapping)
}

# 2. Load and examine the data
df <- read_csv(data_file, col_types = cols(.default = col_guess()))
cat("Loaded data with", ncol(df), "columns and", nrow(df), "rows\n")

# 3. Get variable mapping from codebook
var_mapping <- extract_var_descriptions(codebook_file)

# 4. Create clean column names
clean_var_names <- sapply(var_mapping, function(desc) {
  # Replace problematic characters and make syntactically valid names
  clean <- gsub("[^a-zA-Z0-9_]", "_", desc)
  clean <- gsub("_+", "_", clean)    # Replace multiple underscores with a single one
  clean <- gsub("^_|_$", "", clean)  # Remove leading/trailing underscores
  return(clean)
})

# 5. Check for duplicates and make names unique
if (any(duplicated(clean_var_names))) {
  cat("Warning: There are duplicate names after cleaning. Making them unique...\n")
  clean_var_names <- make.unique(clean_var_names)
}

# 6. Identify ID variables to preserve (common ones in NHGIS data)
id_vars <- c("GISJOIN", "YEAR", "STATE", "COUNTY", "STATEA", "COUNTYA", "PLACEA", "PLACEA_NAME", 
             "TRACTA", "NAME", "DATANUM", "DATANUM_NAME")
id_vars <- id_vars[id_vars %in% names(df)]

# 7. Check which variables exist in our dataset
existing_vars <- intersect(names(clean_var_names), names(df))

# 8. Create a renamed dataframe
df_renamed <- df

if (length(existing_vars) > 0) {
  # Create a proper named vector for renaming
  rename_vector <- clean_var_names[existing_vars]
  
  # Now rename using the correct format
  df_renamed <- df %>%
    rename_with(~rename_vector[.x], .cols = existing_vars)
}

###
a = df_renamed |>
  rename_all(tolower) |>
  mutate(across(where(is.character), tolower)) |>
  mutate(year = substr(row_source_year, 6, 9)) |>
  # Remove columns containing "margin" in their names
  select(-contains("margin")) |>
  # Create total_persons by summing over variables beginning with "persons_" followed by a number and ending with years
  mutate(total_persons = rowSums(across(matches("^persons_[0-9].*years$"), ~as.numeric(.x)), na.rm = TRUE)) |>
  # Drop row_source_year
  select(-row_source_year) |>
  # Reorder columns to bring year to the front
  select(year, total_persons, everything())

a = a |> filter(!is.na(persons_16_years_and_over_not_in_labor_force))

a2 = a |>
  rename_all(tolower) |>
  mutate(across(where(is.character), tolower)) |>
  mutate(
    # Calculate counts instead of shares
    no_vacant = housing_units_vacant,
    no_poverty = persons_poverty_status_is_determined_income_below_poverty_level,
    no_white = persons_white_single_race,
    no_black = persons_black_or_african_american_single_race,
    no_native = persons_american_indian_and_alaska_native_single_race,
    no_asian = persons_asian_and_pacific_islander_and_other_race_single_race,
    no_hispanic = persons_hispanic_or_latino,
    no_unemployed = persons_16_years_and_over_in_labor_force_civilian_unemployed,
    no_cnip = persons_16_years_and_over_in_labor_force_civilian
  ) |>
  # Select only required variables
  select(
    year, 
    no_cnip,
    total_persons, 
    fips_county_code, 
    fips_state_code, 
    housing_units_total,
    no_vacant,
    no_poverty,
    no_white,
    no_black,
    no_native,
    no_asian,
    no_hispanic,
    no_unemployed
  )


write.csv(a2, "~/Downloads/thesis/data/clean/final/acs-clean-pt2.csv", row.names = FALSE)
