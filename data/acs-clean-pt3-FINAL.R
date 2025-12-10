library(tidyverse)

a = all_data_renamed |>
  rename_all(tolower) |>
  mutate(across(where(is.character), tolower)) |>
  mutate(year2 = substr(year, 6, 9),
         year = as.numeric(year2)) |>
  select(-c(year2))



keywords <- c(
  "occupants_per_room",
  "built_1939_or_earlier",
  "median_gross_rent",
  "va_health_care",
  "medicare",
  "medicaid",
  "other_public_only",
  "no_health_insurance",
  "with_a_disability"
)

a_subset <- a |> select(gisjoin, year, state, county, statea, countya, matches(paste(keywords, collapse = "|")))
# Convert only year-prefixed columns to character to avoid pivot_longer error
a_fixed <- a_subset |>
  mutate(across(matches("^\\d{5}_"), as.character))

# Now pivot_longer
a_long <- a_fixed %>%
  pivot_longer(
    cols = matches("^\\d{5}_"),
    names_to = "varname",
    values_to = "value"
  ) %>%
  mutate(
    year.new = as.numeric(str_sub(varname, 1, 4))#,
    #var_clean = str_sub(varname, 7)
  ) %>%
  select(gisjoin, year, state, county, statea, countya, year.new, varname, value)


a_long2 <- a_long %>%
 filter(!is.na(value)) %>%
  filter(str_detect(varname,
                    "1_01|1_51|2_01|1939|disability|65_to_74_years|median_gross_rent|medicare|medicaid|tricare|va_health|public_only|no_health"
  )) %>%
  filter(!str_detect(varname, "_5_|_18_|_19_|_employer_|_direct_|_no_disability"))


# Step 1: Clean var_clean (remove first 4 characters)


a_cleaned = a_long2 |>
  mutate(varname = str_sub(varname, 11),
         varname = str_remove(varname, "^_")
  )
# Step 2: Pivot wider
a_wide <- a_cleaned %>%
  pivot_wider(
    id_cols = c(gisjoin, year, state, county, statea, countya, year.new),
    names_from = varname,
    values_from = value) 

a_wide = a_wide |>
  select(-c("female_75_years_and_over_with_a_disability", "male_75_years_and_over_with_a_disability"))

a_wide = a_wide |> filter(!is.na(median_gross_rent))


write.csv(a_wide, "~/Downloads/thesis/data/clean/final/acs-clean-pt1.csv", row.names = FALSE)

a_wide = read.csv("~/Downloads/thesis/data/clean/final/acs-clean-pt1.csv")

a_wide = a_wide |>
  rename("countyfips" =  "countya",
         "statefips" = "statea")
a2 = a2 |>
  rename("countyfips" = "fips_county_code",
         "statefips" = "fips_state_code")
a2$countyfips = as.numeric(a2$countyfips)
a2$statefips = as.numeric(a2$statefips)
a2$year = as.numeric(a2$year)
a3 = left_join(a_wide, a2, by = c("countyfips", "statefips", "year"))


a3 <- a3 |>
  mutate(
    # Calculate no_overcrowded (sum of overcrowded housing metrics)
    no_overcrowded = rowSums(across(c(
      "owner_occupied_1_01_to_1_50_occupants_per_room",
      "owner_occupied_1_51_to_2_00_occupants_per_room",
      "owner_occupied_2_01_or_more_occupants_per_room",
      "renter_occupied_1_01_to_1_50_occupants_per_room",
      "renter_occupied_1_51_to_2_00_occupants_per_room",
      "renter_occupied_2_01_or_more_occupants_per_room"
    ), ~as.numeric(.x)), na.rm = TRUE),
    
    # Calculate no_disability (sum of disability metrics)
    no_disability = rowSums(across(c(
      "male_35_to_64_years_with_a_disability",
      "female_35_to_64_years_with_a_disability",
      "female_65_to_74_years_with_a_disability",
      "male_65_to_74_years_with_a_disability",
    ), ~as.numeric(.x)), na.rm = TRUE),
    
    # Calculate no_healthins_none (sum of no health insurance metrics)
    no_healthins_none = rowSums(across(c(
      "X35_to_64_years_no_health_insurance_coverage",
      "X65_years_and_over_no_health_insurance_coverage"
    ), ~as.numeric(.x)), na.rm = TRUE),
    
    # Calculate no_healthins_public_only (sum of public-only health insurance metrics)
    no_healthins_public_only = rowSums(across(c(
      "X35_to_64_years_with_one_type_of_health_insurance_coverage_with_medicare_coverage_only",
      "X35_to_64_years_with_one_type_of_health_insurance_coverage_with_medicaid_means_tested_public_coverage_only",
      "X35_to_64_years_with_one_type_of_health_insurance_coverage_with_va_health_care_only",
      "X35_to_64_years_with_two_or_more_types_of_health_insurance_coverage_other_public_only_combinations",
      "X35_to_64_years_with_two_or_more_types_of_health_insurance_coverage_with_medicare_and_medicaid_means_tested_public_coverage",
      "X65_years_and_over_with_one_type_of_health_insurance_coverage_with_medicare_coverage_only",
      "X65_years_and_over_with_one_type_of_health_insurance_coverage_with_va_health_care_only",
      "X65_years_and_over_with_two_or_more_types_of_health_insurance_coverage_with_medicare_and_medicaid_means_tested_public_coverage",
      "X65_years_and_over_with_two_or_more_types_of_health_insurance_coverage_other_public_only_combinations"
    ), ~as.numeric(.x)), na.rm = TRUE)
  ) |>
  select(
    -c(
      # Variables used for no_overcrowded
      "owner_occupied_1_01_to_1_50_occupants_per_room",
      "owner_occupied_1_51_to_2_00_occupants_per_room",
      "owner_occupied_2_01_or_more_occupants_per_room",
      "renter_occupied_1_01_to_1_50_occupants_per_room",
      "renter_occupied_1_51_to_2_00_occupants_per_room",
      "renter_occupied_2_01_or_more_occupants_per_room",
      
      # Variables used for no_disability
      "male_35_to_64_years_with_a_disability",
      "female_35_to_64_years_with_a_disability",
      "female_65_to_74_years_with_a_disability",
      "male_65_to_74_years_with_a_disability",
      
      # Variables used for no_healthins_none
      "X35_to_64_years_no_health_insurance_coverage",
      "X65_years_and_over_no_health_insurance_coverage",
      
      # Variables used for no_healthins_public_only
      "X35_to_64_years_with_one_type_of_health_insurance_coverage_with_medicare_coverage_only",
      "X35_to_64_years_with_one_type_of_health_insurance_coverage_with_medicaid_means_tested_public_coverage_only",
      "X35_to_64_years_with_one_type_of_health_insurance_coverage_with_va_health_care_only",
      "X35_to_64_years_with_two_or_more_types_of_health_insurance_coverage_with_medicare_and_medicaid_means_tested_public_coverage",
      "X35_to_64_years_with_two_or_more_types_of_health_insurance_coverage_other_public_only_combinations",
      "X65_years_and_over_with_one_type_of_health_insurance_coverage_with_medicare_coverage_only",
      "X65_years_and_over_with_one_type_of_health_insurance_coverage_with_va_health_care_only",
      "X65_years_and_over_with_two_or_more_types_of_health_insurance_coverage_with_medicare_and_medicaid_means_tested_public_coverage",
      "X65_years_and_over_with_two_or_more_types_of_health_insurance_coverage_other_public_only_combinations"
    )
  )

a3 = a3 |> select(-c(year.new))

write.csv(a3, "~/Downloads/thesis/data/clean/final/covariates/acs-clean-almost-final.csv", row.names = FALSE)
acs = read.csv("~/Downloads/thesis/data/clean/final/covariates/acs-clean-FINAL.csv")

acs <- acs |>
  mutate(
    median_gross_rent = na_if(median_gross_rent, "N/A"),
    median_gross_rent = na_if(median_gross_rent, "."),
    median_gross_rent = as.numeric(median_gross_rent)
  )

acs <- acs |>
  group_by(coc.number, year) |>
  summarize(
    total_population = sum(total_persons, na.rm = TRUE),
    total_vacant = sum(no_vacant, na.rm = TRUE),
    total_housing_units = sum(housing_units_total, na.rm = TRUE),
    total_in_poverty = sum(no_poverty, na.rm = TRUE),
    total_white = sum(no_white, na.rm = TRUE),
    total_black = sum(no_black, na.rm = TRUE),
    total_native = sum(no_native, na.rm = TRUE),
    total_asian = sum(no_asian, na.rm = TRUE),
    total_hispanic = sum(no_hispanic, na.rm = TRUE),
    total_unemployed = sum(no_unemployed, na.rm = TRUE),
    total_overcrowded = sum(no_overcrowded, na.rm = TRUE),
    total_disabled = sum(no_disability, na.rm = TRUE),
    total_no_healthins = sum(no_healthins_none, na.rm = TRUE),
    total_public_healthins = sum(no_healthins_public_only, na.rm = TRUE),
    total_cnip = sum(no_cnip, na.rm = TRUE),
    total_built_pre_1940 = sum(built_1939_or_earlier, na.rm = TRUE),
    median_gross_rent = mean(as.numeric(median_gross_rent), na.rm = TRUE)  # average CoC-level median
  )

write.csv(acs, "~/Downloads/thesis/data/clean/final/covariates/acs-clean-counts-at-coc-level.csv", row.names = FALSE)


