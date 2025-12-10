library(dplyr)
library(tidyr)
library(knitr)
library(stringr) # Added for str_detect function

# Function to add significance stars based on p-values
add_stars <- function(p_value) {
  case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**", 
    p_value < 0.1 ~ "*",
    TRUE ~ ""
  )
}

# Function to perform t-test and return formatted results
perform_ttest <- function(data, variable, group_var = "d.esg.extra") {
  # Split data by treatment group
  control_data <- data[data[[group_var]] == 0, variable]
  treatment_data <- data[data[[group_var]] == 1, variable]
  
  # Remove NAs
  control_data <- control_data[!is.na(control_data)]
  treatment_data <- treatment_data[!is.na(treatment_data)]
  
  # Perform t-test if we have data in both groups
  if(length(control_data) > 1 && length(treatment_data) > 1) {
    t_result <- t.test(treatment_data, control_data)
    p_value <- t_result$p.value
    stars <- add_stars(p_value)
    
    # Calculate standard error of the difference
    # SE = (upper_CI - lower_CI) / (2 * t_critical)
    # For 95% CI, t_critical ≈ 1.96, but we use the actual t-statistic for precision
    diff_means <- t_result$estimate[1] - t_result$estimate[2]  # Treatment - Control
    t_stat <- t_result$statistic
    se_diff <- abs(diff_means / t_stat)
    
    return(list(p_value = p_value, stars = stars, se_diff = se_diff, diff_means = diff_means))
  } else {
    return(list(p_value = NA, stars = "", se_diff = NA, diff_means = NA))
  }
}

# --- Load data ---
acs = read.csv("~/Downloads/stats209/data/clean/final/acs-FINAL-male.csv")
cocs = read.csv("/Users/viyan/Downloads/stats209/output/tables/common_cocs_baseline.csv") |>
  mutate(d.esg.extra = ifelse(combined.loss.abs > 100000, 1, 0))
esg <- read.csv("/Users/viyan/Downloads/stats209/data/clean/final/esg-awards-coc-by-year-FINAL.csv")
pcepi <- read.csv("/Users/viyan/Downloads/stats209/data/raw/pcepi/2012-2024-pcepi.csv")
coc = read.csv("/Users/viyan/Downloads/stats209/data/clean/coc-grants-clean-v2.csv") |>
  mutate(across(where(is.character), tolower))
pit = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/FINAL-pit.csv")
mort = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/mortality-FINAL-with-merged-cocs.csv")

# --- Adjust ESG dollars for inflation ---
base.pcepi <- pcepi$pcepi[pcepi$year == 2024]
esg$esg.total.real <- esg$esg.total.nominal * base.pcepi / pcepi$pcepi[match(esg$year, pcepi$year)]

acs2 = left_join(acs, pit, by = c("coc.number.new", "year"))
acs3 = left_join(acs2, mort, by = c("coc.number.new", "year"))

# --- Merge ACS and treatment indicator ---
sample <- left_join(acs3, cocs, by = "coc.number.new") |>
  filter(!is.na(d.esg.extra))

# --- Calculate new metrics ---
sample <- sample %>%
  mutate(
    homeless_per_10k = (ov.homeless / total_persons) * 10000
  )

# --- Step 1: CoC-level averages ---
panel_a_coc_means <- sample %>%
  group_by(coc.number.new) %>%
  summarize(
    d.esg.extra = first(d.esg.extra),  # keep treatment assignment
    population = mean(total_persons),
    share_male = mean(no_male /total_persons),
    share_poverty = mean(no_poverty / total_persons),
    share_white = mean(no_white / total_persons),
    share_black = mean(no_black / total_persons),
    share_hispanic = mean(no_hispanic / total_persons),
    share_unemployed = mean(no_unemployed / no_cnip),
    share_disability = mean(no_disability / total_persons), 
    share_overcrowded = mean(no_overcrowded / housing_units_total),
    share_vacant = mean(no_vacant / housing_units_total),
    share_pre1940 = mean(built_1939_or_earlier / housing_units_total),
    .groups = "drop"
  )

# --- Step 2: Treatment vs Control summary with t-tests ---
# Count number of observations in each category
obs_counts <- panel_a_coc_means %>%
  group_by(d.esg.extra) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = d.esg.extra,
    values_from = count,
    names_prefix = "n_"
  )

print("Number of observations in each group:")
print(obs_counts)

# Calculate means and SDs
panel_a <- panel_a_coc_means %>%
  pivot_longer(cols = -c(coc.number.new, d.esg.extra), names_to = "Variable") %>%
  group_by(d.esg.extra, Variable) %>%
  summarize(
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = d.esg.extra,
    values_from = c(Mean, SD),
    names_glue = "{.value}_{d.esg.extra}"
  )

# Rename for clarity
colnames(panel_a) <- c("Variable", "Mean_Control", "Mean_Treatment", "SD_Control", "SD_Treatment")

# Add t-tests for Panel A
panel_a_ttests <- data.frame(
  Variable = panel_a$Variable,
  p_value = NA,
  stars = "",
  se_diff = NA,
  diff_means = NA
)

# Perform t-tests for each variable in Panel A
variables_to_test <- c("population", "share_male", "share_poverty", "share_white", 
                       "share_black", "share_hispanic", "share_unemployed", 
                       "share_disability", "share_overcrowded", "share_vacant", 
                       "share_pre1940")

for(i in 1:nrow(panel_a_ttests)) {
  var_name <- panel_a_ttests$Variable[i]
  if(var_name %in% variables_to_test) {
    ttest_result <- perform_ttest(panel_a_coc_means, var_name)
    panel_a_ttests$p_value[i] <- ttest_result$p_value
    panel_a_ttests$stars[i] <- ttest_result$stars
    panel_a_ttests$se_diff[i] <- ttest_result$se_diff
    panel_a_ttests$diff_means[i] <- ttest_result$diff_means
  }
}

# Add t-test results to Panel A
panel_a$p_value <- panel_a_ttests$p_value
panel_a$stars <- panel_a_ttests$stars
panel_a$se_diff <- panel_a_ttests$se_diff
panel_a$diff_means <- panel_a_ttests$diff_means

# --- Panel B: ESG totals with t-tests ---
# --- ESG Funding Aggregation (mean and sd) ---
esg_data_for_ttest <- sample %>%
  left_join(esg, by = c("coc.number.new" = "coc.number", "year")) %>%
  filter(year >= 2013 & year <= 2019) %>%
  select(coc.number.new, d.esg.extra, year, esg.total.real) %>%
  distinct()

# Create period-specific datasets for t-tests
esg_2013_2016 <- esg_data_for_ttest %>%
  filter(year >= 2013 & year <= 2016) %>%
  group_by(coc.number.new, d.esg.extra) %>%
  summarize(mean_esg = mean(esg.total.real, na.rm = TRUE), .groups = "drop")

esg_2018_2019 <- esg_data_for_ttest %>%
  filter(year >= 2018 & year <= 2019) %>%
  group_by(coc.number.new, d.esg.extra) %>%
  summarize(mean_esg = mean(esg.total.real, na.rm = TRUE), .groups = "drop")

esg_binned <- sample %>%
  left_join(esg, by = c("coc.number.new" = "coc.number", "year")) %>%
  filter(year >= 2013 & year <= 2019) %>%
  mutate(
    period = case_when(
      year >= 2013 & year <= 2016 ~ "ESG_2013_2016",
      year >= 2018 & year <= 2019 ~ "ESG_2018_2019",
      TRUE ~ paste0("NA")
    )
  ) %>%
  group_by(d.esg.extra, period) %>%
  summarize(
    Mean = mean(esg.total.real, na.rm = TRUE),
    SD = sd(esg.total.real, na.rm = TRUE),
    .groups = "drop"
  )

# Create separate rows for Mean and SD
esg_mean <- esg_binned %>%
  select(d.esg.extra, period, Mean) %>%
  mutate(Variable = paste0(period, "_Mean")) %>%
  select(-period) %>%
  pivot_wider(names_from = d.esg.extra, values_from = Mean, names_prefix = "Mean_") %>%
  rename(Mean_Control = Mean_0, Mean_Treatment = Mean_1) %>%
  mutate(SD_Control = NA, SD_Treatment = NA)

esg_sd <- esg_binned %>%
  select(d.esg.extra, period, SD) %>%
  mutate(Variable = paste0(period, "_SD")) %>%
  select(-period) %>%
  pivot_wider(names_from = d.esg.extra, values_from = SD, names_prefix = "SD_") %>%
  rename(SD_Control = SD_0, SD_Treatment = SD_1) %>%
  mutate(Mean_Control = NA, Mean_Treatment = NA) %>%
  select(Variable, Mean_Control, Mean_Treatment, SD_Control, SD_Treatment)

# Add t-tests for ESG periods
esg_ttest_2013_2016 <- perform_ttest(esg_2013_2016, "mean_esg")
esg_ttest_2018_2019 <- perform_ttest(esg_2018_2019, "mean_esg")

# Initialize p_value and stars columns for esg_mean
esg_mean$p_value <- NA
esg_mean$stars <- ""
esg_mean$se_diff <- NA
esg_mean$diff_means <- NA

# Match t-test results to correct rows
for(i in 1:nrow(esg_mean)) {
  if(grepl("ESG_2013_2016_Mean", esg_mean$Variable[i])) {
    esg_mean$p_value[i] <- esg_ttest_2013_2016$p_value
    esg_mean$stars[i] <- esg_ttest_2013_2016$stars
    esg_mean$se_diff[i] <- esg_ttest_2013_2016$se_diff
    esg_mean$diff_means[i] <- esg_ttest_2013_2016$diff_means
  } else if(grepl("ESG_2018_2019_Mean", esg_mean$Variable[i])) {
    esg_mean$p_value[i] <- esg_ttest_2018_2019$p_value
    esg_mean$stars[i] <- esg_ttest_2018_2019$stars
    esg_mean$se_diff[i] <- esg_ttest_2018_2019$se_diff
    esg_mean$diff_means[i] <- esg_ttest_2018_2019$diff_means
  }
}

esg_sd$p_value <- NA
esg_sd$stars <- ""
esg_sd$se_diff <- NA
esg_sd$diff_means <- NA

# --- CoC Funding Aggregation (mean and sd) ---
coc_data_for_ttest <- sample %>%
  select(coc.number.new, d.esg.extra) %>%
  distinct() %>%
  left_join(coc, by = c("coc.number.new" = "coc.number")) %>%
  filter(year >= 2013 & year <= 2019)

# Create period-specific datasets for t-tests
coc_2013_2016 <- coc_data_for_ttest %>%
  filter(year >= 2013 & year <= 2016) %>%
  group_by(coc.number.new, d.esg.extra) %>%
  summarize(mean_coc = mean(funding.coc.2024.dollars, na.rm = TRUE), .groups = "drop")

coc_2018_2019 <- coc_data_for_ttest %>%
  filter(year >= 2018 & year <= 2019) %>%
  group_by(coc.number.new, d.esg.extra) %>%
  summarize(mean_coc = mean(funding.coc.2024.dollars, na.rm = TRUE), .groups = "drop")

coc_binned <- sample %>%
  select(coc.number.new, d.esg.extra) %>%
  distinct() %>%
  left_join(coc, by = c("coc.number.new" = "coc.number")) %>%
  filter(year >= 2013 & year <= 2019) %>%
  mutate(
    period = case_when(
      year >= 2013 & year <= 2016 ~ "CoC_2013_2016",
      year >= 2018 & year <= 2019 ~ "CoC_2018_2019",
      TRUE ~ paste0("NA")
    )
  ) %>%
  group_by(d.esg.extra, period) %>%
  summarize(
    Mean = mean(funding.coc.2024.dollars, na.rm = TRUE),
    SD = sd(funding.coc.2024.dollars, na.rm = TRUE),
    .groups = "drop"
  )

# Create separate rows for Mean and SD for CoC
coc_mean <- coc_binned %>%
  select(d.esg.extra, period, Mean) %>%
  mutate(Variable = paste0(period, "_Mean")) %>%
  select(-period) %>%
  pivot_wider(names_from = d.esg.extra, values_from = Mean, names_prefix = "Mean_") %>%
  rename(Mean_Control = Mean_0, Mean_Treatment = Mean_1) %>%
  mutate(SD_Control = NA, SD_Treatment = NA)

coc_sd <- coc_binned %>%
  select(d.esg.extra, period, SD) %>%
  mutate(Variable = paste0(period, "_SD")) %>%
  select(-period) %>%
  pivot_wider(names_from = d.esg.extra, values_from = SD, names_prefix = "SD_") %>%
  rename(SD_Control = SD_0, SD_Treatment = SD_1) %>%
  mutate(Mean_Control = NA, Mean_Treatment = NA) %>%
  select(Variable, Mean_Control, Mean_Treatment, SD_Control, SD_Treatment)

# Add t-tests for CoC periods
coc_ttest_2013_2016 <- perform_ttest(coc_2013_2016, "mean_coc")
coc_ttest_2018_2019 <- perform_ttest(coc_2018_2019, "mean_coc")

# Initialize p_value and stars columns for coc_mean
coc_mean$p_value <- NA
coc_mean$stars <- ""
coc_mean$se_diff <- NA
coc_mean$diff_means <- NA

# Match t-test results to correct rows
for(i in 1:nrow(coc_mean)) {
  if(grepl("CoC_2013_2016_Mean", coc_mean$Variable[i])) {
    coc_mean$p_value[i] <- coc_ttest_2013_2016$p_value
    coc_mean$stars[i] <- coc_ttest_2013_2016$stars
    coc_mean$se_diff[i] <- coc_ttest_2013_2016$se_diff
    coc_mean$diff_means[i] <- coc_ttest_2013_2016$diff_means
  } else if(grepl("CoC_2018_2019_Mean", coc_mean$Variable[i])) {
    coc_mean$p_value[i] <- coc_ttest_2018_2019$p_value
    coc_mean$stars[i] <- coc_ttest_2018_2019$stars
    coc_mean$se_diff[i] <- coc_ttest_2018_2019$se_diff
    coc_mean$diff_means[i] <- coc_ttest_2018_2019$diff_means
  }
}

coc_sd$p_value <- NA
coc_sd$stars <- ""
coc_sd$se_diff <- NA
coc_sd$diff_means <- NA

# --- Combine ESG and CoC Funding ---
panel_b <- bind_rows(esg_mean, esg_sd, coc_mean, coc_sd) %>%
  select(Variable, Mean_Control, Mean_Treatment, SD_Control, SD_Treatment, p_value, stars, se_diff, diff_means)

# --- Create a row for sample sizes to add to Panel A ---
sample_size_row <- data.frame(
  Variable = "Number of CoCs",
  Mean_Control = obs_counts$n_0,
  Mean_Treatment = obs_counts$n_1,
  SD_Control = NA,
  SD_Treatment = NA,
  p_value = NA,
  stars = "",
  se_diff = NA,
  diff_means = NA
)

# --- Panel C: Outcome variables in 2016 with t-tests ---
panel_c_data_for_ttest <- sample %>%
  filter(year == 2016) %>%
  group_by(coc.number.new) %>%
  summarize(
    d.esg.extra = first(d.esg.extra),
    homeless_per_10k_2016 = mean(homeless_per_10k, na.rm = TRUE),
    .groups = "drop"
  )

panel_c_baseline <- panel_c_data_for_ttest %>%
  pivot_longer(cols = -c(coc.number.new, d.esg.extra), names_to = "Variable") %>%
  group_by(d.esg.extra, Variable) %>%
  summarize(
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = d.esg.extra,
    values_from = c(Mean, SD),
    names_glue = "{.value}_{d.esg.extra}"
  )

# Rename columns for clarity
colnames(panel_c_baseline) <- c("Variable", "Mean_Control", "Mean_Treatment", "SD_Control", "SD_Treatment")

# Add t-test for Panel C
homeless_ttest <- perform_ttest(panel_c_data_for_ttest, "homeless_per_10k_2016")
panel_c_baseline$p_value <- homeless_ttest$p_value
panel_c_baseline$stars <- homeless_ttest$stars
panel_c_baseline$se_diff <- homeless_ttest$se_diff
panel_c_baseline$diff_means <- homeless_ttest$diff_means
panel_c_baseline$Panel <- "Panel C. Outcome variables in 2016"

# --- Combine panels with section headers ---
panel_a <- bind_rows(sample_size_row, panel_a)
panel_a$Panel <- "Panel A. CoC-by-year covariates"
panel_b$Panel <- "Panel B. ESG allocations (real 2024 dollars)"

combined_table <- bind_rows(panel_a, panel_b, panel_c_baseline) %>%
  relocate(Panel, Variable, Mean_Control, Mean_Treatment, SD_Control, SD_Treatment, diff_means, se_diff, p_value, stars) |>
  filter(!str_detect(Variable, "NA"))

# Print the table
print(combined_table)

# Optionally save the table
write.csv(combined_table, "~/Downloads/stats209/output/tables/balance_table_baseline_with_ttests.csv", row.names = FALSE)
