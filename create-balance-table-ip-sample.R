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

# Function to perform t-test and return formatted results including SE
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
    
    # Calculate standard error of the difference in means
    # SE = sqrt(s1^2/n1 + s2^2/n2)
    n1 <- length(treatment_data)
    n2 <- length(control_data)
    s1_sq <- var(treatment_data)
    s2_sq <- var(control_data)
    se_diff <- sqrt(s1_sq/n1 + s2_sq/n2)
    
    return(list(p_value = p_value, stars = stars, se_diff = se_diff))
  } else {
    return(list(p_value = NA, stars = "", se_diff = NA))
  }
}

# --- Load data ---
acs = read.csv("~/Downloads/stats209/data/clean/final/acs-FINAL-male.csv")
cocs = read.csv("/Users/viyan/Downloads/stats209/output/tables/sid_cocs_in_bandwidth.csv") |>
  mutate(d.esg.extra = ifelse(combined.loss.abs > 100000, 1, 0))
esg <- read.csv("/Users/viyan/Downloads/stats209/data/clean/final/esg-awards-coc-by-year-FINAL.csv")
pcepi <- read.csv("/Users/viyan/Downloads/stats209/data/raw/pcepi/2012-2024-pcepi.csv")
coc = read.csv("/Users/viyan/Downloads/stats209/data/clean/coc-grants-clean-v2.csv") |>
  mutate(across(where(is.character), tolower))
pit = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/FINAL-pit.csv")
mort = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/mortality-FINAL-with-merged-cocs.csv")  
sid = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/sid-FINAL-with-merged-cocs.csv") |>
  rename(year = fyear) |>
  filter(coc.number.new != "ny-519")

# --- Adjust ESG dollars for inflation ---
base.pcepi <- pcepi$pcepi[pcepi$year == 2024]
esg$esg.total.real <- esg$esg.total.nominal * base.pcepi / pcepi$pcepi[match(esg$year, pcepi$year)]

acs2 = left_join(acs, pit, by = c("coc.number.new", "year"))
acs3 = left_join(acs2, sid, by = c("coc.number.new", "year"))

# --- Merge ACS and treatment indicator ---
sample <- left_join(acs3, cocs, by = "coc.number.new") |>
  filter(!is.na(d.esg.extra))

# --- Calculate new metrics ---
sample <- sample %>%
  mutate(
    ip_per_10k_homeless = (health_ip+1 / ov.homeless) * 10000
  )

# --- Calculate change in ip_per_10k_homeless from 2015 to 2016 ---
# First create a dataset with just 2015 values
ip_2015 <- sample %>%
  filter(year == 2015) %>%
  select(coc.number.new, ip_per_10k_homeless) %>%
  rename(ip_per_10k_2015 = ip_per_10k_homeless)

# Then create a dataset with just 2016 values
ip_2016 <- sample %>%
  filter(year == 2016) %>%
  select(coc.number.new, ip_per_10k_homeless) %>%
  rename(ip_per_10k_2016 = ip_per_10k_homeless)

# Join them together (fixed variable names)
ip_change <- full_join(ip_2015, ip_2016, by = "coc.number.new") %>%
  mutate(
    ip_change_2015_2016 = (ip_per_10k_2016 - ip_per_10k_2015)/(ip_per_10k_2015)
  )

# Add this back to the main sample
sample <- left_join(sample,
                    select(ip_change, coc.number.new, ip_change_2015_2016),
                    by = "coc.number.new")

# --- Step 1: CoC-level averages ---
panel_a_coc_means <- sample %>%
  group_by(coc.number.new) %>%
  summarize(
    d.esg.extra = first(d.esg.extra),  # keep treatment assignment
    population = mean(total_persons),
    share_male = mean(no_male /total_persons),
    share_white = mean(no_white / total_persons),
    share_black = mean(no_black / total_persons),
    share_hispanic = mean(no_hispanic / total_persons),
    share_unemployed = mean(no_unemployed / no_cnip),
    share_disability = mean(no_disability / total_persons), 
    share_poverty = mean(no_poverty / total_persons),
    share_overcrowded = mean(no_overcrowded / housing_units_total),
    share_vacant = mean(no_vacant / housing_units_total),
    share_pre1940 = mean(built_1939_or_earlier / housing_units_total),
    share_healthins_none = mean(no_healthins_none / total_persons),
    share_healthins_public_only = mean(no_healthins_public_only / total_persons),
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

# Calculate means, SDs, and t-tests for Panel A
panel_a_vars <- c("population", "share_male", "share_white", "share_black",
                  "share_hispanic", "share_unemployed", "share_disability",
                  "share_poverty", "share_overcrowded", "share_vacant",
                  "share_pre1940", "share_healthins_none", "share_healthins_public_only")

panel_a_results <- data.frame()

for(var in panel_a_vars) {
  # Calculate means and SDs
  summary_stats <- panel_a_coc_means %>%
    group_by(d.esg.extra) %>%
    summarize(
      Mean = mean(.data[[var]], na.rm = TRUE),
      SD = sd(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Perform t-test
  ttest_result <- perform_ttest(panel_a_coc_means, var)
  
  # Create row for this variable
  var_row <- data.frame(
    Variable = var,
    Mean_Control = summary_stats$Mean[summary_stats$d.esg.extra == 0],
    Mean_Treatment = summary_stats$Mean[summary_stats$d.esg.extra == 1],
    SD_Control = summary_stats$SD[summary_stats$d.esg.extra == 0],
    SD_Treatment = summary_stats$SD[summary_stats$d.esg.extra == 1],
    P_Value = ttest_result$p_value,
    SE_Diff = ttest_result$se_diff,
    Stars = ttest_result$stars
  )
  
  panel_a_results <- bind_rows(panel_a_results, var_row)
}

# --- Panel B: ESG totals with t-tests ---
# --- ESG Funding Aggregation (mean and sd) ---
esg_data <- sample %>%
  left_join(esg, by = c("coc.number.new" = "coc.number", "year")) %>%
  filter(year >= 2013 & year <= 2019) %>%
  mutate(
    period = case_when(
      year >= 2013 & year <= 2016 ~ "ESG_2013_2016",
      year >= 2018 & year <= 2019 ~ "ESG_2018_2019",
      TRUE ~ paste0("NA")
    )
  ) %>%
  filter(period != "NA") %>%
  group_by(coc.number.new, d.esg.extra, period) %>%
  summarize(esg.total.real = mean(esg.total.real, na.rm = TRUE), .groups = "drop")

# Calculate stats and t-tests for ESG funding by period
panel_b_results <- data.frame()

for(period in c("ESG_2013_2016", "ESG_2018_2019")) {
  period_data <- esg_data %>% filter(period == !!period)
  
  # Calculate means and SDs
  summary_stats <- period_data %>%
    group_by(d.esg.extra) %>%
    summarize(
      Mean = mean(esg.total.real, na.rm = TRUE),
      SD = sd(esg.total.real, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Perform t-test
  ttest_result <- perform_ttest(period_data, "esg.total.real")
  
  # Create row
  var_row <- data.frame(
    Variable = paste0(period, "_Mean"),
    Mean_Control = summary_stats$Mean[summary_stats$d.esg.extra == 0],
    Mean_Treatment = summary_stats$Mean[summary_stats$d.esg.extra == 1],
    SD_Control = summary_stats$SD[summary_stats$d.esg.extra == 0],
    SD_Treatment = summary_stats$SD[summary_stats$d.esg.extra == 1],
    P_Value = ttest_result$p_value,
    SE_Diff = ttest_result$se_diff,
    Stars = ttest_result$stars
  )
  
  panel_b_results <- bind_rows(panel_b_results, var_row)
}

# --- CoC Funding Aggregation with t-tests ---
coc_data <- sample %>%
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
  filter(period != "NA") %>%
  group_by(coc.number.new, d.esg.extra, period) %>%
  summarize(funding.coc.2024.dollars = mean(funding.coc.2024.dollars, na.rm = TRUE), .groups = "drop")

# Calculate stats and t-tests for CoC funding by period
for(period in c("CoC_2013_2016", "CoC_2018_2019")) {
  period_data <- coc_data %>% filter(period == !!period)
  
  # Calculate means and SDs
  summary_stats <- period_data %>%
    group_by(d.esg.extra) %>%
    summarize(
      Mean = mean(funding.coc.2024.dollars, na.rm = TRUE),
      SD = sd(funding.coc.2024.dollars, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Perform t-test
  ttest_result <- perform_ttest(period_data, "funding.coc.2024.dollars")
  
  # Create row
  var_row <- data.frame(
    Variable = paste0(period, "_Mean"),
    Mean_Control = summary_stats$Mean[summary_stats$d.esg.extra == 0],
    Mean_Treatment = summary_stats$Mean[summary_stats$d.esg.extra == 1],
    SD_Control = summary_stats$SD[summary_stats$d.esg.extra == 0],
    SD_Treatment = summary_stats$SD[summary_stats$d.esg.extra == 1],
    P_Value = ttest_result$p_value,
    SE_Diff = ttest_result$se_diff,
    Stars = ttest_result$stars
  )
  
  panel_b_results <- bind_rows(panel_b_results, var_row)
}

# --- Panel C: Outcome variables in 2016 with t-tests ---
panel_c_data <- sample %>%
  filter(year == 2016) %>%
  group_by(coc.number.new) %>%
  summarize(
    d.esg.extra = first(d.esg.extra),
    ip_change_2015_2016 = mean(ip_change_2015_2016, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate stats and t-test for outcome variable
summary_stats <- panel_c_data %>%
  group_by(d.esg.extra) %>%
  summarize(
    Mean = mean(ip_change_2015_2016, na.rm = TRUE),
    SD = sd(ip_change_2015_2016, na.rm = TRUE),
    .groups = "drop"
  )

ttest_result <- perform_ttest(panel_c_data, "ip_change_2015_2016")

panel_c_results <- data.frame(
  Variable = "ip_change_2015_2016",
  Mean_Control = summary_stats$Mean[summary_stats$d.esg.extra == 0],
  Mean_Treatment = summary_stats$Mean[summary_stats$d.esg.extra == 1],
  SD_Control = summary_stats$SD[summary_stats$d.esg.extra == 0],
  SD_Treatment = summary_stats$SD[summary_stats$d.esg.extra == 1],
  P_Value = ttest_result$p_value,
  SE_Diff = ttest_result$se_diff,
  Stars = ttest_result$stars
)

# --- Create sample size row ---
sample_size_row <- data.frame(
  Variable = "Number of CoCs",
  Mean_Control = obs_counts$n_0,
  Mean_Treatment = obs_counts$n_1,
  SD_Control = NA,
  SD_Treatment = NA,
  P_Value = NA,
  SE_Diff = NA,
  Stars = ""
)

# --- Combine all panels ---
panel_a_final <- bind_rows(sample_size_row, panel_a_results)
panel_a_final$Panel <- "Panel A. CoC-by-year covariates"
panel_b_results$Panel <- "Panel B. ESG allocations (real 2024 dollars)"
panel_c_results$Panel <- "Panel C. Outcome variables in 2016"

# Combine all panels
combined_table <- bind_rows(panel_a_final, panel_b_results, panel_c_results) %>%
  relocate(Panel, Variable, Mean_Control, Mean_Treatment, SD_Control, SD_Treatment, P_Value, SE_Diff, Stars) %>%
  mutate(
    # Format the means with stars
    Mean_Control_Formatted = ifelse(is.na(Mean_Control), "",
                                    sprintf("%.3f", Mean_Control)),
    Mean_Treatment_Formatted = ifelse(is.na(Mean_Treatment), "",
                                      paste0(sprintf("%.3f", Mean_Treatment), Stars)),
    # Format SDs in parentheses
    SD_Control_Formatted = ifelse(is.na(SD_Control), "",
                                  paste0("(", sprintf("%.3f", SD_Control), ")")),
    SD_Treatment_Formatted = ifelse(is.na(SD_Treatment), "",
                                    paste0("(", sprintf("%.3f", SD_Treatment), ")")),
    # Format SE of difference
    SE_Diff_Formatted = ifelse(is.na(SE_Diff), "",
                               sprintf("%.3f", SE_Diff))
  ) %>%
  select(Panel, Variable, Mean_Control_Formatted, Mean_Treatment_Formatted,
         SD_Control_Formatted, SD_Treatment_Formatted, SE_Diff_Formatted, P_Value)

# Rename columns for final output
colnames(combined_table) <- c("Panel", "Variable", "Control", "Treatment",
                              "Control_SD", "Treatment_SD", "SE_Diff", "P_Value")

print("Balance table with t-test significance stars and standard errors:")
print(combined_table)

# Add significance note
cat("\nNote: *** p<0.01, ** p<0.05, * p<0.10\n")
cat("SE_Diff reports the standard error of the difference in means used in the t-test\n")

# Write to CSV
write.csv(combined_table, "~/Downloads/stats209/output/tables/balance_table_sid_with_ttests_and_se.csv", row.names = FALSE)
