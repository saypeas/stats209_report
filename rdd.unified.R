####################################################
# Unified RDD Analysis for HIC, Mortality, and PIT data
####################################################

# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(rdrobust)
library(ggplot2)

####################################################
# Data Loading and Preparation
####################################################



# Create common base sample with RDD treatment variables
rdd.sample = left_join(rdd, sample, by = "coc.number") %>%
  group_by(coc.number.new) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(coc.number.new)) %>% # exclude islands
  filter(d.sample.bos.statewide == 0) |>  # exclude BoS and statewide CoCs
  filter(d.sample.merged == 0)

# /!\ RUN THIS FIRST THEN THE FIRST CHUNK. Common datasets used across all analyses
sample.cut = read.csv("~/Downloads/thesis/data/clean/final/FINAL-sample-hic-pit-mort.csv") |>
  filter(coc.number %in% rdd.sample$coc.number.new)
sample = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/final-sample.csv")
rdd = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/covariates/rdd-treatment.csv")

# Load HIC datasets
hic.extra = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/FINAL-hic-extra.csv") %>%
  filter(coc.number.new %in% sample.cut$coc.number)
hic.norm = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/FINAL-hic-norm.csv") %>%
  filter(coc.number.new %in% sample.cut$coc.number)

# Load Mortality dataset
mort = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/mortality-FINAL-with-merged-cocs.csv") %>%
  filter(coc.number.new %in% sample.cut$coc.number)

# Load PIT dataset
pit = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/FINAL-pit.csv") %>%
  filter(coc.number.new %in% sample.cut$coc.number)
pop = read.csv("~/Downloads/thesis/data/clean/final/coc-pop-by-year-FINAL.csv")

####################################################
# Data Preparation - HIC
####################################################

# HIC Extra data - 2018
hic.extra.final <- hic.extra %>%
  select(year, coc.number.new, beds_current, beds_new, beds_dev, beds_facility, beds_other, beds_voucher) %>%
  left_join(rdd.sample, by = "coc.number.new") |>
  left_join(pit, by = c("coc.number.new", "year")) |>
  filter(year == 2018) %>%
  group_by(coc.number.new) %>%
  summarise(
    log.beds_current = log((beds_current + 1) / ov.homeless),
    log.beds_new = log((beds_new + 1) / ov.homeless),
    combined.loss.abs = -1 * first(rdd.combined.loss)
  ) %>%
  ungroup() %>%
  distinct(coc.number.new, .keep_all = TRUE)

# HIC Normal data - 2019
hic.norm.final <- hic.norm %>%
  select(year, coc.number.new, beds_total, beds_es, beds_psh, beds_th, beds_rrh) %>%
  left_join(rdd.sample, by = "coc.number.new") %>%
  left_join(pit, by = c("coc.number.new", "year")) |>
  filter(year %in% c(2018, 2019)) %>%
  group_by(coc.number.new) %>%
  summarise(
    log.beds_total = log((sum(beds_total + 1)) / mean(ov.homeless)),
    log.beds_es    = log((sum(beds_es + 1)) / mean(ov.homeless)),
    log.beds_psh   = log((sum(beds_psh + 1)) /  mean(ov.homeless)),
    log.beds_th    = log((sum(beds_th + 1)) /  mean(ov.homeless)),
    log.beds_rrh   = log((sum(beds_rrh + 1)) /  mean(ov.homeless)),
    combined.loss.abs = -1 * first(rdd.combined.loss)
  ) %>%
  ungroup() %>%
  distinct(coc.number.new, .keep_all = TRUE)

####################################################
# Data Preparation - Mortality
####################################################

mort.final = mort %>%
  left_join(rdd.sample, by = "coc.number.new") %>%
  left_join(pit, by = c("coc.number.new", "year")) %>%
  filter(year %in% c(2018, 2019)) %>%
  group_by(coc.number.new) %>%
  summarise(
    log.total.deaths = log((sum(deaths_total, na.rm = TRUE)) / mean(ov.homeless, na.rm = TRUE)),
    combined.loss.abs = -1 * first(rdd.combined.loss)
  ) %>%
  ungroup() %>%
  distinct(coc.number.new, .keep_all = TRUE)


####################################################
# Data Preparation - PIT
####################################################

pit.final = pit %>%
  select(c(year, coc.number.new, ov.homeless, sh.tot.homeless, us.homeless, ov.chronic.homeless)) %>%
  left_join(rdd.sample, by = "coc.number.new") %>%
  left_join(pop, by = c("coc.number.new", "year")) %>% 
  filter(year %in% c(2018, 2019)) %>%
  group_by(coc.number.new) %>%
  summarise(
    log.ov.homeless = log(sum(ov.homeless + 1) / mean(pop.total + 1)),
    log.sh.tot.homeless = log(sum(sh.tot.homeless + 1) / mean(pop.total + 1)),
    log.us.homeless = log(sum(us.homeless + 1) / mean(pop.total + 1)),
    log.ov.chronic.homeless = log(sum(ov.chronic.homeless + 1) / mean(pop.total + 1)),
    combined.loss.abs = -1 * first(rdd.combined.loss)
  ) |>
  ungroup() %>%
  distinct(coc.number.new, .keep_all = TRUE)

####################################################
# Define outcomes for each dataset
####################################################

outcomes_hic_norm <- c("log.beds_total", "log.beds_es", "log.beds_psh", "log.beds_th", "log.beds_rrh")
outcomes_hic_extra <- c("log.beds_current", "log.beds_new")
outcomes_mort <- c("log.total.deaths")
outcomes_pit <- c("log.ov.homeless", "log.sh.tot.homeless", "log.us.homeless", "log.ov.chronic.homeless")

# Combine all outcomes for common bandwidth calculation
all_outcomes <- list(
  hic_norm = list(data = hic.norm.final, outcomes = outcomes_hic_norm),
  hic_extra = list(data = hic.extra.final, outcomes = outcomes_hic_extra),
  mort = list(data = mort.final, outcomes = outcomes_mort),
  pit = list(data = pit.final, outcomes = outcomes_pit)
)

####################################################
# Calculate common minimum bandwidth across all datasets
####################################################

# Function to calculate ISME bandwidths for a dataset and its outcomes
get_bandwidths <- function(data_list) {
  result <- data.frame(dataset = character(), outcome = character(), bandwidth = numeric())
  
  for (dataset_name in names(data_list)) {
    dataset <- data_list[[dataset_name]]
    for (outcome in dataset$outcomes) {
      tryCatch({
        rd_tmp <- rdrobust(
          y = dataset$data[[outcome]],
          x = dataset$data$combined.loss.abs,
          c = 100000,
          p = 1
        )
        # Use the mean of left and right bandwidth
        bw <- mean(rd_tmp$bws[1:2])
        result <- rbind(result, data.frame(dataset = dataset_name, outcome = outcome, bandwidth = bw))
      }, error = function(e) {
        warning(paste("Error calculating bandwidth for", dataset_name, outcome, ":", e$message))
      })
    }
  }
  
  return(result)
}

# Calculate all bandwidths
all_bandwidths <- get_bandwidths(all_outcomes)
print("All calculated bandwidths:")
print(all_bandwidths)

# Find the minimum bandwidth across all datasets and outcomes
min_bandwidth <- min(all_bandwidths$bandwidth, na.rm = TRUE)
cat("\nMinimum bandwidth across all datasets:", min_bandwidth, "\n")

####################################################
# General RD estimation function
####################################################

run_rd_specs <- function(data, outcomes, dataset_label, common_bandwidth) {
  results_linear_local <- list()
  results_linear_full <- list()
  results_poly_local <- list()
  
  for (outcome in outcomes) {
    # 1. Local Linear with common minimum bandwidth
    rd_linear_local <- rdrobust(
      y = data[[outcome]],
      x = data$combined.loss.abs,
      cluster = data$coc.number.new,
      c = 100000,
      p = 1,
      h = common_bandwidth
    )
    results_linear_local[[outcome]] <- rd_linear_local
    
    # 3. Local Polynomial (p = 3) with common minimum bandwidth
    rd_poly_local <- rdrobust(
      y = data[[outcome]],
      x = data$combined.loss.abs,
      cluster = data$coc.number.new,
      c = 100000,
      p = 3,
      h = common_bandwidth
    )
    results_poly_local[[outcome]] <- rd_poly_local
  }
  
  # Helper to tidy results
  extract_rd_estimates <- function(rd_list, spec_label) {
    map_dfr(names(rd_list), function(name) {
      out <- rd_list[[name]]
      tibble(
        dataset = dataset_label,
        spec = spec_label,
        outcome = name,
        coef = out$coef[3,1],
        se.robust = out$se[3,1],
        ci_lower = out$ci[3,1],
        ci_upper = out$ci[3,2],
        p_value = out$pv[3,1],
        n_left = out$N_h[1],
        n_right = out$N_h[2]
      )
    })
  }
  
  # Bind all spec results
  bind_rows(
    extract_rd_estimates(results_linear_local, "linear_common_bw"),
    extract_rd_estimates(results_linear_full, "linear_full_bw"),
    extract_rd_estimates(results_poly_local, "poly_common_bw")
  )
}

####################################################
# Run RD Estimation for all datasets
####################################################

# Run RD for each dataset
rd_estimates_hic_norm <- run_rd_specs(hic.norm.final, outcomes_hic_norm, "hic_norm", min_bandwidth)
rd_estimates_hic_extra <- run_rd_specs(hic.extra.final, outcomes_hic_extra, "hic_extra", min_bandwidth)
rd_estimates_mort <- run_rd_specs(mort.final, outcomes_mort, "mort", min_bandwidth)
rd_estimates_pit <- run_rd_specs(pit.final, outcomes_pit, "pit", min_bandwidth)

# Combine all results
rd_estimates_all <- bind_rows(
  rd_estimates_hic_norm,
  rd_estimates_hic_extra,
  rd_estimates_mort,
  rd_estimates_pit
)

# Print combined results
print(rd_estimates_all)

# Save combined results to CSV
write.csv(
  rd_estimates_all, 
  file = "~/Downloads/thesis/output/tables/unified_rdd_estimates_DROP_MERGE.csv", 
  row.names = FALSE
)

####################################################
# Function for RD robustness plots
####################################################

run_rd_for_df <- function(df, outcome_var, cutoff = 100000) {
  # Compute absolute distance from cutoff
  df <- df %>%
    mutate(dist = abs(combined.loss.abs - cutoff)) %>%
    arrange(dist)  # Sort by distance
  
  # Define the range of observations (from 50 to max in steps of 10)
  obs_grid <- seq(50, nrow(df), by = 10)
  
  # Function to run RD given a desired number of observations
  run_rd_by_n <- function(n_target) {
    tryCatch({
      # Select the n closest observations to the cutoff
      h_max <- df$dist[n_target]  # the max distance in that subset
      rd <- rdrobust(
        y = df[[outcome_var]],
        x = df$combined.loss.abs,
        c = cutoff,
        h = h_max,
        p = 1,
        q = 2
      )
      data.frame(
        n_obs = n_target,
        h_used = h_max,
        coef = rd$coef[3, 1],
        ci_low = rd$ci[3, 1],
        ci_high = rd$ci[3, 2]
      )
    }, error = function(e) NULL)
  }
  
  # Run RDD for each sample size and combine results
  results <- map_dfr(obs_grid, run_rd_by_n)
  return(results)
}

create_rd_plot <- function(rd_results, title_suffix = "") {
  ggplot(rd_results, aes(x = n_obs, y = coef)) +
    geom_hline(yintercept = 0, color = "grey30", linewidth = 0.2) +  # horizontal 0 line
    geom_point(shape = 18, size = 3) +  # diamond markers
    geom_line() + 
    geom_line(aes(y = ci_high), linetype = "dashed", color = "grey50") + 
    geom_line(aes(y = ci_low), linetype = "dashed", color = "grey50") + 
    scale_y_continuous(
      n.breaks = 6  # Request approximately 5 ticks on the y-axis
    ) + 
    labs(
      x = "Number of CoCs in sample",
      y = "Estimated effect of\n extra ESG funding",
    ) + 
    scale_x_continuous(
      limits = c(50, NA),  # Start at 50, leave the upper limit open
      breaks = seq(50, max(rd_results$n_obs, na.rm = TRUE), by = 50)
    ) +
    theme_minimal(base_size = 15) + # Increase from default 11
    theme(
      text = element_text(family = "Times"),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", size = 0.2),
      axis.title.x = element_text(color = "black"),
      axis.title.y = element_text(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
}

####################################################
# Create robustness plots for each dataset
####################################################

# HIC Total Beds plot
hic_total_beds_results <- run_rd_for_df(hic.norm.final, "log.beds_total")
p_hic_total <- create_rd_plot(hic_total_beds_results, "HIC Total Beds")
ggsave(
  filename = "~/Downloads/thesis/output/figures/rdd_plot_hic.png",
  plot = p_hic_total,
  width = 4.5,
  height = 3,
  dpi = 300
)

# Mortality plot
mort_results <- run_rd_for_df(mort.final, "log.total.deaths")
p_mort <- create_rd_plot(mort_results, "Mortality")
ggsave(
  filename = "~/Downloads/thesis/output/figures/rdd_plot_mort.png",
  plot = p_mort,
  width = 4.5,
  height = 3,
  dpi = 300
)

# PIT Overall Homeless plot
pit_results <- run_rd_for_df(pit.final, "log.ov.homeless")
p_pit <- create_rd_plot(pit_results, "PIT Overall Homeless")
ggsave(
  filename = "~/Downloads/thesis/output/figures/rdd_plot_pit.png",
  plot = p_pit,
  width = 4.5,
  height = 3,
  dpi = 300
)

# Optional: Additional RD plots for other outcomes
# Uncomment and modify as needed for other outcomes

####################################################
# Additional PIT Raw RD Plots (as in original PIT script)
####################################################

# Function to create RD plots for a dataset and its outcomes
# Function to create RD plots for a dataset and its outcomes
# Function to create RD plots for a dataset and its outcomes (with bandwidth filtering)
create_rd_plots <- function(data, outcomes, dataset_name, output_dir, bandwidth = min_bandwidth, nbins = c(60, 60)) {
  rd_plot_list <- list()
  
  for (outcome in outcomes) {
    # Ensure that data for outcome and combined.loss.abs have the same number of rows
    valid_data <- data[!is.na(data[[outcome]]) & !is.na(data$combined.loss.abs), ]
    
    # Filter data to only include observations within the bandwidth
    filtered_data <- valid_data %>%
      mutate(distance_to_cutoff = abs(combined.loss.abs - 100000)) %>%
      filter(distance_to_cutoff <= bandwidth)
    
    # Check if we have enough data after filtering
    if (nrow(filtered_data) == 0) {
      warning(paste("No valid data within bandwidth for", dataset_name, outcome))
      next
    }
    
    if (nrow(filtered_data) < 10) {
      warning(paste("Very few observations within bandwidth for", dataset_name, outcome, "- only", nrow(filtered_data), "observations"))
    }
    
    # Create rdplot with filtered data
    tryCatch({
      rd_plot <- rdplot(
        y = filtered_data[[outcome]],
        x = filtered_data$combined.loss.abs,
        c = 100000,
        p = 1,
        title = "",
        kernel = "triangular",
        binselect = "esmv"
      )
      
      rd_plot_gg <- rd_plot$rdplot
      
      # Get appropriate y-axis label based on outcome
      y_label <- case_when(
        grepl("beds", outcome) ~ "Log of beds\n per homeless person",
        grepl("homeless", outcome) ~ "Log of homeless share\n of total population",
        grepl("deaths", outcome) ~ "Log of deaths\n per homeless person",
        grepl("log.2018.2019", outcome) ~ "Log of visits\n per homeless person",
        TRUE ~ "Outcome value"
      )
      
      # Modify plot appearance
      modified_plot <- rd_plot_gg +
        theme(
          text = element_text(family = "Times", size = 14),
          axis.title = element_text(family = "Times", size = 14, color = "black"),
          axis.text = element_text(family = "Times", size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        xlab("Combined loss in CoC grant program\n (2015-2016)") +
        ylab(y_label) +
        scale_x_continuous(labels = scales::comma) #+
        # Add subtitle to indicate bandwidth filtering
        #labs(subtitle = paste("Observations within bandwidth =", round(bandwidth, 0)))
      
      # Store the plot
      rd_plot_list[[outcome]] <- modified_plot
      
      # Generate the file name based on the dataset and outcome
      file_name <- paste0(output_dir, "rdd_raw_", dataset_name, "_", outcome, ".png")
      
      # Save the plot for the current outcome
      ggsave(file_name, plot = modified_plot, width = 4.5, height = 3, dpi = 300)
      cat("Created bandwidth-filtered plot for", dataset_name, "-", outcome, 
          "with", nrow(filtered_data), "observations\n")
    }, error = function(e) {
      warning(paste("Could not create RD plot for", dataset_name, outcome, ":", e$message))
    })
  }
  
  return(rd_plot_list)
}

####################################################
  # Batch: Create and Save RD Robustness Plots for All Outcomes
####################################################
# Define datasets and corresponding outcomes
robustness_targets <- list(
  hic_norm = list(data = hic.norm.final, outcomes = outcomes_hic_norm),
  hic_extra = list(data = hic.extra.final, outcomes = outcomes_hic_extra),
  mort = list(data = mort.final, outcomes = outcomes_mort),
  pit = list(data = pit.final, outcomes = outcomes_pit)
)

# Directory for saving plots
output_dir <- "~/Downloads/thesis/output/figures/robustness/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Set your desired bandwidth
custom_bandwidth <- 269517

# Loop through the robustness_targets list with the custom bandwidth
for (target in names(robustness_targets)) {
  dataset_name <- target
  data <- robustness_targets[[target]]$data
  outcomes <- robustness_targets[[target]]$outcomes
  
  # Call with custom bandwidth of 269,517
  create_rd_plots(data = data, outcomes = outcomes, 
                  dataset_name = dataset_name, 
                  output_dir = "~/Downloads/thesis/output/figures/robustness/",
                  bandwidth = custom_bandwidth)
}

####################################################
# Extract CoCs within the minimum bandwidth window
####################################################

extract_cocs_in_bandwidth <- function(data, cutoff = 100000, bandwidth) {
  # Filter to CoCs within the bandwidth window around the cutoff
  cocs_in_bandwidth <- data %>%
    mutate(distance_to_cutoff = abs(combined.loss.abs - cutoff)) %>%
    filter(distance_to_cutoff <= bandwidth) %>%
    select(coc.number.new, combined.loss.abs, distance_to_cutoff) %>%
    arrange(distance_to_cutoff)
  
  return(cocs_in_bandwidth)
}

# Extract CoCs within the minimum bandwidth for each dataset
cat("Minimum bandwidth across all datasets:", min_bandwidth, "\n")

hic_norm_cocs <- extract_cocs_in_bandwidth(hic.norm.final, bandwidth = min_bandwidth)
cat("\nCoCs in HIC Norm dataset within minimum bandwidth:", nrow(hic_norm_cocs), "\n")
print(head(hic_norm_cocs, 10))

hic_extra_cocs <- extract_cocs_in_bandwidth(hic.extra.final, bandwidth = min_bandwidth)
cat("\nCoCs in HIC Extra dataset within minimum bandwidth:", nrow(hic_extra_cocs), "\n")
print(head(hic_extra_cocs, 10))

mort_cocs <- extract_cocs_in_bandwidth(mort.final, bandwidth = min_bandwidth)
cat("\nCoCs in Mortality dataset within minimum bandwidth:", nrow(mort_cocs), "\n")
print(head(mort_cocs, 10))

pit_cocs <- extract_cocs_in_bandwidth(pit.final, bandwidth = min_bandwidth)
cat("\nCoCs in PIT dataset within minimum bandwidth:", nrow(pit_cocs), "\n")
print(head(pit_cocs, 10))

# Find common CoCs across all datasets within the minimum bandwidth
common_cocs <- Reduce(intersect, list(
  hic_norm_cocs$coc.number.new,
  hic_extra_cocs$coc.number.new,
  mort_cocs$coc.number.new,
  pit_cocs$coc.number.new
))

cat("\nNumber of common CoCs across all datasets within minimum bandwidth:", length(common_cocs), "\n")
cat("Common CoCs: ", paste(common_cocs, collapse=", "), "\n")

# Save the list of CoCs within the minimum bandwidth for each dataset
cocs_in_bandwidth <- list(
  hic_norm = hic_norm_cocs,
  hic_extra = hic_extra_cocs,
  mortality = mort_cocs,
  pit = pit_cocs,
  common = data.frame(coc.number.new = common_cocs)
)

# Export common CSVS
write.csv(
  pit_cocs,
  file = "~/Downloads/thesis/output/tables/common_cocs_baseline.csv",
  row.names = FALSE
)
