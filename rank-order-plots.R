library(dplyr)
library(ggplot2)
library(purrr)

# Load datasets
sample = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/final-sample.csv")
rdd = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/covariates/rdd-treatment.csv")
pop = read.csv("~/Downloads/thesis/data/clean/final/coc-pop-by-year-FINAL.csv") 
sample.cut = read.csv("~/Downloads/thesis/data/clean/final/FINAL-sample-hic-pit-mort.csv") 
mort = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/mortality-FINAL-with-merged-cocs.csv") |>
  filter(coc.number.new %in% sample.cut$coc.number)
pit = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/FINAL-pit.csv") |> 
  select(coc.number.new, ov.homeless, year) |>
  filter(coc.number.new %in% sample.cut$coc.number)
sedd.cocs = read.csv("/Users/viyan/Downloads/thesis/output/tables/sedd_cocs_in_bandwidth.csv")
sid.cocs = read.csv("/Users/viyan/Downloads/thesis/output/tables/sid_cocs_in_bandwidth.csv")
baseline.cocs = read.csv("/Users/viyan/Downloads/thesis/output/tables/common_cocs_baseline.csv")

# Load new dataframes
sedd = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/sedd-FINAL-with-merged-cocs.csv") |> 
  rename(year = fyear)
sid = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/sid-FINAL-with-merged-cocs.csv") |> 
  rename(year = fyear)

# Join rdd with sample for PIT data
rdd.sample = rdd |> 
  left_join(sample, by = "coc.number") |>
  select(coc.number.new, rdd.combined.loss, d.sample.bos.statewide)

pit.final = pit |>
  left_join(rdd.sample, by = "coc.number.new") |> 
  left_join(pop, by = c("coc.number.new", "year")) |> 
  filter(year %in% c(2013:2016)) |> 
  filter(d.sample.bos.statewide == 0) |> 
  group_by(coc.number.new) |> 
  summarise(
    log.outcome = log(sum(ov.homeless + 1) / mean(pop.total)),
    combined.loss.abs = -1 * first(rdd.combined.loss)
  ) |> 
  ungroup() |> 
  distinct(coc.number.new, .keep_all = TRUE) |> 
  mutate(d.esg.extra = ifelse(combined.loss.abs > 100000, 1, 0)) |> 
  filter(!is.na(log.outcome)) |> 
  arrange(log.outcome) |> 
  mutate(rank = row_number(),
         # Flag CoCs in baseline.cocs
         is_baseline = coc.number.new %in% baseline.cocs$coc.number.new)

# Function to prepare data for SID, SEDD, and MORT
prepare_data <- function(data, outcome_column, cocs_to_highlight = NULL) {
  result <- data |> 
    select(year, coc.number.new, all_of(outcome_column)) |>
    left_join(rdd.sample, by = "coc.number.new") |>
    left_join(pit, by = c("coc.number.new", "year")) |>
    filter(year %in% c(2013:2016)) |>
    filter(d.sample.bos.statewide == 0) |>
    group_by(coc.number.new) |>
    summarise(
      log.outcome = log(sum(!!sym(outcome_column) + 1) / mean(ov.homeless)),
      combined.loss.abs = -1 * first(rdd.combined.loss)
    ) |>
    ungroup() |>
    distinct(coc.number.new, .keep_all = TRUE) |>
    mutate(d.esg.extra = ifelse(combined.loss.abs > 100000, 1, 0)) |>
    filter(!is.na(log.outcome)) |>
    arrange(log.outcome) |>
    mutate(rank = row_number())
  
  # If cocs_to_highlight is provided, add a flag for highlighted CoCs
  if (!is.null(cocs_to_highlight)) {
    result <- result |>
      mutate(is_highlighted = coc.number.new %in% cocs_to_highlight$coc.number.new)
  } else {
    result <- result |>
      mutate(is_highlighted = FALSE)
  }
  
  return(result)
}

# Prepare datasets with highlighting information
mort.final <- prepare_data(mort, "deaths_total", baseline.cocs)
sid.final <- prepare_data(sid, "health_ip", sid.cocs)
sedd.final <- prepare_data(sedd, "health_ed", sedd.cocs)

# Modified function to create the plot
create_plot <- function(data, y_var, y_label, x_label) {
  # Calculate max rank for x-axis limits
  max_rank <- max(data$rank)
  
  if (max_rank <= 100) {
    step_size <- 20  # For smaller datasets (max ≤ 100), use steps of 20
  } else if (max_rank <= 200) {
    step_size <- 50  # For medium datasets (max ≤ 200), use steps of 50
  } else {
    step_size <- 50  # For larger datasets (max > 200), use steps of 100
  }
  
  # Create breaks that end in 0 with appropriate spacing
  max_break <- ceiling(max_rank/step_size) * step_size  # Round up to nearest step_size
  breaks <- c(1, seq(step_size, max_break, by = step_size))
  # Filter out breaks beyond max_rank
  breaks <- breaks[breaks <= max_rank]
  
  # Create the plot with different colors
  ggplot(data, aes(x = rank, y = !!sym(y_var))) +
    # Non-treated points (control group)
    geom_point(data = subset(data, d.esg.extra == 0), 
               aes(x = rank, y = !!sym(y_var)), 
               color = "black")  + # No fill inside the circle
    # Treatment points that are not in special sample (blue diamonds)
    geom_point(data = subset(data, d.esg.extra == 1 & !is_highlighted), 
               aes(x = rank, y = !!sym(y_var)), 
               color = "dodgerblue2", shape = 18, size = 3) +
    # Treatment points that are ALSO in the special sample (red diamonds)
    geom_point(data = subset(data, d.esg.extra == 1 & is_highlighted), 
               aes(x = rank, y = !!sym(y_var)), 
               color = "orange2", shape = 18, size = 3) +
    scale_x_continuous(
      limits = c(1, max_rank), 
      breaks = breaks  # Use our custom breaks that end in 0
    ) +
    labs(x = x_label, y = y_label) +
    theme_minimal() +
    theme(
      text = element_text(family = "Times"), 
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", size = 0.2),
      axis.ticks = element_line(color = "black"),
      legend.position = "none"
    )
}

# Create plots for each dataset
mort_plot <- create_plot(mort.final, "log.outcome", 
                         "Log of deaths\n per homeless person (2013–2016)", 
                         "CoCs' rank order in log deaths\n per homeless person (2013–2016)")

sid_plot <- create_plot(sid.final, "log.outcome", 
                        "Log of homeless inpatient visits\n per homeless person (2013–2016)", 
                        "CoCs' rank order in log inpatient visits\n per homeless person (2013–2016)")

sedd_plot <- create_plot(sedd.final, "log.outcome", 
                         "Log of emergency department visits\n per homeless person (2013–2016)", 
                         "CoCs' rank order in log emergency department visits\n per homeless person (2013–2016)")

# For pit_plot, we need to add the highlighting flag first
pit.final <- pit.final %>% 
  mutate(is_highlighted = coc.number.new %in% baseline.cocs$coc.number.new)

pit_plot <- create_plot(pit.final, "log.outcome", 
                        "Log of homeless individuals\n per person (2013–2016)", 
                        "CoCs' rank order in log homeless individuals\n per person (2013–2016)")

# Display the plots
mort_plot
sid_plot
sedd_plot
pit_plot

# Define output directory
out_dir <- "~/Downloads/thesis/output/figures/"

# Save each plot
ggsave(filename = file.path(out_dir, "rank_order_mort_plot.png"), plot = mort_plot, width = 4, height = 3, units = "in")
ggsave(filename = file.path(out_dir, "rank_order_sid_plot.png"),  plot = sid_plot,  width = 4, height = 3, units = "in")
ggsave(filename = file.path(out_dir, "rank_order_sedd_plot.png"), plot = sedd_plot, width = 4, height = 3, units = "in")
ggsave(filename = file.path(out_dir, "rank_order_pit_plot.png"),  plot = pit_plot,  width = 4, height = 3, units = "in")