#### Load data ####
acs = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/covariates/acs-clean-FINAL.csv")
rdd = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/covariates/rdd-treatment.csv")
hic.extra = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/FINAL-hic-extra.csv")
hic.norm = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/FINAL-hic-norm.csv")
pit = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/FINAL-pit.csv")
mort = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/mortality-FINAL-with-merged-cocs.csv")
sedd = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/sedd-FINAL-with-merged-cocs.csv") |>
  rename(year = fyear)
sid = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/sid-FINAL-with-merged-cocs.csv") |>
  rename(year = fyear)
sample = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/final-sample.csv") 
####

#### LaTeX font ####
library(extrafont)
loadfonts()
par(family = "Times")
#####

final = left_join(rdd, sample, by = "coc.number") |>
  filter(d.sample.bos.statewide == 0) |>
  group_by(coc.number.new) |>
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") |>
  filter(!is.na(coc.number.new)) # these are the islands

process_data <- function(final_data, join_data, value_column, new_value_column) {
  final_data %>%
    left_join(join_data, by = "coc.number.new") %>%
    left_join(pit, by = c("coc.number.new", "year")) |>
    select(c(year, coc.number.new, value_column, everything())) %>%
    filter(!is.na(get(value_column)) & year %in% c(2018, 2019)) %>%
    group_by(coc.number.new) %>%
    summarise(
      !!new_value_column := log(sum(get(value_column))/mean(ov.homeless)),
      combined.loss.abs = -1 * rdd.combined.loss  # or however you want to collapse loss
    ) %>%
    ungroup() %>%
    distinct(coc.number.new, .keep_all = TRUE)
}

# Apply the function to each dataset
final.sid <- process_data(final, sid, "health_ip", "log.2018.2019")
final.sedd <- process_data(final, sedd, "health_ed", "log.2018.2019")


datasets <- list(
  "final.sid" = final.sid,
  "final.sedd" = final.sedd
)

# Define a function to run the RD analysis and return results
run_rd_for_df <- function(df, cutoff) {
  # Compute absolute distance from cutoff
  df <- df %>%
    mutate(dist = abs(combined.loss.abs - cutoff)) %>%
    arrange(dist)  # Sort by distance
  
  # Define the range of observations (e.g., from 30 to max in steps of 5)
  obs_grid <- seq(50, nrow(df), by = 2)
  
  # Function to run RD given a desired number of observations
  run_rd_by_n <- function(n_target) {
    tryCatch({
      # Select the n closest observations to the cutoff
      h_max <- df$dist[n_target]  # the max distance in that subset
      rd <- rdrobust(
        y = df$log.2018.2019,
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
  
  # Run RDD for each dataset and combine results
  rd_results <- map_dfr(obs_grid, run_rd_by_n)
  return(rd_results)
}

# Run the RD analysis for each dataset and store results
cutoff <- 100000
all_rd_results <- lapply(datasets, run_rd_for_df, cutoff = cutoff)

# Function to plot RD results for each dataset separately
plot_rd_results <- function(rd_results, dataset_name) {
  
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
      y = "Estimated effect of\n extra ESG funding") + 
    scale_x_continuous(
      limits = c(50, NA),  # Start at 50, leave the upper limit open
      breaks = seq(50, max(rd_results$n_obs, na.rm = TRUE), by = 10)
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

# Plot RD results for each dataset separately
for (dataset_name in names(all_rd_results)) {
  rd_results <- all_rd_results[[dataset_name]]
  print(plot_rd_results(rd_results, dataset_name))  # Print each plot separately
}

# Directory to save plots
output_dir <- "~/Downloads/stats209/output/figures"

# Loop to plot and save each dataset
for (dataset_name in names(all_rd_results)) {
  rd_results <- all_rd_results[[dataset_name]]
  
  # Generate the plot
  p <- plot_rd_results(rd_results, dataset_name)
  
  # Save the plot
  ggsave(
    filename = paste0("rdd_plot_", dataset_name, ".png"),
    plot = p,
    path = output_dir,
    width = 4.5,
    height = 3,
    dpi = 300
  )
  
  # Optionally print for visual confirmation in RStudio
  print(p)
}


# Define the robustness targets with only sid and sedd
robustness_targets <- list(
  sid = list(data = final.sid, outcomes = "log.2018.2019"),
  sedd = list(data = final.sedd, outcomes = "log.2018.2019")
)

# Directory for saving plots
output_dir <- "~/Downloads/stats209/output/figures/robustness/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Loop through the robustness_targets list and run create_rd_plots for each dataset
for (target in names(robustness_targets)) {
  # Get the name of the dataset (e.g., "sid" or "sedd")
  dataset_name <- target
  
  # Access the corresponding dataframe and outcome variables
  data <- robustness_targets[[target]]$data
  outcomes <- robustness_targets[[target]]$outcomes
  
  # Call the create_rd_plots function with the correct data and outcomes
  create_rd_plots(data = data, outcomes = outcomes, dataset_name = dataset_name, output_dir = output_dir)
}
