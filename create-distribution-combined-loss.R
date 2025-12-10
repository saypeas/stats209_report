a = read.csv("/Users/viyan/Downloads/stats209/data/raw/norm/esg-supp-calcs-FINAL.csv")

a = a |>
  select(coc = coc.number.name,
         combined.loss)

library(dplyr)
library(ggplot2)
library(scales)

x <- a$combined.loss[a$combined.loss > 0]

# Freedman–Diaconis binwidth (recommended)
bw <- 2 * IQR(x) / (length(x)^(1/3))

a |>
  filter(combined.loss %in% c(-1000000:1000000)) |>
  ggplot(aes(x = combined.loss)) +
  geom_histogram(binwidth = 40000, fill = "dodgerblue3", 
                 color = "black",
                 alpha = 0.7) +
  geom_vline(xintercept = 100000, color = "firebrick3", linewidth = 0.6, lty = 2) +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  labs(
    x = "Combined loss relative to extra funding cutoff",
    y = "Number of CoCs") + 
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Times"))
ggsave("~/Downloads/esg_running_variable_histogram.png", height = 4, width = 6, dpi = 300)
