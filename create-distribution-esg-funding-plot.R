#rdd = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/covariates/rdd-treatment.csv")
sample = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/final-sample.csv") |>
  filter(d.sample.bos.statewide == 0)
grants = read.csv("/Users/viyan/Downloads/stats209/data/clean/coc-grants-clean-v2.csv") |>
  mutate(across(where(is.character), tolower))
esg = read.csv("~/Downloads/stats209/data/clean/final/covariates/esg-treated-ALL.csv") |>
  filter(coc.number %in% sample$coc.number) |>
  mutate(year = 2017)
esf = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/esg-awards-coc-by-year-FINAL.csv") |>
  filter(coc.number %in% sample$coc.number) |>
  left_join(esg, by = "coc.number")
pcepi = read.csv("/Users/viyan/Downloads/stats209/data/raw/pcepi/2012-2024-pcepi.csv")
# Using the pre-merged CoCs
pit = read.csv("/Users/viyan/Downloads/stats209/data/clean/final/outcomes/pit-clean-final-with-bos.csv") |> 
  select(coc.number, ov.homeless, year)

# Convert to real dollars
pcepi_2024 <- 123.502  # Placeholder value for 2024 PCEPI, replace with the actual value
pcepi_2017 <- 100  # 2017 PCEPI value

esf <- esf |>
  left_join(pcepi, by = "year") |>
  mutate(real.esg.total = esg.total.nominal * (pcepi_2024 / pcepi)) 

esg2 = esg |>
  left_join(pit, by = c("coc.number", "year")) |>
  mutate(esg.extra.per.homeless = esg.extra.2017/ov.homeless) |>
  filter(esg.extra.2017 != 0) |>
  mutate(esg.extra.per.homeless.real = esg.extra.per.homeless * (pcepi_2024 / pcepi_2017))


# Convert ESG extra funding to 2024 dollars
esg$esg.extra.real <- esg$esg.extra.2017 * (pcepi_2024 / pcepi_2017)


a = full_join(esg, sample, by = "coc.number") |>
  filter(d.esg.extra == 1) |>
 # filter(coc.number.new != "ak-500") |> # Remove ak-500, not balanced in PIT or HIC or something
 select(c(coc.number.new, esg.extra.2017, esg.reg.2017, esg.extra.real))





# Assuming esg.extra.real is already defined in your environment
esg.dist = a |> 
  ggplot(aes(x = esg.extra.real / 1e6)) +
  geom_histogram(color = "darkorange4", fill = "darkorange3", alpha = 0.7, bins = 20) +
  theme_minimal(base_size = 11) + 
  theme(
    text = element_text(family = "Times"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", size = 0.2),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(x = "Extra ESG funding\n(millions of 2024 dollars)", y = "Number of CoCs") +
  scale_x_continuous(
    labels = scales::comma,
    expand = c(0, 0),
    breaks = seq(0, ceiling(max(a$esg.extra.real, na.rm = TRUE) / 1e6), by = 1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12))


esg.per.homeless.dist = esg2 |> 
  ggplot(aes(x = esg.extra.per.homeless/1000)) +
  geom_histogram(color = "darkolivegreen", fill = "darkolivegreen4", alpha = 0.7, 
                 bins = 20) +
  theme_minimal(base_size = 11) + 
  theme(
    text = element_text(family = "Times"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", size = 0.2),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(x = "Extra ESG funding per homeless person\n (thousands of 2024 dollars)", y = "Number of CoCs") +
  scale_x_continuous(
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12))


esg.dist = grid.arrange(esg.dist, esg.per.homeless.dist, ncol = 2)

### CoC Plot
## #Prep data for plots

coc.grant = left_join(grants, sample, by = "coc.number") |>
  filter(d.sample.bos.statewide == 0)
b = left_join(coc.grant, esg, by = "coc.number")

trend.esf =esf |>
  group_by(year, d.esg.extra) |>
  summarize(mean.esg.funding = mean(real.esg.total))

### COC PROGRAM PLOTS
#########################
# Shared color scheme, labels, and line types
color_vals <- c("1" = "darkorange3", "0" = "dodgerblue4")
label_vals <- c("1" = "Extra ESG funding", 
                "0" = "No extra ESG funding")

# CoC grant trends with shaded region
coc.grant.trends = b2 |> 
  filter(year %in% 2013:2019) |> 
  ggplot(aes(x = year, y = mean.coc.grant, group = d.esg.extra, 
             color = factor(d.esg.extra, levels = c(1, 0)))) + 
  # Add shaded region for 2014-2017
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf, 
           fill = "grey90", alpha = 0.5) +
  geom_line() + 
  geom_point(size = 2.5, shape = 18) + 
  scale_color_manual(values = color_vals, labels = label_vals, name = "") + 
  scale_x_continuous(breaks = 2013:2019) + 
  scale_y_continuous(
    labels = scales::comma,
    breaks = scales::pretty_breaks(n = 4),
    limits = c(5000000, NA)
  ) +
  labs(x = "", y = "CoC grant funding (2024 dollars)") + 
  theme_minimal(base_size = 12) + 
  theme(
    text = element_text(family = "Times"), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black", size = 0.2),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )  

# ESG Funding Plot with shaded region
esg.trends = trend.esf |>
  filter(year %in% 2013:2019) |>
  ggplot(aes(x = year, y = mean.esg.funding, group = d.esg.extra,
             color = factor(d.esg.extra, levels = c(1, 0)))) +
  # Add shaded region for 2014-2017
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf, 
           fill = "grey90", alpha = 0.5) +
  geom_point(size = 2.5, shape = 18) +
  geom_line() +
  scale_color_manual(values = color_vals, labels = label_vals, name = "") +
  scale_x_continuous(breaks = 2013:2019) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "ESG funding (2024 dollars)") +
  theme_minimal(base_size = 12) + 
  theme(
    text = element_text(family = "Times"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", size = 0.2),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )



# Save individual plots with their own legends
out_dir <- "~/Downloads/stats209/output/figures/"
ggsave(filename = file.path(out_dir, "trends.esg.png"), plot = esg.trends, width = 6, height = 4, units = "in")
ggsave(filename = file.path(out_dir, "trends.coc.png"), plot = coc.grant.trends, width = 6, height = 4, units = "in")
ggsave(filename = file.path(out_dir, "dist.esg.png"), plot = esg.dist, width = 5.8, height = 4, units = "in")
