#### Load packages ####
pacman::p_load(purrr, dplyr, tidyr, stringr, janitor, ggplot2, readxl, data.table)
####


cdbg = read.csv("~/Downloads/thesis/data/temp/ec-year-esg-awards.csv") # ec-by-year df with nominal ESG awards for 2012-2019
cw = read.csv("~/Downloads/thesis/data/clean/final/all-ec-coc-crosswalk-FINAL.csv") 
esg.shock = read.csv("~/Downloads/thesis/data/clean/final/cocs-esg-shock-v3.csv")


#### Clean crosswalk ####
cwc = cw |>
  rename("state" = "coc.state") |>
  filter(d.esg.coterm == 1)
####
#### Clean cdbg ####
cdbg = cdbg |>
  mutate(d.received.esg = ifelse(esg.nominal != 0 , 1, 0))

cdbg.esg.years = cdbg |> # this new df contains only ec.name-state pairs that received ESG funding 2012-2019
  group_by(ec.name, state) |>
  summarize(years.received.esg = sum(d.received.esg)) |>
  filter(years.received.esg != 0 ) |> # Filter out ECs that never directly received ESG funding from HUD
  mutate(ec.name.state = paste0(ec.name, state, ""))

# Keep only ECs that received at least one year of ESG funding 2013-2019
cdbg.clean = cdbg |>
  mutate(ec.name.state = paste0(ec.name, state, "")) |>
  filter(ec.name.state %in% cdbg.esg.years$ec.name.state)

# Drop all nonentitlement ECs because those ESG fundings go to state
cdbg.clean = cdbg.clean |>
  filter(!(str_detect(ec.name, "nonentitleme")))

# Select vars for merge
# Select final variables for merge
cdbg.clean = cdbg.clean |>
  select(c(state, ec.name, year, esg.nominal)) # Drop dummy vars

cdbg.clean = cdbg.clean |>
  mutate(ec.name = case_when(
    state == "fl" & ec.name == "jacksonville-duval count" ~ "duval county",
    state == "ga" & ec.name == "de kalb county" ~ "dekalb county",
    state == "ga" & ec.name == "macon" ~ "macon-bibb county",
    state == "il" & ec.name == "du page county" ~ "dupage county",
    state == "ky" & ec.name == "louisville" ~ "louisville-cdbg",
    state == "md" & ec.name == "prince georges county" ~ "prince george's county",
    state == "me" & ec.name == "portland" ~ "portland city",
    TRUE ~ ec.name  # keep ec.name unchanged if no match
  )) 

####

#### Merge cdbg with crosswalk ####
merge = left_join(cdbg.clean, cwc, by = c("ec.name", "state")) |>
  mutate(coc.number = case_when(
    ec.name == "osceola county" ~ "fl-507",
    ec.name == "johnson county" ~ "ks-505",
    ec.name == "kansas city" ~ "mo-604",
    ec.name == "brookhaven town" ~ "ny-603",
    TRUE ~ coc.number  # leave unchanged if no match
  )) |>
  select(c(year, state, coc.number, ec.name, esg.nominal))

# only NA are those I manually renamed that were unmatched from the final crosswalk
####


### Now coc by year ESG funding
final = merge |>
  group_by(year, coc.number) |>
  summarize(esg.total.nominal = sum(esg.nominal)) # THe values in 2017 here include ESG extra

# check no CoCs received 0 ESG funding all years
final.check = final |>
  mutate(d.no.esg = ifelse(esg.total.nominal == 0, 1, 0))
final.check |>
  group_by(coc.number) |>
  summarize(no.missing.years = sum(d.no.esg)) |>
  filter(no.missing.years == 8)

write.csv(final.check, "~/Downloads/thesis/data/clean/final/esg-awards-coc-by-year.csv", row.names = FALSE)


###### Merge in dummy for esg shock
a = read.csv("/Users/viyan/Downloads/thesis/data/clean/final/cocs-esg-shock-v2.csv")
a2 = a |>
  group_by(coc.number) |>
  summarize(d.esg.extra = sum(d.esg.extra),
            esg.reg.2017 = sum(esg17.reg),
            esg.extra.2017 = sum(esg17.extra),
            esg.total.2017 = sum(esg17.total)
            )

a2 = a2 |>
  mutate(d.esg.extra = ifelse(d.esg.extra >= 1, 1, 0))
write.csv(a2, "~/Downloads/thesis/data/clean/final/cocs-esg2017-info-FINAL.csv", row.names = FALSE)

a1 = left_join(final.check, a2, by = "coc.number")
# Number of ECs that received funding matches ESG norm deleting the EC ones and removing Guam, PR
write.csv(a1, "~/Downloads/thesis/data/clean/final/covariates/esg2017.csv", row.names = FALSE)
