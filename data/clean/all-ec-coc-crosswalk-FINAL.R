pacman::p_load(purrr, dplyr, tidyr, stringr, janitor, ggplot2, readxl, data.table)
cw = read.csv("/Users/viyan/Downloads/thesis/data/raw/coc-ec-cw-from-pdf-RAW.csv")

cwc = cw |>
  rename_all(tolower) |>
  mutate(across(where(is.character), tolower)) |>
  rename("var" = "alabama")


cwc = cwc |>
  mutate(coc.number = ifelse(str_detect(var, "\\w{2}-\\d{3}"), var, NA),
           d.esg.type = case_when(
           str_detect(var, "coterm") ~ 1,
           str_detect(var, "bounce") ~ 2,
           TRUE ~ NA_real_  # Default value if neither condition is met
         ),
         d.grantee.type = case_when(
           str_detect(var, "metro") ~ "metro",
           str_detect(var, "urban") ~ "urban",
           str_detect(var, "non-entitlement") ~ "non-ec",
           TRUE ~ NA_character_
         ))

cwc = cwc |>
  fill(coc.number, .direction = "down") |>
  fill(d.esg.type, .direction = "down") |>
  fill(d.grantee.type, .direction = "down")

    cwc <- cwc |> 
      mutate(
        ec.geocode = stringr::str_extract(var, "\\d{6}"),
        ec.name = if_else(
          !is.na(ec.geocode),
          stringr::str_trim(stringr::str_remove(var, "^.*?\\d{6}")),
          NA_character_
        )
      )
    
    
    cwc = cwc |>
      select(c(coc.number, ec.geocode, ec.name, d.esg.type, d.grantee.type))

    
  cwc.clean = cwc |>
    filter(!is.na(ec.name)) |>
    mutate(ec.name = str_replace(ec.name, "urban county", ""),
           ec.name = str_replace(ec.name, "metro city", ""),
           d.esg.coterm = ifelse(d.esg.type == 1, 1, 0),
           d.esg.bounce = ifelse(d.esg.type == 2, 1, 0)) |>
    mutate(d.grantee.metro = ifelse(d.grantee.type == "metro", 1, 0),
           d.grantee.urban = ifelse(d.grantee.type == "urban", 1, 0),
           d.grantee.nonec = ifelse(d.grantee.type == "non-ec", 1, 0)) |>
    select(-c(d.esg.type)) |>
    mutate(across(where(is.character), trimws))
  
  cwc.clean = cwc.clean |>
    mutate(coc.state = substr(coc.number, 1, 2)) |>
    relocate(coc.state, .before = 1)
  
    
  cwc.clean <- cwc.clean |> 
    mutate(
      ec.name = str_trim(ec.name),  # Just in case, trim leading/trailing whitespace first
      extra_code = str_extract(ec.name, "^\\d{4}"),  # Extract leading 4-digit code if present
      ec.geocode = if_else(
        !is.na(extra_code),
        paste0(ec.geocode, extra_code),  # Append to ec.geocode
        ec.geocode
      ),
      ec.name = if_else(
        !is.na(extra_code),
        str_trim(str_remove(ec.name, "^\\d{4}")),  # Remove the 4-digit prefix
        ec.name
      )
    ) |> 
    select(-extra_code)  # Optional: clean up helper column

  cwc.clean <- cwc.clean |> 
    mutate(
      ec.name = if_else(
        str_detect(ec.name, regex("^\\*\\s*hillsborough county", ignore_case = TRUE)),
        "hillsborough county",
        ec.name
      )
    ) 
    
    # Drop territories
    cwc.clean = cwc.clean |>
    mutate(!coc.state %in% c("as", "gu", "mp", "pr", "vi"))
  
  
  
  # Save
  write.csv(cwc.clean, "~/Downloads/thesis/data/clean/final/all-ec-coc-crosswalk-FINAL.csv", row.names = FALSE)

  
  coc.names = read.csv("~/Downloads/thesis/data/clean/final/coc-names.csv")
  
  
merge = left_join(cwc.clean, coc.names, by = "coc.number")
