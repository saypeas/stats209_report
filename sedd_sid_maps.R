  library(haven)
  library(dplyr)
  library(ggplot2)
  library(tigris)
  library(sf)
  library(viridis)
  library(scales)
  library(patchwork)
  
  # Assuming sid2019 and sedd2019 are already loaded
  # If not, uncomment these lines:
  sid = read_dta("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/old/county_by_year_ip.dta")
  sedd = read_dta("/Users/viyan/Downloads/thesis/data/clean/final/outcomes/old/county_by_year_ed.dta") 
  sid2019 = sid |> filter(fyear == 2019)
  sedd2019 = sedd |> filter(fyear == 2019)
  
  # Get the actual states in the data
  states_in_data <- unique(c(sid2019$state, sedd2019$state))
  print(paste("States in data:", paste(states_in_data, collapse = ", ")))
  
  # Define states east of Missouri (excluding Arizona)
  # Missouri is at approximately -91.8 degrees longitude
  eastern_of_illinois_states <- c(
    "nj", "de", "md", "dc",
    "ny", "pa", "va", "wv", "nc", "sc", "ga", "fl", "al", "tn",
    "ky", "oh", "in", "ms"
  )
  
  # Filter states in our data to only include those east of Missouri
  states_in_data_east <- intersect(states_in_data, eastern_of_illinois_states)
  print(paste("States in data (east of Missouri):", paste(states_in_data_east, collapse=", ")))
  
  # All eastern states not in our data will be colored grey
  grey_states <- setdiff(eastern_of_illinois_states, states_in_data_east)
  print(paste("States to color grey:", paste(grey_states, collapse=", ")))
  
  # All states to show on the map
  states_to_show <- eastern_of_illinois_states
  
  # Function to format FIPS codes properly
  format_fips <- function(fips) {
    sprintf("%05d", fips)
  }
  
  # Format FIPS codes in the data
  sid2019$fips_str <- format_fips(sid2019$fips)
  sedd2019$fips_str <- format_fips(sedd2019$fips)
  
  # Filter data to only include states east of Missouri
  sid2019_east <- sid2019 %>% filter(state %in% states_in_data_east)
  sedd2019_east <- sedd2019 %>% filter(state %in% states_in_data_east)
  
  # Get all state boundaries for the map area
  all_states_sf <- states(cb = TRUE) %>%
    filter(STUSPS %in% toupper(states_to_show)) %>%
    st_transform(4326) %>%
    mutate(state_lower = tolower(STUSPS),
           state_type = case_when(
             state_lower %in% states_in_data_east ~ "data_state",
             TRUE ~ "other_state"
           ))
  
  # Get county geometries only for states in our data (east of Missouri)
  counties_sf <- counties(state = toupper(states_in_data_east), cb = TRUE) %>%
    st_transform(4326)  # Transform to WGS84
  
  # Join data with geometries
  ip_map_data <- counties_sf %>%
    left_join(sid2019_east, by = c("GEOID" = "fips_str"))
  
  ed_map_data <- counties_sf %>%
    left_join(sedd2019_east, by = c("GEOID" = "fips_str"))
  
  # Create a function to make maps with better zoom
  create_county_map <- function(data, value_col, title, all_states) {
    # Calculate the bounding box for all eastern states to show
    map_bbox <- st_bbox(all_states)
    
    # Add tighter zoom (reduce the bounding box by 2% on each side)
    x_range <- map_bbox["xmax"] - map_bbox["xmin"]
    y_range <- map_bbox["ymax"] - map_bbox["ymin"]
    
    # Reduce the bounding box for zoom-in effect (2% reduction)
    map_bbox["xmin"] <- map_bbox["xmin"] + 0.08 * x_range  # Shrink the left side by 2%
    map_bbox["xmax"] <- map_bbox["xmax"] - 0.08 * x_range  # Shrink the right side by 2%
    map_bbox["ymin"] <- map_bbox["ymin"] + 0.08 * y_range  # Shrink the bottom side by 2%
    map_bbox["ymax"] <- map_bbox["ymax"] - 0.08 * y_range  # Shrink the top side by 2%
    
    
    ggplot() +
      # First add all non-data states with light grey fill
      geom_sf(data = filter(all_states, state_type == "other_state"), 
              fill = "lightgrey", color = "white", size = 0.3) +
      # Then add county data
      geom_sf(data = data, aes(fill = !!sym(value_col)), color = "white", size = 0.1) +
      # Add state borders with thicker white lines for states in our data
      geom_sf(data = filter(all_states, state_type == "data_state"), 
              fill = NA, color = "white", linewidth = 0.5) +
      
      # Set the map bounds to focus on our area of interest
      coord_sf(xlim = c(map_bbox["xmin"], map_bbox["xmax"]), 
               ylim = c(map_bbox["ymin"], map_bbox["ymax"])) +
      scale_fill_viridis(
        option = "plasma",
        trans = "sqrt",
        na.value = NA,  # Transparent instead of grey
        labels = comma,
        name = "Count"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm")
      ) +
      labs(
        title = title,
        caption = "Data from 2019"
      )
  }
  
  # Create the maps
  create_county_map(ip_map_data, "total_ip_homeless", "Inpatient Homeless Count by County", all_states_sf)
  create_county_map(ed_map_data, "total_ed_homeless", "Emergency Department Homeless Count by County", all_states_sf)
  
