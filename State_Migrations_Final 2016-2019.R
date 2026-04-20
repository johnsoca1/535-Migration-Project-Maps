library(readxl)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(ggplot2)
library(scales)

options(tigris_use_cache = TRUE)

# reading excel file
state_migration_files <- sort(list.files(
  path = "/Users/niaallen/Documents/State_Migrations",
  pattern = "\\.xlsx?$",
  full.names = TRUE
))

state_migrations <- lapply(state_migration_files, read_excel, col_names= FALSE)

#cleaning columns, defining areas, extracting only estimate column for state migrations
state_migrations_df <- function(x, year_value) {
  states<- c(state.name, "Puerto Rico", "District of Columbia")
  
  header <- trimws(as.character(unlist(x[7, ])))
  columns<- which(header %in% states)
  
  filtered <- x[, c(1, columns)] 
  colnames(filtered) <- c("current_state", header[columns])
  
  filtered<- filtered[-c(1:11), ]
  filtered$current_state <- trimws(as.character(filtered$current_state))
  filtered <- filtered[filtered$current_state %in% states, ]
  filtered$year <- year_value
  return(filtered)
}

#combining year files into df
years<- 2016:2019
mapped_df <- Map(state_migrations_df,state_migrations,years)
combined_df <-do.call(rbind, mapped_df) 
View(combined_df)

#creating current, origin, and migrant columns

long_df <- pivot_longer(
  combined_df,
  cols= -c(current_state, year),
  names_to = "origin_state",
  values_to= "migrants",
)

long_df$current_state <- trimws(as.character(long_df$current_state))
long_df$origin_state <- trimws(as.character(long_df$origin_state))
long_df$migrants <- trimws(as.character(long_df$migrants))
long_df$migrants <- gsub(",", "", long_df$migrants)
long_df$migrants[long_df$migrants %in% c("N/A", "", "NA")] <- NA
long_df$migrants <- as.numeric(long_df$migrants)

#states receiving highest migration inflows
top_destinations <- long_df %>%
  group_by(current_state) %>%
  summarise(inflow = sum(migrants, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(inflow))

#state inflow changes from 2016-2019
inflow_change <- long_df %>%
  filter(origin_state != current_state) %>%
  group_by(year, current_state) %>%
  summarise(inflow = sum(migrants, na.rm = TRUE), .groups = "drop") %>%
  filter(year %in% c(2016, 2019)) %>%
  pivot_wider(names_from = year, values_from = inflow) %>%
  mutate(change = `2019` - `2016`) %>%
  arrange(desc(change))

#inflow and outflow variables
state_inflow<- long_df %>%
  filter(origin_state != current_state) %>%
  group_by(current_state) %>%
  summarise(inflow= sum(migrants, na.rm = TRUE), .groups = "drop") %>%
  rename(state= current_state)

state_outflow<- long_df %>%
  filter(origin_state != current_state) %>%
  group_by(origin_state) %>%
  summarise(outflow= sum(migrants, na.rm = TRUE), .groups = "drop") %>%
  rename(state= origin_state)

#calculating net migration
state_net_migration <- full_join(state_inflow, state_outflow, by = "state") %>%
  mutate(
    inflow = replace_na(inflow, 0),
    outflow = replace_na(outflow, 0),
    net_migration = inflow - outflow
  ) %>%
  arrange(desc(net_migration))

#defining rust belt
rust_belt <- c(
  "Illinois","Indiana","Michigan","Ohio",
  "Pennsylvania","Wisconsin","New York"
)

#rust belt inflow, outflow, net migrations

rust_belt_inflow <- long_df %>%
  filter(current_state %in% rust_belt) %>%
  filter(origin_state != current_state) %>%
  group_by(year, state= current_state) %>%
  summarise(total_in = sum(migrants, na.rm = TRUE), .groups = "drop")

rust_belt_outflow <- long_df %>%
  filter(origin_state %in% rust_belt) %>%
  filter(origin_state != current_state) %>%
  group_by(year, state= origin_state) %>%
  summarise(total_out= sum(migrants, na.rm = TRUE), .groups = "drop") 

#rust_belt_net 
rust_belt_net <- full_join(
  rust_belt_inflow,
  rust_belt_outflow,
  by= c("year", "state")
) %>%
  mutate(
    total_in = replace_na(total_in, 0),
    total_out= replace_na(total_out, 0),
    net_migration= total_in-total_out
  )


#top states by net migration
top_states <- state_net_migration %>%
  slice_max(net_migration, n=5)

#Michigan, California, Florida analysis
focus_states <- c("Michigan", "California", "Florida")

focus_data <- long_df %>%
  filter(current_state %in% focus_states | origin_state %in% focus_states)

focus_flows <- long_df %>%
  filter(
    (current_state %in% focus_states & origin_state != current_state) |
      (origin_state %in% focus_states & origin_state != current_state)
  ) %>%
  group_by(origin_state, current_state) %>%
  summarise(migrants = sum(migrants, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    flow_type = case_when(
      current_state %in% focus_states ~ "Inflow",
      origin_state %in% focus_states ~ "Outflow"
    )
  )

top_focus_flows <- focus_flows %>%
  group_by(flow_type) %>%
  slice_max(order_by = migrants, n=15, with_ties= FALSE) %>%
  ungroup()


us_states <- states(cb = TRUE, year = 2020) %>%
  st_as_sf() %>%
  select(NAME, geometry)

data_states <- c(state.name, "District of Columbia")

state_points <- us_states %>%
  st_transform(4326) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(NAME, lon, lat)

top_flow_coord <- top_focus_flows %>%
  left_join(state_points, by= c("origin_state"= "NAME")) %>%
  rename(lon_from= lon, lat_from = lat) %>%
  left_join(state_points, by= c("current_state" = "NAME")) %>%
  rename(lon_to= lon, lat_to = lat) %>%
  filter(!is.na(lon_from), !is.na(lon_to), !is.na(lat_from), !is.na(lat_to))

#VISUALIZATIONS

# chloropleth of US net migration

net_migration_map <- us_states %>%
  left_join(state_net_migration, by= c("NAME"= "state"))

ggplot(net_migration_map) +
  geom_sf(aes(fill=net_migration), color= "black", linewidth=0.2) +
  scale_fill_gradient2(
    low= "red",
    mid= "white",
    high= "navy",
    midpoint= 0,
    labels= comma,
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand= FALSE) +
  labs(
    title= "Net Migration by State (2016-2019)",
    fill= "Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title= element_blank(),
    axis.text= element_blank()
  )


#migration flows in and out of our three states

ggplot() +
  geom_sf(data = us_states, fill = "lightgrey", color = "black", linewidth = 0.2) +
  geom_curve(
    data = top_flow_coord,
    aes(
      x = lon_from, y = lat_from,
      xend = lon_to, yend = lat_to,
      linewidth = migrants,
      color= flow_type
    ),
    curvature = 0.2,
    alpha = 0.7,
    arrow= arrow(length= unit(0.12, "inches"))
  ) +
  geom_point(
    data = state_points %>% filter(NAME %in% focus_states),
    aes(x = lon, y = lat),
    size = 3,
    color = "black"
  ) +
  coord_sf(xlim= c(-125, -66), ylim= c(24,50), expand= FALSE)+
  scale_linewidth(range = c(0.8, 3)) +
  labs(
    title = "Migration Flows in and out of Florida, Michigan, and California (2016–2019)",
    linewidth = "Migrants",
    color= "Flow Type"
  ) +
  theme_minimal() +
  theme(
    axis.title= element_blank(),
    axis.text= element_blank(),
  )

#rust belt net visual

rust_net_migration_map <- us_states %>%
  left_join(state_net_migration, by= c("NAME"= "state")) %>%
  filter(NAME %in% rust_belt)
ggplot(rust_net_migration_map)+
  geom_sf(aes(fill= net_migration), color = "black", linewidth= 0.3)+
  scale_fill_gradient2(
    low= "red",
    mid= "white",
    high= "navy",
    midpoint= 0,
    labels= scales::comma,
  ) +
  labs(
    title= " Net Migration by State- Rust Belt (2016-2019)",
    fill= "Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title= element_blank(),
    axis.text= element_blank()
  )