library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tmap)
library(sf)
library(tigris)
library(stringr)

#reading data, accessing multiple sheets for each state, renaming columns to those I need
file_path <- "/Users/niaallen/Downloads/county-to-county-2016-2020-ins-outs-nets-gross.xlsx"

sheets <- excel_sheets(file_path)

full_county_data<-lapply(sheets, function(sheet) {
  df <- read_excel(file_path, sheet=sheet)
  df <- df[-c(1,2), ]
  county_flows_clean <- df %>%
    rename(
      state_from = ...5,
      county_from = ...6,
      state_to = ...7,
      county_to = ...8,
      inflow = ...9,
      outflow = ...11,
      net_migration = ...13
    ) %>%
    select(state_from, county_from, state_to, county_to, inflow, outflow, net_migration)
  
  return(county_flows_clean)
})

full_county_data <- bind_rows(full_county_data)

#filtering out NAs
full_county_data <- full_county_data %>%
  mutate(across(where(is.character), ~ na_if(., "-"))) %>%
  drop_na()

#converting characters to numbers
full_county_data <- full_county_data %>%
  mutate(
    inflow= as.numeric(inflow),
    outflow= as.numeric(outflow),
    net_migration= as.numeric(net_migration)
  )

#ANALYSIS

#counties people are migrating to overall
top_counties_in <- full_county_data%>%
  group_by(state_from, county_from) %>%
  summarise(c_inflow = sum(inflow, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(c_inflow))

head(top_counties_in, 10)

#counties people are leaving from overall
top_counties_out <- full_county_data%>%
  group_by(state_from, county_from) %>%
  summarise(c_outflow = sum(outflow, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(c_outflow))

head(top_counties_out, 10)

#net migration by county (gain or loss for counties)
net_migration_counties <- full_county_data %>%
  group_by(state_from, county_from) %>%
  summarise(net= sum(net_migration, na.rm= TRUE), .groups = "drop") %>%
  arrange(desc(net))
head(net_migration_counties, 10)

#defining rust_belt
rust_belt <- c(
  "Illinois","Indiana","Michigan","Ohio",
  "Pennsylvania","Wisconsin","New York"
)

#county net for rust_belt
rust_belt_net <- full_county_data %>%
  filter(state_from %in% rust_belt) %>%
  group_by(state_from, county_from) %>%
  summarise(net= sum(net_migration, na.rm= TRUE), .groups = "drop") %>%
  arrange(desc(net))

#top 10 counties
head(rust_belt_net, 10)
#lowest 10 counties
tail(rust_belt_net, 10)

#VISUALIZATIONS

#county net migration map (static)
#redder/peachy tones showing counties losing people
#more purple tones showing counties gaining people

options(tigris_use_cache= TRUE)

counties_in_us <- counties(cb= TRUE, year = 2020) %>%
  st_as_sf() %>%
  select(NAME, STATE_NAME, geometry)

states_in_us <- states(cb= TRUE, year = 2020) %>%
  st_as_sf() 

net_migration_counties_clean <- net_migration_counties %>%
  mutate(
    county_from = str_remove(county_from, " County$"),
    county_from = str_remove(county_from, " Parish$"),
    county_from = str_remove(county_from, " Borough$"),
    county_from = str_remove(county_from, " Census Area$"),
    county_from = str_remove(county_from, " Municipality$"),
    county_from = str_remove(county_from, " City and Borough$")
  )

county_map <- counties_in_us %>%
  left_join(
    net_migration_counties_clean,
    by = c("STATE_NAME" = "state_from", "NAME" = "county_from")
  )

ggplot(county_map) +
  geom_sf(aes(fill = net), color = NA) +
  geom_sf(data= states_in_us, fill= NA, color= "black", linewidth= 0.2) +
  scale_fill_steps2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    na.value= "lightgrey"
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(
    title = "Net Migration by U.S County, (2016-2020)",
    fill = "Net Migrants"
  ) +
  theme_minimal()+
  theme(
    axis.title = element_blank()
  )

#interactive net migration map

# red means large net outflow (more people left than moved)
#pink means moderate outflow
#white means little change (inflow and outflow about the same)
# light blue shades represents smaller net inflow
#darker blue represents larger net inflow 


county_map_filt <- county_map %>%
  filter(!STATE_NAME %in% c("Alaska", "Hawaii"))

tmap_mode("view")
tm_shape(county_map_filt) +
  tm_polygons(
    col= "net",
    palette = c("red", "pink", "white", "lightblue", "blue"),
    style= "fixed",
    title= "Net Migrants",
    breaks = c(-Inf, -10000, -1000, 1000, 10000, Inf),
    id= "NAME",
    popup.vars= c(
      "STATE_NAME",
      "County"= "NAME",
      "Net Migration" = "net")
    ) 

#rust belt net migration map interactive

# red means large net outflow (more people left than moved)
#pink means moderate outflow
#white means little change (inflow and outflow about the same)
# light blue shades represents smaller net inflow
#darker blue represents larger net inflow 

rust_belt_clean <- rust_belt_net %>%
  mutate(county_from = str_remove(county_from, " County$"),
         county_from = str_remove(county_from, " Parish$"),
         county_from = str_remove(county_from, " Borough$"),
         county_from = str_remove(county_from, " Census Area$"),
         county_from = str_remove(county_from, " Municipality$"),
         county_from = str_remove(county_from, " City and Borough$")
  )

counties_in_us <- counties(cb= TRUE, year = 2020) %>%
  st_as_sf() %>%
  select(NAME, STATE_NAME, geometry)

rust_belt_county_map <- counties_in_us %>%
  filter(STATE_NAME %in% rust_belt) %>%
  left_join(
    rust_belt_clean,
    by= c("STATE_NAME"= "state_from", "NAME" = "county_from")
  )

rust_county_map_filt <- rust_belt_county_map %>%
  filter(!STATE_NAME %in% c("Alaska", "Hawaii"))

tmap_mode("view")
tm_shape(rust_county_map_filt) +
  tm_polygons(
    col= "net",
    palette = c("red", "pink", "white", "lightblue", "blue"),
    style= "fixed",
    title= "Net Migration in Rust Belt Counties, (2016-2020)",
    breaks = c(-Inf, -10000, -1000, 1000, 10000, Inf),
    id= "NAME",
    popup.vars= c(
      "STATE_NAME",
      "County"= "NAME",
      "Net Migration" = "net")
  ) 
