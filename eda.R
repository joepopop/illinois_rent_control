# load pacakges ----
library(tidyverse)
library(skimr)
library(sf)
library(leaflet)

# load data ----

# provided data
basic_dat <- read_csv("data/basic_dat.csv") %>% 
  janitor::clean_names() %>% 
  rename(properties_owned = properties_held_by_taxpayer_match_code) %>% 
  rename(address = property_address) %>% 
  select(address, community_area, taxpayer_match_code, lat, long, properties_owned) 

# illinois community area data from web
map_dat <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") %>% 
  rename(community_area = community) %>% 
  mutate(community_area = tools::toTitleCase(tolower(community_area))) 

# combined data
dat <- basic_dat %>% 
  left_join(map_dat)


# eda ----

# missingness and summary
basic_dat %>% 
  skim_without_charts()

# gegraphic distribution
dat %>% 
  group_by(community_area) %>% 
  mutate(count = n()) %>% 
  select(count, community_area, geometry) %>% 
  distinct(community_area, .keep_all = TRUE) %>% 
  mutate(count = cut(count, c(0, 100, 1000, Inf))) %>% 
  ggplot(aes(fill = count, geometry = geometry)) + 
  geom_sf() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Number of properties in each community area"
  )