if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview,
               here)

## read fish data
df_fish <- read_csv(here::here("data/data_finsync_nc.csv"))

df_fish
sf_site <- df_fish %>% 
  distinct(site_id, 
           lon, 
           lat) %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)
df_fish
sf_site

## data on the map
mapview(sf_site,
        legend = FALSE)

## export the data
saveRDS(sf_site,
        file = here::here("data/sf_finsync_nc.rds"))

# conversion from geodetic to projected
sf_site %>% 
  slice(c(1, 2))
sf_ft_wgs <- sf_site %>% 
  slice(c(1, 2))
sf_ft_wgs
sf_ft_utm <- sf_ft_wgs %>% 
  st_transform(crs = 32617)

mapview(sf_ft_wgs)
st_distance(sf_ft_utm)

# exercise
## load data
quakes
## lat and long represent latitude and longitude
df_quakes <- as_tibble(quakes)
df_quakes
## convert to an 'sf' object
sf_quakes <- df_quakes %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)
## map 'sf_quakes'
mapview(sf_quakes)

st_bbox(sf_quakes)

## select the first two sites
sf_ft_quakes <- sf_quakes %>% 
  slice(c(1, 2))
sf_ft_quakes

## convert geodetic CRS into projected CRS (UTM 60S)
sf_ft_quakes_proj <- sf_ft_quakes %>% 
  st_transform(crs = 32760)
sf_ft_quakes_proj

## calculate geographic distance
st_distance(sf_ft_quakes_proj)
st_distance(sf_ft_quakes)

## export
saveRDS(sf_quakes, file = here::here("data/sf_quakes.rds"))

