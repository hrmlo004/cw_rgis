
if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview,
               here)

# spatial join ------------------------------------------------------------

## point vector
sf_site <- readRDS(here("data/sf_finsync_nc.rds"))
sf_site

## polygon vector
sf_nc_county <- readRDS(file = here("data/sf_nc_county.rds"))
sf_nc_county

## st_join() evaluates two geometry layers
sf_site_join <- st_join(x = sf_site, 
                        y = sf_nc_county)
sf_site_join

# check how it works
sf_one <- sf_site %>% 
  slice(1)
sf_one

mapview(sf_nc_county) + mapview(sf_one)

sf_site_join %>% slice(1)

# get data by county
sf_site_guilford <- sf_site_join %>% 
  filter(county == "guilford")
sf_site_guilford

sf_nc_guilford <- sf_nc_county %>% 
  filter(county == "guilford")
sf_nc_guilford

sf_str_guilford <- readRDS(here("data/sf_stream_gi.rds"))
sf_str_guilford

# create a map
ggplot() +
  geom_sf(data = sf_nc_guilford) +
  geom_sf(data = sf_str_guilford,
          color = "steelblue") +
  geom_sf(data = sf_site_guilford,
          color = "forestgreen")

# count the number of sites in each county
# identify the county that has the most sites
## function "n()" many be useful to count # of rows in each group
## "group_by" county & summarize, giving the column the name "n_site"
## "as_tibble" to make it clean & "arrange" have the max up top

df_n <- sf_site_join %>% 
  as_tibble() %>% 
  group_by(county) %>% 
  summarize(n_site = n()) %>% 
  arrange(desc(n_site))
df_n

## "sf_nc_county" ~ this is a "geospatial" object
## "df_n" ~ the number of sites by county
## combine them with left_join()
## assign to "sf_nc_n"

sf_nc_n <- sf_nc_county %>% 
  left_join(df_n, 
            by = "county") %>% 
  mutate(n_site = ifelse(is.na(n_site),
                         0,
                         n_site))
sf_nc_n

## mapping
ggplot() +
  geom_sf(data = sf_nc_n,
          aes(fill = n_site))

# geometric analysis ------------------------------------------------------

# length calculation
## change CRS first!
sf_str_guilford
sf_str_proj <- st_transform(sf_str_guilford,
                            crs = 32617)
sf_str_proj

v_str_l <- st_length(sf_str_proj)
v_str_l
head(v_str_l)


sf_str_w_len <- sf_str_guilford %>% 
  mutate(length = as.numeric(v_str_l))
sf_str_w_len

ggplot() + 
  geom_sf(data = sf_str_w_len,
          aes(color = length))

# area calculation
sf_nc_county_proj <- st_transform(sf_nc_county, crs = 32617)
sf_nc_county_proj

v_area <- st_area(sf_nc_county_proj)
v_area

sf_nc_county_w_area <- sf_nc_county %>% 
  mutate(area = as.numeric(v_area))
sf_nc_county_w_area

## draw a map
ggplot() + 
  geom_sf(data = sf_nc_county_w_area, 
          aes(fill = area))


# Exercise ----------------------------------------------------------------

#  Spatial join of survey sites and counties
sf_quakes <- readRDS(here("data/sf_quakes.rds"))
sf_quakes
sf_nz <- readRDS(here("data/sf_nz.rds"))
sf_nz
mapview(sf_nz) + mapview(sf_quakes)
sf_quakes_join <- st_join(x = sf_quakes,
                          y = sf_nz)
sf_quakes_join
sf_quakes_nz <- drop_na(sf_quakes_join, fid)
sf_quakes_nz
nrow(sf_quakes_nz)
## 3 rows!

# Count survey sites per county
sf_n_site <- sf_site_join %>% 
  group_by(county)%>% 
  summarize(n_site = n())
sf_n_site

# Subset counties with more than ten sites
sf_n10 <- sf_n_site %>% 
  filter(n_site > 10)
sf_n10 

# Visualize the distribution on a stacked map
ggplot()+
  geom_sf(data = sf_n_site,
          color = "grey") +
  geom_sf(data = sf_n10,
          color = "salmon")