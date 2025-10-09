
# Raster Data -------------------------------------------------------------

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)


# read/export raster data -------------------------------------------------

## read geotiff
spr_ex <- rast(here("data/spr_example.tif"))
spr_ex

## export geotiff
writeRaster(spr_ex,
            filename = here("data/spr_elev.tif"),
            overwrite = TRUE)

## mapping
ggplot() + 
  geom_spatraster(data = spr_ex)

## mapview function
star_ex <- st_as_stars(spr_ex)
star_ex
class(spr_ex)
class(star_ex)
mapview(star_ex)


# raster data type --------------------------------------------------------
v_elev <- values(spr_ex)
head(v_elev, 10)

na.omit(v_elev) %>% 
  mean()

## extract data from a given location
## xy specifies longitude/latitude
xy <- cbind(6.0000, 50.0000)
extract(spr_ex, xy)

## xy can be a multiple site
df_point <- tibble(lon = c(6, 5.9),
                   lat = c(50, 49.96))
df_point
extract(spr_ex,
        y = df_point)

## discrete raster
spr_for <- rast(here("data/spr_forest_nc.tif"))
spr_for

ggplot() + 
  geom_spatraster(data = spr_for)

### real quick: comparing values between forest and example
unique(spr_for)
unique(spr_ex)

v_binary <- values(spr_for)
v_binary
mean(v_binary)

## discrete, coded values
spr_land <- rast(here("data/spr_land_reclass.tif"))
unique(spr_land)

extract(spr_land, cbind(-79.8063, 36.0701))


# reclass -----------------------------------------------------------------

# create a conversion matrix
cm <- cbind(c(0, 1001, 1010, 1100),
      c(0, 1, 0, 0))
cm

# reclass 
spr_bin <- classify(spr_land,
                    rcl = cm)
unique(spr_bin)
v_bin <- values(spr_bin)
mean(v_bin)


# Exercise ----------------------------------------------------------------

# 1) Read a GeoTIFF file
spr_prec_ncne <- rast(here("data/spr_prec_ncne.tif"))
spr_prec_ncne

# 2) Inspect raster properties
spr_prec_ncne
## There are 162 rows and 532 columns.
## The size of each cell in degrees is 0.0083 by 0.0083.
## The spatial extent for the x-axis is at minimum -79.89181 and at maximum
## -75.45847. For the y-axis it is at minimum 35.24153 and at maximum 36.59153.
## The coordinate reference system is WGS 84/EPSG:4326.
## The minimum precipitation value is 1063.1 and the maximum precipitation
## value is 1501.5.

# 3) Visualize the raster
ggplot() +
  geom_spatraster(data = spr_prec_ncne)

# 4) Extract values
sf_site <- readRDS(here("data/sf_finsync_nc.rds"))
df_xy <- st_coordinates(sf_site)
df_land <- extract(spr_land, df_xy)
df_land
table(df_land)
## The most common land type use is forest

# 5) Reclassify
c_urban <- cbind(c(0, 1001, 1010, 1100),
                 c(0, 0, 0, 1))

spr_urban <- classify(spr_land, 
                      rcl = c_urban)
spr_urban
v_urban <- values(spr_urban)
mean(v_urban)
## The mean is 0.03169528