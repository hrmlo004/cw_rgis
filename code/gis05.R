if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)


# crop --------------------------------------------------------------------

## us-wide precipitation layer
(spr_prec <- rast(here("data/spr_prec_us.tif")))

## visualization
ggplot() +
  geom_spatraster(data = spr_prec)

## ext return the extent of the layer
ext(spr_prec)

## crop function, direct enrty of lat/lon
## the order matters: c(xmin, xmax, ymin, ymax)
(spr_prec_crop <- crop(x = spr_prec,
     y = c(-80, -75, 34, 37)))
ext(spr_prec_crop)

## check coverage visually
## load county vector 
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

ggplot() +
  geom_spatraster(data = spr_prec_crop) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

## use vector layer as a mask layer
## there is no need to enter raw lat/lon values directly
## crop function extracts the extent from the vector layer
spr_prec_nc <- crop(x = spr_prec,
                    y = sf_nc_county)

ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)


# merge -------------------------------------------------------------------

spr_nw <- rast(here("data/spr_prec_ncnw.tif")) # Northwest NC
spr_ne <- rast(here("data/spr_prec_ncne.tif")) # Northeast NC
spr_sw <- rast(here("data/spr_prec_ncsw.tif")) # Southwest NC
spr_se <- rast(here("data/spr_prec_ncse.tif")) # Southeast NC

## visualize northwest
ggplot() +
  geom_spatraster(data = spr_nw) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

## use merge() function
spr_n <- merge(spr_nw, spr_ne)

ggplot() +
  geom_spatraster(data = spr_n) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

## compare extent b/n spr_nw and spr_n
ext(spr_nw)
ext(spr_n)

## merge multiple raster layers
## 1st step: create a list of raster lvls
(list_spr <- list(spr_ne,
     spr_nw,
     spr_se,
     spr_sw))

(spr_col <- sprc(list_spr))

spr_merge <- merge(spr_col)

ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)


# export ------------------------------------------------------------------

writeRaster(spr_merge, 
            filename = here("data/spr_prec_nc.tif"),
            overwrite = TRUE)

# stack -------------------------------------------------------------------

spr_prec_nc <- rast(here("data/spr_prec_nc.tif"))
spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif"))

(spr_pt_nc <- c(spr_prec_nc,
               spr_tmp_nc))

## to acess each layer separately
spr_pt_nc$precipitation
spr_pt_nc$temperature


# reprojection ------------------------------------------------------------

print(spr_prec_nc)

## reprojection for raster
(spr_prec_nc_proj <- project(x = spr_prec_nc,
                             y = "EPSG:32617"))

## a "safer" way to reproject
(spr_prec_nc_proj <- project(x = spr_prec_nc,
                             y = "EPSG:32617",
                             method = "bilinear"))


# Exercise ----------------------------------------------------------------

# 1) Merge raster files
spr_nw <- rast(here("data/spr_prec_ncnw.tif"))
spr_ne <- rast(here("data/spr_prec_ncne.tif"))
spr_sw <- rast(here("data/spr_prec_ncsw.tif"))
spr_se <- rast(here("data/spr_prec_ncse.tif"))

(list_spr <- list(spr_ne,
                  spr_nw,
                  spr_se,
                  spr_sw))

(spr_col <- sprc(list_spr))

spr_merge <- merge(spr_col)

ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

# 2) Crop raster to a defined extent

sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

sf_camdem <- filter(sf_nc_county, county == "camden")

ext(sf_camdem)

spr_tmp_camden <- crop(x = spr_merge,
                       y = sf_camdem)

ggplot() +
  geom_spatraster(data = spr_tmp_camden) +
  geom_sf(data = sf_camdem,
          alpha = 0.25)

# 3) Reproject raster and explore resampling
(spr_tmp_camdem_proj <- project(x = spr_tmp_camden,
                              y = "EPSG:32618",
                              method = "bilinear"))
