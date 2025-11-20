if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               ggeffects,
               sf,
               terra,
               tidyterra,
               exactextractr,
               mapview,
               here)


# fish data ---------------------------------------------------------------
(df_finsync <- read_csv(here("data/data_finsync_nc.csv")))

# selecting only "Catostomus commersonii"
df_finsync %>% 
  filter(latin == "Catostomus commersonii")

(df_white_sucker <- df_finsync %>% 
    mutate(presence = 1) %>% 
    pivot_wider(id_cols = c(site_id, lon, lat),
              names_from = latin,
              values_from = presence,
              values_fill = 0) %>% 
    select(site_id, 
           lon, 
           lat, 
           "Catostomus commersonii") %>% 
    rename(y = "Catostomus commersonii"))

# creating it as an sf object
(sf_white_sucker <- st_as_sf(df_white_sucker, 
                             coords = c("lon", "lat"), 
                             crs = 4326))

# air temperature data ----------------------------------------------------
(spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif")))

# extracting the air temperature data from the fish data
(sf_w_s_tmp <- extract(x = spr_tmp_nc,
        y = sf_white_sucker,
        bind = TRUE) %>% 
  st_as_sf())


# mapping -----------------------------------------------------------------
ggplot() +
  geom_spatraster(data = spr_tmp_nc) +
  geom_sf(data = sf_w_s_tmp,
          aes(color = factor(y))) +
  scale_fill_viridis_c()


# statistical analysis ----------------------------------------------------

# creating fish-based air temperature data as a tibble
(df_w_s_tmp <- as_tibble(sf_w_s_tmp))

# drawing a figure on the presence and absence of the fish of analysis
# at each survey site based on air temperature 
df_w_s_tmp %>% 
  ggplot(aes(x = temperature,
             y = y)) +
  geom_point() +
  theme_bw()

# using the Generalized Linear Model to analyze the dataset
m_white_sucker <- glm(y ~ temperature, 
                       data = df_w_s_tmp,
                       family = "binomial")

summary(m_white_sucker)
## I believe both the intercept p-value and the temperature p-value are
## statisically non-signficant due to the coefficiens being greater than
## 0.01.

# now to visualize the data by drawing a predicted line
(df_pred_w_s <- ggpredict(m_white_sucker, 
                          terms = "temperature [all]"))
ggplot() +
  geom_point(data = df_w_s_tmp,
             aes(x = temperature,
                 y = y)) +
  geom_line(data = df_pred_w_s,
            aes(x = x,
                y = predicted)) +
  geom_ribbon(data = df_pred_w_s,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high),
              fill = "salmon",
              alpha = 0.2) +
  labs(x = "Air temperature",
       y = "Probability of occurrence") +
  theme_bw()
