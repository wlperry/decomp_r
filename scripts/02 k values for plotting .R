# LIBRARIES ----
library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(car)
library(emmeans)
library(patchwork)

# TOTAL BIOMASS ----

# read in the data ----
total_biomass.df <- 
  read_csv("output/cleaned data and estimates of k/from import and clean/total biomass.csv")

# make column for proportion 
total_biomass.df <- total_biomass.df %>%
  mutate(prop_mass_remain = pct_mass_remain/100)

# plot the negative exponentials ----
total_biomass.df %>%
  # filter(soil_block ==2) %>% 
  ggplot(mapping = aes(days, pct_mass_remain, color = spp)) +
  # stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_point(position = position_dodge2(width=5))+
  theme_classic() +
  geom_smooth( aes(x = days, y = pct_mass_remain, color=spp),
               method = "nls", formula = y ~ 100 * exp(-k*x), 
               method.args = list(start = c(k=0.001)), se = FALSE)

# calculate k values----

k_nonlin_biomass.df <- total_biomass.df %>%
  filter(spp %in% c("PC", "GM_PC", "AR", "CR")) %>%
  filter(soil_block %in% c(1,2) ) %>%
  nest(data = -c(spp, soil_block)) %>%
  mutate(
    fit = map(data, ~nls(pct_mass_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k=0.001), 
                         data = .x))
  ) 

# pull out fitted values for plotting
biomass_predicted.df <- k_nonlin_biomass.df %>% 
  mutate(augmented = map(fit, augment)) %>% 
  unnest(augmented)

# get rid of the data and fit columns
biomass_predicted.df <- biomass_predicted.df %>%
  select(spp, soil_block, days, pct_mass_remain, .fitted, .resid)

# save values for plotting
write_csv(biomass_predicted.df, file = 
            "output/cleaned data and estimates of k/k values/for plotting/fitted biomass.csv")


# plot the values ----
biomass.plot <- biomass_predicted.df %>% 
  ggplot(aes(days, pct_mass_remain, color=spp))+
  geom_point() +
  labs(x="Species", y="Biomass Decay coefficient (k)") +
  theme_classic()

biomass.plot

# NITROGEN ----

# read in the data for both carbon and nitrogen ----
nutrient.df <-
  read_csv("output/cleaned data and estimates of k/from import and clean/nutrients.csv")

# plot the negative exponentials ----
nutrient.df %>%
  # filter(soil_block ==2) %>% 
  ggplot(mapping = aes(days, pct_n_remain, color = spp)) +
  # stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_point(position = position_dodge2(width=5))+
  theme_classic() +
  geom_smooth( aes(x = days, y = pct_n_remain, color=spp),
               method = "nls", formula = y ~ 100 * exp(-k*x), 
               method.args = list(start = c(k=0.001)), se = FALSE)

# calculate k values----

k_nonlin_nitrogen.df <- nutrient.df %>%
  filter(spp %in% c("PC", "GM_PC", "AR", "CR")) %>%
  # filter(soil_block %in% c(1,2) ) %>%
  nest(data = -c(spp, soil_block)) %>%
  mutate(
    fit = map(data, ~nls(pct_n_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k=0.001), 
                         data = .x))
  ) 

# pull out fitted values for plotting
nitrogen_predicted.df <- k_nonlin_nitrogen.df %>% 
  mutate(augmented = map(fit, augment)) %>% 
  unnest(augmented)

# get rid of the data and fit columns
nitrogen_predicted.df <- nitrogen_predicted.df %>%
  select(spp, soil_block, days, pct_n_remain, .fitted, .resid)

# save values for plotting
write_csv(nitrogen_predicted.df, file = 
            "output/cleaned data and estimates of k/k values/for plotting/fitted nitrogen.csv")


# plot the values ----
nitrogen.plot <- nitrogen_predicted.df %>% 
  ggplot(aes(days, pct_n_remain, color=spp))+
  geom_point() +
  labs(x="Species", y="Biomass Decay coefficient (k)") +
  theme_classic()

nitrogen.plot

# CARBON ----

# plot the negative exponentials ----
nutrient.df %>%
  # filter(soil_block ==2) %>% 
  ggplot(mapping = aes(days, pct_c_remain, color = spp)) +
  # stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_point(position = position_dodge2(width=5))+
  theme_classic() +
  geom_smooth( aes(x = days, y = pct_c_remain, color=spp),
               method = "nls", formula = y ~ 100 * exp(-k*x), 
               method.args = list(start = c(k=0.001)), se = FALSE)

# calculate k values----

k_nonlin_carbon.df <- nutrient.df %>%
  filter(spp %in% c("PC", "GM_PC", "AR", "CR")) %>%
  filter(soil_block %in% c(1,2) ) %>%
  nest(data = -c(spp, soil_block)) %>%
  mutate(
    fit = map(data, ~nls(pct_c_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k=0.001), 
                         data = .x))
  ) 

# pull out fitted values for plotting
carbon_predicted.df <- k_nonlin_carbon.df %>% 
  mutate(augmented = map(fit, augment)) %>% 
  unnest(augmented)

# get rid of the data and fit columns
carbon_predicted.df <- carbon_predicted.df %>%
  select(spp, soil_block, days, pct_c_remain, .fitted, .resid)

# save values for plotting
write_csv(carbon_predicted.df, file = 
            "output/cleaned data and estimates of k/k values/for plotting/fitted carbon.csv")


# plot the values ----
carbon.plot <- carbon_predicted.df %>% 
  ggplot(aes(days, pct_c_remain, color=spp))+
  geom_point() +
  labs(x="Species", y="Biomass Decay coefficient (k)") +
  theme_classic()

carbon.plot

# put the plots together to see how things look ----
biomass.plot + nitrogen.plot + carbon.plot + plot_layout(guides = "collect")







