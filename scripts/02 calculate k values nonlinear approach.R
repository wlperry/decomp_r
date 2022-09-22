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
  read_csv("output/final files from import and k calcs/from import and clean/total biomass.csv")

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

# nonlinear equation THIS IS FOR PERCENT MASS REMAINING WITH IT BEING A FUNCTION OF 
# 100, PROP WOULD BE 1 
k_nonlin_biomass.df <- total_biomass.df %>%
   filter(spp %in% c("PC", "GM_PC", "AR", "CR")) %>%
   filter(soil_block %in% c(1,2) ) %>%
  nest(data = -c(spp, row_no, soil_block)) %>%
  mutate(
    fit = map(data, ~nls(pct_mass_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k=0.001), 
                         data = .x))
  ) %>%
  gather(name, model, fit) %>%        # <--- consolidate before tidying
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# arrange the data to see easier 
k_nonlin_biomass.df <- k_nonlin_biomass.df %>% 
  arrange(soil_block, spp, row_no)

# now to get only the k term
k_nonlin_biomass_summary.df <- k_nonlin_biomass.df %>% 
  select (soil_block, spp, row_no, term, estimate, std.error, statistic, p.value) 

# save output ----

write_csv(k_nonlin_biomass_summary.df, file =
            "output/final files from import and k calcs/k values/k nonlin biomass summary.csv")

# plot the values ----
biomass.plot <- k_nonlin_biomass_summary.df %>% 
  ggplot(aes(spp, estimate, color=spp))+
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width=.2) +
  labs(x="Species", y="Biomass Decay coefficient (k)") +
  theme_classic()

biomass.plot

# NITROGEN ----

# read in the data for both carbon and nitrogen ----
nutrient.df <-
  read_csv("output/final files from import and k calcs/from import and clean/nutrients.csv")

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

# nonlinear equation THIS IS FOR PERCENT MASS REMAINING WITH IT BEING A FUNCTION OF 
# 100, PROP WOULD BE 1 
k_nonlin_nitrogen.df <- nutrient.df %>%
  filter(spp %in% c("PC", "GM_PC", "AR", "CR")) %>%
  filter(soil_block %in% c(1,2) ) %>%
  nest(data = -c(spp, row, soil_block)) %>%
  mutate(
    fit = map(data, ~nls(pct_n_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k=0.001), 
                         data = .x))
  ) %>%
  gather(name, model, fit) %>%        # <--- consolidate before tidying
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# arrange the data to see easier 
k_nonlin_nitrogen.df <- k_nonlin_nitrogen.df %>% 
  arrange(soil_block, spp, row)

# now to get only the k term
k_nonlin_nitrogen_summary.df <- k_nonlin_nitrogen.df %>% 
  select (soil_block, spp, row, term, estimate, std.error, statistic, p.value) 

# save output ----

write_csv(k_nonlin_nitrogen_summary.df, file =
            "output/final files from import and k calcs/k values/k nonlin nitrogen summary.csv")

nitrogen.plot <- k_nonlin_nitrogen_summary.df %>% 
  ggplot(aes(spp, estimate, color=spp))+
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width=.2) +
  labs(x="Species", y="Nitrogen Decay coefficient (k)") +
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

# nonlinear equation THIS IS FOR PERCENT MASS REMAINING WITH IT BEING A FUNCTION OF 
# 100, PROP WOULD BE 1 
k_nonlin_carbon.df <- nutrient.df %>%
  filter(spp %in% c("PC", "GM_PC", "AR", "CR")) %>%
  filter(soil_block %in% c(1,2) ) %>%
  nest(data = -c(spp, row, soil_block)) %>%
  mutate(
    fit = map(data, ~nls(pct_c_remain ~ 100 * exp(-k*days), trace =TRUE, 
                         start = list(k=0.001), 
                         data = .x))
  ) %>%
  gather(name, model, fit) %>%        # <--- consolidate before tidying
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# arrange the data to see easier 
k_nonlin_carbon.df <- k_nonlin_carbon.df %>% 
  arrange(soil_block, spp, row)

# now to get only the k term
k_nonlin_carbon_summary.df <- k_nonlin_carbon.df %>% 
  select (soil_block, spp, row, term, estimate, std.error, statistic, p.value) 

# save output ----

write_csv(k_nonlin_carbon_summary.df, file =
            "output/final files from import and k calcs/k values/k nonlin carbon summary.csv")

carbon.plot <- k_nonlin_carbon_summary.df %>% 
  ggplot(aes(spp, estimate, color=spp))+
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width=.2) +
  labs(x="Species", y="Carbon Decay coefficient (k)") +
  theme_classic()

carbon.plot

biomass.plot + nitrogen.plot + carbon.plot + plot_layout(guides = "collect")







