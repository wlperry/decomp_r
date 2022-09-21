# LIBRARIES ----
library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(car)
library(emmeans)
library(patchwork)

# READ IN THE DECOMP DATA ----
# read in file 
decomp.df <- read_csv("data/decomp_biomass.csv") %>% clean_names()

# CLEAN DECOMP DATA ----
decomp.df <- decomp.df %>%
  arrange(desc(soil_block), spp, row, days)

# make row a number
decomp.df <- decomp.df %>% 
  mutate(row_no = case_when(
    row == "A" ~ 1,
    row == "B" ~ 2,
    row == "C" ~ 3,
    row == "D" ~ 4,
    row == "E" ~ 5,
    row == "F" ~ 6,
    row == "G" ~ 7,
    row == "H" ~ 8,
    row == "I" ~ 9,
    row == "J" ~ 10,
    TRUE ~ 99999
  ) )

# math modifications to get pct mass remaining  
decomp.df <- decomp.df %>% 
  mutate(pct_mass_remain = ((coll_wt_g/ initial_wt_g)*100)) %>% 
  mutate(soil_block = as.factor(soil_block),
         spp = as.factor(spp))

# remove missing values 
decomp.df <- decomp.df %>% 
  filter(!is.na(initial_wt_g))

# correct for day 0 mass remaining  
corr_t0.df <- decomp.df %>%
  filter(days == 0) %>%
  group_by(spp) %>%
  summarize(pct_mass_remain = 100 - mean(pct_mass_remain, na.rm=TRUE))

# correct mass remaining from time 0 
# Do we adjust all masses to this correction 
decomp.df <- decomp.df %>%
  mutate(pct_mass_remain_corr = case_when(
    spp == "PC"    & days == 0  ~ pct_mass_remain + 2.0582047,
    spp == "GM_PC" & days == 0  ~ pct_mass_remain + -0.2269507,
    spp == "AR"    & days == 0  ~ pct_mass_remain + 7.1806311,
    spp == "CR"    & days == 0  ~ pct_mass_remain + 7.4213231,
    TRUE ~ pct_mass_remain
  ))

# set proportion column
decomp.df <- decomp.df %>%
  mutate(prop_mass_remain = pct_mass_remain/100)

# PLOT TO LOOK AT THE NEGATIVE EXPONENTIALS ----
decomp.df %>%
  # filter(soil_block ==2) %>% 
  ggplot(mapping = aes(days, pct_mass_remain, color = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  theme_classic() +
  geom_smooth( aes(x = days, y = pct_mass_remain, color=spp),
    method = "nls", formula = y ~ 100 * exp(-k*x), 
    method.args = list(start = c(k=0.001)), se = FALSE)

# decomp.df %>%
#   filter(soil_block ==1) %>% 
#   ggplot(mapping = aes(days, pct_mass_remain, color = spp)) +
#   geom_point()+
#   stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
#   theme_classic()



# cr.df <- decomp.df %>% 
#   filter(soil_block ==2) %>% 
#   filter(spp=="CR") %>% 
#   filter(row_no ==1)
# 
# nonlin = nls(pct_mass_remain ~ 100 * exp(-k*days), trace=TRUE, start = list(k = .01), data=cr.df)

# this does all of the regressions for each row and species and soil block
# saves output to k_linear.df
# now to put this in the output

k_nonlinear.df <- decomp.df %>%
  filter(spp %in% c("PC", "GM_PC", "AR", "CR")) %>%
  # filter(soil_block %in% c(1,2) ) %>%
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
k_nonlinear.df <- k_nonlinear.df %>% 
  arrange(soil_block, spp, row_no)

# now to get only the k term
k_nonlinear_summary.df <- k_nonlinear.df %>% 
  select (soil_block, spp, row_no, term, estimate, std.error, statistic, p.value) 

write_csv(k_nonlinear_summary.df, "output/biomass_nonlinear_k_values.csv")

k_nonlinear_summary.df %>% 
  ggplot(aes(spp, estimate, color=spp))+
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width=.2) +
  labs(x="Species", y="Decay coefficient (k)") 

