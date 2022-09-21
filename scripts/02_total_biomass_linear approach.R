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

# PLOT TO LOOK AT THE NEGATIVE EXPONENTIALS ----
decomp.df %>%
  mutate(spp = as.factor(spp)) %>% 
  mutate(spp = fct_reorder2(spp, days, pct_mass_remain)) %>%
  ggplot(mapping = aes(log(days), log(pct_mass_remain), color = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()

decomp.df %>%
  mutate(spp = as.factor(spp)) %>% 
  mutate(spp = fct_reorder2(spp, days, pct_mass_remain)) %>%
  ggplot(mapping = aes(log(days), log(pct_mass_remain), color = interaction(spp, soil_block))) +
  # stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_point()+
  geom_smooth(method="lm", se=FALSE) 


# filter out day 0 and do log transformations
decomp_log.df <- decomp.df %>% 
  filter(days !=0 ) %>% 
  mutate(log_days = log(days),
         log_pct_mass_remain = log(pct_mass_remain
                                   ))

# this does all of the regressions for each row and sppecies and soil block
# saves output to k_linear.df
# now to put this in the output

k_linear.df <- decomp_log.df %>%
  nest(data=-c(spp, row_no, soil_block)) %>%
  mutate(
    fit = map(data, ~lm(log_pct_mass_remain ~ days, data = .x))
  ) %>%
  gather(name, model, fit) %>%        # <--- consolidate before tidying
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# arrange the data to see easier 
k_linear.df <- k_linear.df %>% 
  arrange(soil_block, spp, row_no)

# now to get only the k term
k_linear_summary.df <- k_linear.df %>% 
  select (soil_block, spp, row_no, term, estimate, std.error, statistic, p.value) %>% 
  filter (term=="days")

write_csv(k_linear_summary.df, "output/biomass_linear_k_values.csv")

k_linear_summary.df %>% 
  ggplot(aes(spp, estimate*-1, color=spp))+
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width=.2) +
  labs(x="Species", y="Decay coefficient (k)")
