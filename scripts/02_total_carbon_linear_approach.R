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
decomp_c.df <- read_csv("data/cleaned/clean carbon.csv") %>% 
  clean_names()

# CLEAN DECOMP DATA ----

# make row a number
decomp_c.df <- decomp_c.df %>% 
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

# # math modifications to get pct mass remaining  
# decomp_c.df <- decomp_c.df %>% 
#   mutate(pct_mass_remain = ((coll_wt_g/ initial_wt_g)*100)) %>% 
#   mutate(soil_block = as.factor(soil_block),
#          spp = as.factor(spp))

# PLOT TO LOOK AT THE NEGATIVE EXPONENTIALS ----
decomp_c.df %>%
  mutate(spp = as.factor(spp)) %>% 
  # mutate(spp = fct_reorder2(spp, days, pct_c_remain)) %>%
  ggplot(mapping = aes(log(days+1), log(pct_c_remain), color = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()

decomp_c.df %>%
  mutate(spp = as.factor(spp)) %>% 
  mutate(spp = fct_reorder2(spp, days, pct_c_remain)) %>%
  ggplot(mapping = aes(log(days+1), log(pct_c_remain), color = interaction(spp, soil_block))) +
  # stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  # stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_point()+
  geom_smooth(method="lm", se=FALSE) 


# filter out day 0 and do log transformations
decomp_log.df <- decomp_c.df %>% 
  filter(days !=0 ) %>% 
  mutate(log_days = log(days+1),
         log_pct_c_remain = log(pct_c_remain))

# this does all of the regressions for each row and sppecies and soil block
# saves output to k_linear.df
# now to put this in the output

k_linear.df <- decomp_log.df %>%
  nest(data=-c(spp, row_no, soil_block)) %>%
  mutate(
    fit = map(data, ~lm(log_pct_c_remain ~ days, data = .x))
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

write_csv(k_linear_summary.df, "output/c_linear_k_values.csv")

k_linear_summary.df %>% 
  ggplot(aes(spp, estimate*-1, color=spp))+
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width=.2) +
  labs(x="Species", y="Decay coefficient (k)")
