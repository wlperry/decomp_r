library(tidyverse)
library(janitor)

# read in file
decomp.df <- read_csv("data/decomp_biomass.csv") %>% clean_names()

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

# lets do hard math
decomp.df <- decomp.df %>% 
  mutate(pct_mass_remain = ((coll_wt_g)/ initial_wt_g)*100)

# # summary of mass remaining
# corr_t0.df <- decomp.df %>% 
#   filter(time ==0) %>% 
#   group_by(spp) %>% 
#   summarize(pct_mass_remain = 100-mean(pct_mass_remain, na.rm=TRUE))
# 
# correct mass remaining
decomp.df <- decomp.df %>%
  mutate(pct_mass_remain = case_when(
    spp == "PC"   & time ==0  ~ pct_mass_remain + 2.0582047,
    spp == "GM_PC"& time ==0 ~ pct_mass_remain + -0.2269507,
    spp == "AR"   & time ==0 ~ pct_mass_remain + 7.1806311,
    spp == "CR"   & time ==0 ~ pct_mass_remain + 7.4213231,
    TRUE ~ pct_mass_remain

  ))



# look at the graph
decomp.df %>% 
  ggplot(aes(time, pct_mass_remain, color=spp)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3,
               position = position_dodge(.2)) +
  stat_summary(fun.data = mean_sdl, na.rm = TRUE, fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(.2)) 


# or this
decomp.df %>% 
  ggplot(aes(time, pct_mass_remain, color=spp)) +
  geom_point( position = position_dodge2(.1)) +
  geom_smooth(method = "lm") + 
  facet_wrap(~spp)

# we want to make sub datasets --
pc_b1.df <- decomp.df %>% 
  filter(spp == "PC" & soil_block == 1) %>%
  select(time, row_no, pct_mass_remain)

# plot again
pc_b1.df  %>% 
  ggplot(aes(time, pct_mass_remain, color=as.factor(row_no))) +
  geom_point() +
  geom_smooth(method = "lm")


# nonlin
nonlin = nls(pct_mass_remain ~ 1*exp(-k*time), trace=TRUE, start = list(k = .01), data=pc_b1.df)
