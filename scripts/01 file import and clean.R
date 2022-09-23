# LIBRARIES ----
library(tidyverse)
library(janitor)

# TOTAL BIOMASS ----

# read in file ----
decomp.df <- read_csv("data/decomp_biomass.csv") %>% clean_names()

# cleaning ----

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

# calcs ----

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

# need to know all of the sampling dates
unique(decomp.df$days)
#  0  7 14 21 28 35 49 63 84 NA

# NITROGEN AND CARBON ----

# read in data ----

nutrients.df <- read_csv("data/decomp_biomass.csv") %>% clean_names()

# n data 
n_data.df <- read.csv("Data/Decomp/n with data.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# clean the data ----

# rename and make factors
nutrients.df <- nutrients.df %>%
  rename(sample = bag_no) %>%
  mutate(sample = as.factor(sample))

# make factor
n_data.df <- n_data.df %>%
  mutate(sample = as.factor(sample))

# join files

# full join 
full_nutrients.df <- full_join(nutrients.df, n_data.df, by = "sample")

# remove old dataframes
rm(nutrients.df, n_data.df)

# clean full dataframe

# remove na from data we didn't send 
full_nutrients.df <- full_nutrients.df %>%
  na.omit()

# make row a number
full_nutrients.df <- full_nutrients.df %>% 
  mutate(row = case_when(
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

# calcs for prop n remaining ----

# first need day 0 porportion of n * initial wt for how much n in initial weight
# calculate proportion n 
full_nutrients.df <- full_nutrients.df %>% 
  mutate(prop_n = percent_n / 100)

# now we need day 0 and by species 
initial_prop_n.df <- full_nutrients.df %>%
  filter(days == 0) %>%
  group_by(spp) %>%
  summarize(initial_prop_n = mean(prop_n))

# we then multiply each sample by the initial prop n to get the initial n for every sample
full_nutrients.df <- full_nutrients.df %>%
  mutate(initial_prop_n = case_when(
    spp == "PC" ~ initial_wt_g * 0.01872000,
    spp == "GM_PC" ~ initial_wt_g * 0.01595455,
    spp == "AR" ~ initial_wt_g * 0.01711000,
    spp == "CR" ~ initial_wt_g * 0.01439000
  ))

# now we take collected weight and multiply by the collected prop n to get collected n
full_nutrients.df <- full_nutrients.df %>%
  mutate(collected_prop_n = coll_wt_g * prop_n)

# finally divide initial by collected to get prop n remaining then * 100 for pct
full_nutrients.df <- full_nutrients.df %>%
  mutate(prop_n_remain = collected_prop_n / initial_prop_n) %>%
  mutate(pct_n_remain = prop_n_remain * 100)

# calcs for prop c remaining ----

# first need day 0 porportion of c * initial wt for how much c in initial weight
# calculate proportion n 
full_nutrients.df <- full_nutrients.df %>% 
  mutate(prop_c = percent_c / 100)

# now we need day 0 and by species 
initial_prop_c.df <- full_nutrients.df %>%
  filter(days == 0) %>%
  group_by(spp) %>%
  summarize(initial_prop_c = mean(prop_c))

# we then multiply each sample by the initial prop n to get the initial n for every sample
full_nutrients.df <- full_nutrients.df %>%
  mutate(initial_prop_c = case_when(
    spp == "PC" ~ initial_wt_g * 0.4549000,
    spp == "GM_PC" ~ initial_wt_g * 0.4313636,
    spp == "AR" ~ initial_wt_g * 0.4131000,
    spp == "CR" ~ initial_wt_g * 0.4298000
  ))

# now we take collected weight and multiply by the collected prop n to get collected n
full_nutrients.df <- full_nutrients.df %>%
  mutate(collected_prop_c = coll_wt_g * prop_c)

# finally divide initial by collected to get prop n remaining then * 100 for pct
full_nutrients.df <- full_nutrients.df %>%
  mutate(prop_c_remain = collected_prop_c / initial_prop_c) %>%
  mutate(pct_c_remain = prop_c_remain * 100)

# quick plot
full_nutrients.df %>%
  ggplot(mapping = aes(days, pct_n_remain, color = spp)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 63), breaks = seq(0, 63, by = 1)) +
  theme_classic()

# have some trouble outlier days that we need to remove ----
# remove 7, 21, 28, 49
unique(full_nutrients.df$days)

full_nutrients.df <- full_nutrients.df %>%
  filter(days == 0 | days == 14 | days == 35 | days == 63)

# plot again to see how it is
full_nutrients.df %>%
  ggplot(mapping = aes(days, pct_n_remain, color = spp)) +
  geom_point() +
  theme_classic()

# think i have a wild outlier for the nitrogen data, need to find it
full_nutrients.df %>%
  mutate(soil_block = as.factor(soil_block)) %>%
  ggplot(mapping = aes(spp, pct_c_remain, color = soil_block, shape = soil_block)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = 1)) +
  theme_classic()

# looks fine  

# SAVE FILES TO OUTPUT ----

# decomp ----
write_csv(decomp.df, file = 
            "output/final files from import and k calcs/from import and clean/total biomass.csv")

# nitrogen and carbon
write_csv(full_nutrients.df, file = 
            "output/final files from import and k calcs/from import and clean/nutrients.csv")






