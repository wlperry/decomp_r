# LIBRARIES
library(tidyverse)
library(janitor)

# TOTAL BIOMASS ----
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

# math modifications to get pct mass remaining  -----
decomp.df <- decomp.df %>% 
  mutate(pct_mass_remain = ((coll_wt_g/ initial_wt_g)*100)) %>% 
  mutate(soil_block = as.factor(soil_block),
         spp = as.factor(spp))

# remove missing values -----
decomp.df <- decomp.df %>% 
  filter(!is.na(initial_wt_g))

# correct for day 0 mass remaining  -----
corr_t0.df <- decomp.df %>%
  filter(days == 0) %>%
  group_by(spp) %>%
  summarize(pct_mass_remain = 100 - mean(pct_mass_remain, na.rm=TRUE))

# correct mass remaining from time 0 -----
# Do we adjust all masses to this correction ----
decomp.df <- decomp.df %>%
  mutate(pct_mass_remain_corr = case_when(
    spp == "PC"    & days == 0  ~ pct_mass_remain + 2.0582047,
    spp == "GM_PC" & days == 0  ~ pct_mass_remain + -0.2269507,
    spp == "AR"    & days == 0  ~ pct_mass_remain + 7.1806311,
    spp == "CR"    & days == 0  ~ pct_mass_remain + 7.4213231,
    TRUE ~ pct_mass_remain
  ))


# save as csv
write_csv(decomp.df, file = "data/cleaned/clean total biomass.csv")


# NITROGEN ----

# read in the data

# decomp
decomp.df <- read_csv("data/decomp_biomass.csv") %>% clean_names()

# n data 
n_data.df <- read.csv("Data/Decomp/n with data.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))


# clean the data

# rename and make factors
decomp.df <- decomp.df %>%
  rename(sample = bag_no) %>%
  mutate(sample = as.factor(sample))

# make factor
n_data.df <- n_data.df %>%
  mutate(sample = as.factor(sample))

# join files

# full join 
full.df <- full_join(decomp.df, n_data.df, by = "sample")

# remove old dataframes
rm(decomp.df, n_data.df)

# clean full dataframe

# remove na from data we didn't send 
full.df <- full.df %>%
  na.omit()

# make row a number
full.df <- full.df %>% 
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

# CALCS FOR PROP N REMAINING 

# first need day 0 porportion of n * initial wt for how much n in initial weight
# calculate proportion n 
full.df <- full.df %>% 
  mutate(prop_n = percent_n / 100)

# now we need day 0 and by species 
initial_prop_n.df <- full.df %>%
  filter(days == 0) %>%
  group_by(spp) %>%
  summarize(initial_prop_n = mean(prop_n))

# we then multiply each sample by the initial prop n to get the initial n for every sample
full.df <- full.df %>%
  mutate(initial_prop_n = case_when(
    spp == "PC" ~ initial_wt_g * 0.01872000,
    spp == "GM_PC" ~ initial_wt_g * 0.01595455,
    spp == "AR" ~ initial_wt_g * 0.01711000,
    spp == "CR" ~ initial_wt_g * 0.01439000
  ))

# now we take collected weight and multiply by the collected prop n to get collected n
full.df <- full.df %>%
  mutate(collected_prop_n = coll_wt_g * prop_n)

# finally divide initial by collected to get prop n remaining then * 100 for pct
full.df <- full.df %>%
  mutate(prop_n_remain = collected_prop_n / initial_prop_n) %>%
  mutate(pct_n_remain = prop_n_remain * 100)

# graph it to see how it looks
full.df %>%
  ggplot(mapping = aes(days, pct_n_remain, color = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  theme_classic()

# gonna remove those problem days 
unique(full.df$days)

full.df <- full.df %>%
  filter(days == 0 | days == 14 | days == 35 | days == 63)

# graph it to see how it looks
full.df %>%
  ggplot(mapping = aes(log(days+1), log(pct_n_remain), color = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  geom_smooth(method="lm", se=F)
theme_classic()

# save as csv
write_csv(full.df, file = "data/cleaned/clean nitrogen.csv")
