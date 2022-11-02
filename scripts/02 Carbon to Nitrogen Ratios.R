# OBJECTIVE ----
# I want to look at the C:N ratios of these crops on day 0 to see how they compare to each
# other and other studies, and then see how they change over time

# LOAD LIBRARIES ----
library(tidyverse)

# READ IN DATA ----
full.df <-
  read_csv("output/cleaned data and estimates of k/from import and clean/nutrients.csv")

# TRANSFORMATIONS

# Now I want to look at how these values change over time so I think I will use the 
# proportion  values from the 01 import and clean file. 
# easiest way to get the prop c values is to just do the calcs really quick so thats what
# I do that here with the code from file import and clean

# calculate proportion c 
full.df <- full.df %>% 
  mutate(prop_c = percent_c / 100)

# now we get the carbon to nitrogen ratios
full.df <- full.df %>%
  mutate(ratio = prop_c / prop_n)

# we need this summarized by date so I do that here
summary.df <- full.df %>%
  group_by(days, spp) %>%
  summarize(mean = mean(ratio),
            sd = sd(ratio))

# I also want to look at this by soils
ratio_soils.df <- full.df %>%
  group_by(days, spp, soil_block) %>%
  summarize(mean = mean(ratio),
            sd = sd(ratio))

# going to need to pase these columns for plotting
ratio_soils.df <- ratio_soils.df %>%
  mutate(spp_soil = paste(spp, soil_block, sep = "_"))

# save the output 
write_csv(summary.df, file = "output/carbon to nitrogen ratios/c to n ratios.csv")

write_csv(ratio_soils.df, file = "output/carbon to nitrogen ratios/ c to n ratios by soil.csv")








