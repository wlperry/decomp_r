# LOAD LIBRARIES ----
library(tidyverse) 
library(patchwork)

# OBJECTIVE ----
# I'm going to use this file for all of the final plotting I need to do so that I can do it all in one place.
# At the end of the biomass, nitrogen, and carbon analysis scripts I saved the relevant files as csv files
# in the filepath: "output/final". The full data frame for the nitrogen and carbon loss data is created and 
# saved in the carbon analysis file so that I only had to do it once and put it in one place. 
# going to start by plotting the percent remaining over time, and them the emmeans graphs with relevant
# statistics. Will see where to go from there. 

# READ IN THE DATA ----

# full nitrogen and carbon data ----
full.df <- read_csv("output/final/full_data.csv")

# full decomp data ----
decomp.df <- read_csv("output/final/biomass_loss.csv")

# now we load in the emmeans for those plots 
# biomass ----
biomass_k_emmeans.df <- read_csv("output/final/biomass_k_emmeans.csv")

# nitrogen ----
nitrogen_k_emmeans.df <- read_csv("output/final/nitrogen_k_emmeans.csv")

# carbon ----
carbon_k_emmeans.df <- read_csv("output/final/carbon_k_emmeans.csv")

# CLEANING OF FILES ----
full.df <- full.df %>%
  rename(pct_c_remain = pct_c_remaing)

biomass_k_emmeans.df <- biomass_k_emmeans.df %>%
  mutate(spp = as.factor(sp)) 

nitrogen_k_emmeans.df <- nitrogen_k_emmeans.df %>%
  mutate(soil_block = as.factor(soil_block))

nitrogen_k_emmeans.df <- nitrogen_k_emmeans.df %>%
  mutate(spp_soil = paste(spp, soil_block, sep = "_"))




# FIGURES ----

# BIOMASS ----

# percent remaining
decomp.plot <- decomp.df %>%
  ggplot(mapping = aes(days, pct_mass_remain_corr, shape = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  labs(x = "Days After Placement", y = "Mean Percent Biomass Remaining") +
  scale_shape_manual(name = "Species", 
                     label = c("Annual Rye", "Cereal Rye", "GE Pennycress", "Pennycress"),
                     values = c(15, 16, 17, 18)) +
  expand_limits(y = 25) +
  theme_classic()

decomp.plot

# emmeans 
decomp_emm.plot <- biomass_k_emmeans.df %>% 
  ggplot(aes(x = factor(spp, level = c("pc", "gm_pc", "cr", "ar")))) +
  geom_point(aes(y=emmean, shape = spp), size = 3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Species", y= "k (% biomass lost per day)")  +
  geom_text(aes(x = 1, y = 0.013, label = "A")) +
  geom_text(aes(x = 2, y = 0.013, label = "A")) +
  geom_text(aes(x = 3, y = 0.013, label = "B"))+
  geom_text(aes(x = 4, y = 0.013, label = "B")) +
  scale_x_discrete(labels = c("pc" = "Pennycress", "gm_pc"= "GE Pennycress",
                              "cr" = "Cereal Rye", "ar" = "Annual Rye")) +
  scale_shape_manual(name = "Species", 
                     label = c("Annual Rye", "Cereal Rye", "GE Pennycress", "Pennycress"),
                     values = c(15, 16, 17, 18)) +
  expand_limits(ymin = 0.005, ymax = 0.015) +
  theme_classic()

decomp_emm.plot

# full
decomp.plot + decomp_emm.plot + plot_layout(ncol = 1, guides = "collect")


# NITROGEN ----

# mass remaining 
nitrogen.plot <- full.df %>%
  ggplot(mapping = aes(days, pct_n_remain, shape = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  labs(x = "Days After Placement", y = "Mean Percent Nitrogen Remaining") +
  scale_shape_manual(name = "Species", 
                     label = c("Annual Rye", "Cereal Rye", "GE Pennycress", "Pennycress"),
                     values = c(15, 16, 17, 18)) +
  expand_limits(y = 25) +
  theme_classic()

nitrogen.plot

# emmeans 
nitrogen_emm.plot <- nitrogen_k_emmeans.df %>%
  mutate(spp = fct_relevel(spp_soil, "pc_1", "pc_2", "gm_pc_1", "gm_pc_2",
                           "cr_1", "cr_2", "ar_1", "ar_2")) %>%
  ggplot(aes(x=spp_soil)) +
  geom_point(aes(y=emmean, shape = spp), 
             position = position_dodge2(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                position = position_dodge2(0.3),
                stat="identity", width = 0.3) + 
  labs(x="Species", y= "k (% nitrogen loss per day)")  +
  geom_text(aes(x = 1, y = .02, label = "AB")) +
  geom_text(aes(x = 2, y = .02, label = "A")) +
  geom_text(aes(x = 3, y = .02, label = "B"))+
  geom_text(aes(x = 4, y = .02, label = "C")) +
  scale_x_discrete(labels = c("pc_1" = "Pennycress 721A", "pc_2" = "Pennycress 145B2" "gm_pc"= "GE Pennycress",
                              "cr" = "Cereal Rye", "ar" = "Annual Rye")) +
  scale_shape_manual(name = "Species",
                     label = c("pc_1", "pc_2", "gm_pc_1", "gm_pc_2",
                               "cr_1", "cr_2", "ar_1", "ar_2"),
                     values = c(15, 16, 17, 18)) +
  expand_limits(ymin = 0.005, ymax = 0.02) +
  theme_classic()

nitrogen_emm.plot

nitrogen_k_emmeans.df %>%
  mutate(spp = fct_relevel(spp, "pc", "gm_pc", "cr", "ar")) %>%
  ggplot(aes(x = spp)) +
  geom_point(aes(y = emmean, shape = spp), position = position_dodge2(0.9)) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), stat = "identity", 
                position = position_dodge2())

# CARBON ----

# mass remaining 
carbon.plot <- full.df %>%
  ggplot(mapping = aes(days, pct_c_remain, shape = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  labs(x = "Days After Placement", y = "Mean Percent Carbon Remaining") +
  scale_shape_manual(name = "spp", 
                     label = c("Annual Rye", "Cereal Rye", "GE Pennycress", "Pennycress"),
                     values = c(15, 16, 17, 18)) +
  expand_limits(y = 25) +
  theme_classic()

carbon.plot 

# emmeans 
carbon_emm.plot <- carbon_k_emmeans.df %>%
  mutate(spp = fct_relevel(spp, "pc", "gm_pc", "cr", "ar")) %>%
  ggplot(aes(x=spp)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Species", y= "k (% carbon loss per day)")  +
  geom_text(aes(x = 1, y = .02, label = "A")) +
  geom_text(aes(x = 2, y = .02, label = "B")) +
  geom_text(aes(x = 3, y = .02, label = "C"))+
  geom_text(aes(x = 4, y = .02, label = "D")) +
  scale_x_discrete(labels = c("pc" = "Pennycress", "gm_pc"= "GE Pennycress",
                              "cr" = "Cereal Rye", "ar" = "Annual Rye")) +
  expand_limits(ymin = 0.005, ymax = 0.02) +
  theme_classic()

carbon_emm.plot


# put them all together
decomp.plot + nitrogen.plot + carbon.plot +
  decomp_emm.plot + nitrogen_emm.plot + carbon_emm.plot +
  plot_layout(ncol = 3, guides = "collect")






