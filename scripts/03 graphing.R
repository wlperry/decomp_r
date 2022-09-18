# LOAD LIBRARIES ----
library(tidyverse) 
library(patchwork)

# OBJECTIVE ----
# I'm going to use this file for all of the final plotting
# I need to do so that I can do it all in one place.
# At the end of the biomass, nitrogen, and carbon analysis scripts
#I saved the relevant files as csv files
# in the filepath: "output/final". The full data frame for
# the nitrogen and carbon loss data is created and 
# saved in the carbon analysis file so that I only had to do it 
# once and put it in one place. 
# going to start by plotting the percent remaining over time, 
# and them the emmeans graphs with relevant
# statistics. Will see where to go from there. 

# STUFF TO DO HERE ----
# 2022-09-13 
# - make the things on the legend match the mass remaining
# - change y axes to % mass reamining 
# - use a regression line with no se rather than a line that follows the points 

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

# nitrogen contrasts ----
nitrogen_k_contrasts.df <- read_csv("output/final/nitrogen_k_emmcontrasts.csv")

# carbon ----
carbon_k_emmeans.df <- read_csv("output/final/carbon_k_emmeans.csv")

# carbon to nitrogen ratios ----
ratios.df <- read_csv("output/final/c to n ratios.csv")

# carbon to nitrogen ratios by soil ----
ratio_soils.df <- read_csv("output/final/ c to n ratios by soil.csv")



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
  mutate(spp = as.factor(spp)) %>%
  mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR")) %>%
  ggplot(mapping = aes(days, pct_mass_remain_corr, shape = spp, linetype = spp)) + 
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 3) +
  geom_smooth(se = FALSE, color = "black", size = 0.4) +
  labs(x = "Days After Placement", y = "% Biomass Remaining") +
  scale_shape_manual(name = "Species",
                     label = c("LG Pennycress", "WT Pennycress", "Cereal Rye", " Annual Rye"),
                     values = c(15, 16, 17, 18)) +
  scale_linetype_manual(name = "Species",
                        label =
                          c("LG Pennycress", "WT Pennycress", "Cereal Rye", " Annual Rye"),
                        values = c(1, 2, 3, 5)) +
  labs(shape = "Species", linetype = "Species") +
  expand_limits(y = 45) +
  theme_classic()

decomp.plot



# emmeans 
decomp_emm.plot <- biomass_k_emmeans.df %>% 
  mutate(spp = as.factor(spp)) %>%
  mutate(spp = fct_relevel(spp, "gm_pc", "pc", "cr", "ar")) %>%
  ggplot(aes(x = spp)) +
  geom_point(aes(y=emmean, shape = spp), size = 3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Species", y= "k (biomass lost per day)")  +
  geom_text(aes(x = 1, y = 0.006, label = "A")) +
  geom_text(aes(x = 2, y = 0.007, label = "A")) +
  geom_text(aes(x = 3, y = 0.01, label = "B"))+
  geom_text(aes(x = 4, y = 0.011, label = "B")) +
  scale_x_discrete(labels = c("pc" = "WT Pennycress", "gm_pc"= "LG Pennycress",
                              "cr" = "Cereal Rye", "ar" = "Annual Rye")) +
  scale_shape_manual(name = "Species", 
                     label = c("LG Pennycress", "WT Pennycress", "Cereal Rye", " Annual Rye"),
                     values = c(15, 16, 17, 18)) +
  theme_classic()

decomp_emm.plot

# full
decomp.plot + decomp_emm.plot + plot_layout(ncol = 1, guides = "collect")


# NITROGEN ----

# mass remaining 
nitrogen.plot <- full.df %>%
   mutate(spp = as.factor(spp)) %>%
   mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR")) %>%
  ggplot(mapping = aes(days, pct_n_remain, shape = spp, linetype = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 3) +
  geom_smooth(se = FALSE, color = "black", size = 0.4) +
  labs(x = "Days After Placement", y = "Mean Percent Nitrogen Remaining") +
  scale_shape_manual(name = "Species", 
                     label = c("LG Pennycress", "WT Pennycress", "Cereal Rye", " Annual Rye"),
                     values = c(15, 16, 17, 18)) +
  scale_linetype_manual(name = "Species",
                        label =
                          c("LG Pennycress", "WT Pennycress", "Cereal Rye", " Annual Rye"),
                        values = c(1, 2, 3, 5)) +
  labs(shape = "Species", linetype = "Species") +
  expand_limits(y = 25) +
  theme_classic()

nitrogen.plot

# emmeans with spp on x
# empty shapes with fill for soil - soil type differentially affects decomposition by species 
# figure of C:N ratios 
nitrogen_emm.plot <- nitrogen_k_emmeans.df %>%
  mutate(spp = fct_relevel(spp, "gm_pc", "pc", "cr", "ar")) %>%
  mutate(spp_soil = fct_relevel(spp_soil,
                               "gm_pc_1", "gm_pc_2", "pc_1", "pc_2", "cr_1", "cr_2",
                               "ar_1", "ar_2")) %>%
  ggplot(aes(x=spp)) +
  geom_point(aes(y=emmean, shape = spp_soil, group = soil_block), 
             position = position_dodge2(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE, group = soil_block), 
                position = position_dodge2(0.3),
                stat="identity", width = 0.3) + 
  labs(x="Species", y= "k (nitrogen loss per day)")  +
  geom_text(aes(x = 0.9, y = .007, label = "A")) +
  geom_text(aes(x = 1.1, y = .007, label = "A")) +
  geom_text(aes(x = 1.9, y = .010, label = "ABC")) +
  geom_text(aes(x = 2.1, y = .008, label = "AB")) +
  geom_text(aes(x = 2.9, y = .009, label = "AB"))+
  geom_text(aes(x = 3.1, y = .011, label = "BCD")) +
  geom_text(aes(x = 3.9, y = .0125, label = "CD"))+
  geom_text(aes(x = 4.1, y = .0143, label = "D")) +
  scale_x_discrete(labels = c("pc" = "WT Pennycress", "gm_pc" = "LG Pennycress",
                              "cr" = "Cereal Rye", "ar" = "Annual Rye")) +
  scale_shape_manual(name = "Soil ",
                     label = c("LG Pennycress SA","LG Pennycress DR",
                               "WT Pennycress SA", "WT Pennycress DR", 
                               "Cereal Rye SA", "Cereal Rye DR",
                               "Annual Rye SA", "Annual Rye DR"),
                     values = c(15, 0, 16, 1, 17, 2, 18, 5)) +
  expand_limits(ymin = 0.005, ymax = 0.015) +
  theme_classic()

nitrogen_emm.plot


# comparing emmeans
nitrogen.plot + nitrogen_emm.plot + plot_layout(ncol = 1)

# CARBON ----

# mass remaining 
carbon.plot <- full.df %>%
  ggplot(mapping = aes(days, pct_c_remain, shape = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 4) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "line") +
  labs(x = "Days After Placement", y = "Mean Percent Carbon Remaining") +
  scale_shape_manual(name = "Species", 
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
  labs(x="Species", y= "k (carbon loss per day)")  +
  geom_text(aes(x = 1, y = .02, label = "A")) +
  geom_text(aes(x = 2, y = .02, label = "B")) +
  geom_text(aes(x = 3, y = .02, label = "C"))+
  geom_text(aes(x = 4, y = .02, label = "D")) +
  scale_x_discrete(labels = c("pc" = "Pennycress", "gm_pc"= "GE Pennycress",
                              "cr" = "Cereal Rye", "ar" = "Annual Rye")) +
  scale_shape_manual(name = "Species", 
                     label = c("Annual Rye", "Cereal Rye", "GE Pennycress", "Pennycress"),
                     values = c(15, 16, 17, 18)) +
  expand_limits(ymin = 0.005, ymax = 0.02) +
  theme_classic()

carbon_emm.plot


# put them all together
decomp.plot + nitrogen.plot + carbon.plot +
  decomp_emm.plot + nitrogen_emm.plot + carbon_emm.plot +
  plot_layout(ncol = 3, guides = "collect")

# PUT THE PLOTS TOGETHER FOR THE PAPER ----

# plan here for the paper is to have 3 plots, one for total biomass, one for
# n and one for c

# full biomass ----
full_biomass.plot <- decomp.plot + decomp_emm.plot + 
  plot_layout(guides = "collect", ncol = 1)

full_biomass.plot

# full nitrogen ----
full_nitrogen.plot <- nitrogen.plot + nitrogen_contrasts.plot +
  plot_layout(guides = "collect", ncol = 1)

full_nitrogen.plot

# full carbon ----
full_carbon.plot <- carbon.plot + carbon_emm.plot +
  plot_layout(guides = "collect", ncol = 1)

full_carbon.plot

# CARBON TO NITROGEN RATIOS PLOTTING ----

# by species ----
ratio.plot <- ratios.df %>%
  ggplot(mapping = aes(days, mean, shape = spp)) +
  geom_point(size = 3) +
  scale_shape_manual(name = "Species", 
                     label = c("Annual Rye", "Cereal Rye", "LG Pennycress", " WT Pennycress"),
                     values = c(15, 16, 17, 18)) +
  labs(x = "Days After Bag Placement", y = "Carbon to Nitrogen Ratio") +
  theme_classic()

# by species and soil ----
ratio_soils.plot <- ratio_soils.df %>%
  ggplot(mapping = aes(days, mean, shape = spp_soil)) +
  geom_point(size = 3) +
  scale_shape_manual(name = "Species",
                     label = c("Annual Rye SA", "Annual Rye DR", "Cereal Rye SA",
                               "Cereal Rye DR", "LG Pennycress SA", "LG Pennycress DR",
                               "WT Pennycress SA", "WT Pennycress DR"),
                     values = c(15, 0, 16, 1, 17, 2, 18, 5)) +
  labs(x = "Days After Bag Placement", y = "Carbon to Nitrogen Ratio") +
  theme_classic()

# putting the above two plots together
ratio.plot + ratio_soils.plot + plot_layout(ncol = 1)

# just looking at initial carbon to nitrogen ratio by species
ratios.df %>%
  filter(days == 0) %>%
  ggplot(mapping = aes(spp, mean, shape = spp)) +
  geom_point(size = 3) +
  scale_shape_manual(name = "Species", 
                     label = c("Annual Rye", "Cereal Rye", "LG Pennycress", " WT Pennycress"),
                     values = c(15, 16, 17, 18)) +
  labs(x = "Species", y = "Initial Carbon to Nitrogen Ratio") +
  theme_classic()
  








