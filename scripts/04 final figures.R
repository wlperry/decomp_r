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

# READ IN THE DATA ----

# full nitrogen and carbon data ----
nutrients.df <- 
  read_csv("output/final files from import and k calcs/from import and clean/nutrients.csv")

# full decomp data ----
biomass.df <- 
  read_csv("output/final files from import and k calcs/from import and clean/total biomass.csv")

# now we load in the emmeans for those plots 
# biomass ----
biomass_k_emmeans.df <- 
  read_csv("output/statistical results/biomass results.csv")

# nitrogen ----
nitrogen_k_emmeans.df <- 
  read_csv("output/statistical results/nitrogen results.csv")

# carbon ----
carbon_k_emmeans.df <- 
  read_csv("output/statistical results/carbon results.csv")

# carbon to nitrogen ratios ----
ratios.df <- 
  read_csv("output/carbon to nitrogen ratios/c to n ratios.csv")

# carbon to nitrogen ratios by soil ----
ratio_soils.df <- 
  read_csv("output/carbon to nitrogen ratios/ c to n ratios by soil.csv")

# READ IN FITTED VALUES FOR PLOTTING ----

# biomass ----
biomass_fitted.df <- 
  read_csv("output/final files from import and k calcs/k values/fitted biomass.csv")
  
# nitrogen ----
nitrogen_fitted.df <-
  read_csv("output/final files from import and k calcs/k values/fitted nitrogen.csv")
  
# carbon ----
carbon_fitted.df <- 
  read_csv("output/final files from import and k calcs/k values/fitted carbon.csv")

# CLEANING OF FILES ----

# add species to biomass data frame 
biomass_k_emmeans.df <- biomass_k_emmeans.df %>%
  mutate(spp = case_when(
    trt == "GM_PC_1" ~ "gm_pc",
    trt == "GM_PC_2" ~ "gm_pc",
    trt == "PC_1" ~ "pc",
    trt == "PC_2" ~ "pc",
    trt == "CR_1" ~ "cr",
    trt == "CR_2" ~ "cr",
    trt == "AR_1" ~ "ar",
    trt == "AR_2" ~ "ar"))

# add soil to biomass data frame
biomass_k_emmeans.df <- biomass_k_emmeans.df %>%
  mutate(soil = case_when(
    trt == "GM_PC_1" ~ "1",
    trt == "GM_PC_2" ~ "2",
    trt == "PC_1" ~ "1",
    trt == "PC_2" ~ "2",
    trt == "CR_1" ~ "1",
    trt == "CR_2" ~ "2",
    trt == "AR_1" ~ "1",
    trt == "AR_2" ~ "2"))

# set up factor for biomass fitted
biomass_fitted.df <- biomass_fitted.df %>%
  mutate(trt = paste(spp, soil_block, sep = "_"))

biomass.df <- biomass.df %>%
  mutate(trt = paste(spp, soil_block, sep = "_"))

biomass_fitted.df <- biomass_fitted.df %>%
  mutate(trt = as.factor(trt)) %>%
  mutate(trt = fct_relevel(trt,
                           "GM_PC_1", "GM_PC_2", "PC_1", "PC_2", "CR_1", "CR_2",
                           "AR_1", "AR_2")) 

# set up the factors for nitrogen fitted
nitrogen_fitted.df <- nitrogen_fitted.df %>%
mutate(spp = as.factor(spp)) %>%
  mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR"))

# set up the factors for carbon fitted 
carbon_fitted.df <- carbon_fitted.df %>%
  mutate(spp = as.factor(spp)) %>%
  mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR"))



# rename treatment to spp_soil to work with the graphs i aleady wrote
biomass_k_emmeans.df<- biomass_k_emmeans.df %>%
  rename(spp_soil = trt)

# FIGURES ----

# biomass ----

# percent remaining
total_biomass_days.plot <- biomass.df %>%
  mutate(spp = as.factor(trt)) %>%
  mutate(trt = fct_relevel(trt,
                                "GM_PC_1", "GM_PC_2", "PC_1", "PC_2", "CR_1", "CR_2",
                                "AR_1", "AR_2")) %>%
  ggplot(mapping = aes(days, pct_mass_remain, 
                       shape = as.factor(trt), fill = trt)) + 
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 2,
               position = position_dodge(width = 5)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 10,
               position = position_dodge(width = 5)) +
  geom_line(data = biomass_fitted.df,
            aes(x= days, y=.fitted, linetype= as.factor(trt), color = as.factor(trt)))+
  labs(x = "Days After Placement", y = "% Biomass Remaining") +
  scale_shape_manual(name = "Species",
                     label = c("LG Pennycress SA","LG Pennycress DR",
                               "WT Pennycress SA", "WT Pennycress DR", 
                               "Cereal Rye SA", "Cereal Rye DR",
                               "Annual Ryegrass SA", "Annual Ryegrass DR"),
                     values = c(22, 22, 21, 21, 24, 24, 23, 23)) +
  scale_linetype_manual(name = "Species",
                        label =
                          c("LG Pennycress SA","LG Pennycress DR",
                            "WT Pennycress SA", "WT Pennycress DR", 
                            "Cereal Rye SA", "Cereal Rye DR",
                            "Annual Ryegrass SA", "Annual Ryegrass DR"),
                        values = c(1, 1, 2, 2, 3, 3, 4, 4)) +
  scale_fill_manual(name = "Species",
                     label = c("LG Pennycress SA","LG Pennycress DR",
                               "WT Pennycress SA", "WT Pennycress DR", 
                               "Cereal Rye SA", "Cereal Rye DR",
                               "Annual Ryegrass SA", "Annual Ryegrass DR"),
                     values = c("black", "gray75", "black", "gray75",
                                "black", "gray75", "black", "gray75")) +
  scale_color_manual(name = "Species",
                    label = c("LG Pennycress SA","LG Pennycress DR",
                              "WT Pennycress SA", "WT Pennycress DR", 
                              "Cereal Rye SA", "Cereal Rye DR",
                              "Annual Ryegrass SA", "Annual Ryegrass DR"),
                    values = c("black", "gray75", "black", "gray75",
                               "black", "gray75", "black", "gray75")) +
  labs(shape = "Species", linetype = "Species") +
  expand_limits(y = 45) +
  ggtitle("A") +
  theme_classic()

total_biomass_days.plot

# emm contrasts
biomass_contrast.plot <- biomass_k_emmeans.df %>%
  mutate(spp = fct_relevel(spp, "gm_pc", "pc", "cr", "ar")) %>%
  mutate(spp_soil = fct_relevel(spp_soil,
                                "GM_PC_1", "GM_PC_2", "PC_1", "PC_2", "CR_1", "CR_2",
                                "AR_1", "AR_2")) %>%
  ggplot(aes(x=spp)) +
  geom_point(aes(y=emmean, shape = spp_soil, group = soil, fill = spp_soil), 
             position = position_dodge2(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE, group = soil), 
                position = position_dodge2(0.3),
                stat="identity", width = 0.3) + 
  labs(x="Species", y= "k (biomass loss constant)")  +
  geom_text(aes(x = 0.9, y = .0045, label = "A")) +
  geom_text(aes(x = 1.1, y = .0055, label = "AB")) +
  geom_text(aes(x = 1.9, y = .0076, label = "ABC")) +
  geom_text(aes(x = 2.1, y = .0082, label = "BC")) +
  geom_text(aes(x = 2.9, y = .0085, label = "CD"))+
  geom_text(aes(x = 3.1, y = .012, label = "DE")) +
  geom_text(aes(x = 3.9, y = .011, label = "E"))+
  geom_text(aes(x = 4.1, y = .0147, label = "F")) +
  scale_x_discrete(labels = c("pc" = "WT Pennycress", "gm_pc" = "LG Pennycress",
                              "cr" = "Cereal Rye", "ar" = "Annual Ryegrass")) +
  scale_shape_manual(name = "Soil ",
                     label = c("LG Pennycress SA","LG Pennycress DR",
                               "WT Pennycress SA", "WT Pennycress DR", 
                               "Cereal Rye SA", "Cereal Rye DR",
                               "Annual Ryegrass SA", "Annual Ryegrass DR"),
                     values = c(22, 22, 21, 21, 24, 24, 23, 23)) +
  scale_fill_manual(name = "Soil",
                    label = c("LG Pennycress SA","LG Pennycress DR",
                              "WT Pennycress SA", "WT Pennycress DR", 
                              "Cereal Rye SA", "Cereal Rye DR",
                              "Annual Ryegrass SA", "Annual Ryegrass DR"),
                    values = c("black", "gray75", "black", "gray75",
                               "black", "gray75", "black", "gray75")) +
  expand_limits(ymin = 0.005, ymax = 0.015) +
  ggtitle("B") +
  theme_classic() +
  theme(legend.position = "none")

biomass_contrast.plot

# full
total_biomass_days.plot + biomass_contrast.plot + plot_layout(ncol = 1, guides = "collect")


# nitrogen ----

# mass remaining 
nitrogen_days.plot <- nutrients.df %>%
   mutate(spp = as.factor(spp)) %>%
   mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR")) %>%
  ggplot(mapping = aes(days, pct_n_remain, shape = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 2,
               position = position_dodge(width = 5)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 10,
               position = position_dodge(width = 5)) +
  geom_line(data = nitrogen_fitted.df,
            aes(x= days, y=.fitted, linetype = as.factor(spp)))+
  labs(x = "Days After Placement", y = "% Nitrogen Remaining") +
  scale_shape_manual(name = "Species", 
                     label = c("LG Pennycress", "WT Pennycress", 
                               "Cereal Rye", " Annual Ryegrass"),
                     values = c(15, 16, 17, 18)) +
  scale_linetype_manual(name = "Species",
                        label =
                          c("LG Pennycress", "WT Pennycress", 
                            "Cereal Rye", " Annual Ryegrass"),
                        values = c(1, 2, 3, 5)) +
  labs(shape = "Species", linetype = "Species") +
  expand_limits(y = 25) +
  ggtitle("A") +
  theme_classic()

nitrogen_days.plot

# emmeans with spp on x
# empty shapes with fill for soil - soil type differentially affects decomposition by species 
nitrogen_emm.plot <- nitrogen_k_emmeans.df %>%
  mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR")) %>%
  ggplot(aes(x=spp, shape = spp)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Species", y= "k (nitrogen loss constant)")  +
  geom_text(aes(x = 1, y = .008, label = "A")) +
  geom_text(aes(x = 2, y = .010, label = "AB")) +
  geom_text(aes(x = 3, y = .012, label = "BC"))+
  geom_text(aes(x = 4, y = .014, label = "C")) +
  scale_x_discrete(labels = c("PC" = "WT Pennycress", "GM_PC"= "LG Pennycress",
                              "CR" = "Cereal Rye", "AR" = "Annual Rye")) +
  scale_shape_manual(name = "Species", 
                     label = c("LG Pennycress", "WT Pennycress", 
                               "Cereal Rye", "Annual Rye"),
                     values = c(15, 16, 17, 18)) +
  ggtitle("B") +
  theme_classic() +
  theme(legend.position = "none")

nitrogen_emm.plot

# full
nitrogen_days.plot + nitrogen_emm.plot + plot_layout(guides = "collect", ncol = 1)

# carbon ----

# mass remaining 
carbon.plot <- nutrients.df %>%
  mutate(spp = as.factor(spp)) %>%
  mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR")) %>%
  ggplot(mapping = aes(days, pct_c_remain, shape = spp)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 2,
               position = position_dodge(width = 5)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 10,
               position = position_dodge(width = 5)) +
  geom_line(data = carbon_fitted.df,
            aes(x= days, y=.fitted, linetype = as.factor(spp)))+
  labs(x = "Days After Placement", y = "% Carbon Remaining") +
  scale_shape_manual(name = "Species", 
                     label = c("LG Pennycress", "WT Pennycress", 
                               "Cereal Rye", " Annual Ryegrass"),
                     values = c(15, 16, 17, 18)) +
  scale_linetype_manual(name = "Species",
                        label =
                          c("LG Pennycress", "WT Pennycress", 
                            "Cereal Rye", " Annual Ryegrass"),
                        values = c(1, 2, 3, 5)) +
  labs(shape = "Species", linetype = "Species") +
  expand_limits(y = 25) +
  ggtitle("A") +
  theme_classic()

carbon.plot


# emmeans 
carbon_emm.plot <- carbon_k_emmeans.df %>%
  mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR")) %>%
  ggplot(aes(x=spp, shape = spp)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="Species", y= "k (carbon loss constant)")  +
  geom_text(aes(x = 1, y = .007, label = "A")) +
  geom_text(aes(x = 2, y = .011, label = "B")) +
  geom_text(aes(x = 3, y = .0162, label = "C"))+
  geom_text(aes(x = 4, y = .02, label = "D")) +
  scale_x_discrete(labels = c("PC" = "WT Pennycress", "GM_PC"= "LG Pennycress",
                              "CR" = "Cereal Rye", "AR" = "Annual Ryegrass")) +
  scale_shape_manual(name = "Species", 
                     label = c("LG Pennycress", "WT Pennycress", 
                               "Cereal Rye", "Annual Ryegrass"),
                     values = c(15, 16, 17, 18)) +
  expand_limits(ymin = 0.005, ymax = 0.02) +
  ggtitle("B") +
  theme_classic() +
  theme(legend.position = "none")

carbon_emm.plot

# carbon plots together
carbon.plot + carbon_emm.plot + plot_layout(ncol = 1)

# PUT THE PLOTS TOGETHER FOR THE PAPER ----

# biomass ----
final_biomass.plot <- 
  total_biomass_days.plot + biomass_contrast.plot + plot_layout(ncol = 1, guides = "collect")

final_biomass.plot

ggsave("output/figures/final biomass.jpg", final_biomass.plot,
       width = 7, height = 7, dpi = 700)

# nitrogen ----
final_nitrogen.plot <- 
  nitrogen_days.plot + nitrogen_emm.plot + plot_layout(guides = "collect", ncol = 1)

final_nitrogen.plot

# save it
ggsave("output/figures/final nitrogen.jpg", final_nitrogen.plot,
       width = 7, height = 7, dpi = 700)


# carbon ----
final_carbon.plot <- 
  carbon.plot + carbon_emm.plot + plot_layout(guides = "collect", ncol = 1)

final_carbon.plot

#save it 
ggsave("output/figures/final carbon.jpg", final_carbon.plot,
       width = 7, height = 7, dpi = 700)


# CARBON TO NITROGEN RATIOS PLOTTING ----
  
# with comparisons to other papers and sources
cn.plot <- ratios.df %>%
  filter(days == 0) %>%
  mutate(spp = fct_relevel(spp, "GM_PC", "PC", "CR", "AR")) %>%
  ggplot(mapping = aes(spp, mean, shape = spp)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), stat = "identity", width = 0.2) +
  scale_shape_manual(name = "Species", 
                     label = c("LG Pennycress", "WT Pennycress", "Cereal Rye", "Annual Ryegrass"),
                     values = c(15, 16, 17, 18)) +
  scale_x_discrete(labels = c("PC" = "WT Pennycress", "GM_PC"= "LG Pennycress",
                              "CR" = "Cereal Rye", "AR" = "Annual Ryegrass")) +
  geom_text(aes(x = 0, y = 12, hjust = -0.1, label = "Hairy Vetch")) +
  geom_text(aes(x = 0, y = 58, hjust = -0.1, label = "Corn Stover")) +
  geom_text(aes(x = 0, y = 25, hjust = -0.1, label = "IMD")) +
  labs(x = "Species", y = "Initial Carbon-to-Nitrogen Ratio") +
  geom_hline(yintercept = 11, linetype = 2) +
  geom_hline(yintercept = 57, linetype = 2) +
  geom_hline(yintercept = 24, linetype = 2) +
  theme_classic()

cn.plot

ggsave("output/figures/carbon to nitrogen.jpg", cn.plot,
       width = 7, height = 7, dpi = 700)


# at cn of 24/25 to 1 there is no leftover carbon or nitrogen!


  




