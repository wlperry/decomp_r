# LIBRARIES ----
library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(car)
library(emmeans)
library(patchwork)
library(multcompView)
library(lsmeans)

# READ IN THE DATA ----

# biomass 
biomass_k.df <- 
  read_csv("output/final files from import and k calcs/k values/k nonlin biomass summary.csv")

# nitrogen 
nitrogen_k.df <- 
  read_csv("output/final files from import and k calcs/k values/k nonlin nitrogen summary.csv")

# carbon
carbon_k.df <- 
  read_csv("output/final files from import and k calcs/k values/k nonlin carbon summary.csv")

# BIOMASS ANALYSIS ----

# create model ----
biomass.lm = lm(estimate ~ spp * soil_block, data = biomass_k.df)

# check assumptions of model ----
residuals <- resid(biomass.lm)
plot(fitted(biomass.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# passes visual inspection for homogeneity of variance and normality 

# run model ----
Anova(biomass.lm, type = 3)

# these results look weird so im gonna plot it up quick
biomass_k.df %>% 
  ggplot(aes(spp, estimate, color=spp, shape=as.factor(soil_block)))+
  stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width=.2) +
  labs(x="Species", y="Biomass Decay coefficient (k)") +
  theme_classic()

# interaction between spp and soil block is significant so i will have to do emmeans

# run all pairwise comparisons as contrasts due to interaction ----
# because we have a signifiant interaction between soil and species, we need to 
# use planned contrasts to determine differences between groups

# have to put spp and soil block together for this
biomass_k.df <- biomass_k.df %>%
  mutate(spp = as.factor(spp),
         soil_block = as.factor(soil_block))

biomass_k.df <- biomass_k.df %>%
  mutate(trt = paste(spp, soil_block, sep = "_"))

biomass_k.df <- biomass_k.df %>%
  mutate(trt = as.factor(trt))

# set up to run contrasts
options(contrasts = c("contr.sum", "contr.poly"))

# contrast model ----
biomass_contrasts.lm = lm(estimate ~ trt, data = biomass_k.df)

# least squares
leastsquare = lsmeans(biomass_contrasts.lm, "trt")

# need the levels
levels(biomass_k.df$trt)

# contrasts
contrasts = list(ar1_ar2 = c(1, -1, 0, 0, 0, 0, 0, 0),
                 cr1_cr2 = c(0, 0, 1, -1, 0, 0, 0, 0),
                 gm1_gm_2 = c(0, 0, 0, 0, 1, -1, 0, 0),
                 pc1_pc2 = c(0, 0, 0, 0, 0, 0, 1, -1),
                 ar1_cr1 = c(1, 0, -1, 0, 0, 0, 0, 0),
                 ar2_cr2 = c(0, 1, 0, -1, 0, 0, 0, 0),
                 ar1_pc1 = c(1, 0, 0, 0, 0, 0, -1, 0),
                 ar2_pc2 = c(0, 1, 0, 0, 0, 0, 0, -1),
                 ar1_gm1 = c(1, 0, 0, 0, -1, 0, 0, 0),
                 ar2_gm2 = c(0, 1, 0, 0, 0, -1, 0, 0),
                 cr1_pc1 = c(0, 0, 1, 0, 0, 0, -1, 0),
                 cr2_pc2 = c(0, 0, 0, 1, 0, 0, 0, -1),
                 cr1_gm1 = c(0, 0, 1, 0, -1, 0, 0, 0),
                 cr2_gm2 = c(0, 0, 0, 1, 0, -1, 0, 0),
                 pc1_gm1 = c(0, 0, 0, 0, 1, 0, -1, 0),
                 pc2_gm2 = c(0, 0, 0, 0, 0, 1, 0, -1))

# run test and save as data frame
biomass_contrasts.df <- as.data.frame(contrast(leastsquare, contrasts, adjust = "bonferroni"))

# emmeans for final results ----
# create emmeans model ----
biomass.emm <- emmeans(biomass_contrasts.lm, ~ trt)

# plot emmeans ----
plot(biomass.emm, comparisons = TRUE)

# mean separation ----
# where are the differences in groups and what are the emmeans?
multcomp::cld(biomass.emm, Letters = letters, adjust = "bonferroni")

# p-values ----
emminteraction = emmeans(biomass.emm, pairwise ~ trt, 
                         adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts

# save as data frame
biomass_contrasts_emm.df <- 
  as.data.frame(multcomp::cld(biomass.emm, Letters = letters, adjust = "Bonferroni"))
biomass_contrasts_emm.df

# plot contrasts
biomass_contrasts_emm.df %>%
  ggplot(aes(x = trt)) +
  geom_point(aes(y = emmean)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), stat = "identity", width = 0.2) +
  theme_classic()

# save the output for plotting ----
write_csv(biomass_contrasts_emm.df, file = 
            "output/statistical results/biomass results.csv")

# NITROGEN ----

# create model ----
nitrogen.lm = lm(estimate ~ spp * soil_block, data = nitrogen_k.df)

# check assumptions of model ----
residuals <- resid(nitrogen.lm)
plot(fitted(nitrogen.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# passes visual inspection for homogeneity of variance and normality 

# run model ----
Anova(nitrogen.lm, type = "3")

# no significant interaction effect, will drop the interaction and run again ----
nitrogen_no_interaction.lm = lm(estimate ~ spp + soil_block, data = nitrogen_k.df)

# check assumptions
residuals <- resid(nitrogen_no_interaction.lm)
plot(fitted(nitrogen_no_interaction.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# run new model 
Anova(nitrogen_no_interaction.lm, type = "3")

# soil is not significant drop it from the model ----
nitrogen_soil.lm = lm(estimate ~ spp, data = nitrogen_k.df)

# check assumptions
residuals <- resid(nitrogen_soil.lm)
plot(fitted(nitrogen_soil.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# run the final model 
Anova(nitrogen_soil.lm, type ="3")

# significant effect of species, no significant effect of soils 

nitrogen.emm <- emmeans(nitrogen_soil.lm, ~ spp)

# plot emmeans ----
plot(nitrogen.emm, comparisons = TRUE)

# mean separation ----
# where are the differences in groups and what are the emmeans?
multcomp::cld(nitrogen.emm, Letters = letters, adjust = "Bonferroni")

# p-values ----
emminteraction = emmeans(nitrogen.emm, pairwise ~ spp, 
                         adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts

# save as data frame
nitrogen_emm.df <- 
  as.data.frame(multcomp::cld(nitrogen.emm, Letters = letters, adjust = "Bonferroni"))
nitrogen_emm.df


# plot contrasts
nitrogen_emm.df %>%
  ggplot(aes(x = spp)) +
  geom_point(aes(y = emmean)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), stat = "identity", width = 0.2) +
  theme_classic()

# save output for final plotting ----
write_csv(nitrogen_emm.df, file = 
           "output/statistical results/nitrogen results.csv")

# CARBON ----

# create model ----
carbon.lm = lm(estimate ~ spp * soil_block, data = carbon_k.df)

# check assumptions of model 
residuals <- resid(carbon.lm)
plot(fitted(carbon.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# passes visual inspection for normality, but homogeneity of variance does not look great

# run model ----
Anova(carbon.lm, type = "3")

# no significant interaction effect wil drop the interaction ----

carbon_no_interaction.lm = lm(estimate ~ spp + soil_block, data = carbon_k.df)

# check assumptions of model ----
residuals <- resid(carbon_no_interaction.lm)
plot(fitted(carbon_no_interaction.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# ehhhh these are okay, may need to do a log, it looks a little better

# run no interaction model
Anova(carbon_no_interaction.lm, type = "3")

# log transformation doesn't change the results

# soil is not significant, will drop that from the model ----

carbon_no_soil.lm = lm(estimate ~ spp, data = carbon_k.df)

# check assumptions of model 
residuals <- resid(carbon_no_interaction.lm)
plot(fitted(carbon_no_interaction.lm), residuals)
qqnorm(residuals)
qqline(residuals)

# run final model 
Anova(carbon_no_soil.lm, type = "3")


# create emmeans model ----

carbon.emm <- emmeans(carbon.lm, ~ spp)

# plot emmeans ----
plot(carbon.emm, comparisons = TRUE)

# mean separation ----
# where are the differences in groups and what are the emmeans?
multcomp::cld(carbon.emm, Letters = letters, adjust = "Bonferroni")

# p-values ----
emminteraction = emmeans(carbon.emm, pairwise ~ spp, 
                         adjust = "bonferroni", alpha = 0.5)
emminteraction$contrasts

# save as data frame
carbon_emm.df <- 
  as.data.frame(multcomp::cld(carbon.emm, Letters = letters, adjust = "Bonferroni"))
carbon_emm.df


# plot contrasts
carbon_emm.df %>%
  ggplot(aes(x = spp)) +
  geom_point(aes(y = emmean)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), stat = "identity", width = 0.2) +
  theme_classic()

# save file for finalplotting ----
write_csv(carbon_emm.df, file = 
            "output/statistical results/carbon results.csv")

