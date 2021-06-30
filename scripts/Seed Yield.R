
# WHAT IS THE SEED YIELD OF THE WILD TYPE PENNYCRESS FROM OUR STUDY PLOTS ----

# libraries ----
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(plotly)
library(colorRamps)
library(car)
library(emmeans)


# this is bills stuff


#read in the data ----
seed_yield.df <- read_csv("Data/Seed Yield.csv") %>%
  clean_names() %>%
  remove_empty(which = c("cols", "rows"))

# determine seed yield in lb/acre and kg/ha ----
# g/m2 -> lb/acre = 8.92179122
# g/m2 -> kg/ha = 10
seed_yield.df <- seed_yield.df %>%
  mutate(
    lb_acre = (seed_weight_g_m2 * 8.92179122)
  ) %>%
  mutate(
    kg_ha = (seed_weight_g_m2 * 10)
  )

# save the output ----
write_csv(seed_yield.df, "Output/Seed Yield/seed_yield.csv")

# graph data ----
# per treatment mean and standard error without blocking
seed_yield_treatment_unblocked.plot <- 
  ggplot(data = seed_yield.df, aes(x = treatment, y = kg_ha, color = "red")) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3))  + theme(axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    panel.background = element_rect(fill = NA)) +labs(y = "kg/ha") + 
  theme(legend.position = "none") + theme(axis.line = element_line(linetype = "solid"))

# per treatment mean and standard error with blocking
seed_yield_treatment_blocked.plot <- seed_yield.df %>%
  mutate(
    block = as.factor(block)
  ) %>%
ggplot(aes(x = treatment, y = kg_ha, shape = block, color = block)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3)) + 
  scale_color_manual(
    name = "Block", labels = c("north", "south"), values = c("red", "blue")) + 
scale_shape_manual(
  name = "Block", labels = c("north", "south"), values = c(22, 25)) +
  theme(axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size = 1),
    axis.text = element_text(size = 13),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 16),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA))

# per plot
seed_yield_per_plot.plot <- 
  ggplot(data = seed_yield.df, aes(x = plot, y = kg_ha, color = treatment)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3))+
  scale_x_continuous(name = "plot", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) + 
  theme(axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    panel.background = element_rect(fill = NA),
    legend.position = "right")

# call the plots
seed_yield_treatment_unblocked.plot
seed_yield_treatment_blocked.plot
seed_yield_per_plot.plot

# save the plots as outputs
ggsave(seed_yield_treatment_blocked.plot, file = "Output/Seed Yield/Unblocked Yield.pdf", 
       units = "in", width = 6, height = 6)
ggsave(seed_yield_treatment_blocked.plot, file = "Output/Seed Yield/Blocked Yield.pdf",
       units = "in", width = 6, height = 6)
ggsave(seed_yield_per_plot.plot, file = "Output/Seed Yield/Yield Per Plot.pdf",
       units = "in", width = 6, height = 6)
       
# stats ----
# make data frame to account for pseudo replication
anova_summary.df <- seed_yield.df %>%
  group_by(treatment, plot) %>%
  summarize(mean_yield = mean(kg_ha, na.rm = TRUE))

# one way blocked anova
seed_yield.aov <- aov(kg_ha ~ treatment + block, data = seed_yield.df)

# anova table
Anova(seed_yield.aov, type = "III")

# estimated marginal means
seed_yield.emm <- emmeans(seed_yield.aov, ~treatment)
plot(seed_yield.emm, comparisons = TRUE) 
seed_yield_results.emminteraction <- emmeans(seed_yield.emm, 
                                             pairwise ~ treatment, adjust = "bonferroni")
seed_yield_results.emminteraction$contrasts

# results ----

# one way blocked anova
# Response: kg_ha
#               Sum Sq   Df    F value     Pr(>F)   
# (Intercept)  507715    1     7.9477     0.011819 * 
#   treatment  1071104   2     8.3835     0.002928 **
#   block      39668     1     0.6210     0.441544   
# Residuals    1085995   17     

#emmeans
# contrast   estimate  SE   df  t.ratio  p.value
# PC - PC_N     -47.8  120  17  -0.398   1.0000 
# PC - ref      621.8  166  17   3.736   0.0049 
# PC_N - ref    669.6  171  17   3.906   0.0034 

# For seed yield, accounting for pseudorepliation and blocking between the north and south plots, 
# there is a statistical difference between PC and ref (p = 0.0049), 
# and PC_N and ref (p = 0.0034) and no statistical difference between PC and PC_N (p = 1.000).
# There was no significant effect of blocking (p = 0.441544).
# Mean seed yield (kg/ha) for PC = 646.5, PC_N = 694.3, ref = 24.7.
