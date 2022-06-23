
               #CAN WE ACCOUNT FOR HANDLING/WATER LOSS FROM BAGS BASED ON PLANT TYPE? 

# LIBRARIES ----
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(plotly)
library(colorRamps)

# READ IN AND MODIFY DATA FILE ----
# read in the data files and clean it up
bag_weight.df <- read_csv("Data/bag_handling_loss.csv") %>% clean_names() %>%
  remove_empty(which = c("cols", "rows"))
  
# rename the column headers to something more concise and informative
bag_weight.df <- bag_weight.df %>%
  rename(
    initial_weight = pre_placement_weight,
    final_weight = post_sample_weight_2_6_17)

# determine the difference between initial and final weights
bag_weight.df <- bag_weight.df %>% 
  mutate(
    difference = initial_weight - final_weight
  )

# WHAT IS THE LOSS AS A PERCENT? ----
#adds a column that determines loss as a percent
bag_weight.df <- bag_weight.df %>%
  mutate(
    percent_loss = (difference / initial_weight) * 100
  )

# SUMMARIZING THE DATA ----
bag_summary.df <- bag_weight.df %>%
  group_by(plant_type) %>%
  summarize(avg_percent_loss = mean(percent_loss, na.rm = TRUE), 
            seloss_percent = sd(percent_loss, na.rm = TRUE) / sqrt(sum(!is.na(percent_loss))))

#save summary file 
write_csv(bag_summary.df, "Output/Bag Loss/Handling Loss Correction.csv") 
write_csv(bag_weight.df, "Output/Bag Loss/Updated T=0 Bag Loss")

# CREATE GRAPHS ----
# average weight loss
mean_weight_loss.plot <- ggplot(data = bag_weight.df, aes(x = plant_type, y = difference, color = plant_type)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3)) + theme(axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA)) +labs(x = "Treatment", y = "Average Weight Loss")

# average percent loss
mean_percent_loss.plot <- ggplot(data = bag_weight.df, aes(x = plant_type, y = percent_loss, color = plant_type, size = 3)) + 
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "point", size = 4,
    position = position_dodge(width = 0.3)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = .3,size=.7,
    position = position_dodge(width = .3)) +
  theme(axis.line = element_line(linetype = "solid"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +labs(x = "Treatment", y = "Average Percent Loss")


# call the plots to check them 
mean_weight_loss.plot
mean_percent_loss.plot

# SAVE PLOTS AS OUTPUTS ----
#saves the plot as a pdf
ggsave(mean_weight_loss.plot, file = "Output/Bag Loss/Mean Bag Weight Loss.pdf", units = "in", width = 6, height = 6)
ggsave(mean_percent_loss.plot, file = "Output/Bag Loss/Mean Bag Percent Loss.pdf", units = "in", width = 6, height = 6)

# Results ----
# Weight loss for annual rye (AR) and cereal rye (CR) is both similar and high relative to 
# the wild type (PC) and genetically modified strands of pennycress (GM_PC).
# When preparing the bags we thought that handling losses would be high for both types of 
# of PC relative to AR and CR. These results differ from this hypothesis and signify that
# the weight losses are likely due to water weight lost when AR and CR were in the drying
# oven post placement in the study plots, noting that these are all T = 0 samples. 

#plant_type          avg_percent_loss
# 1 AR                    6.789 
# 2 CR                    6.016
# 3 GM_PC                 0.766
# 4 PC                    2.058

# plant_type        avg_percent_loss   seloss_percent  
# Length:5          Min.   :0.7663     Min.   :0.2979  
# Class :character  1st Qu.:1.7352     1st Qu.:0.4199  
# Mode  :character  Median :4.0371     Median :0.4660  
# Mean   :3.9075    Mean   :0.6933  
# 3rd Qu.:6.2093    3rd Qu.:0.7394  
# Max.   :6.7893    Max.   :1.5431  
