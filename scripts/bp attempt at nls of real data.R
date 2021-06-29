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

