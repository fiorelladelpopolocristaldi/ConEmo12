## ------------------------------------------------------------------------
## Project: Study2_auditory
##
## Script: Pre-processing
## ------------------------------------------------------------------------

# Environment -------------------------------------------------------------

library(tidyverse)
library(here)

# Importing Data ----------------------------------------------------------

data_path <- here("data", "data_auditory")

dat <- read_rds(here(data_path, "dat_raw.rds"))

# Creating Trial indexes --------------------------------------------------

dat_clean <- dat %>% 
    group_by(workerId) %>% 
    mutate(trial_order = 1:n(), # this is the trial order in temporal terms
           block = ifelse(trial_order <= 40, 1, 2), # this is the block = 1 first 40 trials, block = 2 41 to 80 trial
           cond = ifelse(EXP == "X", "exp", "val_arr")) %>% # better cond variable
    group_by(workerId, cond) %>% 
    mutate(trial_cond = 1:n()) %>% # create trials from 1 to 20 for each condition (valrating, arrating) * valence
    ungroup()

# Color Variable ----------------------------------------------------------

# here we recode the cue variable in order to assign the color-valence
# match of the main experiment

dat_clean <- dat_clean %>% 
    mutate(s1_color = case_when(Proc == "A" & S1 == "red" ~ "neg",
                                Proc == "A" & S1 == "blue" ~ "neu",
                                Proc == "B" & S1 == "red" ~ "neu",
                                Proc == "B" & S1 == "blue" ~ "neg",
                                Proc == "C" & S1 == "red" ~ "neg", 
                                Proc == "C" & S1 == "blue" ~ "neu",
                                Proc == "D" & S1 == "red" ~ "neu",
                                Proc == "D" & S1 == "blue" ~ "neg",
                                TRUE ~ S1))

# Final Cleaning ----------------------------------------------------------

dat_clean_final <- dat_clean %>% 
    rename("valence" = S2_val,
           "group" = GROUP,
           "valrating" = samvalrating,
           "arrating" = samarrating) %>% 
    mutate(s1_color = factor(s1_color),
           group = factor(group),
           valence = factor(valence),
           workerId = factor(workerId),
           Cong = factor(Cong),
           block = factor(block),
           sex = factor(sex),
           cond = ifelse(EXP == "X", "exp", "val_arr"),
           timing_S1 = 250, # timing stimuli
           timing_S2 = 1000) %>% # timing stimuli
    
    # select relevant variables
    
    select(workerId, group, cond, valence, exprating, valrating, 
           arrating, block, trial_order, trial_cond, s1_color, Cong, age, sex,
           ITI, ISI, response_time_ISI_EXP, response_time_SAM_valence, 
           response_time_SAM_arousal, timing_S1, timing_S2)

# Saving ------------------------------------------------------------------

saveRDS(dat_clean_final, here(data_path, "data_cleaned.rds"))
write_csv(dat_clean_final, here(data_path, "data_cleaned.csv"))