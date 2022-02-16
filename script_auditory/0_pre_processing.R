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

dat <- read.csv(here(data_path, "raw", "jatos.csv"),
                sep = ";", header = T) %>% tibble() # full dataset
prolific <- read.csv(here(data_path, "raw", "prolific.csv"),
                     sep = ";", header = T) %>% tibble() # prolific data (only subjects to keep)

# General Variables for debug ---------------------------------------------

# To check if the joining process is correct

ntrials <- 80 # number of trials
ntrials_exp <- 40 # number of trials exp
ntrials_val_arr <- 40 # number of trials val_arr
nsubjects <- nrow(prolific) # number of subjects
nrow_dataset <- ntrials * nsubjects

# Data Cleaning -----------------------------------------------------------

prolific_clean <- prolific %>% 
    rename("sex" = Sex) %>% 
    rename("PROLIFIC_PID" = participant_id)

dat_clean <- dat %>% 
    filter(PROLIFIC_PID %in% prolific_clean$PROLIFIC_PID) %>%  # filtering good subjects from the main dataset
    filter(phase == "T") %>% # selecting only test phase
    filter(AttCheck != "X") %>% # filtering out attention check trials
    filter(Proc != "P") # filtering out practice trials

# Adding sex and gender and questionnaires

dat_clean <- dat_clean %>% 
    left_join(., prolific_clean %>% select(PROLIFIC_PID, age, sex),
              by = "PROLIFIC_PID")

# Creating Trial indexes --------------------------------------------------

dat_clean <- dat_clean %>% 
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
           "arrating" = samarrating,
           "prolific_id" = PROLIFIC_PID) %>%
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
    
    select(workerId, prolific_id, group, cond, valence, exprating, valrating, 
           arrating, block, trial_order, trial_cond, s1_color, Cong, age, sex,
           ITI, ISI, response_time_ISI_EXP, response_time_SAM_valence, 
           response_time_SAM_arousal, timing_S1, timing_S2)

# Saving ------------------------------------------------------------------

saveRDS(dat_clean_final, here(data_path, "data_cleaned.rds"))
write_csv(dat_clean_final, here(data_path, "data_cleaned.csv"))