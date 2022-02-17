# Environment -------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(emmeans)
library(broom.mixed)
library(here)

# Functions ---------------------------------------------------------------

# This is a list of functions that are applied to all models
# if a function is added here, it will be applied later

fun_list <- list(anova_table = tidy_anova)

# This apply the list of fun to all models within another list

complete_model_analysis <- function(mod_list, fun_list){
    out <- map(fun_list, function(fun) map(mod_list, fun))
    index <- length(out) + 1
    out[[index]] <- mod_list
    names(out)[index] <- "mods"
    return(out)
}

# Data --------------------------------------------------------------------

dat <- read_rds(here("data", "data_visual", "data_no_outlier.rds"))

# Modelling ---------------------------------------------------------------

# Effect coding

contrasts(dat$group) <- c(0.5, -0.5)
contrasts(dat$valence) <- c(0.5, -0.5)
contrasts(dat$s1_color) <- c(0.5, -0.5)
contrasts(dat$Cong) <- c(0.5, -0.5)

# Centering

dat$trial_cond_mc <- dat$trial_cond - mean(dat$trial_cond)

# Data for models

dat_exp <- dat %>% dplyr::filter(cond == "exp")
dat_val_arr <- dat %>% dplyr::filter(cond == "val_arr")

# Congruency ---------------------------------------------------------------

# Fit valrating

fit_val_cong <- lmer(valrating ~ group * valence * Cong + (valence|workerId), data = dat_val_arr)

val_cong_list <- list(
    fit_val_cong = fit_val_cong
)

val_mods_cong <- complete_model_analysis(val_cong_list, fun_list)

# Fit arrating

fit_arr_cong <- lmer(arrating ~ group * valence * Cong + (valence|workerId), data = dat_val_arr)

arr_cong_list <- list(
    fit_arr_cong = fit_arr_cong
)

arr_mods_cong <- complete_model_analysis(arr_cong_list, fun_list)

# Time --------------------------------------------------------------------

fit_exp_block <- lmer(exprating ~ group * block * s1_color + (s1_color|workerId), data = dat_exp)
fit_exp_group_trial <- lmer(exprating ~ group * trial_cond * s1_color + (s1_color|workerId), data = dat_exp)

# List of models, only for convenience

exp_mods_time_list <- list(
    fit_exp_block = fit_exp_block,
    fit_exp_group_trial = fit_exp_group_trial
)

# Apply the complete analysis

exp_mods_time <- complete_model_analysis(exp_mods_time_list, fun_list)

# Saving ------------------------------------------------------------------

exp_analysis <- list(
    val_mods_cong = val_mods_cong,
    arr_mods_cong = arr_mods_cong,
    exp_mods_time = exp_mods_time
)

saveRDS(exp_analysis, file = here("objects", "obj_visual", "expl_list.rds"))