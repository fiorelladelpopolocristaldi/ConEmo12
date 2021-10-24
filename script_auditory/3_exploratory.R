auditory_exploratory <- function(){

## ------------------------------------------------------------------------
## Project: Study2
##
## Script: exploratory
## ------------------------------------------------------------------------

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

fun_list <- list(anova_table = get_anova_table) # broom::tidy(anova(fit))

# This apply the list of fun to all models within another list

complete_model_analysis <- function(mod_list, fun_list){
    out <- map(fun_list, function(fun) map(mod_list, fun))
    index <- length(out) + 1
    out[[index]] <- mod_list
    names(out)[index] <- "mods"
    return(out)
}

# Data --------------------------------------------------------------------

dat <- read_rds(here("data", "data_auditory", "data_no_outlier.rds"))

# Modelling ---------------------------------------------------------------

# Effect coding

contrasts(dat$group) <- c(0.5, -0.5)
contrasts(dat$valence) <- c(0.5, -0.5)
contrasts(dat$s1_color) <- c(0.5, -0.5)
contrasts(dat$Cong) <- c(0.5, -0.5)

# Centering relevant variables

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

# Saving ------------------------------------------------------------------

exp_analysis <- list(
    val_mods_cong = val_mods_cong,
    arr_mods_cong = arr_mods_cong
)

saveRDS(exp_analysis, file = here("objects", "obj_auditory", "expl_list.rds"))
}