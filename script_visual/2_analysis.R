visual_analysis <- function(){

# Environment -------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(influence.ME)
library(broom)
library(broom.mixed)
library(emmeans)
library(here)

emmeans::emm_options(lmerTest.limit = 10000) # for t-test in emmeans

# Data --------------------------------------------------------------------

dat <- read_rds(here("data", "data_visual", "data_no_outlier.rds"))

# Modelling ---------------------------------------------------------------

dat_exp <- dat %>% dplyr::filter(cond == "exp")
dat_val_arr <- dat %>% dplyr::filter(cond == "val_arr")

# Preregistred models

fit_exp <- lmer(exprating ~ group * s1_color + (s1_color|workerId),
                data = dat_exp,
                na.action = na.fail)
fit_val <- lmer(valrating ~ group * valence + (valence|workerId),
                data = dat_val_arr,
                na.action = na.fail)
fit_arr <- lmer(arrating ~ group * valence + (valence|workerId),
                data = dat_val_arr,
                na.action = na.fail)

mods <- list(
    fit_exp = fit_exp,
    fit_val = fit_val,
    fit_arr = fit_arr
)

# Post-Hoc Contrast -------------------------------------------------------

post_fit_exp <- get_contrast_and_effect_size(fit_exp, term = "s1_color|group", sd = "total")
post_fit_val <- get_contrast_and_effect_size(fit_val, term = "valence", sd = "total")
post_fit_arr <- get_contrast_and_effect_size(fit_arr, term = "valence",  sd = "total")

post_hoc_effsize <- list(
    post_fit_exp = post_fit_exp,
    post_fit_val = post_fit_val,
    post_fit_arr = post_fit_arr
)

# Anova -------------------------------------------------------------------

anova_models <- map(mods, function(x) broom.mixed::tidy(anova(x)))

#  Confidence Intervals ---------------------------------------------------

confint_mods <- map(mods, function(mod) confint(mod, level = 0.95, method = "Wald"))

# Saving ------------------------------------------------------------------

prereg_list <- list(
    mods = mods,
    anova_models = anova_models,
    post_hoc_effsize = post_hoc_effsize,
    confint_mods = confint_mods
)

saveRDS(prereg_list, file = here("objects", "obj_visual", "prereg_list.rds"))
}