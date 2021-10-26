# Environment -------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(influence.ME)
library(broom)
library(cowplot)
library(broom.mixed)
library(emmeans)
library(here)

# Importing Data ----------------------------------------------------------

dat <- read_rds(here("data", "data_12", "data_studies_cleaned.rds"))

# Analysis ----------------------------------------------------------------

# Effect coding

contrasts(dat$study) <- c(0.5, -0.5)
contrasts(dat$group) <- c(0.5, -0.5)
contrasts(dat$valence) <- c(0.5, -0.5)
contrasts(dat$s1_color) <- c(0.5, -0.5)

# Data for models

dat_exp <- dat %>% dplyr::filter(cond == "exp")
dat_val_arr <- dat %>% dplyr::filter(cond == "val_arr")

# Preregistred models

fit_exp_st <- lmer(exprating ~ study * group * s1_color + (s1_color|workerId),
                data = dat_exp,
                na.action = na.fail)
fit_val_st <- lmer(valrating ~ study * group * valence + (valence|workerId),
                data = dat_val_arr,
                na.action = na.fail)
fit_arr_st <- lmer(arrating ~ study * group * valence + (valence|workerId),
                data = dat_val_arr,
                na.action = na.fail)

# Model effects

## Exprating

plot_exp_12 <- dat_exp %>%
    group_by(workerId, study, group, s1_color) %>%
    summarise(exprating = mean(exprating)) %>%
    mutate(study = ifelse(study == 1, "Experiment 1", "Experiment 2")) %>%
    ggplot(aes(x = s1_color, y = exprating, fill = s1_color)) +
    geom_point(aes(color = s1_color),
               alpha = 0.5,
               position=position_jitterdodge(jitter.width = 0.4, jitter.height = 0)) +
    geom_boxplot(outlier.shape=NA, alpha = 0.7) +
    ggh4x::facet_nested("Expectancy"~study+group, scales = "free_x") +
    theme_paper() +
    scale_color_manual(name = "", values=c("#00BFC4", "#F8766D")) +
    scale_fill_manual(name = "", values=c("#00BFC4", "#F8766D")) +
    ylab("Rating (%)") +
    theme(legend.position = "none") +
    ylim(c(0,100))

## Valrating

plot_val_12 <- dat_val_arr %>%
    group_by(workerId, study, valence) %>%
    summarise(valrating = mean(valrating)) %>%
    mutate(study = ifelse(study == 1, "Experiment 1", "Experiment 2")) %>%
    ggplot(aes(x = valence, y = valrating, fill = valence)) +
    geom_point(aes(color = valence),
               alpha = 0.5,
               position=position_jitterdodge(jitter.width = 0.4, jitter.height = 0)) +
    geom_boxplot(outlier.shape=NA, alpha = 0.7, width=0.4) +
    facet_grid("Valence"~study, scales = "free_x") +
    theme_paper() +
    scale_color_manual(name = "", values=c("#4DCA87", "#F09A0F")) +
    scale_fill_manual(name = "", values=c("#4DCA87", "#F09A0F")) +
    ylab("Rating (%)") +
    theme(legend.position = "none") +
    ylim(c(0,100))

## Arrating

plot_arr_12 <- dat_val_arr %>%
    group_by(workerId, study) %>%
    summarise(arrating = mean(arrating)) %>%
    mutate(study = ifelse(study == 1, "Experiment 1", "Experiment 2")) %>%
    ggplot(aes(y = arrating, fill = study, x = 1)) +
    geom_point(aes(color = study),
               alpha = 0.8,
               position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0)) +
    geom_boxplot(outlier.shape=NA, alpha = 0.7, width=0.2) +
    facet_grid("Arousal"~study) +
    theme_paper() +
    ylab("Rating (%)") +
    theme(legend.position = "none") +
    scale_fill_manual(name = "", values=c("#787878", "#787878")) +
    scale_color_manual(name = "", values=c("grey", "grey")) +
    ylim(c(0,100))

# Combine plots

study12_plot <- plot_grid(
    plot_exp_12,
    plot_val_12 %>% remove_facets_text(),
    plot_arr_12 %>% remove_facets_text() %>% remove_axis_text(axes = "x"),
    ncol = 1,
    align = "hv"
)

save_plot(here("figures", "fig_study_12", "study12_plot.png"),
          plot = study12_plot,base_height = 9, base_width = 8)

mods_st <- list(
    fit_exp_st = fit_exp_st,
    fit_val_st = fit_val_st,
    fit_arr_st = fit_arr_st
)

r2_table <- tibble(
    mod = names(mods_st),
    r2_marginal = map_dbl(mods_st, r2, "marg"),
    r2_conditional = map_dbl(mods_st, r2, "cond"),
    formula = map_chr(mods_st, get_formula))

# Anova

anova_models_st <- map(mods_st, function(x) broom.mixed::tidy(anova(x)))

# Post-Hoc Contrast

post_fit_exp_st <- get_contrast_and_effect_size(fit_exp_st, term = "study|s1_color|group", df = "asymptotic")
post_fit_val_st <- get_contrast_and_effect_size(fit_val_st, term = "study|valence", df = "asymptotic")
post_fit_arr_st <- get_contrast_and_effect_size(fit_arr_st, term = "study", df = "asymptotic")

post_hoc_effsize_st <- list(
    post_fit_exp_st = post_fit_exp_st,
    post_fit_val_st = post_fit_val_st,
    post_fit_arr_st = post_fit_arr_st
)

#  Confidence Intervals

confint_mods_st <- map(mods_st, function(mod) confint(mod, level = 0.95, method = "Wald"))

# Saving ------------------------------------------------------------------

prereg_list_st <- list(
    mods_st = mods_st,
    study12_plot = study12_plot,
    anova_models_st = anova_models_st,
    post_hoc_effsize_st = post_hoc_effsize_st,
    confint_mods_st = confint_mods_st
)

saveRDS(prereg_list_st, file = here("objects", "obj_12", "prereg_studies_list.rds"))