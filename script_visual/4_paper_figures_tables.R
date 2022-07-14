# Packages ----------------------------------------------------------------

library(tidyverse)
library(broom)
library(broom.mixed)
library(ggeffects)
library(effects)
library(cowplot)
library(ggthemes)
library(flextable)
library(officer)
library(emmeans)
library(here)

# Functions ---------------------------------------------------------------

save_table <- function(tab, prop, name){
    save_as_docx(tab, path = name, pr_section = prop)
}

# Loading -----------------------------------------------------------------

prereg_list <- read_rds(here("objects", "obj_visual", "prereg_list.rds"))
exp_list <- read_rds(here("objects", "obj_visual", "expl_list.rds"))

full_dat <- read_rds(here("data", "data_visual", "data_no_outlier.rds"))

dat_exp <- full_dat %>% dplyr::filter(cond == "exp")
dat_val_arr <- full_dat %>% dplyr::filter(cond == "val_arr")

mod_list_pre <- c(
    fit_val = prereg_list$mods$fit_val,
    fit_arr = prereg_list$mods$fit_arr,
    fit_exp = prereg_list$mods$fit_exp
)

mod_list_cong <- c(
    fit_arr = exp_list$arr_mods_cong$mods$fit_arr_cong,
    fit_val = exp_list$val_mods_cong$mods$fit_val_cong
)

all_mods <- list(mod_list_pre = mod_list_pre,
                 mod_list_cong = mod_list_cong)

all_table <- tibble(
    models = all_mods,
    names = names(all_mods)
)

# General Setting ---------------------------------------------------------

sect_properties <- prop_section(
    page_size = page_size(orient = "landscape",
                          width = 8.3, height = 11.7),
    type = "continuous",
    page_margins = page_mar()
)

# Tables -------------------------------------------------------------------

# Models

all_table_mod <- all_table %>%
    mutate(tidy = map_depth(models, tidy_fit, .depth = 2),
           tidy = map_depth(tidy, prep_names_model, .depth = 2),
           tidy = map(tidy, bind_rows, .id = "mod"),
           table = map(tidy, model_table),
           save_names = str_remove(names, "mod_list_"),
           save_names = paste0("tables/", "table_visual/", "mod_table_", save_names, "_visual.docx"))

# Anova

all_table_anova <- all_table %>%
    mutate(tidy = map_depth(models, tidy_anova, .depth = 2),
           tidy = map_depth(tidy, prep_names_anova, .depth = 2),
           tidy = map(tidy, bind_rows, .id = "mod"),
           table = map(tidy, anova_table),
           save_names = str_remove(names, "mod_list_"),
           save_names = paste0("tables/", "table_visual/", "anova_table_", save_names, "_visual.docx"))

# Saving

map2(all_table_mod$table, all_table_mod$save_names, function(table, name) {
    save_table(table, prop = sect_properties, name)
})

map2(all_table_anova$table, all_table_anova$save_names, function(table, name) {
    save_table(table, prop = sect_properties, name)
})

saveRDS(all_table_anova, file = here("objects", "obj_visual", "anova_paper.rds"))
saveRDS(all_table_mod, file = here("objects", "obj_visual", "mod_paper.rds"))

# Figures -----------------------------------------------------------------

# Preregistration

eff_exp_pre <- get_effects(mod_list_pre$fit_exp,
                             y = exprating, workerId, group, s1_color)
eff_val_pre <- get_effects(mod_list_pre$fit_val,
                             y = valrating, workerId, group, valence)
eff_arr_pre <- get_effects(mod_list_pre$fit_arr,
                            y = arrating, workerId, group, valence)

dat_plot_pre <- bind_rows(eff_exp_pre, eff_val_pre, eff_arr_pre) %>%
    clean_names_plot(., mod = "prereg") %>%
    unite(cond, valence, s1_color, sep = "") %>% 
    mutate(cond = ifelse(resp == "Expectancy",
                         sprintf("Cue<sub>%s</sub>", cond),
                         sprintf("S2<sub>%s</sub>", cond)))

pre_plot <- box_plot(dat_plot_pre, cond)

# Congruency

eff_arr_cong <- get_effects(mod_list_cong$fit_arr, y = arrating,
                            workerId, group, valence, Cong)

eff_val_cong <- get_effects(mod_list_cong$fit_val, y = valrating,
                            workerId, group, valence, Cong)

dat_plot_cong <- bind_rows(eff_arr_cong, eff_val_cong) %>%
    clean_names_plot(., mod = "cong")

cong_plot <- dat_plot_cong %>% 
    mutate(resp = case_when(resp == "Exprating" ~ "Expectancy",
                            resp == "Valrating" ~ "Valence",
                            resp == "Arrating" ~ "Arousal",
                            TRUE ~ resp),
           resp = factor(resp, levels = c("Expectancy", "Valence", "Arousal"))) %>%
    filter(resp != "Expectancy") %>% 
    ggplot(aes(x = Cong, y = .mean, fill = group)) +
    geom_point(aes(color = group),
               position=position_jitterdodge(jitter.width = 0.4, jitter.height = 0)) +
    geom_boxplot(outlier.shape=NA, alpha = 0.7) +
    ggh4x::facet_nested(~resp+valence, scales = "free_x") +
    theme_paper() +
    scale_color_manual(name = "", values=c("#4DCA87", "#F09A0F")) +
    scale_fill_manual(name = "", values=c("#4DCA87", "#F09A0F")) +
    ylab("Rating (%)")

# Saving

plot_list <- list(
    pre_plot = pre_plot,
    cong_plot = cong_plot
)

plot_names <- c("plot_prereg_visual.png",
                "plot_cong_visual.png")

plot_names <- file.path("figures", "fig_visual", plot_names)

map2(plot_names, plot_list, function(name, plot) cowplot::save_plot(name, plot, base_height = 6))

saveRDS(plot_list, file = here("objects", "obj_visual", "plot_paper.rds"))

# Model Info to Report ----------------------------------------------------

r2_table <- all_table_mod %>%
    select(models, names) %>%
    unnest(models) %>%
    mutate(mod_names = names(models),
           r2_marg = map_dbl(models, r2, "marg"),
           r2_cond = map_dbl(models, r2, "cond"),
           formula = map_chr(models, get_formula)) %>%
    select(-models)

saveRDS(r2_table, here("objects", "obj_visual", "r2_table.rds"))

emmeans_list <- c(
    fit_exp = list(prereg_list$post_hoc_effsize$post_fit_exp),
    fit_val = list(prereg_list$post_hoc_effsize$post_fit_val),
    fit_arr = list(prereg_list$post_hoc_effsize$post_fit_arr)
)

emmeans_tab <- tibble(
    emmeans = emmeans_list
)

emmeans_tab <- emmeans_tab %>%
    mutate(mod = names(emmeans),
           tab = map(emmeans, table_emmeans)) %>%
    unnest(tab) %>%
    select(mod, term, contrast, estimate, std.error, df, conf.low,
           conf.high, statistic, p.value, effect.size_es)

saveRDS(emmeans_tab, file = here("objects", "obj_visual", "emmeans_tab_prereg.rds"))