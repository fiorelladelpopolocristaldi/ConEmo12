visual_outliers <- function(){ 

# Environment -------------------------------------------------------------

library(lme4)
library(lmerTest)
library(influence.ME)
library(tidyverse)
library(Routliers)
library(here)

# Loading Data ------------------------------------------------------------

dat <- read_rds(here("data", "data_visual", "data_cleaned.rds"))

# General Parameters ------------------------------------------------------

threshold_mad <- 3 # threshold for MAD
threshold_mcd <- 0.25 # threshold for MCD
cut_off_cook <- 1 # cut off for cook distances
cut_off_dfbeta <- 2/sqrt(length(unique(dat$workerId))) # cut off of dfbetas

# Fitting Pre-Registraton Models ------------------------------------------

# Effect coding

contrasts(dat$group) <- c(0.5, -0.5)
contrasts(dat$valence) <- c(0.5, -0.5)
contrasts(dat$s1_color) <- c(0.5, -0.5)

# Data for models

dat_exp <- dat %>% dplyr::filter(cond == "exp")
dat_val_arr <- dat %>% dplyr::filter(cond == "val_arr")

# Fit

fit_exp <- lmer(exprating ~ group * s1_color + (1|workerId), data = dat_exp, na.action = na.fail)
fit_val <- lmer(valrating ~ group * valence + (valence|workerId), data = dat_val_arr,  na.action = na.fail)
fit_arr <- lmer(arrating ~ group * valence + (valence|workerId), data = dat_val_arr,  na.action = na.fail)

mods <- list(
    fit_exp = fit_exp,
    fit_val = fit_val,
    fit_arr = fit_arr
)

# MAD and MCD -------------------------------------------------------------

# Creating dataset for outliers (MAD and MCD) computing the mean for each
# variable

dat %>%
    dplyr::filter(cond == "exp") %>%
    select(workerId, exprating, s1_color) -> temp_exp

dat %>%
    dplyr::filter(cond == "val_arr") %>%
    group_by(workerId, valence) -> temp_val_arr

temp_exp %>%
    group_by(workerId, s1_color) %>%
    summarise(exprating = mean(exprating)) %>%
    mutate(s1_color = paste0("exprating_", s1_color)) %>%
    pivot_wider(names_from = s1_color, values_from = exprating) %>%
    ungroup() -> out_data_exp

temp_val_arr %>%
    group_by(workerId, valence) %>%
    summarise(arrating = mean(arrating),
              valrating = mean(valrating)) %>%
    pivot_wider(names_from = valence, values_from = c(arrating, valrating)) %>%
    left_join(out_data_exp, by = "workerId") %>%
    pivot_longer(2:ncol(.), names_to = "resp", values_to = "value")  %>%
    group_by(workerId) %>%
    mutate(id = cur_group_id()) %>%
    ungroup() -> out_data

# Univariate analysis - MAD

out_data %>%
    group_by(resp) %>%
    nest() %>%
    mutate(
        mad = map(data, function(x) {
            outliers_mad(x$value, threshold = threshold_mad)$outliers_pos
            }),
        out_mad = map2(data, mad, function(x,y) ifelse(x$id %in% y, TRUE, FALSE))) %>%
    select(resp, data, out_mad) %>%
    unnest(c(data, out_mad)) %>%
    select(-value, -id) %>%
    rename("resp_mad" = resp) -> out_mad

# Multivariate analysis - MCD

get_mcd_outlier <- function(data){
    outliers_mcd(x = data.frame(data[, c("arrating", "valrating")]),
                 h = threshold_mcd)$outliers_pos
}

out_data %>%
    dplyr::filter(!str_detect(resp, "exprating")) %>%
    separate(resp, into = c("resp", "cond")) %>%
    group_by(cond) %>%
    nest() %>%
    mutate(data_wide = map(data, function(x) x %>% pivot_wider(names_from = resp, values_from = value)), # from long to wide
           mcd = map(data_wide, get_mcd_outlier), # get id of outliers
           out_mcd = map2(data, mcd, function(x,y) ifelse(x$id %in% y, TRUE, FALSE))) %>% # get TRUE if outlier
    ungroup() %>%
    select(data, out_mcd, cond) %>%
    unnest(c(data, out_mcd)) %>%
    mutate(resp_mcd = paste0("val_arr_", cond)) %>%
    select(-resp, -value, -cond, -id) %>%
    distinct() -> out_mcd

# Cook Distance ------------------------------------------------------

cook_list <- map(mods, function(mod) get_cook_table(mod, "workerId")) # get influence table for each model

cook_table <- bind_rows(cook_list, .id = "mod")

cook_table <- cook_table %>%
  mutate(out = ifelse(cook > cut_off_cook, TRUE, FALSE))

# Visualizing outliers ----------------------------------------------------

filter_outliers <- function(out_data, resp = "all"){

    if(resp == "all"){
        out_data %>%
            select(workerId, out_mad, resp_mad) %>%
            dplyr::filter(out_mad) %>%
            ungroup()
    }else{
        out_data %>%
            select(workerId, out_mad, resp_mad) %>%
            dplyr::filter(out_mad & str_detect(resp_mad, resp)) %>%
            ungroup()
    }

}

dat %>%
    dplyr::filter(cond == "exp") %>%
    group_by(workerId, s1_color, group) %>%
    summarise(exprating = mean(exprating)) %>%
    ungroup() %>%
    mutate(out = ifelse(workerId %in% filter_outliers(out_mad, "exprating")$workerId,
                        "yes",
                        "no")) %>%
    ggplot(aes(x = s1_color, color = out, y = exprating, group = workerId)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_line(alpha = 0.4, size = 1) +
    facet_wrap(~group)

dat %>%
    dplyr::filter(cond == "val_arr") %>%
    group_by(workerId, valence, group) %>%
    summarise(valrating = mean(valrating)) %>%
    ungroup() %>%
    mutate(out = ifelse(workerId %in%filter_outliers(out_mad, "valrating")$workerId,
                        "yes",
                        "no")) %>%
    ggplot(aes(x = valence, color = out, y = valrating, group = workerId)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_line(alpha = 0.4, size = 1) +
    facet_wrap(~group)

dat %>%
    dplyr::filter(cond == "val_arr") %>%
    group_by(workerId, valence, group) %>%
    summarise(arrating = mean(arrating)) %>%
    ungroup() %>%
    mutate(out = ifelse(workerId %in% filter_outliers(out_mad, "arrating")$workerId,
                        "yes",
                        "no")) %>%
    ggplot(aes(x = valence, color = out, y = arrating, group = workerId)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_line(alpha = 0.4, size = 1) +
    facet_wrap(~group)

# Excluding outliers from dataset -----------------------------------------

dat_no_out <- dat %>%
    dplyr::filter(!(workerId %in% filter_outliers(out_mad)$workerId))

# Saving ------------------------------------------------------------------

outliers <- list(
    out_mad = out_mad,
    out_mcd = out_mcd,
    cook_table = cook_table
)

saveRDS(outliers, here("objects", "obj_visual", "prereg_outliers.rds"))
saveRDS(dat_no_out, here("data", "data_visual", "data_no_outlier.rds"))

}
