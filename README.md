# Repository

This is the repository for the analysis script of the paper *Does your past define you? The role of previous visual experience in predicting new affective pictures and sounds*. This folder allows to fully reproduce the analysis reported in the paper, and producing tables, figures and the supplementary materials.

# Folder's organization

The folder is organized as using `.Rprojects`, and all functions within the `R/` folder are automatically loaded in the global environment.

- `data/`: contains raw data for each experiment, both in `.rds` and `.csv` format
- `figures/`: contains all figures
- `tables/`: contains all tables
- `objects/`: contains all objects created by analysis scripts
- `R/`: contains all custom functions used in the project
- `script_visual/auditory`: contains all script to reproduce the analysis:
  - `*_outliers`: detect and remove relevant outliers
  - `*_analysis`: the main analysis reported in the paper
  - `*_exploratory`: extra analysis included in the supplementary materials
  - `*_paper_figures_tables`: create figures and tables
- `script_1vs2`: contains the analysis script for comparing experiment 1 vs experiment 2
- `supplementary`: contains the `.Rmd` file to create the supplementary materials in `.pdf` format
- `renv/`: contains the R environment with all packages version in order to reproduce the analysis
- `main_script.R`: is the main script that call each sub-script to run the full analysis

## Suggested workflow

1. Open the `analysis.Rproj` file. This file automatically load all functions and the `renv` environment
2. Run the `main_script.R` file. This script can be launched directly `source("main_script.R")` or each analysis step separately using `run_script()`.

# Dataset description

- `workerId`: unique identifier for each subject
- `group`: between-subject variable that identifies the experimental group (UG = uncertain group and CG = certain group)
- `cond`: relevant response required in each trial: exp = Expectancy rating, val_arr = Valence/Arousal rating
- `S1_color`: the color of the S1 stimulus (red and blue)
- `valence`: the valence of the S2 stimulus (Negative and Neutral)
- `Cong`: the S2 congruency: 0 = congruent (trials of the test phase in which the S1-S2 pairing was more likely, i.e., 75%), 1 = incongruent (trials of the test phase in which S1-S2 pairing was less likely, i.e., 25%)
- `exprating/valrating/arrating`: response variables: exprating = expectancy rating, valrating = valence rating, arrating = arousal rating

The dataset is organized in long format, where each line is a trial. The `cond` variable indicate which response variable is relevant for that specific trial.

## Packages

- `lme4`
- `lmerTest`
- `influence.ME`
- `tidyverse`
- `Routliers`
- `here`
- `effects`
- `broom`
- `broom.mixed`
- `emmeans`
- `ggeffects`
- `cowplot`
- `ggthemes`
- `flextable`
- `officer`
