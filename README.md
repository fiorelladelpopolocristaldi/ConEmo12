# Repository

This is the repository for the analysis script of the paper *Does your past define you? The role of previous visual experience in predicting new affective pictures and sounds*. This folder allow to fully reproduce the analysis reported in the paper and producing tables, figures and the supplementary materials.

# Folders organization

The folder is organized as using `.Rprojects` and all function within the `R/` folder are automatically loaded in the global environment.

- `data/`: contains raw data for each study both in `.rds` and `.csv` format
- `figures/`: contains all figures
- `tables/`: contains all tables
- `objects/`: contains all objects created by analysis scripts
- `R/`: contains all custom functions used in the project
- `script_visual/auditory`: contains all script to reproduce the analysis:
  - `*_outliers`: detect and remove relevant outliers
  - `*_analysis`: the main analysis reported in the paper
  - `*_exploratory`: extra analysis included in the supplementary materials
  - `*_paper_figures_tables`: create figures and tables
- `script_1vs2`: contains the analysis script for comparing study 1 vs study 2
- `supplementary`: contains the `.Rmd` file to create the supplementary materials in `.pdf` format
- `renv/`: contains the R environment with all packages version in order to reproduce the analysis
- `main_script.R`: is the main script that call each sub-script to run the full analysis

## Suggested workflow

1. Check if `renv` is correctly loaded (eventually use `renv::activate()`)
2. Run the `main_script.R` file

# Dataset description

- `workerId`: is the unique identifier for each subject
- `group`: is the between-subject variable that identify the experimental group (UG = and CG = )
- `cond`: is the relevant condition in that trial: (exp = Experience rating and val_arr = Valence/Arousal rating)
- `valence`: the valence of the stimulus (Negative and Neutral)
- `S1_color`: the color of the stimulus
- `Cong`: 
- `arrating/valrating/exprating`: are the response variables

The dataset is organized in long format where each line is a trial. The `cond` variable indicate which response variable is relevant for that specific trial.

## Packages

- lme4
- lmerTest
- influence.ME
- tidyverse
- Routliers
- here
- effects
- broom
- broom.mixed
- emmeans
- ggeffects
- cowplot
- ggthemes
- flextable
- officer
