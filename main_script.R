#  Visual Analysis (Exp 1) --------------------------------------------------------

run_script("0_pre_processing.R", analysis = "visual")
run_script("1_outliers.R", analysis = "visual")
run_script("2_analysis.R", analysis = "visual")
run_script("3_exploratory.R", analysis = "visual")
run_script("4_paper_figures_tables.R", analysis = "visual")

# Auditory Analysis (Exp 2) -------------------------------------------------------

run_script("0_pre_processing.R", analysis = "auditory")
run_script("1_outliers.R", analysis = "auditory")
run_script("1_outliers.R", analysis = "auditory")
run_script("2_analysis.R", analysis = "auditory")
run_script("3_exploratory.R", analysis = "auditory")
run_script("4_paper_figures_tables.R", analysis = "auditory")

# Visual vs Auditory Analysis (Exp 1 vs Exp 2--------------------------------------

run_script("study1vs2.R", analysis = "1vs2")

# Create supplementary materials --------------------------------------------------

rmarkdown::render("supplementary/supplementary.Rmd", quiet = TRUE)