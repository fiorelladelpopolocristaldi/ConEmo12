#  Visual Analysis --------------------------------------------------------

run_script("1_outliers.R", analysis = "visual")
run_script("2_analysis.R", analysis = "visual")
run_script("3_exploratory.R", analysis = "visual")
run_script("4_paper_figures_tables.R", analysis = "visual")

# Auditory Analysis -------------------------------------------------------

run_script("1_outliers.R", analysis = "auditory")
run_script("2_analysis.R", analysis = "auditory")
run_script("3_exploratory.R", analysis = "auditory")
run_script("4_paper_figures_tables.R", analysis = "auditory")

# Visual vs Auditory Analysis ---------------------------------------------

run_script("study1vs2.R", analysis = "1vs2")

# Create supplementary ----------------------------------------------------

rmarkdown::render("supplementary/supplementary.Rmd", quiet = T)