#  Visual Analysis --------------------------------------------------------

run_script(visual_outliers)
run_script(visual_analysis)
run_script(visual_exploratory)
run_script(visual_figures_tables)

# Auditory Analysis -------------------------------------------------------

run_script(auditory_outliers)
run_script(auditory_analysis)
run_script(auditory_exploratory)
run_script(auditory_figures_tables)

# Visual vs Auditory Analysis ---------------------------------------------

run_script(study_1vs2_analysis)

# Create supplementary ----------------------------------------------------

rmarkdown::render("supplementary/supplementary.Rmd", quiet = T)