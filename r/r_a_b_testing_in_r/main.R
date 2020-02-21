Sys.setenv(RSTUDIO_PANDOC='/Applications/RStudio.app/Contents/MacOS/pandoc')

# ---- Install Packages ----
list.of.packages <- c('ggplot2')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages)

# Step One: Simulate Data:
source("code/data.R")

# Step Two: Create Plots for Report:
source("code/plots.R")

# Step Three: Create Analysis:
rmarkdown::render('rmd/analysis.Rmd', 'github_document', '../README.md')
