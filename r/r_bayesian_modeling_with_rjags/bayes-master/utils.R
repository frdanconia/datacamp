library(rjags)
#library(glm)

files = c("test_analysis.R", "mcmc_plot.R", "iliadis_plot.R", "corner_plot.R")
prefix = "~/Documents/school/iliadis/bayes report/utils"

for(f in files){
    source(paste(prefix,f,sep="/"))
}