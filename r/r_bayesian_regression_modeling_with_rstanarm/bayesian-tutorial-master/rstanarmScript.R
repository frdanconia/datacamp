################################################
#
# Single-level and hierarchical linear models with rstanarm
# Data from BeingWellProject
#
################################################


# Step 0
# Install software and prepare data
#
# note: installation is only necessary the first time you run the code, but we
# recommend checking for updates frequently
# install.packages("rstanarm")
# install.packages("bayesplot")
# install.packages("ggplot2") 
# install.packages("broom")

library(rstanarm)
library(ggplot2)
library(broom)
library(bayesplot)

# default plotting theme
theme_set(bayesplot::theme_default(base_size = 15))

# note: the line below to run chains in parallel (saves time)
options(mc.cores = parallel::detectCores())


# # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # #
# MODEL 1
# Simple regression: predicting valence from arousal
#    * repeated measures are treated independent, 
#     and within-person dependendy ignored
# # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # #

# Load data set and make a few scatterplots
dat <- read.csv("sampledata.csv", header=TRUE)
head(dat)

ggplot(dat, aes(x = arousal, y = valence)) +
  geom_point(size = 2, color = "#0065cc", alpha = 0.8) + 
  facet_wrap("PID", scales = "free") + 
  coord_cartesian(xlim = range(dat$arousal), ylim = range(dat$valence))
ggsave("plots/dataplot.pdf", width = 8, height = 8)


#------------------------
# Steps 1 & 2 (performed jointly)
#   1. Specify model formula and prior distributions 
#       (default priors used here, see ?stan_glm and ?priors)
#   2. Estimate model parameters
#------------------------

# Run model 1 with stan_glm
SingleLevelModel <- stan_glm(valence ~ arousal, data = dat)

# View summary of priors used
prior_summary(SingleLevelModel)

# Example of adapting priors and sampler settings
SingleLevelModelMod <-  stan_glm(
  valence ~ arousal,
  data = dat,
  prior = normal(0, 1, autoscale = FALSE),
  prior_intercept = normal(50, 100, autoscale = FALSE),
  iter = 4000,
  adapt_delta = 0.99
)



#------------------------
#Step 3. 
# Check sampling quality
#------------------------
#------------------------
#Step 4. 
# Look at results
#------------------------


# View model summary
summarySingleLevelModel <- summary(SingleLevelModel)
print(summarySingleLevelModel)

# Plot traceplot
plot(SingleLevelModel, "trace", pars = "arousal")

# Plot traceplot with slightly modified color scheme and axis labels
color_scheme_set("mix-brightblue-red")  # see help("color_scheme_set")
plot(SingleLevelModel, "trace", pars = "arousal") +
  labs(x = "Post-warmup Iteration", y = "Slope on arousal")      
ggsave("plots/traceplot.pdf", width = 7, height = 3.5)

# Use shinystan to explore the results
launch_shinystan(SingleLevelModel)


# Check pp plots: compare posterior predictive distribution of valence with 
# distribution of observed valence
color_scheme_set("brightblue")
pp_dist1 <- pp_check(SingleLevelModel, nreps = 100) + xlab("valence")
plot(pp_dist1)

posteriorSamples <- as.matrix(SingleLevelModel, pars = "arousal")
mean(posteriorSamples > 0.7)

#############################################
###### Figure 6: plotting regression lines ##
##########################################
# Extract the (post-warmup) posterior draws
posterior1 <- as.matrix(SingleLevelModel)
colnames(posterior1)
means1 <- colMeans(posterior1)

# Take random 100 posterior draws of intercept and slope
# 100 isn't special, but enough to show uncertainty without
# making the graph unreadable
betas <- posterior1[sample(nrow(posterior1), 100), 1:2]


# Plot regression lines implied by the betas
blues <- color_scheme_get("brightblue")
mod1p1 <- ggplot(dat, aes(x = arousal, y = valence)) +
  geom_point(color = "gray30") +
  geom_abline(
    intercept = betas[, 1], 
    slope = betas[, 2],
    color = blues[[2]], 
    size = 0.15, 
    alpha = 0.5
  ) +
  geom_abline(
    intercept = means1[1], 
    slope = means1[2],
    size = 1.25, 
    color = blues[[6]]
  ) +
  ylim(0, 100)

plot(mod1p1)
ggsave("plots/regression1.pdf", width = 8, height = 6)
#######################################################################


# Optional 
#    Estimate out-of-sample predictive performance
#    Can use later to compare to the hierarchical model
looSingle <- loo(SingleLevelModel)


# # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # #
#  MODEL 2
# Two-level HLM: predicting valence from arousal with person-specific variation 
#     repeated measures nested in person (day-level measures, drawn 14 times) 
# # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # #

#------------------------
# Steps 1 & 2 (performed jointly)
#   1. Specify model formula and prior distributions (defaults here)
#   2. Estimate model
#------------------------

# Run model 2 with lmer 
# note: uncomment the line with 'cores' to run chains in parallel (saves time)
TwoLevelModel <- stan_lmer(valence ~ arousal + (1 + arousal | PID), data = dat)

# View summary of priors used
prior_summary(TwoLevelModel)


#------------------------
#Step 3. 
# Check sampling quality
#------------------------


# Check sampling quality and results
summary(TwoLevelModel)
summaryTwoLevelModel<-summary(TwoLevelModel)

# Check convergence statistics - R-hat and ESS
print(summaryTwoLevelModel[, c("mcse", "Rhat", "n_eff")])

# Explore trace plots: for example, the covariance matrix
plot(TwoLevelModel, "trace", regex_pars = "Sigma")

# for a particular participant 
# (the parameters are person-specific deviations from the population parameters)
color_scheme_set("mix-brightblue-red")
plot(TwoLevelModel, "trace", regex_pars = "PID:19", 
     facet_args = list(nrow = 2))

# Or just simply use shinystan to explore the results
launch_shinystan(TwoLevelModel)


# Posterior predictive plot
color_scheme_set("purple")
pp_dist2 <- pp_check(TwoLevelModel, nreps=100) + xlab("valence")
plot(pp_dist2)

# compare to model 1
comparison <- bayesplot_grid(
  pp_dist1, pp_dist2, 
  xlim = c(-30, 145), ylim = c(0, 0.025),
  legends = FALSE,
  subtitles = c("Single-level model", "Two-level model"), 
  grid_args = list(ncol = 2)
)
ggsave(plot = comparison, filename = "plots/ppc_compare.pdf", 
       width = 8, height = 4)


#------------------------
#Step 4. 
# Check fit and interpret results
#------------------------
# View model summaries - now with different levels of hierarchical output
#
#    Large summary table with all parameters, with convergence statistics
summaryTwoLevelModel <- summary(TwoLevelModel)
print(TwoLevelModel)

# write.csv(summary2, file = "rstanarm_2HLM_summary.csv") # uncomment to write to csv

# tidy summary tables for HLM parameters 
#   Population-level estimates
summaryTwoLevelModelPop <- tidy(TwoLevelModel, intervals=TRUE, prob=.95, parameters = "non-varying")
print(summaryTwoLevelModelPop, digits = 2)

#   Variance estimates
summaryTwoLevelModelVar<- tidy(TwoLevelModel, intervals=TRUE, prob=.95, parameters = "hierarchical")
print(summaryTwoLevelModelVar, digits = 2)

#   Person-specific estimates
summaryTwoLevelModelPerson <- tidy(TwoLevelModel, intervals=TRUE, prob=.95, parameters = "varying")
print(summaryTwoLevelModelPerson, digits = 2)
# uncomment to write to csv
# write.csv(mod2summarynv, file = "mod2summarynv.csv")
# write.csv(mod2summaryps, file = "mod2summaryps.csv")
# write.csv(mod2summaryv, file = "mod2summaryvy.csv")

# Use shinystan to explore the results
launch_shinystan(TwoLevelModel)


# Plots for each participant  ----------------------------------------------

PIDs <- sort(unique(dat$PID))
posterior2 <- as.matrix(TwoLevelModel)
samp <- posterior2[sample(nrow(posterior2), 100), ]

plotdata <- lapply(PIDs, FUN = function(j) {
  data.frame(
    alpha = samp[, "(Intercept)"] + samp[, paste0("b[(Intercept) PID:", j, "]")],
    beta = samp[, "arousal"] + samp[, paste0("b[arousal PID:", j, "]")]
  )
})
plotdata <- do.call(rbind, plotdata)
plotdata$PID <- rep(PIDs, each = nrow(samp))
means2 <- aggregate(cbind(alpha, beta) ~ PID, data = plotdata, FUN = "mean")

blues <- color_scheme_get("brightblue")
reg_by_pid_grid <- 
  ggplot(dat, aes(arousal, valence)) +
  geom_abline(
    aes(intercept = alpha, slope = beta), 
    data = plotdata,
    color = blues[[2]],
    size = 0.1
  ) +
  geom_abline(
    aes(intercept = alpha, slope = beta), 
    data = means2,
    color = blues[[6]]
  ) + 
  geom_abline(
    # add line from single-level model
    intercept = means1[1],
    slope = means1[2],
    linetype = 2,
    size = 0.25
  ) +
  geom_point(
    size = 1,
    color = "gray30",
    alpha = 0.8
  ) +
  facet_wrap("PID", scales = "free") + 
  coord_cartesian(xlim = range(dat$arousal), ylim = range(dat$valence))

plot(reg_by_pid_grid)
ggsave("plots/regression_by_pid.pdf", width = 8, height = 8)




# Model comparison
#    Estimate out-of-sample predictive performance
#    and compare to model1. A positive elpd_diff 
#    indicates a preference for model2 in terms of 
#    estimated out-of-sample predictive performance

looSingle <- loo(SingleLevelModel) 
looTwo <- loo(TwoLevelModel) 
compare_models(looSingle, looTwo)



###########################
# Extra material
# Inference via prediction
# 
# Predictions for participant 19 and various arousal values
newdat <- data.frame(arousal = c(30, 60, 90), PID = 19)
preds19 <- posterior_predict(TwoLevelModel, newdata = newdat)
colMeans(preds19)
qplot(preds19[, 1]) + 
  ggtitle("Participant 19\n(arousal = 30)") + 
  labs(x = "Distribution of valence predictions", y = NULL)

# What does model predict for a new participant with arousal level
# at the sample average? 
# (note: compared to the previous plot we have a lot more uncertainty about 
# a new participant than an observed participant, as we should)
newdat <- data.frame(arousal = mean(dat$arousal), PID = "new")
qplot(posterior_predict(TwoLevelModel, newdata = newdat)) + 
  ggtitle("New participant\n(average arousal)") + 
  labs(x = "Distribution of valence predictions", y = NULL)

# Uncomment to save workspace for later reference
# save.image(sprintf("rstanarmHLMs%s.Rdata", Sys.Date()))

