#Model diagnostic script for guard-time_xp.R
#Author : Patrice Leveille
#Date : September 23 2022


# =======================================================================

#                       Model diagnostics                               #

# =======================================================================





# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

options(mc.cores = parallel::detectCores())

library(data.table)
library(brms)
library(bayesplot)
library(ggpubr)

# Load models -----------------------------------------------------------

model <- readRDS("outputs/R_objects/guard_time_xp_base_model.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Model diagnostics
# =======================================================================


# Basic MCMC plots ------------------------------------------------------

# Trace plots and parameter posterior distributions
plot(model)




# Predictions diagnostics -----------------------------------------------

# Observed y outcomes vs posterior predicted outcomes
dens_plot <- brms::pp_check(model,
                            type = "dens_overlay",
                            ndraws = 100)

# Error scatter for predicted values
error_plot <- brms::pp_check(model,
                             type = 'error_scatter_avg',
                             ndraws = 100)

# Parameter value around posterior distribution
param_plot <- brms::pp_check(model,
                             type = 'stat',
                             stat = 'mean',
                             ndraws = 100)

# Export the plots
ggexport(dens_plot,
         filename = "./outputs/model_diagnostics/GT_xp_diagnostic1.png",
         width = 1500, height = 1500, res = 300)

ggexport(error_plot,
         filename = "./outputs/model_diagnostics/GT_xp_diagnostic2.png",
         width = 1500, height = 1500, res = 300)

ggexport(param_plot,
         filename = "./outputs/model_diagnostics/GT_xp_diagnostic3.png",
         width = 1500, height = 1500, res = 300)




# =======================================================================
# =======================================================================