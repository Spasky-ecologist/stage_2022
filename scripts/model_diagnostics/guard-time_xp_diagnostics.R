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


# Load models -----------------------------------------------------------

model <- readRDS("D:/uni_r/github_repos/stage_2022/outputs/R_objects/guard_time_xp_base_model.rds")

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Model diagnostics
# =======================================================================


# Basic MCMC plots ------------------------------------------------------

# Trace plots and parameter posterior distributions
#plot(model)

# Effective sample sizes
neff_vals <- neff_ratio(model, 
                        pars = c("b_Intercept",
                                 "b_IZspeedE2",
                                 "b_IZspace_covered_rateE2",
                                 "b_IZprox_mid_PreyGuardingE2",
                                 "b_IZhook_start_timeE2",
                                 "b_IZprey_avg_speedE2",
                                 "b_IZprey_avg_space_covered_rateE2",
                                 "b_Zspeed",
                                 "b_Zspace_covered_rate",
                                 "b_Zprox_mid_PreyGuarding",
                                 "b_Zhook_start_time",
                                 "b_Zprey_avg_speed",
                                 "b_Zprey_avg_space_covered_rate",
                                 "b_Zgame_duration",
                                 "b_Zspeed:Zspace_covered_rate",
                                 "b_Zspeed:Zprox_mid_PreyGuarding",
                                 "b_Zspeed:Zhook_start_time",
                                 "b_Zspace_covered_rate:Zprox_mid_PreyGuarding",
                                 "b_Zspace_covered_rate:Zhook_start_time",
                                 "b_Zprox_mid_PreyGuarding:Zhook_start_time",
                                 "b_Zspeed:Zprey_avg_speed",
                                 "b_Zspeed:Zprey_avg_space_covered_rate",
                                 "b_Zspace_covered_rate:Zprey_avg_speed",
                                 "b_Zspace_covered_rate:Zprey_avg_space_covered_rate",
                                 "b_Zprox_mid_PreyGuarding:Zprey_avg_speed",
                                 "b_Zprox_mid_PreyGuarding:Zprey_avg_space_covered_rate",
                                 "sd_map_name__Intercept",
                                 "sd_obs__Intercept",
                                 "sd_player_id__Intercept"))

neff_table <- as.data.table(mcmc_neff_data(neff_vals))

saveRDS(neff_table, file = "./outputs/model_diagnostics/03C_neff-model.rds")


# Rhat
rhat_vals <- rhat(model, 
                  pars = c("b_Intercept",
                           "b_IZspeedE2",
                           "b_IZspace_covered_rateE2",
                           "b_IZprox_mid_PreyGuardingE2",
                           "b_IZhook_start_timeE2",
                           "b_IZprey_avg_speedE2",
                           "b_IZprey_avg_space_covered_rateE2",
                           "b_Zspeed",
                           "b_Zspace_covered_rate",
                           "b_Zprox_mid_PreyGuarding",
                           "b_Zhook_start_time",
                           "b_Zprey_avg_speed",
                           "b_Zprey_avg_space_covered_rate",
                           "b_Zgame_duration",
                           "b_Zspeed:Zspace_covered_rate",
                           "b_Zspeed:Zprox_mid_PreyGuarding",
                           "b_Zspeed:Zhook_start_time",
                           "b_Zspace_covered_rate:Zprox_mid_PreyGuarding",
                           "b_Zspace_covered_rate:Zhook_start_time",
                           "b_Zprox_mid_PreyGuarding:Zhook_start_time",
                           "b_Zspeed:Zprey_avg_speed",
                           "b_Zspeed:Zprey_avg_space_covered_rate",
                           "b_Zspace_covered_rate:Zprey_avg_speed",
                           "b_Zspace_covered_rate:Zprey_avg_space_covered_rate",
                           "b_Zprox_mid_PreyGuarding:Zprey_avg_speed",
                           "b_Zprox_mid_PreyGuarding:Zprey_avg_space_covered_rate",
                           "sd_map_name__Intercept",
                           "sd_obs__Intercept",
                           "sd_player_id__Intercept"))

rhat_table <- as.data.table(mcmc_rhat_data(rhat_vals))

saveRDS(rhat_table, file = "./outputs/model_diagnostics/03C_rhat-model.rds")



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
         filename = "./outputs/model_diagnostics/03C_diagnostic1.png",
         width = 1500, height = 1500, res = 300)

ggexport(error_plot,
         filename = "./outputs/model_diagnostics/03C_diagnostic2.png",
         width = 1500, height = 1500, res = 300)

ggexport(param_plot,
         filename = "./outputs/model_diagnostics/03C_diagnostic3.png",
         width = 1500, height = 1500, res = 300)



# Residual vs covariate plots -------------------------------------------

# Predator behaviors
plot1 <- brms::pp_check(model,
                        x = "Zspeed",
                        type = "error_scatter_avg_vs_x",
                        ndraws = 100)

plot2 <- brms::pp_check(model,
                        x = "Zspace_covered_rate",
                        type = "error_scatter_avg_vs_x",
                        ndraws = 100)

plot3 <-brms::pp_check(model,
                       x = "Zprox_mid_PreyGuarding",
                       type = "error_scatter_avg_vs_x",
                       ndraws = 100)

plot4 <- brms::pp_check(model,
                        x = "Zhook_start_time",
                        type = "error_scatter_avg_vs_x",
                        ndraws = 100)

# Prey behaviors
plot5 <- brms::pp_check(model,
                        x = "Zprey_avg_speed",
                        type = "error_scatter_avg_vs_x",
                        ndraws = 100)

plot6 <- brms::pp_check(model,
                        x = "Zprey_avg_space_covered_rate",
                        type = "error_scatter_avg_vs_x",
                        ndraws = 100)

# Arrange figures
fig1 <- ggarrange(plot1,
                  plot2,
                  plot3,
                  plot4,
                  ncol = 2, nrow = 2)

fig2 <- ggarrange(plot5,
                  plot6,
                  ncol = 2, nrow = 1)

# Export the figures
ggexport(fig1,
         filename = "./outputs/model_diagnostics/03C_diagnostic4a.png",
         width = 3000, height = 2500, res = 300)

ggexport(fig2,
         filename = "./outputs/model_diagnostics/03C_diagnostic4b.png",
         width = 3000, height = 1500, res = 300)

# =======================================================================
# =======================================================================