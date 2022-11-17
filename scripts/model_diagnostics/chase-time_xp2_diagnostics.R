#Model diagnostic script for chase-time_xp.R
#Author : Patrice Leveille


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
library(ggplot2)



# Load models -----------------------------------------------------------

model <- readRDS("outputs/R_objects/chase-time_xp_base_model2.rds")


# Prepare model draws --------------------------------------------------------------


# Extract posterior draws
posterior_fit <- as_draws_df(model)


# =======================================================================
# =======================================================================





# =======================================================================
# 2. Model diagnostics
# =======================================================================


# Basic MCMC plots ------------------------------------------------------

# Trace plots and parameter posterior distributions
plot(model)


#Model relation plot
plot(conditional_effects(model))


#La moyenne de l'echantillon (noir) vs les moyennes des sims
bayesplot_grid(pp_check(model, type = 'stat', stat = "mean"))


#Distribution de notre echantillon vs les sims
bayesplot_grid(pp_check(model))


#Resume du modele
summary(model)



# Plot prior and posterior draws ---------------------------------------------------

prior_summary(model)

# Intercept
ggplot(posterior_fit) +
  geom_density(aes(prior_Intercept),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(Intercept),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) + 
  theme_classic()


# Cumul xp
ggplot(posterior_fit) +
  geom_density(aes(prior_b),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  geom_density(aes(b_expertise),
               fill = "#FC4E07",
               color = "black",
               alpha = 0.6) + 
  theme_classic()


# Standard deviation of predator ID
ggplot(posterior_fit) +
  geom_density(aes(prior_sd_predator_id),
               fill = "steelblue",
               color = "black",
               alpha = 0.6) +
  #geom_density(aes(sds_spredator_id_1),
   #            fill = "#FC4E07",
    #           color = "black",
     #          alpha = 0.6) + 
  theme_classic()








# Setup a custom theme for the plot ----------------------------------------

custom_theme <- theme(# axis values size
  axis.text.x = element_text(face = "plain", 
                             size = 15,
                             color = "black"),
  axis.text.y = element_text(face = "plain", 
                             size = 15,
                             color = "black"),
  # axis ticks lenght
  axis.ticks.length = unit(.15, "cm"),
  # axis ticks width
  axis.ticks = element_line(size = 0.90, 
                            color = "black"),
  # axis titles size
  axis.title = element_text(size = 17, 
                            face = "plain",
                            color = "black"),
  axis.line = element_line(size = 0.95,
                           color = "black"),
  legend.position = "none",
  panel.grid = element_blank(),
  panel.background = element_blank())

# ==========================================================================
# ==========================================================================





# ==========================================================================
# 2. Plot 1 : GAMM fitted line
# ==========================================================================



# Prepare the plot ---------------------------------------------------------

# With intercept using built-in function
fig1 <- conditional_effects(model, method = "fitted", robust = FALSE)

# Extract values in a table
tab <- fig1$expertise

# Transform as data.table
tab <- data.table(tab)

# Back transform x-axis values
sequence <- (seq(0, 500, 100) - mean(donnees_unique$cumul_xp_killer))
standev <- sd(donnees_unique$cumul_xp_killer)
scaled_breaks <- sequence / standev

# Back transform y-axis values and confidence intervals
tab[, ":=" (estimate_unsqrt = (estimate__ ^ 2))]
tab[, ":=" (lower_unsqrt = (lower__ ^ 2))]
tab[, ":=" (upper_unsqrt = (upper__ ^ 2))]

#Back transform the standard error from sqrt to normal values
tab[, ":=" (se_unsqrt = (se__ ^ 2))]


# Produce the plot --------------------------------------------------------

glmm_plot <- ggplot(tab,
                    aes(x = expertise,
                        y = estimate__)) +
  geom_boxplot() +
  geom_errorbar(data = tab, aes(ymin = estimate__ - se__, ymax = estimate__ + se__)) +
  ylab("Guarding time\n") +
  scale_y_continuous(breaks = seq(8, 10, 0.1),
                     limits = c(8, 10)) +
  scale_x_discrete(limits = c("novice", "interm", "expert")) +
  xlab("\nExperience level") +
  custom_theme


#Save the plot image
ggexport(glmm_plot,
         filename = "./outputs/figures/GT_xp_glmm_pred_avatar_expertise_sqrt.png",
         width = 1500, height = 1500, res = 300)

# ==========================================================================
# ==========================================================================




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
                             stat = 'mean')

# Export the plots
ggexport(dens_plot,
         filename = "./outputs/model_diagnostics/GT_xp_pred_avatar_outcomes.png",
         width = 1500, height = 1500, res = 300)

ggexport(error_plot,
         filename = "./outputs/model_diagnostics/GT_xp_diagnostic2.png",
         width = 1500, height = 1500, res = 300)

ggexport(param_plot,
         filename = "./outputs/model_diagnostics/GT_xp_pred_avatar_mean.png",
         width = 1500, height = 1500, res = 300)




# =======================================================================
# =======================================================================