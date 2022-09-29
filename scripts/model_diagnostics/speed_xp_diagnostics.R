#Model diagnostic script for speed_xp.R
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

# Load models -----------------------------------------------------------

model <- readRDS("outputs/R_objects/speed_xp_base_model.rds")

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
bayesplot_grid(pp_check(model, type = 'stat', stat = mean))


#Distribution de notre echantillon vs les sims
bayesplot_grid(pp_check(model, ndraws = 100))


#Resume du modele
summary(model)


#Mettre les residus du modele dans un variable
residus = resid(model)


# Verifier la normalite des residus
hist(residus, xlab = "Résidus", ylab = "Nombre d’observations", 
     main = '', col = 'darkgray', cex.lab = 1.5)



#QQplot de normalite pour les residus du modele
qqnorm(resid(model))
qqline(resid(model))


# Homogénéité des résidus
plot(resid(model) ~ fitted(model), ylab = "Résidus", xlab = "Valeurs prédites")
abline(h = 0, lty = 2, col = "red")




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