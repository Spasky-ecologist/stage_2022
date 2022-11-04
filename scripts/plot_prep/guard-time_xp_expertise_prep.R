#Plot preperation script for guard-time_xp_pred_avatar_expertise.R
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



# Load data -------------------------------------------------------------

data <- fread("C:/Users/Spasky/OneDrive - UQAM/20220621 - Stage Patrice Leveille/data/02_final-data.csv",
              select = c("match_encode_id", "pred_game_duration", "latency_1st_capture",
                         "predator_id", "predator_avatar_id", "guard_time_total", "cumul_xp_killer"))

data <- unique(data)

#Remove zeros in guarding time for matches with no capture (guarding is theorically NA and not 0)
data <- data[!(guard_time_total == 0 & latency_1st_capture == "NaN")]

#Add in expertise level ----------------------------------------------------

data[cumul_xp_killer <= 100, expertise := "novice"]
data[cumul_xp_killer %between% c(101, 350), expertise := "interm"]
data[cumul_xp_killer >= 351, expertise := "expert"]

#Make expertise a factor
data$expertise = as.factor(data$expertise)


# Load models -----------------------------------------------------------

model <- readRDS("outputs/R_objects/guard_time_xp_base_model_pred_avatar_expertise.rds")


# Prepare model draws --------------------------------------------------------------


# Extract posterior draws
posterior_fit <- as_draws_df(model)

#find columns containing a string
select_intercept <- select(posterior_fit, contains("Intercept"))



# Extract posterior draws for sd of mu and sigma
draws <- data.table(
  as_draws_df(
    model,
    variable = c("b_Intercept", "b_expertiseinterm", "b_expertisenovice", "b_sigma_Intercept",
                 "b_sigma_expertiseinterm", "b_sigma_expertisenovice"),
    regex = TRUE
    )
)



# Transform the deviations to their mean or sd

#mean
draws$b_expertiseinterm <- draws$b_expertiseinterm + draws$b_Intercept
draws$b_expertisenovice <- draws$b_expertisenovice + draws$b_Intercept

#sd
draws$b_sigma_expertiseinterm <- draws$b_sigma_expertiseinterm + draws$b_sigma_Intercept
draws$b_sigma_expertisenovice <- draws$b_sigma_expertisenovice + draws$b_sigma_Intercept


# Long format
table <- melt(draws,
              measure = patterns("^b_"),
              variable.name = "Parameter")


# Add xp level
table[, xp_level := ifelse(Parameter %like% "novice",
                           "novice",
                           "unknown")]
table[Parameter %like% "interm", xp_level := "interm"]
table[Parameter %like% "Intercept", xp_level := "advanced"]


# Add variable
table[, variable := ifelse(Parameter %like% "Intercept",
                           "guard_time",
                           "guard_time")]


# Change parameter factor levels to sigma or mu
table[, Parameter := ifelse(Parameter %like% "sigma", "sigma", "mu")]


# Re-order the columns
table <- table[, c(7,6,4,5,1,2,3)]


#1e graphique guard time moyen
#Extraire les colonnes, additionner les colonnes intercept et novice
#detransform sqrt en 1e et delog en 2e (exp)



# Sdev to variances -----------------------------------------------------

table[Parameter == "mu", value := value^2]
table[Parameter == "sigma", value := exp(value^2)]

#Backtransform the sqrt (?)
table[Parameter == "mu", value := value^2]



# Extract the mean of traits at each xp level ---------------------------

# Predator speed
mean_guard1 <- mean(data[expertise == "novice", guard_time_total])
mean_guard2 <- mean(data[expertise == "interm", guard_time_total])
mean_guard3 <- mean(data[expertise == "expert", guard_time_total])


# Add values in a column
table[xp_level == "novice" & variable == "guard_time", mean := mean_guard1]
table[xp_level == "interm" & variable == "guard_time", mean := mean_guard2]
table[xp_level == "advanced" & variable == "guard_time", mean := mean_guard3]


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
bayesplot_grid(pp_check(model, ndraws = 100))


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