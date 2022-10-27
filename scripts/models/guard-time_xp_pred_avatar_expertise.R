#Model script for guard time as a function of predator experience - Patrice Leveille

# Detect number of cores
options(mc.cores = parallel::detectCores())


# ==========================================================================
# 1. Load libraries and import the data
# ==========================================================================


# Packages -----------------------------------------------------------------

library(data.table)
library(brms)
library(parallel)
library(ggpubr)
library(ggplot2)
#library(cmdstanr)



# Import the data ----------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "ab991036", "projects", "def-monti",
                    "ab991036", "stage_2022", "data")

# Import the data
data <- fread(file.path(folder, "02_final-data.csv"),
              select = c("match_encode_id", "pred_game_duration", "latency_1st_capture",
                         "predator_id", "predator_avatar_id", "guard_time_total", "cumul_xp_killer"))

data <- unique(data)

#Remove zeros in guarding time for matches with no capture (guarding is theorically NA and not 0)
data <- data[!(guard_time_total == 0 & latency_1st_capture == "NaN")]


# ==========================================================================
# ==========================================================================






# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------

data[, ":=" (guard_time_total_sqrt = sqrt(guard_time_total))]


#Add in expertise level ----------------------------------------------------

data[cumul_xp_killer <= 100, expertise := "novice"]
data[cumul_xp_killer %between% c(101, 350), expertise := "interm"]
data[cumul_xp_killer >= 351, expertise := "expert"]

#Make expertise a factor
data$expertise = as.factor(data$expertise)


# Standardise the variables (Z-scores) -------------------------------------

#Standardisation function
standardize <- function(x) {(x - mean(x, na.rm = TRUE)) /
    sd(x, na.rm = TRUE)}

#Use standardisation formula on game duration and add a new column
data[, c("Zpred_game_duration", "Zcumul_xp_killer") :=
              lapply(.SD, standardize),
            .SDcols = c(2, 7)]



# ==========================================================================
# ==========================================================================









# ==========================================================================
# 3. Build the model(s)
# ==========================================================================


# linear model formula -----------------------------------------------------


#Formula to have the strength of the relation for each player
form_guard_pred_avatar_expertise = brmsformula(guard_time_total_sqrt ~ 1 +
                                            expertise +
                                            Zpred_game_duration +
                                            (1 + expertise | predator_id) +
                                            (1 | predator_avatar_id), 
                                          sigma ~ 1 + expertise + Zpred_game_duration +
                                            (1 + expertise | predator_id)) +
                                    gaussian()


  


# priors ----------------------------------------------------------------

priors <- c(
  # priors on fixed effects (experience)
  set_prior("normal(0, 2)",
            class = "b"),
  # prior on the intercept (guard time)
  set_prior("normal(1, 1)",
            class = "Intercept",
            lb = 0),
  # priors on variance parameters (predator id et avatar?)
  set_prior("normal(0, 1)",
            class = "sd")
)


# ==========================================================================
# ==========================================================================







# ==========================================================================
# 4. Run the model(s)
# ==========================================================================


# Model specifications -----------------------------------------------------

#Modele complet
mod_pred_avatar_expertise <- brm(formula = form_guard_pred_avatar_expertise,
                  warmup = 700,
                  iter = 6200,
                  thin = 22,
                  chains = 4,
                  threads = threading(12),
                  backend = "cmdstanr",
                  seed = 123,
                  prior = priors,
                  control = list(adapt_delta = 0.95),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = TRUE,
                  data = data)




# Save the model object ----------------------------------------------------

saveRDS(mod_pred_avatar_expertise, file = "guard_time_xp_base_model_pred_avatar_expertise.rds")

#Save plots and outputs ----------------------------------------------------

#Parameter value around posterior distribution
mean_plot <- brms::pp_check(mod_pred_avatar_expertise,
                             type = 'stat',
                             stat = 'mean')

#Observed y outcomes vs posterior predicted outcomes
dens_plot <- brms::pp_check(mod_pred_avatar_expertise,
                            type = "dens_overlay")




# Prepare the plot for population-level relation ---------------------------------------------------------

# With intercept using built-in function
fig1 <- conditional_effects(mod_pred_avatar_expertise, method = "fitted", robust = FALSE)

# Extract values in a table
tab <- fig1$Zcumul_xp_killer

# Transform as data.table
tab <- data.table(tab)

# Back transform x-axis values
sequence <- (seq(0, 500, 100) - mean(data$cumul_xp_killer))
standev <- sd(data$cumul_xp_killer)
scaled_breaks <- sequence / standev

# Back transform y-axis values and confidence intervals
tab[, ":=" (estimate_unsqrt = (estimate__ ^ 2))]
tab[, ":=" (lower_unsqrt = (lower__ ^ 2))]
tab[, ":=" (upper_unsqrt = (upper__ ^ 2))]

#Population-level relation plot
glmm_plot <- ggplot(tab,
                    aes(x = Zcumul_xp_killer,
                        y = estimate_unsqrt)) +
  geom_ribbon(aes(x = Zcumul_xp_killer,
                  ymin = lower_unsqrt,
                  ymax = upper_unsqrt),
              alpha = 0.5,
              fill = "gray") +
  geom_line(#linetype = "dashed",
    size = 1,
    color = "black") +
  ylab("Guarding time\n") +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     limits = c(0, 100)) +
  scale_x_continuous(breaks = scaled_breaks,
                     labels = seq(0, 500, 100)) +
  xlab("\nCumulative experience") +
  custom_theme


#Save the plot image
ggexport(glmm_plot,
         filename = "./outputs/plots/GT_xp_glmm_pred_avatar_expertise.png",
         width = 1500, height = 1500, res = 300)


#Export the plots
ggexport(mean_plot,
         filename = "./outputs/plots/GT_xp_pred_avatar_expertise_mean.png",
         width = 1500, height = 1500, res = 300)


ggexport(dens_plot,
         filename = "./outputs/plots/GT_xp_pred_avatar_expertise_outcomes.png",
         width = 1500, height = 1500, res = 300)

#Export txt file for summary
sink("./outputs/plots/GT_xp_expertise_summary.txt")
print(summary(mod_pred_avatar_expertise))



# Capture the session ------------------------------------------------------

session <- sessionInfo()
capture.output(session, file = "session-guard_time-models.txt")

# ==========================================================================
# ==========================================================================
