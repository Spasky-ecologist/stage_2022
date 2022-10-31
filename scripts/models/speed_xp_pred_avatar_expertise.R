#Model script for movement speed as a function of predator experience - Patrice Leveille

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
              select = c("match_encode_id", "pred_game_duration", "predator_id",
                         "predator_avatar_id", "pred_speed", "cumul_xp_killer"))

data <- unique(data)

# ==========================================================================
# ==========================================================================






# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------



#Add in expertise level ----------------------------------------------------

data[cumul_xp_killer <= 100, expertise := "novice"]
data[cumul_xp_killer %between% c(101, 350), expertise := "interm"]
data[cumul_xp_killer >= 351, expertise := "expert"]

#Make expertise a factor
data$expertise = as.factor(data$expertise)




# Standardise the variables (Z-scores) -------------------------------------

#Fonction pour standardiser
standardize = function (x) {(x - mean(x, na.rm = TRUE)) / 
    sd(x, na.rm = TRUE)}

#Use standardisation formula on game duration and add a new column
data[, c("Zpred_game_duration") :=
       lapply(.SD, standardize),
     .SDcols = 2]

# ==========================================================================
# ==========================================================================









# ==========================================================================
# 3. Build the model
# ==========================================================================


# linear model formula -----------------------------------------------------

form_speed_pred_avatar_expertise = brmsformula(pred_speed ~ 1 +
                                       expertise +
                                       Zpred_game_duration +
                                       (1 + expertise | predator_id) +
                                       (1 | predator_avatar_id), 
                                     sigma ~ 1 + expertise + Zpred_game_duration +
                                       (1 + expertise | predator_id)) +
                                gaussian()


# priors ----------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior("normal(0, 2)",
            class = "b"),
  # prior on the intercept
  set_prior("normal(1, 1)",
            class = "Intercept",
            lb = 0),
  # priors on variance parameters
  set_prior("normal(0, 1)",
            class = "sd")
)

# ==========================================================================
# ==========================================================================







# ==========================================================================
# 4. Run the model
# ==========================================================================


# Model specifications -----------------------------------------------------


#Modele complet
modele_speed_xp_pred_avatar_expertise <- brm(formula = form_speed_pred_avatar_expertise,
                       warmup = 700,
                       iter = 7500,
                       thin = 15,
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

saveRDS(modele_speed_xp_pred_avatar_expertise, file = "speed_xp_base_model_pred_avatar_expertise.rds")


#Save plots and outputs ----------------------------------------------------

#Parameter value around posterior distribution
mean_plot <- brms::pp_check(modele_speed_xp_pred_avatar_expertise,
                            type = 'stat',
                            stat = 'mean')

#Observed y outcomes vs posterior predicted outcomes
dens_plot <- brms::pp_check(modele_speed_xp_pred_avatar_expertise,
                            type = "dens_overlay")



#Export the plots ---------------------------------------------------------
ggexport(mean_plot,
         filename = "./outputs/plots/SP_xp_pred_avatar_expertise_mean.png",
         width = 1500, height = 1500, res = 300)


ggexport(dens_plot,
         filename = "./outputs/plots/SP_xp_pred_avatar_expertise_outcomes.png",
         width = 1500, height = 1500, res = 300)

#Export txt file for summary
sink("./outputs/plots/SP_xp_expertise_summary.txt")
print(summary(modele_speed_xp_pred_avatar_expertise))
sink()


# Capture the session ------------------------------------------------------

session = sessionInfo()
capture.output(session, file = "session-speed-models.txt")

# ==========================================================================
# ==========================================================================
