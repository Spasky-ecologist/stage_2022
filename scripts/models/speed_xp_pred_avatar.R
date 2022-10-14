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
#library(cmdstanr)



# Import the data ----------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "ab991036", "projects", "def-monti", 
                    "ab991036", "stage_2022", "data")

# Import the data
data <- fread(file.path(folder, "02_final-data.csv"),
              select = c("match_encode_id", "predator_id", "predator_avatar_id", "pred_speed", "cumul_xp_killer"))

data <- unique(data)

# ==========================================================================
# ==========================================================================






# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------


# Standardise the variables (Z-scores) -------------------------------------

#Fonction pour standardiser
standardize = function (x) {(x - mean(x, na.rm = TRUE)) / 
    sd(x, na.rm = TRUE)}

#Utiliser la fonction de standardisation sur les variables des colonnes specifiees et creer des nouvelles colonnes
data[, c("Zcumul_xp_killer") :=
              lapply(.SD, standardize), 
            .SDcols = 5]

# ==========================================================================
# ==========================================================================









# ==========================================================================
# 3. Build the model
# ==========================================================================


# linear model formula -----------------------------------------------------

form_speed_pred_avatar = brmsformula(pred_speed ~ 1 + Zcumul_xp_killer + predator_avatar_id +
                           (1 + Zcumul_xp_killer | predator_id), 
                         sigma ~ 1 + Zcumul_xp_killer + predator_avatar_id) +
  gaussian()


# priors ----------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior("normal(0, 2)",
            class = "b"),
  # prior on the intercept
  set_prior("normal(0, 2)",
            class = "Intercept"),
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
modele_speed_xp_pred_avatar <- brm(formula = form_speed_pred_avatar,
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

saveRDS(modele_speed_xp_pred_avatar, file = "speed_xp_base_model_pred_avatar.rds")



# Capture the session ------------------------------------------------------

session = sessionInfo()
capture.output(session, file = "session-speed-models.txt")

# ==========================================================================
# ==========================================================================
