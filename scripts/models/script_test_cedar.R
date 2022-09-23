#Script test pour 1e envoi Cedar - Patrice Leveille

# Detect number of cores
options(mc.cores = parallel::detectCores())


# ==========================================================================
# 1. Load libraries and import the data 
# ==========================================================================


# Packages -----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(brms)
library(bayesplot)
library(cmdstanr)


# Import the data ----------------------------------------------------------

# Folder path Compute Canada
folder = file.path("/home", "ab991036", "projects", "def-monti", 
                    "ab991036", "stage_2022", "data")

# Import the data
data = fread(file.path(folder, "02_final-data.csv"),
              select = c("predator_id", "guard_time_total", "cumul_xp_killer"))



# ==========================================================================
# ==========================================================================






# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------


#Ajouter colonne pour le sqrt de cumul xp killer
data[, ":=" (cumul_xp_killer_sqrt = sqrt(cumul_xp_killer))]



# Standardise the variables (Z-scores) -------------------------------------

#Fonction pour standardiser
standardize = function (x) {(x - mean(x, na.rm = TRUE)) / 
    sd(x, na.rm = TRUE)}

#Utiliser la fonction de standardisation sur les variables des colonnes specifiees et creer des nouvelles colonnes
data[, c("Zcumul_xp_killer") :=
              lapply(.SD, standardize), 
            .SDcols = 3]

# ==========================================================================
# ==========================================================================









# ==========================================================================
# 3. Build the model
# ==========================================================================


# linear model formula -----------------------------------------------------

form_guard = brmsformula(guard_time_total ~ 1 + Zcumul_xp_killer + (1 | predator_id), 
                         sigma ~ 1 + Zcumul_xp_killer) +
  gaussian()


# ==========================================================================
# ==========================================================================







# ==========================================================================
# 4. Run the model
# ==========================================================================


# Model specifications -----------------------------------------------------

#Modele test
modele_guard_xp = brm(formula = form_guard,
                      warmup = 500,
                      iter = 1500,
                      warmup = 500,
                      thin = 4,
<<<<<<<< HEAD:guard-time_xp.R
                      chains = 4,
========
                      chains = 4, 
>>>>>>>> 015f13f92870c7bc88a8d4855496fbbd72cca270:scripts/script_test_cedar.R
                      backend = "cmdstanr",
                      seed = 123,
                      control = list(adapt_delta = 0.95),
                      save_pars = save_pars(all = TRUE),
                      sample_prior = FALSE,
                      data = data)

#Modele complet
modele_guard_xp = brm(formula = form_guard,
                  warmup = 500,
                  prior = NULL,
                  iter = 1500,
                  warmup = 500,
                  thin = 4,
                  chains = 4, 
                  threads = threading(10),
                  backend = "cmdstanr",
                  seed = 123,
                  control = list(adapt_delta = 0.95),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = FALSE,
                  data = data)




# Save the model object ----------------------------------------------------

saveRDS(modele_guard_xp, file = "guard_time_xp_base_model.rds")



# Capture the session ------------------------------------------------------

session = sessionInfo()
capture.output(session, file = "session-guard_time-models.txt")

# ==========================================================================
# ==========================================================================
