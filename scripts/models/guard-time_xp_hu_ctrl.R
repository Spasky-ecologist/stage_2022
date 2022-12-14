#Model script for guard time as a function of predator experience 
#Hurdle model with game duration control
#Patrice Leveille

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
              select = c("match_encode_id", "pred_game_duration", "hunting_success",
                         "predator_id", "guard_time_total", "cumul_xp_killer"))

data <- unique(data)

#Remove false zeros in guarding time for a hunting success > 0
data <- data[!(hunting_success > 0 & guard_time_total == 0)]


# ==========================================================================
# ==========================================================================






# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------


# Standardise the variables (Z-scores) -------------------------------------

#Standardisation function
standardize <- function(x) {(x - mean(x, na.rm = TRUE)) /
    sd(x, na.rm = TRUE)}

#Use standardisation formula on game duration and add a new column
data[, c("Zpred_game_duration") :=
              lapply(.SD, standardize),
            .SDcols = 2]


#Use standardisation formula on predator experience and add a new column
data[, c("Zcumul_xp_killer") :=
       lapply(.SD, standardize),
     .SDcols = 6]

# ==========================================================================
# ==========================================================================









# ==========================================================================
# 3. Build the model(s)
# ==========================================================================


# linear model formula -----------------------------------------------------


#Formula to have the strength of the relation for each player
form_guard_hu_ctrl = brmsformula(guard_time_total ~ 1 +
                                Zcumul_xp_killer +
                                Zpred_game_duration +
                                (1 + Zcumul_xp_killer | predator_id), 
                                hu ~ 1 + Zcumul_xp_killer + Zpred_game_duration + (1 | predator_id)) +
                      hurdle_lognormal()

  


# priors ----------------------------------------------------------------

priors <- c(
  # priors on fixed effects (experience)
  set_prior("normal(0, 2)",
            class = "b"),
  # prior on the intercept (guard time)
  set_prior("normal(0, 2)",
            class = "Intercept"),
  # priors on variance parameters (predator id ?)
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
modele_guard_xp_hu_ctrl <- brm(formula = form_guard_hu_ctrl,
                  warmup = 700,
                  iter = 5000,
                  thin = 12,
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

saveRDS(modele_guard_xp_hu_ctrl, file = "guard_time_xp_base_model_hu_ctrl.rds")



# Capture the session ------------------------------------------------------

session <- sessionInfo()
capture.output(session, file = "session-guard_time-models.txt")

# ==========================================================================
# ==========================================================================
