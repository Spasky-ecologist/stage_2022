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
#library(cmdstanr)



# Import the data ----------------------------------------------------------

# Folder path Compute Canada
folder <- file.path("/home", "ab991036", "projects", "def-monti",
                    "ab991036", "stage_2022", "data")

# Import the data
data <- fread(file.path(folder, "FraserFrancoetalXXXX-data.csv"),
              select = c("match_encode_id", "pred_game_duration", "latency_1st_capture",
                         "predator_id", "predator_avatar_id", "guard_time_total", "cumul_xp_killer"))

data <- unique(data)

#Remove the 2 matches with no sacrificed preys since the guarding time is not 0, it's not existant
donnees2_unique <- donnees2_unique[!(guard_time_total == 0 & sacrificed_count == 0)]

#Remove matches with NAs in latency before 1st capture cause no opportunity to guard
donnees2_unique <- donnees2_unique[!is.na(donnees2_unique$latency_1st_capture),]


# ==========================================================================
# ==========================================================================






# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------

#data[, ":=" (guard_time_total_sqrt = sqrt(guard_time_total))]

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
form_guard_pred_avatar_2 = brmsformula(guard_time_total ~ 1 +
                                       Zcumul_xp_killer +
                                       Zpred_game_duration +
                                       (1 + Zcumul_xp_killer | predator_id) +
                                       (1 | predator_avatar_id), 
                                     sigma ~ 1 + Zcumul_xp_killer + Zpred_game_duration +
                                       (1 + Zcumul_xp_killer | predator_id)) +
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
mod_pred_avatar_2 <- brm(formula = form_guard_pred_avatar_2,
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

saveRDS(mod_pred_avatar, file = "guard_time_xp_base_model_pred_avatar_2.rds")



# Capture the session ------------------------------------------------------

session <- sessionInfo()
capture.output(session, file = "session-guard_time-models.txt")

# ==========================================================================
# ==========================================================================
