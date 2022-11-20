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
              select = c("match_encode_id", "pred_game_duration", "predator_id", "pred_speed",
                         "predator_avatar_id", "total_chase_duration", "cumul_xp_killer",
                         "pred_amount_tiles_visited"))

data <- unique(data)


#Remove the 739 matches with a speed less than 0.21 with 2 or less tiles visited (there's a spike in the data)
data <- (data[!(pred_amount_tiles_visited <= 2 & pred_speed < 0.21)])

# ==========================================================================
# ==========================================================================






# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------

data[, ":=" (total_chase_duration_sqrt = sqrt(total_chase_duration))]

#Add in expertise level ----------------------------------------------------

data[cumul_xp_killer <= 100, expertise := "novice"]
data[cumul_xp_killer %between% c(101, 350), expertise := "interm"]
data[cumul_xp_killer >= 351, expertise := "expert"]

#Make expertise a factor
data$expertise = as.factor(data$expertise)


# Standardise the variables (Z-scores) -------------------------------------

#Standardisation function
standardize <- function (x) {(x - mean(x, na.rm = TRUE)) / 
    sd(x, na.rm = TRUE)}

#Use standardisation formula on predator experience and add a new column
data[, c("Zpred_game_duration") :=
              lapply(.SD, standardize), 
            .SDcols = 2]

# ==========================================================================
# ==========================================================================









# ==========================================================================
# 3. Build the model(s)
# ==========================================================================


# linear model formula -----------------------------------------------------

form_chase_pred_avatar_expertise <- brmsformula(total_chase_duration_sqrt ~ 1 +
                                        expertise +
                                        Zpred_game_duration +
                                        (1 + expertise | predator_id), 
                                      sigma ~ 1 + expertise + Zpred_game_duration +
                                        (1 + expertise | predator_id)) +
                                  gaussian()



# priors ----------------------------------------------------------------

priors <- c(
  # priors on fixed effects
  set_prior("normal(0, 2)",
            class = "b",
            lb = 0),
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
# 4. Run the model(s)
# ==========================================================================


# Model specifications -----------------------------------------------------


#Modele complet
modele_chase_xp_pred_avatar_expertise <- brm(formula = form_chase_pred_avatar_expertise,
                  warmup = 1000,
                  iter = 21000,
                  thin = 80,
                  chains = 4, 
                  threads = threading(12),
                  backend = "cmdstanr",
                  seed = 123,
                  prior = priors,
                  control = list(adapt_delta = 0.99),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = TRUE,
                  init = 0,
                  data = data)




# Save the model object ----------------------------------------------------

saveRDS(modele_chase_xp_pred_avatar_expertise, file = "chase_time_xp_base_model_pred_avatar_expertise.rds")


# Capture the session ------------------------------------------------------

session <- sessionInfo()
capture.output(session, file = "session-chase_time-models.txt")

# ==========================================================================
# ==========================================================================
