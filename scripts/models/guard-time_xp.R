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
data <- fread(file.path(folder, "FraserFrancoetalXXXX-data.csv"),
              select = c("match_encode_id", "pred_game_duration", "latency_1st_capture",
                         "predator_id", "predator_avatar_id", "guard_time_total", "cumul_xp_killer",
                         "sacrificed_count"))

data <- unique(data)

#Remove the 2 matches with no sacrificed preys since the guarding time is not 0, it's not existant
data <- data[!(guard_time_total == 0 & sacrificed_count == 0)]

#Remove the 11 721 matches with NAs in latency before 1st capture cause no opportunity to guard
data <- data[!is.na(data$latency_1st_capture),]


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
data[, c("Zpred_game_duration") :=
              lapply(.SD, standardize),
            .SDcols = 2]



# ==========================================================================
# ==========================================================================









# ==========================================================================
# 3. Build the model(s)
# ==========================================================================


# linear model formula -----------------------------------------------------


#Formula to have the strength of the relation for each player
form_guard_pred_avatar_expertise2 = brmsformula(guard_time_total_sqrt ~ 1 +
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
mod_pred_avatar_expertise2 <- brm(formula = form_guard_pred_avatar_expertise2,
                  warmup = 1000,
                  iter = 10500,
                  thin = 38,
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

saveRDS(mod_pred_avatar_expertise2, file = "guard_time_xp_base_model_pred_avatar_expertise2.rds")

#Save plots and outputs ----------------------------------------------------

#Parameter value around posterior distribution
mean_plot <- brms::pp_check(mod_pred_avatar_expertise2,
                             type = 'stat',
                             stat = 'mean')

#Observed y outcomes vs posterior predicted outcomes
dens_plot <- brms::pp_check(mod_pred_avatar_expertise2,
                            type = "dens_overlay")




#Export the plots
ggexport(mean_plot,
         filename = "./outputs/plots/GT_xp_pred_avatar_expertise2_mean.png",
         width = 1500, height = 1500, res = 300)


ggexport(dens_plot,
         filename = "./outputs/plots/GT_xp_pred_avatar_expertise2_outcomes.png",
         width = 1500, height = 1500, res = 300)

#Export txt file for summary
sink("./outputs/plots/GT_xp_expertise2_summary.txt")
print(summary(mod_pred_avatar_expertise2))
sink()



# Capture the session ------------------------------------------------------

session <- sessionInfo()
capture.output(session, file = "session-guard_time-models.txt")

# ==========================================================================
# ==========================================================================
