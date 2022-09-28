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
folder = file.path("/home", "ab991036", "projects", "def-monti", 
                    "ab991036", "stage_2022", "data")

# Import the data
data = fread(file.path(folder, "02_final-data.csv"),
              select = c("predator_id", "guard_time_total", "cumul_xp_killer", "match_encode_id"))

data = unique(data)

# ==========================================================================
# ==========================================================================






# ==========================================================================
# 2. Prepare variables for the model
# ==========================================================================


# Transform ----------------------------------------------------------------


# Standardise the variables (Z-scores) -------------------------------------

#Standardisation function
standardize = function (x) {(x - mean(x, na.rm = TRUE)) / 
    sd(x, na.rm = TRUE)}

#Use standardisation formula on predator experience and add a new column
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


#Modele complet
modele_guard_xp = brm(formula = form_guard,
                  warmup = 500,
                  iter =2500,
                  thin = 8,
                  chains = 4, 
                  threads = threading(12),
                  backend = "cmdstanr",
                  seed = 123,
                  control = list(adapt_delta = 0.95),
                  save_pars = save_pars(all = TRUE),
                  data = data)




# Save the model object ----------------------------------------------------

saveRDS(modele_guard_xp, file = "guard_time_xp_base_model.rds")



# Capture the session ------------------------------------------------------

session = sessionInfo()
capture.output(session, file = "session-guard_time-models.txt")

# ==========================================================================
# ==========================================================================
