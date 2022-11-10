#Plot preperation script for guard-time_xp_pred_avatar_expertise.R
#Author : Patrice Leveille


# =======================================================================

#                   Table preparation for variance plot                 #

# =======================================================================





# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

options(mc.cores = parallel::detectCores())

library(data.table)
library(brms)
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



# Extract posterior draws for mu and sigma ("b")
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


# Add response variable
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

table[Parameter == "sigma", value := exp(value^2)]

#Backtransform the sqrt of guard time (?)
table[Parameter == "mu", value := value^2]



# Apply transformation to add coefficient of variation ------------------

# Mu pred
table[Parameter == "mu",
      cv_mu := sqrt(value / mean),
      by = c("xp_level", "variable")]


# Sigma
table[Parameter == "sigma", 
      cv_sigma := sqrt(value - 1),
      by = c("xp_level", "variable")]




# Extract the mean of traits at each xp level ---------------------------

# Predator guard time
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
# Prepare a synthetic table
# =======================================================================


# Rearrange -------------------------------------------------------------

tab1 <- table[Parameter == "mu", c("Parameter", "xp_level",
                                   "variable", "cv_mu")]
tab2 <- table[Parameter == "sigma", c("Parameter", "xp_level",
                                      "variable", "cv_sigma")]


setnames(tab1, "cv_mu", "cv")
setnames(tab2, "cv_sigma", "cv")

cv_tab <- rbind(tab1, tab2)


# Summarize the values (means + 95% CI) ---------------------------------

# Intervals
lower_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[1]}
upper_interval <- function (x) {coda::HPDinterval(as.mcmc(x), 0.95)[2]}


cv_tab[, ":=" (mean = mean(cv),
               lower_ci = lower_interval(cv),
               upper_ci = upper_interval(cv)),
       by = .(Parameter, xp_level, variable)]

cv_tab <- unique(cv_tab[, c(1:3, 5:7)])

# Round values to 3 digits
cv_tab[, c(4:6) := lapply(.SD, function (x) {round(x, digits = 3)}),
       .SDcols = c(4:6)]


# Save the table --------------------------------------------------------

path <- "./outputs/tables"
saveRDS(cv_tab, file = file.path(path, "GT_xp_table_cv.rds"))

# =======================================================================
# =======================================================================



