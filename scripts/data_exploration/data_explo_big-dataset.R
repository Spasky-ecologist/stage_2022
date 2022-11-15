#Data exploration script for the 2nd dataset (the much bigger one) 
#Internship Summer/Fall 2022
# Patrice Leveille

#Load necessary packages
library(data.table)
library(dplyr)
library(ggplot2)
library(brms)
library(bayesplot)


#Read the data within the excel doc
donnees2 <- read.csv("C:/Users/Spasky/OneDrive - UQAM/20220621 - Stage Patrice Leveille/data/FraserFrancoetalXXXX-data.csv")

#Select columns wanted for a smaller table
donnees2_small <- select(donnees2, match_encode_id, environment_id, predator_id, pred_game_duration,
                         predator_avatar_id, total_chase_duration, pred_speed, pred_amount_tiles_visited,
                         points_predator, total_chase_duration, prey_avg_speed, prey_var_speed,
                         guard_time_close, guard_time_total, latency_1st_capture, hook_count,
                         prey_total_unhook_count, killed_count, sacrificed_count, hunting_success,
                         cumul_xp_killer, total_xp_killer)


#Table with unique lines (not every line for a single match for each prey)
donnees2_unique <- unique(donnees2_small)


#Transform table into data table to be able to find and remove the 0s
is.data.table(donnees2_unique)
setDT(donnees2_unique)



# Guarding time section -------------------------------------


#Count the number of matches that have a guarding time of 0 but where prey were sacrificed (false 0s)
count(donnees2_unique[(sacrificed_count == 0 & guard_time_total == 0)])

  #Remove the 7276 matches with no sacrificed preys since the guarding time is not 0, it's not existant
  donnees2_unique <- donnees2_unique[!(guard_time_total == 0 & sacrificed_count == 0)]
  
  

#Count the number of matches that have a guarding time of 0 but prey were killed
count(donnees2_unique[(hunting_success > 0 & guard_time_total == 0)])



#Count the number of matches that have a guarding time of 0
#but no captures were made (no opportunity to guard) - doesn't detect them
count(donnees2_unique[(guard_time_total == 0 & latency_1st_capture == "NA")])

  #Remove the 11 721 matches with NAs in latency before 1st capture cause no opportunity to guard
  donnees2_unique <- donnees2_unique[!is.na(donnees2_unique$latency_1st_capture),]

  
  
#Data points of guard time as function of the xp of killers 
qplot(x = cumul_xp_killer, y = guard_time_total, data = donnees2_unique)


#Distribution of the response variable (guarding time) with and without a square root transformation
hist(donnees2_unique$guard_time_total)
hist(sqrt(donnees2_unique$guard_time_total))



# Speed section --------------------------------------------


#Distribution of predator speed data
hist(donnees2_unique$pred_speed)
hist(donnees2_unique$pred_speed, xlim = c(0, 0.1), ylim = c(0, 500), breaks = 5550)


#Standardize function
standardize = function (x) {(x - mean(x, na.rm = TRUE)) / 
    sd(x, na.rm = TRUE)}

#Utiliser la fonction de standardisation sur les variables des colonnes specifiees et creer des nouvelles colonnes
donnees2_unique[, c("Zpred_speed") :=
              lapply(.SD, standardize), 
            .SDcols = 7]


hist(donnees2_unique$Zpred_speed)


min(donnees2$pred_speed)



hist(donnees2_unique$pred_amount_tiles_visited, xlim = c(0, 10), ylim = c(0, 1000), breaks = 100)

#Count the number of matches that have a speed of 0
count(donnees2_unique[(pred_amount_tiles_visited <= 2 & pred_speed < 0.21)])



#Remove the 739 matches with a speed less than 0.21 with 2 or less tiles visited (there's a spike in the data)
donnees2_unique <- (donnees2_unique[!(pred_amount_tiles_visited <= 2 & pred_speed < 0.21)])



#Distribution of speed after the removal of matches
hist(donnees2_unique$pred_speed)

#Distribution of tiles visited by predator after the removal of matches
hist(donnees2_unique$pred_amount_tiles_visited)




# Chase time section --------------------------------------

#Distribution of chase time in the data
hist(donnees2_unique$total_chase_duration, breaks = 100)

hist(donnees2_unique$total_chase_duration, xlim = c(0, 10), ylim = c(0, 250), breaks = 5000)

#Minimum value of chase time
min(donnees2_unique$total_chase_duration)


#How many 0s in chase time
count(donnees2_unique[(total_chase_duration == 0)])

count(donnees2_unique[(total_chase_duration > 0 & total_chase_duration <= 2)])



#Remove the 739 matches with a speed less than 0.21 with 2 or less tiles visited (there's a spike in the data)
  #The predator is most likely afk
donnees2_unique <- (donnees2_unique[!(pred_amount_tiles_visited <= 2 & pred_speed < 0.21)])

