#Script test pour explorer les manip de donnees - Stage Ete/Automne 2022
# Patrice Leveille


#Load les packages necessaires
library(data.table)
library(dplyr)
library(ggplot2)
library(brms)
library(bayesplot)


#Rappel des fonctions dans le package brms
objects(grep("brms",search()))

#Lire le fichier excel de donnees
donnees = read.csv("C:/Users/Spasky/OneDrive - UQAM/20220621 - Stage Patrice Leveille/data/02_final-data.csv")

#Selectionner les variables d'interets dans la table "donnees"
donnees_select = select(donnees, match_encode_id, game_mode, environment_id, prey_avatar_id, predator_avatar_id, predator_id, 
                        prey_id, rank, pred_game_duration, prey_game_duration, total_chase_duration, avg_chase_duration, 
                        pred_speed, prey_avg_speed, prey_var_speed, guard_time_close, guard_time_total, latency_1st_capture, 
                        prey_total_unhook_count, hunting_success, cumul_xp_killer)

#Selectionner les colonnes d'interets mais moins pour faire un tableau des predateur unique
donnees_small = select(donnees, environment_id, predator_id, pred_game_duration, total_chase_duration, avg_chase_duration, 
                       pred_speed, prey_avg_speed, prey_var_speed, guard_time_close, guard_time_total, latency_1st_capture, 
                       prey_total_unhook_count, hunting_success, cumul_xp_killer)


#Tableau unique des predateurs
donnees_unique = unique(donnees_small)

#Transformer en datatable pour mettre les colonnes
is.data.table(donnees_unique)
setDT(donnees_unique) 

#Ajouter une colonne au tableau unique pour le niveau d'expertise
donnees_unique[cumul_xp_killer <= 100, expertise := "novice"]
donnees_unique[cumul_xp_killer %between% c(101, 350), expertise := "interm"]
donnees_unique[cumul_xp_killer >= 351, expertise := "expert"]

#Rendre l'expertise un facteur
donnees_unique$expertise = as.factor(donnees_unique$expertise)
  #Verifier les niveaux
  levels(donnees_unique$expertise)

  
#Regarder la structure de la table (type de data pour les colonnes)
str(donnees_unique)


#Changer le game_mode en facteur
donnees_select$game_mode = as.factor(donnees_select$game_mode)

  #Regarder les niveaux du facteur
  levels(donnees_select$game_mode)

  #Regarder la distribution pour les game_mode
  barplot(prop.table(table(donnees_select$game_mode)))

  
#Distribution des points entre l'xp du predateur et le temps a guarder (total)  
qplot(x = cumul_xp_killer, y = guard_time_total, data = donnees_select)


#----- Test frequentistes --------------

#Modele lineaire entre les deux (pas bayesien)
modele1 = lm(guard_time_total ~ cumul_xp_killer, data = donnees_select)

layout(matrix(1:4), nrow = 2)
plot(modele1)
summary(modele1)

#------- Creer fonction pour verifier l'occurrence de valeurs unique (=4)

#Creer une fonction pour verifier si chaque valeur unique egale 4 (nb lignes par matches dans notre cas)
equal_occurrence <- function(x){
  occurrence = rle(sort(x))
  result = all(occurrence$lengths == 4)
  
  if(result == TRUE){
    print("All matches appear exactly 4 times") 
  }
  else {
    print("Not all matches appear exactly 4 times") 
  }
}

#Utiliser la fonction pour verifier que le nb de lignes par match = 4
equal_occurrence(donnees$match_encode_id)



#---------Zone de sous-echantillonnage parce que sinon mon ordi va me tuer-------------------------------------

#Histogramme de la distribution de l'xp des killers
hist(donnees_select$cumul_xp_killer, breaks = 500)

#Histogramme de combien y'a de killers a 460 et + xp
hist(donnees_select$cumul_xp_killer, xlim = c(460,500), ylim = c(0, 175), breaks = 500)


#CREER UN TABLEAU POUR CHAQUE TRANCHE DE 30 XP

#Combien d'observations (lignes) y'a pour l'xp des pred entre 480 et 500
sum(donnees_select$cumul_xp_killer >= 480 & donnees_select$cumul_xp_killer <= 500)

  #Creer un tableau avec seulement les lignes qui ont un cumul_xp de + de 480 (total 36)
  xp_480_500_subset = subset(donnees_select, cumul_xp_killer >= 480)

  

#Creer un tableau avec 50 lignes / 260 lignes des xp entre 450 et 480 (les 50 1e lignes)
xp_450_480 = subset(donnees_select, donnees_select$cumul_xp_killer >= 450 & donnees_select$cumul_xp_killer < 480)
xp_450_480_subset = xp_450_480[1:50, ]




#Combien d'observations (lignes) y'a pour l'xp des pred entre 420 et 450
sum(donnees_select$cumul_xp_killer >= 420 & donnees_select$cumul_xp_killer < 450)

  #Creer un tableau avec 50 lignes des xp entre 420 et 450 (les 50 1e lignes)
  xp_420_450 = subset(donnees_select, donnees_select$cumul_xp_killer >= 420 & donnees_select$cumul_xp_killer < 450)
  xp_420_450_subset = xp_420_450[1:50, ]
  
  
  
#Creer un tableau avec 50 lignes des xp entre 390 et 420 (les 50 1e lignes)
xp_390_420 = subset(donnees_select, donnees_select$cumul_xp_killer >= 390 & donnees_select$cumul_xp_killer < 420)
xp_390_420_subset = xp_390_420[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 360 et 390 (les 50 1e lignes)
xp_360_390 = subset(donnees_select, donnees_select$cumul_xp_killer >= 360 & donnees_select$cumul_xp_killer < 390)
xp_360_390_subset = xp_360_390[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 330 et 360 (les 50 1e lignes)
xp_330_360 = subset(donnees_select, donnees_select$cumul_xp_killer >= 330 & donnees_select$cumul_xp_killer < 360)
xp_330_360_subset = xp_330_360[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 300 et 330 (les 50 1e lignes)
xp_300_330 = subset(donnees_select, donnees_select$cumul_xp_killer >= 300 & donnees_select$cumul_xp_killer < 330)
xp_300_330_subset = xp_300_330[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 270 et 300 (les 50 1e lignes)
xp_270_300 = subset(donnees_select, donnees_select$cumul_xp_killer >= 270 & donnees_select$cumul_xp_killer < 300)
xp_270_300_subset = xp_270_300[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 240 et 270 (les 50 1e lignes)
xp_240_270 = subset(donnees_select, donnees_select$cumul_xp_killer >= 240 & donnees_select$cumul_xp_killer < 270)
xp_240_270_subset = xp_240_270[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 210 et 240 (les 50 1e lignes)
xp_210_240 = subset(donnees_select, donnees_select$cumul_xp_killer >= 210 & donnees_select$cumul_xp_killer < 240)
xp_210_240_subset = xp_210_240[1:50, ]
  


#Creer un tableau avec 50 lignes des xp entre 180 et 210 (les 50 1e lignes)
xp_180_210 = subset(donnees_select, donnees_select$cumul_xp_killer >= 180 & donnees_select$cumul_xp_killer < 210)
xp_180_210_subset = xp_180_210[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 150 et 180 (les 50 1e lignes)
xp_150_180 = subset(donnees_select, donnees_select$cumul_xp_killer >= 150 & donnees_select$cumul_xp_killer < 180)
xp_150_180_subset = xp_150_180[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 120 et 150 (les 50 1e lignes)
xp_120_150 = subset(donnees_select, donnees_select$cumul_xp_killer >= 120 & donnees_select$cumul_xp_killer < 150)
xp_120_150_subset = xp_120_150[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 90 et 120 (les 50 1e lignes)
xp_90_120 = subset(donnees_select, donnees_select$cumul_xp_killer >= 90 & donnees_select$cumul_xp_killer < 120)
xp_90_120_subset = xp_90_120[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 60 et 90 (les 50 1e lignes)
xp_60_90 = subset(donnees_select, donnees_select$cumul_xp_killer >= 60 & donnees_select$cumul_xp_killer < 90)
xp_60_90_subset = xp_60_90[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 30 et 60 (les 50 1e lignes)
xp_30_60 = subset(donnees_select, donnees_select$cumul_xp_killer >= 30 & donnees_select$cumul_xp_killer < 60)
xp_30_60_subset = xp_30_60[1:50, ]



#Creer un tableau avec 50 lignes des xp entre 0 et 30 (les 50 1e lignes)
xp_0_30 = subset(donnees_select, donnees_select$cumul_xp_killer >= 0 & donnees_select$cumul_xp_killer < 30)
xp_0_30_subset = xp_0_30[1:50, ]


#COMBINER TOUS CES BEAUX TABLEAUX

  #Combiner les tableaux de chaque subset
    subset_final = rbind(xp_480_500_subset, xp_450_480_subset, xp_420_450_subset, xp_390_420_subset, xp_360_390_subset, 
                        xp_330_360_subset, xp_300_330_subset, xp_270_300_subset, xp_240_270_subset, xp_210_240_subset, 
                        xp_180_210_subset, xp_150_180_subset, xp_120_150_subset, xp_90_120_subset, xp_60_90_subset, 
                        xp_30_60_subset, xp_0_30_subset)
  
    
  #Distribution de l'xp dans le subset
    hist(subset_final$cumul_xp_killer, breaks = 10)
    
    
    
    
    
#------------ SOUS-ECHANTILLONNAGE 2 EN SACHANT COMMENT LA BASE DE DONNEES EST FAITE ------------
    
    

#Creer un tableau avec seulement les lignes qui ont un cumul_xp de + de 480 (total 36)
xp_480_500_subset = subset(donnees_select, cumul_xp_killer >= 480)
    
    #Ordonner la table en ordre de match ID (ascendant et grouper ensemble les 4 lignes par match)
    xp_480_500_subset = xp_480_500_subset[order(xp_480_500_subset$match_encode_id),]
    
    #Combien de matchs il y a dans le tableau (valeurs uniques)
    length(unique(xp_480_500_subset$match_encode_id))
    
    #Verifier si chaque match revient bien 4 fois
    table(unlist(xp_480_500_subset$match_encode_id))

    
    
    
#Creer un tableau avec 50 lignes / 260 lignes des xp entre 450 et 480
xp_450_480 = subset(donnees_select, donnees_select$cumul_xp_killer >= 450 & donnees_select$cumul_xp_killer < 480)

    #Ordonner la table en ordre de match ID (ascendant et grouper ensemble les 4 lignes par match)
    xp_450_480 = xp_450_480[order(xp_450_480$match_encode_id),]
    
    #Combien de matchs il y a dans le tableau (valeurs uniques)
    length(unique(xp_450_480$match_encode_id))
    
    #Verifier si chaque match revient bien 4 fois
    table(unlist(xp_450_480$match_encode_id))

    
    
    
#Tableau de tous les matchs du predator qui a le plus d'xp cumul a la fin
top_xp_pred = subset(donnees_select, donnees_select$predator_id == "4690186")
  
    #Ordonner la table en ordre d'xp (ascendant)
    top_xp_pred = top_xp_pred[order(top_xp_pred$cumul_xp_killer),]
    
    
    
#----------- TEST STATS SUR LE SOUS-ECHANTILLON 1 ----------------------------------
  
    
    
  #Graphique de la relation entre l'xp et le temps a garder
    ggplot(subset_final, aes(x = cumul_xp_killer, y = guard_time_total)) +
      geom_point() +
      labs(x = "Cumul XP", y = "Time spent guarding",
           title = "Guarding time and experience")
    
    #Faire le modele
    fit1 = brm(guard_time_total ~ cumul_xp_killer, data = subset_final)
    
    #Graphique de l'intercept, de la variable x et des chaines
    plot(fit1)
    
    #La moyenne de l'echantillon (noir) vs les moyennes des sims
    bayesplot_grid(pp_check(fit1, type = 'stat', stat = mean))
    
    #Distribution de notre echantillon vs les sims
    bayesplot_grid(pp_check(fit1, ndraws = 100))
    
    #Resume du modele
    summary(fit1)
    
    #Mettre les residus du modele dans un variable
    residus = resid(fit1)
    
    # Vérifier la normalité des résidus
    hist(residus, xlab = "Résidus", ylab = "Nombre d’observations", 
         main = '', col = 'darkgray', cex.lab = 1.5)
    
    
    #QQplot de normalite pour les residus du modele
    qqnorm(resid(fit1))
    qqline(resid(fit1))
    
    # Homogénéité des résidus
    plot(resid(fit1) ~ fitted(fit1), ylab = "Résidus", xlab = "Valeurs prédites")
    abline(h = 0, lty = 2, col = "red")
    
    
    
    
    
    
    #Transformer en log pour voir si ca regle les problemes
    subset_final$logGuard_time_total = log10(subset_final$guard_time_total + 1)
    
    
    
    ggplot(subset_final, aes(x = cumul_xp_killer, y = logGuard_time_total)) +
      geom_point() +
      labs(x = "Cumul XP", y = "Time spent guarding",
           title = "Guarding time and experience")

    
    
    
    fit2 = brm(logGuard_time_total ~ cumul_xp_killer, data = subset_final)
    
    
    residus2 = resid(fit2)
    
    # Vérifier la normalité des résidus
    hist(residus2, xlab = "Résidus", ylab = "Nombre d’observations", 
         main = '', col = 'darkgray', cex.lab = 1.5)
    
    
    
    qqnorm(resid(fit2))
    qqline(resid(fit2))
    
    # Homogénéité des résidus
    plot(resid(fit2) ~ fitted(fit2), ylab = "Résidus", xlab = "Valeurs prédites")
    abline(h = 0, lty = 2, col = "red")
    
    
    plot(donnees$cumul_xp_killer)
    
    
    
    
    
    
#------------------- DATA MANIP ET TEST STATS SUR LE SOUS-ECHANTILLON 3 AVEC 19 EXPERTS --------------------
    
    #Combien d'observations dans chaque niveau du facteur expertise
    setDT(donnees_unique)[ , .N, keyby = expertise]
    
    #Sous-echantillonner 19 experts uniques (350+ xp) et storer les id
    id_expert = sample(unique(donnees_unique[cumul_xp_killer >= 350]$predator_id, 19))
    
      #Ordonner les id
      id_expert = sort(id_expert, decreasing = FALSE)
    
    #Prendre les observations des experts stored dans id_expert
    data_expert = donnees_unique[donnees_unique$predator_id %in% id_expert,]
  
      #Ordonner la table en ordre d'id (ascendant)
      data_expert = data_expert[order(data_expert$predator_id),]
    
    #Verifier si le vecteur id_expert est pareil a la colonne dans data_expert
    identical(unique(id_expert), unique(data_expert$predator_id))
  
    
    #Combien de matches chacun des predateur a joue dans l'echantillon des 19 predateurs
    table(unlist(data_expert$predator_id))
    
      #Ajouter colonne pour le sqrt de cumul xp killer
      data_expert[, ":=" (cumul_xp_killer_sqrt = sqrt(cumul_xp_killer))]
    
      #Fonction pour standariser
      standardize = function (x) {(x - mean(x, na.rm = TRUE)) / 
        sd(x, na.rm = TRUE)}
    
      #Utiliser la fonction de standardisation sur les variables des colonnes specifiees et creer des nouvelles colonnes
      data_expert[, c("Zcumul_xp_killer") :=
           lapply(.SD, standardize), 
         .SDcols = 16]
    
    

  #Graphiques de la vitesse et temps a garder selon l'experience
    
    #Temps garde
    ggplot(data_expert, aes(x = expertise, y = guard_time_total)) +
      geom_point() +
      labs(x = "Cumul XP", y = "Time spent guarding",
           title = "Guarding time and experience")
    
        #version boxplot
        plot(data_expert$guard_time_total ~ factor(data_expert$expertise))
    
    #Vitesse
    ggplot(data_expert, aes(x = expertise, y = pred_speed)) +
      geom_point() +
      labs(x = "Cumul XP", y = "Vitesse du predateur",
           title = "Speed and experience")
    
        #version boxplot
        plot(data_expert$pred_speed ~ factor(data_expert$expertise))
    
    
    #Modele du temps a garder selon l'experience
    mod_guard_xp = brm(guard_time_total ~ s(cumul_xp_killer), data = data_expert, control = list(adapt_delta = 0.99))
    
      #Formule pour le modele
      form_guard = brmsformula(guard_time_total ~ 1 + Zcumul_xp_killer + (1 | predator_id), 
                         sigma ~ 1 + Zcumul_xp_killer) +
          gaussian()
      
      
      
      #Formule pour avoir un pente pour chaque joueur
      form_guard = brmsformula(guard_time_total ~ 1 + Zcumul_xp_killer + (1 + Zcumul_xp_killer | predator_id), 
                               sigma ~ 1 + Zcumul_xp_killer) +
        gaussian()
    
      #Modele brm plus complet
      fit_guard <- brm(formula = form_guard,
                  prior = NULL,
                  iter = 1500,
                  warmup = 500,
                  thin = 4,
                  chains = 4,
                  seed = 123,
                  control = list(adapt_delta = 0.99),
                  save_pars = save_pars(all = TRUE),
                  sample_prior = FALSE,
                  data = data_expert)
    
    
    
    #Graphique de l'intercept, de la variable x et des chaines
    plot(fit_guard)
    
    #La moyenne de l'echantillon (noir) vs les moyennes des sims
    bayesplot_grid(pp_check(fit_guard, type = 'stat', stat = mean))
    
    #Distribution de notre echantillon vs les sims
    bayesplot_grid(pp_check(fit_guard, ndraws = 100))
    
    #Resume du modele
    summary(fit_guard)
    
    #Mettre les residus du modele dans un variable
    residus = resid(fit_guard)
    
    # Verifier la normalite des residus
    hist(residus, xlab = "Résidus", ylab = "Nombre d’observations", 
         main = '', col = 'darkgray', cex.lab = 1.5)
    
    
    #QQplot de normalite pour les residus du modele
    qqnorm(resid(fit_guard))
    qqline(resid(fit_guard))
    
    # Homogénéité des résidus
    plot(resid(fit_guard) ~ fitted(mod_guard_xp), ylab = "Résidus", xlab = "Valeurs prédites")
    abline(h = 0, lty = 2, col = "red")
    
    
    #Graphique de la relation (non-lineaire)
    plot(conditional_effects(mod_guard_xp), points = TRUE)
    
    
    
#--------------------- ARCHIVES ET REFERENCES ---------------------------------
    
    
    
#Code du gars dans disc pour tester si la fonction fait ce qu'elle est suppose faire 
#aka checker si chaque valeur unique est la 4 fois
    
    # Function to answer the question:
    #  Do each of the values occur exactly four times?
    my_fun2 <- function( vals ){
      runs <- rle(sort(vals))
      result <- all(runs$lengths == 4)
      return(result)
    }
    
    # Test that should return TRUE
    t1_vals <- c(rep(1,4), rep(2,4), rep(3,4))
    t1_result <- my_fun2( t1_vals )
    if(t1_result == TRUE){
      print("Expect TRUE test passes") 
    } else {
      print("Expect TRUE test fails") 
    }
    
    # Test that should return FALSE
    #   3 only occurs 3 times.
    t2_vals <- c(rep(1,4), rep(2,4), rep(3,3))
    t2_result <- my_fun2( t2_vals )
    if(t2_result == FALSE){
      print("Expect FALSE test passes") 
    } else {
      print("Expect FALSE test fails") 
    }
    
    # Test that should return TRUE
    #   numbers are out of order but all numbers are there 4 times.
    t3_vals <- c(1,1,2,2,3,3,1,1,2,2,3,3)
    t3_result <- my_fun2( t3_vals )
    if(t3_result == TRUE){
      print("Out of order test passes") 
    } else {
      print("Out of order test fails") 
    }
    
    
    # Test that should return FALSE
    #   numbers are in order but all numbers aren't there 4 times.
    t4_vals <- c(1,1,1,2,2,2,2,1)
    t4_result <- equal_occurrence( t4_vals )
    if(t4_result == TRUE){
      print(" In order AND equal occurrence test passes") 
    } else {
      print("In order AND equal occurrence test fails") 
    }
    
        
