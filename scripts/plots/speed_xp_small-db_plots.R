#Script to generate the plots for speed_xp_pred_avatar_expertise.R
#Author : Patrice Leveille


# =======================================================================

#                   Plot the mean and standard deviation

# =======================================================================






# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(ggpubr)
library(ggplot2)



# Load models -----------------------------------------------------------

# CV table
path <- "./outputs/tables"

# MFF je change le chemin pour un chemin de notre dossier
path <- file.path(getwd(), "model_outputs")

tab <- readRDS(file.path(path, "SP_xp_table_small-db.rds"))


# Rename the XP variable
tab[xp_level == "novice", xp_level := "Novice"]
tab[xp_level == "interm", xp_level := "Intermédiaire"]
tab[xp_level == "advanced", xp_level := "Avancé"]

# Rename the behaviour names
tab[variable == "pred_speed", variable := "Vitesse de déplacement (m/s)"]


# Reorder factors
tab[, xp_level := factor(xp_level, levels = c("Novice",
                                              "Intermédiaire",
                                              "Avancé"))]

# Encode Parameter and variable as factor
tab[, ":=" (Parameter = as.factor(Parameter),
            variable = as.factor(variable))]

# =======================================================================
# =======================================================================






# =======================================================================
# 2. Prepare the figure options
# =======================================================================


# Set custom theme -----------------------------------------------------

custom_theme <- theme(# axis values size
  axis.text.x = element_text(face = "plain", 
                             size = 15,
                             color = "black"),
  axis.text.y = element_text(face = "plain", 
                             size = 15,
                             color = "black"),
  # axis ticks lenght
  axis.ticks.length = unit(.15, "cm"),
  # axis ticks width
  axis.ticks = element_line(size = 0.90, 
                            color = "black"),
  # axis titles size
  axis.title = element_text(size = 17, 
                            face = "plain",
                            color = "black"),
  axis.line = element_line(size = 0.95,
                           color = "black"),
  panel.grid = element_blank(),
  panel.background = element_blank())

# =======================================================================
# =======================================================================





# =======================================================================
# 3. Make the plot for mean and variance
# =======================================================================


# Plot mu --------------------------------------------------------------

# MFF version :
plot1 <- ggplot(tab[Parameter == "mu"],
                aes(x = xp_level, y = mean,
                    color = xp_level,
                    shape = xp_level)) +
  
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  size = 1.3,
                  position = position_dodge(width = 0.3)) +
  
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#00AFBB")) +
  
  scale_y_continuous(breaks = seq(3.2, 3.4, 0.05),
                     limits = c(3.2, 3.4)) +
  scale_x_discrete(expand = c(0,1)) +
  ylab("\nVitesse moyenne (m/s)") +
  xlab("\nNiveau d'expérience") +
  custom_theme +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  coord_flip()



# Plot sigma ------------------------------------------------------------

# MFF version :
plot2 <- ggplot(tab[Parameter == "sigma"],
       aes(x = xp_level, y = mean,
           color = xp_level,
           shape = xp_level)) +
  
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  size = 1.3,
                  position = position_dodge(width = 0.3)) +
  
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#00AFBB")) +
  
  scale_y_continuous(breaks = seq(0.005, 0.02, 0.005),
                     limits = c(0.005, 0.02)) +
  scale_x_discrete(expand = c(0,1)) +
  ylab("\nÉcart-type de la vitesse (m/s)") +
  xlab("\nNiveau d'expérience") +
  custom_theme +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  coord_flip()



# Combine as one figure ------------------------------------------------

# Folder path
#path <- "./outputs/figures"
path <- file.path(getwd(), "figures")

# Arrange paneled figure
figure <- ggarrange(plot1, plot2,
                    ncol = 2, nrow = 1,
                    labels = c("(A)", "(B)")
                    #common.legend = TRUE,
                    #legend = "top"
                    )


# Save figure
ggexport(figure,
         filename = file.path(path, "SP_xp_small-db.png"),
         width = 3500,
         height = 1600,
         res = 300)

# =======================================================================
# =======================================================================
