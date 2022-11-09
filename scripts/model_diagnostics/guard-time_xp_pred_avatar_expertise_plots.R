#Model diagnostic script for guard-time_xp.R
#Author : Patrice Leveille


# =======================================================================

#                   Plot the coefficients of variation

# =======================================================================






# =======================================================================
# 1. Load libraries and models
# =======================================================================


# Librairies ------------------------------------------------------------

library(data.table)
library(dplyr)
library(ggpubr)
library(ggplot2)



# Load models -----------------------------------------------------------

# CV table
path <- "./outputs/tables"
tab <- readRDS(file.path(path, "GT_xp_table.rds"))


# Rename the XP variable
tab[xp_level == "novice", xp_level := "Novice"]
tab[xp_level == "interm", xp_level := "Intermédiaire"]
tab[xp_level == "advanced", xp_level := "Avancé"]

# Rename the behaviour names
tab[variable == "guard_time", variable := "Temps de garde"]

# Reorder factors
tab <- tab %>% arrange(factor(xp_level, levels = c("Novice",
                                            "Intermédiaire",
                                            "Avancé")), Parameter)


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

plot1 <- ggplot(tab[Parameter == "mu"],
                   aes(x = variable, y = mean,
                       color = xp_level,
                       shape = xp_level)) +
  
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  size = 0.8,
                  position = position_dodge(width = 0.3)) +
  
  scale_shape_manual(name = "Expérience totale :",
                     values = c(15, 16, 17)) +
  scale_color_manual(name = "Expérience totale :",
                     values = c("#999999", "#E69F00", "#00AFBB")) +
  
  scale_y_continuous(breaks = seq(70, 96, 2),
                     limits = c(70, 96)) +
  
  ylab("Moyenne\n") +
  xlab("\nBehavior") +
  
  custom_theme +
  theme(axis.title.x = element_blank(),
        legend.position = "top",
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))



# Plot sigma ------------------------------------------------------------

plot2 <- ggplot(tab[Parameter == "sigma"],
                   aes(x = variable, y = mean,
                       color = xp_level,
                       shape = xp_level)) +
  
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  size = 0.8,
                  position = position_dodge(width = 0.3)) +
  
  scale_shape_manual(name = "Expérience totale :",
                     values = c(15, 16, 17)) +
  scale_color_manual(name = "Expérience totale :",
                     values = c("#999999", "#E69F00", "#00AFBB")) +
  scale_y_continuous(breaks = seq(1.5, 2.2, 0.1),
                     limits = c(1.5, 2.2)) +
  
  ylab("Variation\n") +
  xlab("\nBehavior") +
  
  custom_theme +
  theme(axis.title.x = element_blank(),
        legend.position = "top",
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))



# Combine as one figure ------------------------------------------------

# Folder path
path <- "./outputs/figures"

# Arrange paneled figure
figure <- ggarrange(plot1, plot2,
                    ncol = 2, nrow = 1,
                    labels = c("(A)", "(B)"),
                    common.legend = TRUE,
                    legend = "top")


# Save figure
ggexport(figure,
         filename = file.path(path, "GT_xp_expertise.png"),
         width = 3500,
         height = 1600,
         res = 300)

# =======================================================================
# =======================================================================






# =======================================================================
# 3. Make the plot for coefficient of variances
# =======================================================================


# Plot mu --------------------------------------------------------------

cv_plot1 <- ggplot(tab[Parameter == "mu"],
                   aes(x = variable, y = mean,
                       color = xp_level,
                       shape = xp_level)) +
  
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  size = 0.8,
                  position = position_dodge(width = 0.3)) +
  
  scale_shape_manual(name = "Expérience totale :",
                     values = c(15, 16, 17)) +
  scale_color_manual(name = "Expérience totale :",
                     values = c("#999999", "#E69F00", "#00AFBB")) +
  
  scale_y_continuous(breaks = seq(0.7, 1.1, 0.05),
                     limits = c(0.7, 1.1)) +
  
  ylab("Coefficient de variation (moyenne)\n") +
  xlab("\nBehavior") +
  
  custom_theme +
  theme(axis.title.x = element_blank(),
        legend.position = "top",
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))



# Plot sigma ------------------------------------------------------------

cv_plot2 <- ggplot(tab[Parameter == "sigma"],
                   aes(x = variable, y = mean,
                       color = xp_level,
                       shape = xp_level)) +
  
  geom_pointrange(aes(ymin = lower_ci,
                      ymax = upper_ci),
                  size = 0.8,
                  position = position_dodge(width = 0.3)) +
  
  scale_shape_manual(name = "Expérience totale :",
                     values = c(15, 16, 17)) +
  scale_color_manual(name = "Expérience totale :",
                     values = c("#999999", "#E69F00", "#00AFBB")) +
  scale_y_continuous(breaks = seq(0.7, 1.1, 0.05),
                     limits = c(0.7, 1.1)) +
  
  ylab("Coefficient de variation (IIV)\n") +
  xlab("\nBehavior") +
  
  custom_theme +
  theme(axis.title.x = element_blank(),
        legend.position = "top",
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))



# Combine as one figure ------------------------------------------------

# Folder path
path <- "./outputs/figures"

# Arrange paneled figure
figure <- ggarrange(cv_plot1, cv_plot2,
                    ncol = 2, nrow = 1,
                    labels = c("(A)", "(B)"),
                    common.legend = TRUE,
                    legend = "top")


# Save figure
ggexport(figure,
         filename = file.path(path, "GT_xp_expertise_cv.png"),
         width = 3500,
         height = 1600,
         res = 300)
