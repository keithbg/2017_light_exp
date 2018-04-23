## Make plots of licor data from cyano light experiment 2017


#### Libraries #################################################################
library(tidyverse)
library(ggplot2)
################################################################################


#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Scripts")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data", "output_figures")
# dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","licor_data", "output_tables")
################################################################################


#### RUN FORMATING SCRIPT ################################################################
source(file.path(dir_input, "licor_data_format.R"))
################################################################################

#### PLOTTING VARIABLES #####################################################
source("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Scripts/ggplot_themes.R")

par.axis.label <- expression(paste("PAR (", "\U00B5","mol / ", m^2, " / day)"))
mean.par.axis.label <- expression(paste("Mean PAR (", "\U00B5","mol / ", m^2, " / day) / replicate"))

################################################################################


#### MAKE PLOTS ####


## Daily sum boxplot
sum.plot.1 <- ggplot(licor.data.stats, aes(x= site, y= sum.par))
sum.plot.1 +
  x.int.line +
  geom_boxplot() +
  labs(x= "", y= par.axis.label) +
  ylim(0, 100000) +
  theme_mat
ggsave(last_plot(), file= "licor_daily_sum_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)


## Boxplot from mean daily sum of par for each replicate

mean.sum.plot <- ggplot(mean.site.par, aes(x= site, y= mean.sum.par))
mean.sum.plot +
  x.int.line +
  geom_boxplot() +
  geom_point(aes(color= factor(rep)), size= 2, alpha= 0.7) +
  labs(x= "", y= mean.par.axis.label) +
  scale_color_discrete(name= "Replicate") +
  ylim(0, 85000) +
  theme_mat
ggsave(last_plot(), file= "licor_daily_mean_sum_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)






#### OTHER PLOTS ###############################################################

  # sum.plot.2 <- ggplot(licor.data.stats, aes(x= site, y= sum.par))
  # sum.plot.2 +
  #   geom_boxplot() +
  #   geom_point(aes(color= factor(rep)), size= 3, alpha= 0.7) +
  #   labs(x= "Site", y= expression(paste("par (", "\U00B5","mol ", m^-2, " ", day^{-1}, ")"))) +
  #   scale_color_discrete(name= "Site Replicate") +
  #   ylim(0, 100000) +
  #   theme_mat
  #
  #
  #
  # sum.density.1 <- ggplot(licor.data.stats, aes(x= sum.par))
  # sum.density.1 +
  #   geom_density(aes(fill= site), alpha= 0.5) +
  #   xlim(0, 125000) +
  #   theme_mat
  #
  # sum.hist.1 <- ggplot(licor.data.stats, aes(x= sum.par))
  # sum.hist.1 +
  #   geom_hline(yintercept = 0, color= "dark gray") +
  #   geom_histogram(aes(fill= site), binwidth = 1000) +
  #   labs(x= expression(paste("par (", "\U00B5","mol ", m^-2, " ", day^{-1}, ")"))) +
  #   scale_y_continuous(breaks= c(0, 1, 2), expand= c(0, 0)) +
  #   xlim(0, 100000) +
  #   facet_free(site~.) +
  #   theme_mat
  #
  # rep.plot.1 <- ggplot(licor.data, aes(x= Time, y= par))
  # rep.plot.1 +
  #   geom_line(aes(color= factor(Date)), size= 0.5) +
  #   facet_grid(site~rep) +
  #   theme_mat
  #


