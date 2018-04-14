## Generate figures from Phormidium light transplant experiment from summer 2017
## measurements from scrubbed cobbles at end of experiment
## data= dry weight, afdm, chla, phycoerythrin (pe)

## site= Elder Creek (el) or SF Eel at Science Center (sf)
## treat= ambient light (l) or shade cloth (s) treatment
## source= location of inoculated Phormidium (el or sf)

#### Libraries #################################################################
library(ggplot2)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Scripts")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data", "output_figures")
################################################################################


#### SOURCE DATA FORMATING FUNCTION  #############################################
source(file.path(dir_input, "phorm_mat_format.R"))
phorm.mat.data <- format_mat_data()
################################################################################


#### CREATE PLOTTING VARIABLES #################################################
## Source theme script
source("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Scripts/ggplot_themes.R")
x.axis.label <- expression(paste(italic("Phormidium"), " source"))
################################################################################



#### MAKE PLOTS ################################################################

#### PHYCOERYTHRIN
pe.plot <- ggplot(phorm.mat.data, aes(x= source, y= pe_ug_cm2))

pe.plot +
  geom_hline(yintercept = 0, size= 0.25, color= "black") +
  geom_boxplot(aes(fill= treat)) +
  labs(x= x.axis.label, y= expression(paste("Phycoerythrin  (", mu, "g / ", cm^{2}, ")"), family= "sans")) +
  scale_fill_manual(values= c("white", "dark gray"), labels= c("Light", "Shade"), name= "Treatment") +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "phorm_pe_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)

#### AFDM
afdm.plot <- ggplot(phorm.mat.data, aes(x= source, y= afdm_mg_cm2))

afdm.plot +
  geom_hline(yintercept = 0, size= 0.25, color= "black") +
  geom_boxplot(aes(fill= treat)) +
  labs(x= x.axis.label, y= expression(paste("AFDM (mg / ", cm^{2}, ")"))) +
  scale_fill_manual(values= c("white", "dark gray"), labels= c("Light", "Shade"), name= "Treatment") +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "phorm_afdm_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)



#### CHLA
chla.plot <- ggplot(phorm.mat.data, aes(x= source, y= chla_ug_cm2))

chla.plot +
  geom_hline(yintercept = 0, size= 0.25, color= "black") +
  geom_boxplot(aes(fill= treat)) +
  labs(x= "Inoculation source", y= expression(paste("Chlorophyll-a  (", mu, "g / ", cm^{2}, ")"), family= "sans")) +
  scale_fill_manual(values= c("white", "dark gray"), labels= c("Light", "Shade"), name= "Treatment") +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")

#### DW
dw.plot <- ggplot(phorm.mat.data, aes(x= source, y= dw_mg_cm2))

dw.plot +
  geom_hline(yintercept = 0, size= 0.25, color= "black") +
  geom_boxplot(aes(fill= treat)) +
  labs(x= "Inoculation source", y= expression(paste("DW (mg / ", cm^{2}, ")"))) +
  scale_fill_manual(values= c("white", "dark gray"), labels= c("Light", "Shade"), name= "Treatment") +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")



