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
dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_tables")
################################################################################


#### SOURCE DATA FORMATING FUNCTION  #############################################
source(file.path(dir_input, "phorm_mat_format.R"))
phorm.mat.data <- format_mat_data()
################################################################################


#### SUMMARY TABLE  ############################################################
## Cobble area
phorm.mat.data %>%
  mutate(treat= recode(.$treat, l="Light", s= "Shade")) %>%
  group_by(site, treat, source) %>%
  summarise(mean_cob_area_cm2= round(mean(cob_area_cm2, na.rm= TRUE), 0),
            sd_cob_area_cm2= round(sd(cob_area_cm2, na.rm= TRUE), 0)) %>%
  ungroup() %>%
  mutate(area_cm2= str_c(.$mean_cob_area_cm2, "\U00B1", .$sd_cob_area_cm2, sep= " ")) %>%
  write_tsv(path= file.path(dir_out_table, "cobble_area_table.txt"))
################################################################################


#### CREATE PLOTTING VARIABLES #################################################
## Source theme script
source("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Scripts/ggplot_themes.R")
################################################################################


#### MAKE PLOTS ################################################################

#### PHYCOERYTHRIN
pe.plot <- ggplot(phorm.mat.data, aes(x= source, y= pe_ug_cm2))

pe.plot +
  x.int.line +
  geom_boxplot(aes(fill= treat)) +
  labs(x= x.axis.source, y= expression(paste("Phycoerythrin  (", "\U00B5", "g / ", cm^{2}, ")"), family= "sans")) +
  fill.treat +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "phorm_pe_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)

#### AFDM
afdm.plot <- ggplot(phorm.mat.data, aes(x= source, y= afdm_mg_cm2))

afdm.plot +
  x.int.line +
  geom_boxplot(aes(fill= treat)) +
  labs(x= x.axis.source, y= expression(paste("AFDM (mg / ", cm^{2}, ")"))) +
  fill.treat +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "phorm_afdm_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)



#### CHLA
chla.plot <- ggplot(phorm.mat.data, aes(x= source, y= chla_ug_cm2))

chla.plot +
  x.int.line +
  geom_boxplot(aes(fill= treat)) +
  labs(x= "Inoculation source", y= expression(paste("Chlorophyll-a  (", "\U00B5", "g / ", cm^{2}, ")"), family= "sans")) +
  fill.treat +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")

#### DW
dw.plot <- ggplot(phorm.mat.data, aes(x= source, y= dw_mg_cm2))

dw.plot +
  x.int.line +
  geom_boxplot(aes(fill= treat)) +
  labs(x= "Inoculation source", y= expression(paste("DW (mg / ", cm^{2}, ")"))) +
  fill.treat +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")



