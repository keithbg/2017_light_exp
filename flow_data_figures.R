## Script to format data from Hobo Pendant deployments for Cyano Light Experiment 2017
## at South Fork Science Center (SF) and Elder Creek (EL) upstream of bridge

## One logger was placed in 5 light and shade replicates at each site (N= 10)
## Lux and temperature readings taken every 15 minutes

#### Libraries #################################################################
library(ggplot2)
################################################################################


#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Scripts")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_figures")
dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_tables")
################################################################################


#### SOURCE DATA FORMATING SCRIPT  #############################################
source(file.path(dir_input, "flow_data_format.R"))
################################################################################


#### GGPLOT THEME AND VARIABLES SCRIPT #########################################
source("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Scripts/ggplot_themes.R")
################################################################################


#### SUMMARY TABLES ############################################################

## Cobble depth
flow.df.l %>%
  group_by(site, treat, source) %>%
  summarise(mean_depth_cm= round(mean(cobble_depth_cm, na.rm= TRUE), 1),
            sd_mean_cm= round(sd(cobble_depth_cm, na.rm= TRUE), 1)) %>%
  ungroup() %>%
  mutate(depth_cm= str_c(.$mean_depth_cm, "\U00B1", .$sd_mean_cm, sep= " ")) %>%
  write_tsv(path= file.path(dir_out_table, "cobble_depth_table.txt"))


## Downstream velocity
flow.df.mean %>%
  group_by(site, treat) %>%
  summarise(mean_vel_cm_s= round(mean(mean_vX, na.rm= TRUE)*100, 1),
            sd_vel_cm_s= round(sd(mean_vX, na.rm= TRUE)*100, 1)) %>%
  ungroup() %>%
  mutate(vel_cm_s= str_c(.$mean_vel_cm_s, "\U00B1", .$sd_vel_cm_s, sep= " ")) %>%
  write_tsv(path= file.path(dir_out_table, "flow_velocity_table.txt"))

#### MAKE PLOTS ################################################################

## COBBLE DEPTH
depth.p1 <- ggplot(flow.df.l, aes(x= source, y= cobble_depth_cm))

depth.p1 +
  x.int.line +
  geom_boxplot(aes(fill= treat)) +
  fill.treat +
  ylim(0, 50) +
  labs(x= x.axis.source, y= "Depth of cobble (cm)") +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "cobble_depth_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)


## DOWNSTREAM FLOW VELOCITY
flow_vX.p1 <- ggplot(flow.df.mean, aes(x= NA, y= mean_vX*100))

flow_vX.p1 +
  x.int.line +
  geom_boxplot(aes(fill= treat)) +
  fill.treat +
  scale_y_continuous(limits= c(0, 70), breaks= seq(0, 70, 10)) +
  labs(x= "", y= "Downstream flow velocity (cm / s)") +
  scale_x_discrete(breaks= NULL) +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "flow_vX_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)

## CROSS-CHANNEL FLOW VELOCITY
flow_vY.p1 <- ggplot(flow.df.mean, aes(x= NA, y= mean_vY*100))

flow_vY.p1 +
  geom_boxplot(aes(fill= treat)) +
  fill.treat +
  scale_y_continuous(limits= c(-15, 15), breaks= seq(-15, 15, 5)) +
  labs(x= "", y= "Cross-channel flow velocity (cm / s)") +
  scale_x_discrete(breaks= NULL) +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "flow_vY_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)


## VERTICAL FLOW VELOCITY
flow_vZ.p1 <- ggplot(flow.df.mean, aes(x= NA, y= mean_vZ*100))

flow_vZ.p1 +
  geom_boxplot(aes(fill= treat)) +
  fill.treat +
  scale_y_continuous(limits= c(-80, 40), breaks= seq(-80, 40, 20)) +
  labs(x= "", y= "Vertical flow velocity (cm / s)") +
  scale_x_discrete(breaks= NULL) +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "flow_vZ_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)




