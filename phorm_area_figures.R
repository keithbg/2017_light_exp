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
################################################################################


#### SOURCE DATA FORMATING SCRIPT  #############################################
source(file.path(dir_input, "phorm_area_format.R"))

################################################################################


#### CREATE PLOTTING VARIABLES #################################################
source("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Scripts/ggplot_themes.R")
source("/Users/KeithBG/R_functions/ggplot_themes.R")
light.theme <- theme(axis.text.x= element_text(angle= 45, hjust= 0.9, size= 10), legend.position = "top")
exp.dates <- unique(phorm.area.curated$date)
exp.day <- c(0, 4, 7, 9, 12)

x.axis.label <- expression(paste(italic("Phormidium"), " source"))
y.axis.label <- expression(paste(italic("Phormidium"), " expansion (", cm^{2}, ")"))
################################################################################


#### MAKE PLOTS ################################################################

## GROWTH AREA BOXPLOTS
plot.growth <- ggplot(phorm.area.curated.growth, aes(x= source, y= growth_cm))
plot.growth +
  geom_hline(yintercept = 0, size= 0.25, color= "black") +
  geom_boxplot(aes(fill= treat)) +
  scale_y_continuous(limits= c(0, 90), breaks= seq(0, 90, by= 15)) +
  scale_fill_manual(values= c("white", "dark gray"), labels= c("Light", "Shade"), name= "Treatment") +
  labs(x= x.axis.label, y= y.axis.label) +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
ggsave(last_plot(), file= "phorm_area_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)


#### TIME SERIES PLOTS
plot.raw <- ggplot(phorm.area, aes(x= date, y= area_cm))
plot.raw +
  geom_line(aes(color= treat, linetype= source)) +
  geom_point(aes(color= treat)) +
  scale_x_date(breaks= exp.dates, labels= exp.day) +
  facet_grid(site~rep) +
  labs(x= "Experiment Day", y= "area cm^2") +
  theme_kbg + light.theme
ggsave(last_plot(), filename= "phorm_area_timeseries_raw.pdf", width= 12, height= 6, units= "in", path= dir_out_fig)


plot.curated <- ggplot(phorm.area.curated, aes(x= date, y= area_cm))
plot.curated +
  geom_line(aes(color= treat, linetype= source)) +
  geom_point(aes(color= treat)) +
  scale_x_date(breaks= exp.dates, labels= exp.day) +
  facet_grid(site~rep) +
  labs(x= "Experiment Day", y= "area cm^2") +
  #ggtitle("b05_sf_s and b08_sf_s removed") +
  theme_kbg + light.theme
ggsave(last_plot(), filename= "phorm_area_timeseries_curated.pdf", width= 12, height= 6, units= "in", path= dir_out_fig)
