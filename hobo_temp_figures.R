## Script to format data from Hobo Pendant deployments for Cyano Light Experiment 2017
## at South Fork Science Center (SF) and Elder Creek (EL) upstream of bridge

## One logger was placed in 5 light and shade replicates at each site (N= 10)
## Lux and temperature readings taken every 15 minutes

#### Libraries #################################################################
library(ggplot2)
################################################################################


#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Scripts")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","hobo_lux_temp_data", "hobo_output_figures")
dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_tables")
################################################################################


#### SOURCE DATA FORMATING SCRIPT  #############################################
source(file.path(dir_input, "hobo_data_format.R"))
# output data frame = hobo.df
################################################################################


#### SUMMARY TABLES ############################################################

## Daily temps
hobo.temp.sum %>%
  mutate(treat= recode(.$treat, l="Light", s= "Shade")) %>%
  group_by(site, treat) %>%
  summarise(mean_temp_C= round(mean(mean.temp, na.rm= TRUE), 1),
            sd_mean_temp_C= round(sd(mean.temp, na.rm= TRUE), 1),
            max_temp_C= round(mean(max.temp, na.rm= TRUE), 1),
            sd_max_temp_C= round(sd(max.temp, na.rm= TRUE), 1)) %>%
  ungroup() %>%
  mutate(mean_temp= str_c(.$mean_temp_C, "\U00B1", .$sd_mean_temp_C, sep= " "),
         max_temp= str_c(.$max_temp_C, "\U00B1", .$sd_max_temp_C, sep= " ")) %>%
  write_tsv(path= file.path(dir_out_table, "hobo_temp_table.txt"))


#### CREATE PLOTTING VARIABLES #################################################
## Source theme script
source("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Scripts/ggplot_themes.R")
################################################################################



#### MAKE PLOTS
temp.plot <- ggplot(hobo.df, aes(x= date_time, y= temp))
temp.plot +
  geom_line(aes(color= rep)) +
  facet_grid(site~.) +
  theme_bw()

temp.mean.BP <- ggplot(hobo.temp.sum, aes(x= NA, y= mean.temp))
temp.mean.BP +
  geom_boxplot(aes(fill= treat)) +
  labs(x= "", y= expression(paste("Mean daily temperature (", "\U00B0", "C)"))) +
  fill.treat +
  scale_x_discrete(breaks= NULL) +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
#ggsave(last_plot(), file= "phorm_pe_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)

temp.max <- ggplot(hobo.temp.sum, aes(x= NA, y= max.temp))
temp.max +
  geom_boxplot(aes(fill= treat)) +
  labs(x= "", y= expression(paste("Max. daily temperature (", "\U00B0", "C)"))) +
  fill.treat +
  scale_x_discrete(breaks= NULL) +
  facet_grid(.~site) +
  theme_mat + theme(legend.position = "bottom")
#ggsave(last_plot(), file= "phorm_pe_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)

