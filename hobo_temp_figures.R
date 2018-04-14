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
################################################################################


#### SOURCE DATA FORMATING SCRIPT  #############################################
source(file.path(dir_input, "hobo_data_format.R"))
# output data frame = hobo.df
################################################################################


#### MAKE PLOTS
temp.plot <- ggplot(hobo.df, aes(x= date_time, y= temp))
temp.plot +
  geom_line(aes(color= rep)) +
  facet_grid(site~.) +
  theme_bw()

temp.mean <- ggplot(hobo.sum.temp, aes(x= site, y= mean.temp))
temp.mean +
  geom_boxplot() +
  facet_grid(treat~.) +
  theme_bw()

temp.max <- ggplot(hobo.sum.temp, aes(x= site, y= max.temp))
temp.max +
  geom_boxplot() +
  facet_grid(treat~.) +
  theme_bw()
