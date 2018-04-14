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
source(file.path(dir_input, "hobo_data_format.R"))
# output data frame = hobo.df
################################################################################

## Source ggplot theme script
source("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Scripts/ggplot_themes.R")


#### MAKE PLOTS

## Sum daily lux
lux.sum <- ggplot(hobo.lux.sum, aes(x= site, y= sum.lux))
lux.sum +
  geom_hline(yintercept = 0, size= 0.25, color= "black") +
  geom_boxplot(aes(fill= treat)) +
  scale_y_continuous(limits= c(0, 2000000), breaks= seq(0, 2000000, by= 500000)) +
  labs(x= "", y= "Total lux / day") +
  scale_fill_manual(values= c("white", "dark gray"), labels= c("Light", "Shade"), name= "Treatment") +
  theme_mat + theme(legend.position = "top")

#ggsave(last_plot(), file= "hobo_lux_sum_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)


## Proportion of daily sum in shade vs. light treatment
## shade treatment lux divided by light treatment lux

## Histogram
lux.proportion <- ggplot(hobo.lux.prop, aes(x= prop.lux))
lux.proportion +
  geom_histogram(aes(fill= rep), binwidth= 0.05, color= "black", size= 0.25) +
  labs(y= "Count", x= "Daily shade trt. lux / daily light trt. lux ") +
  scale_x_continuous(limits= c(0, 1.5), breaks= seq(0, 1.5, by= 0.5), expand= c(0, 0)) +
  scale_fill_manual(values= colorRampPalette(c("tan", "white", "dark gray"))(6), guide= guide_legend(nrow= 1, title= "Replicate")) +
  geom_hline(yintercept = 0, size= 0.25, color= "black") +
  facet_grid(site ~.) +
  theme_mat + theme(legend.position = "top")
#ggsave(last_plot(), file= "hobo_lux_prop_HIST.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)

## Boxplot
lux.prop.bp <- ggplot(hobo.lux.prop, aes(x= site, y= prop.lux))

lux.prop.bp +
  geom_boxplot(aes(fill= rep)) +
  geom_hline(yintercept = 0, size= 0.25, color= "black") +
  labs(x= "", y= "Daily shade trt. lux / \n Daily light trt. lux ") +
  scale_y_continuous(limits= c(0, 1.5), breaks= seq(0, 1.5, by= 0.5)) +
  scale_fill_manual(values= colorRampPalette(c("tan", "white", "dark gray"))(6), guide= guide_legend(nrow= 1, title= "Replicate")) +
  #facet_grid(.~rep) +
  theme_mat + theme(legend.position = "top")
#ggsave(last_plot(), file= "hobo_lux_prop_BP.pdf", height= 4.5, width= 6, units= "in", path= dir_out_fig)

## ADDITIONAL EXPLORATORY PLOTS

hobo.df %>%
  filter(hour(time) >= 8 & hour(time) <= 21) %>%
  filter(lux < 1000) %>%
  ggplot(aes(x= time, y= lux)) +
  geom_point(aes(color= rep)) +
  facet_grid(site~treat) +
  theme_bw()


head(hour(hobo.df$time))



lux.plot <- ggplot(subset(hobo.df, hobo_id == "10775908"), aes(x= date_time, y= lux))
lux.plot +
  geom_line(aes(color= rep)) +
  facet_grid(site~treat) +
  theme_bw()


## Mean daily lux
lux.mean <- ggplot(hobo.lux.sum, aes(x= site, y= mean.lux))
lux.mean +
  geom_boxplot(aes(fill= treat)) +
  theme_bw()


hobo.df %>%
filter(site== "Eel site" & treat == "l" & rep == "02")


### LOOP TO MAKE TIME SERIES PLOT FOR EACH HOBO SENSOR

summary(hobo.df$lux)

hoboID <-  unique(hobo.df$hobo_id)
for (i in 1:length(hoboID)){

file_name <- hobo.df %>%
              filter(hobo_id == hoboID[i]) %>%
              mutate(file_name= str_c(site, rep, treat, sep= "_")) %>%
              select(file_name) %>%
              slice(1)

hobo.df %>%
  filter(hobo_id == hoboID[i]) %>%
  ggplot(aes(x= date_time, y= lux)) +
  geom_line(size= 0.5) +
  ylim(0, 340000) +
  ggtitle(paste(file_name)) +
  theme_bw()

ggsave(last_plot(), filename = str_c(file_name, ".pdf"), height= 6, width= 8, units= "in", path= file.path(dir_out_fig, "hobo_lux_timeseries"))
}




