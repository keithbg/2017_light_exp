## Script to input and format water chemistry data from Phormidium light experiment
## summer 2017.


#### Libraries #################################################################
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","water_chem_data")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_figures")
dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_tables")
################################################################################



#### READ AND FORMAT WATER CHEMISTRY DATA
water <- read_tsv(file.path(dir_input, "water_chem_data.txt")) %>%
  mutate(date= dmy(date),
         NH4_ugL= NH4_uM*14.0067,
         NO3_ugL= NO3_uM*14.0067,
         DOC_ugL= (DOC_mM*12.0107)*1000,
         TDN_ugL= (TDN_mM*14.0067)*1000,
         NP= (TDN_mM*1000)/(TDP_ugL/30.9737)) %>%
  select(date, site, contains("_ugL"), NP)


#### SUMMARY TABLE

## Water chemistry
water %>%
  group_by(site) %>%
  summarise(mean_TDP= mean(TDP_ugL, na.rm= TRUE),
            sd_TDP= sd(TDP_ugL, na.rm= TRUE),
            mean_NO3= mean(NO3_ugL, na.rm= TRUE),
            sd_NO3= sd(NO3_ugL, na.rm= TRUE),
            mean_NH4= mean(NH4_ugL, na.rm= TRUE),
            sd_NH4= sd(NH4_ugL, na.rm= TRUE),
            mean_DOC= mean(DOC_ugL, na.rm= TRUE),
            sd_DOC= sd(DOC_ugL, na.rm= TRUE),
            mean_TDN= mean(TDN_ugL, na.rm= TRUE),
            sd_TDN= sd(TDN_ugL, na.rm= TRUE),
            mean_NP= mean(NP, na.rm= TRUE),
            sd_NP= sd(NP, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate_at(vars(mean_TDP:sd_NP), funs(round(., 2))) %>%
  mutate_at(vars(mean_TDP:sd_NP), funs(as.character(.))) %>%
  mutate(TDP_ugL= str_c(.$mean_TDP, "\U00B1", .$sd_TDP, sep= " "),
         TDN_ugL= str_c(.$mean_TDN, "\U00B1", .$sd_TDN, sep= " "),
         NO3_ugL= str_c(.$mean_NO3, "\U00B1", .$sd_NO3, sep= " "),
         NH4_ugL= str_c(.$mean_NH4, "\U00B1", .$sd_NH4, sep= " "),
         `N:P`= str_c(.$mean_NP, "\U00B1", .$sd_NP, sep= " ")) %>%
  select(site, TDP_ugL, TDN_ugL, NO3_ugL, NH4_ugL, `N:P`) %>%
  write_tsv(path= file.path(dir_out_table, "water_chem_table.txt"))

#### MAKE PLOT
# water %>%
#   gather(key= analyte, value= value, TDP_ugL:NP) %>%
#   ggplot(aes(x= site, y= value)) +
#   geom_boxplot(aes(fill= analyte), alpha= 0.5) +
#   geom_point(color= "black", size= 3) +
#   facet_grid(analyte~., scales= "free_y") +
#   theme_bw()
# ggsave(last_plot(), file= "water_chem_BP.pdf", height= 8, width= 8, units= "in", path= dir_out_fig)



