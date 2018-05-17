## Script to input and format environmental data from Phormidium light experiment
## summer 2017.

## Dissolved oxygen, pH, alkalinity, conductivity, temperature

#### Libraries #################################################################
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","water_chem_data")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_figures")
dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_tables")
################################################################################



#### READ AND FORMAT ENVIRONMENTAL DATA
env.df <- read_tsv(file.path(dir_input, "env_data.txt"))

#### ENVIRONMENTAL DATA SUMMARY TABLE

env.df %>%
  group_by(site) %>%
  summarise(mean_temp_C= mean(temp_C, na.rm= TRUE),
            sd_temp_C= sd(temp_C, na.rm= TRUE),
            mean_DO_mgL= mean(DO_mgL, na.rm= TRUE),
            sd_DO_mgL= sd(DO_mgL, na.rm= TRUE),
            mean_pH= mean(pH, na.rm= TRUE),
            sd_pH= sd(pH, na.rm= TRUE),
            mean_conductivity_ms= mean(conductivity_ms, na.rm= TRUE),
            sd_conductivity_ms= sd(conductivity_ms, na.rm= TRUE),
            mean_alkalinity= mean(alkalinity, na.rm= TRUE),
            sd_alkalinity= sd(alkalinity, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate_at(vars(mean_temp_C:sd_alkalinity), funs(round(., 2))) %>%
  mutate_at(vars(mean_temp_C:sd_alkalinity), funs(as.character(.))) %>%
  mutate(temp_C= str_c(.$mean_temp_C, "\U00B1", .$sd_temp_C, sep= " "),
         DO_mgL= str_c(.$mean_DO_mgL, "\U00B1", .$sd_DO_mgL, sep= " "),
         pH= str_c(.$mean_pH, "\U00B1", .$sd_pH, sep= " "),
         conductivity_ms= str_c(.$mean_conductivity_ms, "\U00B1", .$sd_conductivity_ms, sep= " "),
         alkalinity= str_c(.$mean_alkalinity, "\U00B1", .$sd_alkalinity, sep= " ")) %>%
  select(site, DO_mgL, pH, conductivity_ms, alkalinity) %>%
  write_tsv(path= file.path(dir_out_table, "env_data_table.txt"))

#### MAKE PLOT
# water %>%
#   gather(key= analyte, value= value, TDP_ugL:NP) %>%
#   ggplot(aes(x= site, y= value)) +
#   geom_boxplot(aes(fill= analyte), alpha= 0.5) +
#   geom_point(color= "black", size= 3) +
#   facet_grid(analyte~., scales= "free_y") +
#   theme_bw()
# ggsave(last_plot(), file= "water_chem_BP.pdf", height= 8, width= 8, units= "in", path= dir_out_fig)



