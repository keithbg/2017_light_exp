## Script to format data from flow velocity measurements for Cyano Light Experiment 2017
## at South Fork Science Center (SF) and Elder Creek (EL) upstream of bridge

## Flow was measured with a Sontek Flowtracker handheld ADV

#### Libraries #################################################################
library(tidyverse)
library(stringr)
#library(lubridate)
#library(hms)
################################################################################


#### File Path ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","adv_flow_data")
################################################################################


#### READ IN AND FORMAT FLOW DATA
  flow.df <- read_tsv(file.path(dir_input, "ADV_flow_data.txt")) %>%
             ## input NA when the vZ probe was exposed (exp) above the water
               mutate(vZ= as.numeric(ifelse(.$vZ == "exp", NA, .$vZ))) %>%
               fill(frame, depth_flow_dft) %>%
             ## Create site id columns
               mutate(site= ifelse(str_detect(.$frame, "a"), "Eel site", "Elder site"),
                      treat= ifelse(str_detect(.$frame, "l"), "Light", "Shade"),
                      rep= str_extract(.$frame, "[0-9]+"),
                      date= dmy(date))

    flow.df.l <- flow.df %>%
                   gather(key= "source", value= "cobble_depth_cm", depth_sf_cm:depth_el_cm) %>%
                 ## Rename the cobbles to indicate if the Phormidium was local or transplant
                   mutate(source= ifelse((.$site == "Elder site" & .$source == "depth_el_cm") | (.$site == "Eel site" & .$source == "depth_sf_cm"), "Local", "Transplant"))

    flow.df.mean <- flow.df %>%
      group_by(date, frame) %>%
      summarise(mean_vX= mean(vX, na.rm= TRUE),
                mean_vY= mean(vY, na.rm= TRUE),
                mean_vZ= mean(vZ, na.rm= TRUE)) %>%
      left_join(., subset(flow.df, select= names(flow.df) %in% c("date", "frame", "site", "treat", "rep")))


