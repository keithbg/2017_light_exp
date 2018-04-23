## Script to format data from Hobo Pendant deployments for Cyano Light Experiment 2017
## at South Fork Science Center (SF) and Elder Creek (EL) upstream of bridge

## One logger was placed in 5 light and shade replicates at each site (N= 10)
## Lux and temperature readings taken every 15 minutes

#### Libraries #################################################################
library(tidyverse)
library(stringr)
library(lubridate)
library(hms)
################################################################################


#### File Path ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","hobo_lux_temp_data")
################################################################################

#### READ IN HOBO FILES AND DEPLOYMENT LOCATION FILE
  hobo_files <- list.files(path= dir_input, pattern="*.csv", full.names= TRUE)
  hobo.list <- lapply(hobo_files, function(x) read_csv(file= x, skip= 2, col_names = FALSE))
  dep.loc <- read_tsv(file.path(dir_input, "hobo_deployment_locations.txt"),
                      col_types= cols(
                        hobo_id = col_character(),
                        site = col_character(),
                        rep = col_character(),
                        treat = col_character()))

  ## Give each element in the list the filename of the hobo logger
  names(hobo.list) <- str_extract(hobo_files, "[0-9]+")

  ## Add Hobo ID column to each element in list
  hobo.list <- Map(cbind, hobo.list, hobo_id= names(hobo.list))

  ## Hobos deployed 29-Jul
  jul29_ids <- c("20189732", "20189733", "20189734", "20189735", "20189736",
                 "20189737", "20189738", "20189739",  "20189750", "20189751",
                 "20189752", "10775953")

#### CREATE DATA FRAME, FORMAT TIME COLUMNS, JOIN WITH DEPLOYMENT LOCATION INFO
  hobo.df <- do.call(rbind, hobo.list) %>%
               as_tibble() %>%
               rename(date_time= X2, temp= X3, lux= X4) %>%
               mutate(date_time= parse_date_time(date_time, c("mdy HMS p!")),
                      date= as.Date(date_time),
                      time= as.hms(paste(hour(date_time),
                                         minute(date_time),
                                         "00",
                                         sep= ":"))) %>%
               filter(date > "2017-07-26" & date < "2017-08-07") %>% # include dates of experiment
               mutate(temp= ifelse(((hobo_id %in% jul29_ids) & date < "2017-07-30"),
                                   NA,
                                   temp)) %>%
               filter(is.na(temp) == FALSE) %>% # filter 29-Jul Hobo deployments
               full_join(., dep.loc) %>% # Join with deployment data
               mutate(site= ifelse(site == "sf", "Eel site", "Elder site")) %>%
               select(site, rep, treat, hobo_id, date_time, date, time, temp, lux)



# Trim Elder 02 shade because lux values on Jul26-29 are too high. I think I mis-placed it in a spot
# that did not capture the effect of the shade cloth. Then I moved it the evening of July 30th

hobo.df[which(hobo.df$hobo_id == "10775908" & hobo.df$date < "2017-07-30"), "lux"] <- NA

# Trim Eel 02 light because the 2nd half of the time series has very low values
# also this is the only pair of replicates that has much higher shade lux values
# than light lux values. I think something weird happened in the 2nd half of the deployment

hobo.df[which(hobo.df$hobo_id == "10775942" & hobo.df$date >= "2017-08-01"), "lux"] <- NA



#### SUMMARIZE DATA

## Temperature data
hobo.temp.sum <- hobo.df %>%
                   group_by(site, rep, treat, date) %>%
                   summarise(mean.temp= mean(temp, na.rm= TRUE),
                             min.temp= min(temp, na.rm= TRUE),
                             max.temp= max(temp, na.rm= TRUE)) %>%
                   ungroup()
## Lux data
hobo.lux.sum <- hobo.df %>%
  group_by(site, rep, treat, date) %>%
  summarise(mean.lux= mean(lux, na.rm= TRUE),
            median.lux= min(lux, na.rm= TRUE),
            sd.lux= sd(lux, na.rm= TRUE),
            max.lux= max(lux, na.rm= TRUE),
            sum.lux= sum(lux, na.rm= TRUE)) %>%
  ungroup()

## Proportion of light reduction by shade treatments
shade.lux <- hobo.lux.sum %>%
  select(site, rep, date,treat, sum.lux) %>%
  filter(treat == "s") %>%
  rename(lux.shade= sum.lux) %>%
  select(site, rep, date, lux.shade)

light.lux <- hobo.lux.sum %>%
  select(site, rep, date, treat, sum.lux) %>%
  filter(treat == "l") %>%
  rename(lux.light= sum.lux) %>%
  select(site, rep, date, lux.light)

hobo.lux.prop <- full_join(shade.lux, light.lux) %>%
                   mutate(prop.lux= lux.shade / lux.light)

#### REMOVE OBSOLETE VARIABLES
rm(dir_input, hobo_files, hobo.list, dep.loc, shade.lux, light.lux)
