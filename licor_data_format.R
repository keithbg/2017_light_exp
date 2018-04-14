## Script to format data from LiCor deployments for Cyano Light Experiment 2017
## LiCor LI193 quantum sensors deployed ~15cm above water in the air
## at South Fork Science Center (SF) and Elder Creek (EL) upstream of bridge
## LI1500 Logger had 2 inputs:
## INPUT1= LI9429
## INPUT2= LI9430
## Hobo Logger had single LI193 quantum sensor= LI9441
## Hobo logger records volts, and has to be converted to PAR using equation given by LiCor


#### Libraries #################################################################
library(tidyverse)
library(stringr)
library(lubridate)
library(zoo)
library(hms)
################################################################################


#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","licor_data")
################################################################################


#### FORMAT LI1500 LOGGER DATA
format_li1500_data <- function(){
## Read in data files
licor_LI1500_files <- file.path(dir_input, "li1500_logger", list.files(path= file.path(dir_input, "li1500_logger"), pattern="*.TXT"))
li1500.data <- lapply(licor_LI1500_files, function(x) read_tsv(file= x, skip= 6))

## Give each element in the list the filename
lnames <- str_sub(licor_LI1500_files, start= nchar(licor_LI1500_files)-13, nchar(licor_LI1500_files)-4)
names(li1500.data) <- lnames

## Add ID column and remove extra X15 column in each data frame in the list
li1500.data <- Map(cbind, li1500.data, id= lnames)
li1500.data <- lapply(li1500.data, function(x) x[!(names(x) %in% c("X15"))])

## Fix the date on SF20170803
li1500.data[[6]]$Date <- as.Date(li1500.data[[6]]$Date, format= "%m/%d/%y")

## Combine into a dataframe and format/create columns
li1500.data.df <- as_tibble(do.call("rbind", li1500.data))

li1500.data.df <- li1500.data.df %>%
                    mutate(INPUT1= as.numeric(INPUT1),
                           INPUT2= as.numeric(INPUT2),
                           id= as.character(id), # add TXT file id
                           site= str_sub(id, start= 1, end= 2), # add site SF or EL
                           date.time= ymd_hms(paste(as.character(Date), as.character(Time), sep= " ")))
rm(li1500.data)

## Subset data to include only INPUT1 and INPUT2 (the average par values per logging event)
li1500.data.par <- li1500.data.df %>%
                      select(Date, Time, INPUT2, INPUT1, date.time, id, site) %>%
                      filter(hour(Time) >= 6 & hour(Time) < 21) %>%
                      gather(key= "sensor", value= "par", c(INPUT1, INPUT2)) %>% # make INPUT 1 and 2 long
                      mutate(sensor= ifelse(sensor == "INPUT1", "LI9429", "LI9430")) # change input to quantum sensor serial numbers
return(li1500.data.par)
}
li1500.data.par <- format_li1500_data()

#### FORMAT HOBO LOGGER DATA
format_hobo_data <- function(){
## Read in data files
licor_hobo_file <- file.path(dir_input, "hobo_logger", "20170806_hobo_20164871_full_TextToColumn.txt")

hobo.data <- read_tsv(file= licor_hobo_file, skip= 2,
                      col_names= c("1", "date.time", "volt", "X4", "X5", "X6")) %>%
  select(c(date.time, volt)) %>%
  filter(is.na(volt)== FALSE) # remove NAs when turning off logger

## Calculate conversion factor for volts to par (micro-mols / m^-2 / s^-1)
convert.volts.to.par <- function(){
  volt.max <- 5 # volts
  par.max <- 1900 # micro-mol m^-2 s^-1
  cal.constant.air <- 6.87 # microamps / 1000 micro-mol m^-2 s^-1
  actual.gain <- 0.375 # volts / micro-amp
  M <- 1 / ((actual.gain/1000) *cal.constant.air)
}
voltage.multiplier <- convert.volts.to.par()


hobo.data.par <- hobo.data %>%
  mutate(par.raw= volt*voltage.multiplier,
         par= c(rep(NA, 4), rollmean(par.raw, k= 5, align= "right")), # calculate 5 minute averages like LI1500
         date.time=  parse_date_time(date.time, c("mdy HM")),
         Date= as_date(date.time),
         Time= as.hms(paste(hour(date.time), minute(date.time), "00", sep= ":")),
         id= "SF20170806_hobo",
         site= "SF",
         sensor= "LI9441",
         num= seq(1:nrow(.))) %>%
  filter(num %% 5 == 0) %>%  # Extract every 5th observation to match the LI1500 data set
  filter(hour(Time) >= 6 & hour(Time) < 21) %>%  # ignore dusk/dawn values
  filter(Date != "2017-08-06") # ignore deployment date b/c <24 hours
return(hobo.data.par)
}
hobo.data.par <- format_hobo_data()

#### COMBINE DATA FRAMES
licor.data <- full_join(li1500.data.par, hobo.data.par) %>%
              select(Date, Time, date.time, id, site, sensor, par)

rm(li1500.data.par)
rm(hobo.data.par)

#### ADD DEPLOYMENT LOCATIONS
deploy.loc <- read_tsv(file.path(dir_input, "licor_deployment_locations.txt"))
licor.data <- left_join(licor.data, deploy.loc) %>%
                mutate(site= ifelse(site=="EL", "Elder site", "Eel site"))
rm(deploy.loc)

#### CALCULATE DAILY SUMMARY STATISTICS FOR PAR VALUES

licor.data.stats <- licor.data %>%
  group_by(site, id, rep, sensor, Date) %>%
  summarize(sum.par= sum(par, na.rm= TRUE),
            mean.par= mean(par, na.rm= TRUE),
            sd.par= sd(par, na.rm= TRUE),
            med.par= median(par, na.rm= TRUE)) %>%
  filter(sum.par > 5000) # remove incomplete sampling day


## Calculate mean daily sum of par for each replicate
mean.site.par <- licor.data.stats %>%
  group_by(site, rep) %>%
  summarize(mean.sum.par= mean(sum.par))








