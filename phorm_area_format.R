  ## Script to format the surface area of Phorm mats
## KBG Feb-2018
## area calculated using imageJ


## Variable metadata
## site: a= South Fork Science Center, b= Elder Cr.
## rep: 01-10, ten reps at each site
## inoc: which site the Phormidium were harvested from: el= elder, sf= south fork science center
## treat: l=light treatment, s=shade treatment
## mo_dy: month and day of measurement
## area: area of Phormidium patch in mm^2


#### Libraries #################################################################
library(tidyverse)
library(stringr)
library(lubridate)
library(broom)
################################################################################


#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_area_data")
dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_area_data", "output_tables")
################################################################################

#### INPUT DATA ####

## Read in file names
area_files <- list.files(path= dir_input, pattern="*.tsv", full.names = TRUE)
phorm.area.list <- lapply(area_files, read_tsv)

## Reformat the identification information
format_ID_info <- function(df){
df2 <- df %>%
    separate(ID, c("info", "inoc", "treat", "date")) %>%
    separate(info, c("site", "rep"), sep=1) %>%
    mutate(date= ymd(paste0("2017", .$date)))
df3 <- cbind(uniqueID= df$ID, df2)
return(df3)
}
phorm.area.format <- lapply(phorm.area.list, function(x) format_ID_info(x))

## Clean up columns from first data frame
phorm.area.format[[1]] <- select(phorm.area.format[[1]], -c(Mean, Min))
names(phorm.area.format[[1]])[8] <- "Perimeter"

## Combine list elements into a single data frame
phorm.area <- do.call("rbind", phorm.area.format) %>%
                  as_tibble() %>%
                  rename(area= Area, notes_area= notes, source= inoc) %>%
                  mutate(cobbleID= str_sub(uniqueID, start= 1, end= 8),
                         site= ifelse(site== "a", "sf", "el"),
                         source= ifelse(site == source, "Local", "Transplant"),
                         area_cm= area/100) %>%
                  mutate(site= ifelse(site == "sf", "Eel site", "Elder site")) %>%
                  select(uniqueID, cobbleID,site, date, rep, treat, source, area, area_cm, notes_area)
rm(phorm.area.list)
rm(phorm.area.format)

#### INTERPOLATE MISSING START AREAS ON 07/25 WITH MEAN OF STARTING AREA
phorm.area[is.na(phorm.area$area)== TRUE, ]
# hist(phorm.area[which(phorm.area$date == "2017-07-25"), "area"])

phorm.area %>%
  filter(date== "2017-07-25") %>%
  summarize(mean= mean(area, na.rm= TRUE),
            sd= sd(area, na.rm= TRUE),
            median= median(area, na.rm= TRUE))

phorm.area[is.na(phorm.area$area)== TRUE & phorm.area$date=="2017-07-25", "area"] <- mean(phorm.area[phorm.area$date=="2017-07-25", ]$area, na.rm= TRUE)
phorm.area[is.na(phorm.area$area)== TRUE & phorm.area$date=="2017-07-25", "area_cm"] <- mean(phorm.area[phorm.area$date=="2017-07-25", ]$area_cm, na.rm= TRUE)


#### QA/QC THE DATA ####

## Investigate the slopes of each rep
# slopes <- phorm.area %>%
#   group_by(cobbleID) %>%
#   do(data.frame(
#     slope= lm(area_cm ~ date, data= .)$coefficients[2],
#     adj.r2= summary(lm(area_cm ~ date, data= .))$adj.r.squared))

#### REPLICATES TO REMOVE

# slopes[slopes$slope < 0.35, ]
# b05_sf_s: plug fell out early on, no data
# a02_el_l: worst slope in the SFSC site
# b08_sf_s: negative slope, never grew out of plug hole
# b07_sf_l: barely grew out of plug hole

# Choosing just to remove b05_sf_s where the plug fell out on ~day 1 and there was no mat that ever grew
# and to remove b08_sf_s where the Phorm never grew out of the plug

replicates.to.remove <- read_tsv("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Data/replicates_to_remove.txt")

phorm.area.curated <- phorm.area %>%
  filter(!(cobbleID %in% unique(replicates.to.remove$cobbleID)))

rm(replicates.to.remove)

#### FORMAT DATA FOR PLOTTING AND STATISTICS ####

## Size increase (growth) of patch at end of experiment on 6-Aug
phorm.area.curated.growth <- phorm.area.curated %>%
  filter(date == "2017-07-25" | date == "2017-08-06") %>%
  select(-c(area, uniqueID, notes_area)) %>%
  spread(key= date, value= area_cm) %>%
  rename(start_area_cm= `2017-07-25`, end_area_cm = `2017-08-06`) %>%
  mutate(growth_cm= end_area_cm - start_area_cm)





