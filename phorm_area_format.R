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

#### Input data ####
## Read in file names
area_files <- file.path(dir_input, list.files(path= dir_input, pattern="*.tsv"))
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

# Clean up columns from first data frame
phorm.area.format[[1]] <- select(phorm.area.format[[1]], -c(Mean, Min))
names(phorm.area.format[[1]])[8] <- "Perimeter"

## Combine list elements into a single data frame
phorm.area <- do.call("rbind", phorm.area.format) %>%
                  as_tibble() %>%
                  rename(area= Area, notes_area= notes, source= inoc) %>%
                  mutate(cobbleID= str_sub(uniqueID, start= 1, end= 8),
                         site= ifelse(site== "a", "sf", "el"),
                         area_cm= area/100) %>%
                  select(uniqueID, cobbleID,site, date, rep, treat, source, area, area_cm, notes_area)
rm(phorm.area.list)
rm(phorm.area.format)

## Interpolate missing start areas on 07/25 with the mean of the starting area
phorm.area[is.na(phorm.area$area)== TRUE, ]
# hist(phorm.area[which(phorm.area$date == "2017-07-25"), "area"])

phorm.area %>%
  filter(date== "2017-07-25") %>%
  summarize(mean= mean(area, na.rm= TRUE),
            sd= sd(area, na.rm= TRUE),
            median= median(area, na.rm= TRUE))

phorm.area[is.na(phorm.area$area)== TRUE & phorm.area$date=="2017-07-25", "area"] <- mean(phorm.area[phorm.area$date=="2017-07-25", ]$area, na.rm= TRUE)
phorm.area[is.na(phorm.area$area)== TRUE & phorm.area$date=="2017-07-25", "area_cm"] <- mean(phorm.area[phorm.area$date=="2017-07-25", ]$area_cm, na.rm= TRUE)

## Convert area to cm^2


#### QA/QC the data ####

## Investigate the slopes of each rep
# slopes <- phorm.area %>%
#   group_by(cobbleID) %>%
#   do(data.frame(
#     slope= lm(area_cm ~ date, data= .)$coefficients[2],
#     adj.r2= summary(lm(area_cm ~ date, data= .))$adj.r.squared))

## Treatments to drop

# slopes[slopes$slope < 0.35, ]
# b05_sf_s: plug fell out early on, no data
# a02_el_l: worst slope in the SFSC site
# b08_sf_s: negative slope
# Choosing just to remove b05_sf_s where the plug fell out on ~day 1 and there was no mat that ever grew


phorm.area.curated <- phorm.area %>%
                        filter(!(site == "b" & rep == "05"))

# %>%
#                         filter(!(site == "a" & rep == "02"))


#### Format data sets for plotting and statistics ####

## Size increase (growth) of patch at end of experiment on 6-Aug
phorm.area.curated.growth <- phorm.area.curated %>%
  filter(date == "2017-07-25" | date == "2017-08-06") %>%
  select(-c(area, uniqueID, notes_area)) %>%
  spread(key= date, value= area_cm) %>%
  rename(start= `2017-07-25`, end = `2017-08-06`) %>%
  mutate(growth_cm= end - start)





