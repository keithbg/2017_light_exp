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
library(broom)
################################################################################


#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_area_data")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_area_data", "output_figures")
dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_area_data", "output_tables")
################################################################################

#### Input data ####
## Read in file names
area_files <- paste(dir_input, list.files(path= dir_input, pattern="*.tsv"), sep="/")
area.data <- lapply(area_files, read_tsv)

## Reformat the identification information
format_ID_info <- function(df){
id_info <- as.data.frame(str_split(df$ID, "_", simplify= TRUE), stringsAsFactors = FALSE)
# day <-  as.numeric(as.factor(id_info[, 4]))
id <- str_sub(df$ID, start=1, end= 8)
site <- str_sub(id_info[, 1], start=1, end=1) # site column
rep <- str_sub(id_info[, 1], start=2, end=3) # rep column
date <- as.Date(paste0("2017", id_info[, 4]), format= "%Y%m%d")

id_info <- cbind(id, site, rep, id_info[, 2:3], date)
colnames(id_info) <- c("id", "site", "rep", "inoc", "treat", "date")

df_ID <- data.frame(id_info, area= df$Area)
return(df_ID)
}
area.data.format <- lapply(area.data, format_ID_info)

## Combine list elements into a single data frame
area.data.df <- do.call("rbind", area.data.format)


## Interpolate missing start areas on 07/25 with the mean of the starting area
area.data.df[is.na(area.data.df$area)== TRUE, ]
# hist(area.data.df[which(area.data.df$date == "2017-07-25"), "area"])

area.data.df %>%
  filter(date== "2017-07-25") %>%
  summarize(mean= mean(area, na.rm= TRUE),
            sd= sd(area, na.rm= TRUE),
            median= median(area, na.rm= TRUE))

area.data.df[is.na(area.data.df$area)== TRUE & area.data.df$date=="2017-07-25", "area"] <- mean(area.data.df[area.data.df$date=="2017-07-25", ]$area, na.rm= TRUE)

## Convert area to cm^2
area.data.df$area_cm <- area.data.df$area/100

## Plot the data
# source("/Users/KeithBG/R_functions/ggplot_themes.R")
# light.theme <- theme(axis.text.x= element_text(angle= 45, hjust= 0.9, size= 10), legend.position = "top")
exp.dates <- unique(area.data.df$date)
exp.day <- c(0, 4, 7, 9, 12)


plot.raw <- ggplot(area.data.df, aes(x= date, y= area_cm))

plot.raw +
  geom_line(aes(color= treat, linetype= inoc)) +
  geom_point(aes(color= treat)) +
  scale_x_date(breaks= exp.dates, labels= exp.day) +
  facet_grid(site~rep) +
  labs(x= "Experiment Day", y= "area cm^2") +
  theme_kbg + light.theme

# ggsave(last_plot(), filename= "area_plot_raw.pdf", width= 12, height= 6, units= "in", path= dir_out_fig)

#### QA/QC the data ####
## Investigate the slopes of each rep
slopes <- area.data.df %>%
  group_by(id) %>%
  do(data.frame(
    slope= lm(area_cm ~ date, data= .)$coefficients[2],
    adj.r2= summary(lm(area_cm ~ date, data= .))$adj.r.squared))

slopes[slopes$slope < 0.35, ]

## Treatments to drop
# b05_sf_s: plug fell out early on, no data
# a02_el_l: worst slope in the SFSC site
# b08_sf_s: negative slope
# by removing 1 replicate from each site, reps = 9, but maintain a balanced design

area.data.df.curated <- area.data.df %>%
                        filter(!(site == "b" & rep == "05"))

# %>%
#                         filter(!(site == "a" & rep == "02"))

# plot.curated <- ggplot(area.data.df.curated, aes(x= date, y= area_cm))
#
# plot.curated +
#   geom_line(aes(color= treat, linetype= inoc)) +
#   geom_point(aes(color= treat)) +
#   scale_x_date(breaks= exp.dates, labels= exp.day) +
#   facet_grid(site~rep) +
#   labs(x= "Experiment Day", y= "area cm^2") +
#   theme_kbg + light.theme
#
# ggsave(last_plot(), filename= "area_plot_curated.pdf", width= 12, height= 6, units= "in", path= dir_out_fig)


#### Format data sets for plotting and statistics ####


## Growth of patch at end of experiment on 6-Aug
area.df.growth <- area.data.df.curated %>%
  as_tibble() %>%
  filter(date == "2017-07-25" | date == "2017-08-06") %>%
  select(-area) %>%
  spread(key= date, value= area_cm) %>%
  rename(start= `2017-07-25`, end = `2017-08-06`) %>%
  mutate(growth_cm= end - start)





