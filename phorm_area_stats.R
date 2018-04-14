## Statistics


#### Libraries #################################################################
library(tidyverse)
################################################################################

#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_area_data")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_area_data", "output_figures")
dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_area_data", "output_tables")
################################################################################



#### ANOVA on size of patch at end of experiment on 06-Aug
area.df.growth <- area.data.df.curated %>%
  as_tibble() %>%
  filter(date == "2017-07-25" | date == "2017-08-06") %>%
  select(-area) %>%
  spread(key= date, value= area_cm) %>%
  rename(start = `2017-07-25`, end = `2017-08-06`) %>%
  mutate(growth= end - start)

fit.growth.sf <- lm(growth_cm ~ treat*inoc, data= subset(area.df.growth,
                                                      site== "a"))
summary(fit.growth.sf)
anova(fit.growth.sf)

fit.growth.el <- lm(growth_cm ~ treat*inoc, data= subset(area.df.growth,
                                                      site== "b"))
summary(fit.growth.el)
anova(fit.growth.el)

# Adjust ANOVA treat p-values for multiple comparisons w/ SF and EL
p.adjust(c(0.001626, 0.005786), method= "bonferroni")
