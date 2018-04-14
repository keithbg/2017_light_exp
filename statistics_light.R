## Statistics for LiCor and Hobo par/lux data from Phormidium transplant experiment in 2017


#### Libraries #################################################################
library(tidyverse)
library(lme4)
library(lmerTest)
library(lmtest)
################################################################################


#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Scripts")
# dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","licor_data", "output_tables")
################################################################################


#### RUN FORMATING SCRIPTS ################################################################
source(file.path(dir_input, "licor_data_format.R"))
rm(licor.data)
source(file.path(dir_input, "hobo_data_format.R"))
rm(hobo.temp.sum)
################################################################################



#### LICOR STATISTICS

## T-test on daily PAR per site
t.test(sum.par ~ site, var.equal= FALSE, data= licor.data.stats)

## T-test on on mean daily par per replicate
t.test(mean.sum.par ~ factor(site), var.equal= FALSE, data= mean.site.par)

#### HOBO STATISTICS
hobo.t.test <- hobo.lux.sum %>%
  filter(treat == "l") %>%
  group_by(site, rep) %>%
  summarise(mean.sum= mean(sum.lux, na.rm= TRUE))

t.test(mean.sum ~ site, var.equal= FALSE, data= hobo.t.test)

unique(hobo.lux.sum$rep)
unique(hobo.lux.sum$rep)


## Recode reps so there is a unique value for each rep in Eel and Elder to run random effects model
recode.Eel <- hobo.lux.sum %>%
  filter(site== "Eel site") %>%
  mutate(rep= recode(.$rep, `02`= "2", `03`="3", `04`= "4", `06`= "6", `08`= "8", `10`= "10a"))

recode.join.full <- hobo.lux.sum %>%
  filter(site== "Elder site") %>%
  full_join(., recode.Eel)
recode.join$rep <- as.factor(recode.join$rep)

recode.join.light <-  recode.join.full%>%
                        filter(treat == "l")



fit.hobo.full <- lmer(sum.lux ~ site*treat + (1|rep), data= recode.join.full)
summary(fit.hobo)
anova(fit.hobo)


fit.hobo.light <- lmer(sum.lux ~ site + (1|rep), data= recode.join.light)
summary(fit.hobo)
anova(fit.hobo.light)


