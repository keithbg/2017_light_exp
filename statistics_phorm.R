## Statistics on Phormidium growth area, AFDM, and Phycoerythrin


#### Libraries #################################################################
library(tidyverse)
################################################################################

#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Scripts")
dir_out_fig <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data", "output_figures")
# dir_out_table <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","output_tables")
################################################################################



#### PHORMIDIUM AREA STATISTICS ################################################
source(file.path(dir_input, "phorm_area_format.R"))

#### ANOVA on size of patch at end of experiment on 06-Aug
fit.growth.eel <- lm(growth_cm ~ treat*source, data= subset(phorm.area.curated.growth, site== "Eel site"))
summary(fit.growth.eel)
anova(fit.growth.eel)

fit.growth.elder <- lm(growth_cm ~ treat*source, data= subset(phorm.area.curated.growth, site== "Elder site"))
summary(fit.growth.elder)
anova(fit.growth.elder)

# Adjust ANOVA treat p-values for multiple comparisons w/ SF and EL
p.adjust(c(anova(fit.growth.eel)[1, 5], anova(fit.growth.elder)[1, 5]), method= "bonferroni")


#### PHORMIDIUM MAT STATISTICS ################################################
source(file.path(dir_input, "phorm_mat_format.R"))
phorm.mat.data <- format_mat_data()


#### ANOVA PHYCOERYTHRIN
fit.pe.eel <- lm(pe_ug ~ treat*source, data= subset(phorm.mat.data, site== "Eel site"))
summary(fit.pe.eel)
anova(fit.pe.eel)

fit.pe.elder <- lm(pe_ug ~ treat*source, data= subset(phorm.mat.data, site== "Elder site"))
summary(fit.pe.elder)
anova(fit.pe.elder)

# Adjust ANOVA treat p-values for multiple comparisons w/ SF and EL
p.adjust(c(anova(fit.pe.eel)[1, 5], anova(fit.pe.elder)[1, 5]), method= "bonferroni")






#### ANOVA AFDM
fit.afdm.eel <- lm(afdm_g ~ treat*source, data= subset(phorm.mat.data, site== "Eel site"))
summary(fit.afdm.eel)
anova(fit.afdm.eel)

fit.afdm.elder <- lm(afdm_g ~ treat*source, data= subset(phorm.mat.data, site== "Elder site"))
summary(fit.afdm.elder)
anova(fit.afdm.elder)

# Adjust ANOVA treat p-values for multiple comparisons w/ SF and EL
p.adjust(c(anova(fit.afdm.eel)[1, 5], anova(fit.afdm.elder)[1, 5]), method= "bonferroni")














