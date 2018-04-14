## Script to input and format AFDM from cyano light experiment 2017
## afdm= grams
## chla= micrograms/L
## pe= absorbance values from spectrophotometer

## area of each cobble calculated with ImageJ (units = cm^2)

format_mat_data <- function(){
#### Libraries #################################################################
library(tidyverse)
library(stringr)
################################################################################


#### FILE PATHS ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_mat_data")
################################################################################


#### READ IN AFDM, CHLA, AND PHYCOERYTHRIN DATA
sampID <- read_tsv(file.path(dir_input, "subsamplingID.txt"))
phorm.afdm <- read_tsv(file.path(dir_input, "AFDM.txt"))
phorm.chla <- read_tsv(file.path(dir_input, "chla.txt"))
phorm.pe <- read_tsv(file.path(dir_input, "phycoerythrin.txt"))
cob.area <- read_tsv(file.path(dir_input, "cobble_area.txt"))

#### FORMAT COBBLE AREA DATA
cob.area.data <- cob.area %>%
                   separate(cob_ID, into= c("site_rep", "source", "treat"), sep= "_") %>%
                   mutate(rep= str_extract(site_rep, "[^a-z]."),
                          site= str_extract(site_rep, "[a-z]")) %>%
                   mutate(site= ifelse(.$site == "a", "sf", "el"),
                          cob_area_cm2= round(cob_area_cm2, 1)) %>%
                   select(site, rep, treat, source, cob_area_cm2)


#### FORMAT AFDM DATA
phorm.afdm.data <- phorm.afdm %>%
                     full_join(sampID, .) %>%
                     mutate(dw_g= (DW_tin_filt_samp - DW_tin_filt) * (Total_vol / AFDM_vol),
                            afdm_g= (DW_tin_filt_samp - ASHED_tin_filt_samp) * (Total_vol / AFDM_vol)) %>%
                     select(site, rep, treat, source, id, dw_g, afdm_g)


#### FORMAT CHLA DATA
phorm.chla$dilution_factor <- ifelse(is.na(phorm.chla$dilution_factor)==TRUE, 1, phorm.chla$dilution_factor)
phorm.chla.data <- phorm.chla %>%
                     full_join(sampID, .) %>%
                     mutate(chla_ug= (chla_raw_ugL / dilution_factor) * (Total_vol / Chla_vol)) %>%
                     select(site, rep, treat, source, id, chla_raw_ugL, chla_ug)


#### FORMAT PHYCOERYTHRIN DATA
  ## Change outlier nm615 value to the median value of the nm615 readings
    # boxplot(phorm.pe$nm615)
    # phorm.pe[which(phorm.pe$nm615 > 0.1), ]
phorm.pe[phorm.pe$id == 4, ]$nm615 <- median(phorm.pe[which(phorm.pe$nm615 < 0.1), ]$nm615)

## Calculate pe values from spectrophotometer measurements
   # phorm_pe_std_curves.Rmd for decisions on which pe measurements to use
calculate.phycoerythrin.mg_mL <- function(df){
  # From Siegelman and Kycia 1978, p. 74
  # units mg / mL
  pc <- (df$nm615 - 0.474*df$nm562) / 5.34
  apc <- (df$nm562 - 0.208*df$nm615) / 5.09
  pe <- (df$nm562 - 2.41*pc - 0.849*apc) / 962
  return(pe)
}

phorm.pe.data <- phorm.pe %>%
  filter(id != "blank") %>%
  mutate(id= as.numeric(id)) %>%
  full_join(sampID, .) %>%
  mutate(pe_ug= (calculate.phycoerythrin.mg_mL(.) * 1000000) * (Total_vol / PE_vol))

#### MERGE ALL DATA FRAMES TOGETHER
phorm.mat.data <- full_join(phorm.afdm.data, phorm.chla.data) %>%
                    full_join(., phorm.pe.data) %>%
                    full_join(., cob.area.data) %>%
                    mutate(site.abv= ifelse(site== "sf", "a", "b"),
                           cobbleID= str_c(str_c(site.abv, rep, sep=""), source, treat, sep= "_")) %>%
                    mutate_at(vars(dw_g, afdm_g), funs(round(., 4))) %>%
                    mutate_at(vars(chla_ug, pe_ug), funs(round(., 1))) %>%
                    mutate(source= ifelse(site == source, "Local", "Transplant")) %>%
                    mutate(site= ifelse(site == "sf", "Eel site", "Elder site")) %>%
                    select(cobbleID, site, rep, treat, source, id, cob_area_cm2, dw_g, afdm_g, chla_ug, pe_ug)

#### REMOVE REPLICATES THAT DID NOT GROW
## See phorm_area_format.R script
replicates.to.remove <- read_tsv("/Users/KeithBG/Documents/UC Berkeley/CyanoMeta_NSF/LightExp/Data/replicates_to_remove.txt")

phorm.mat.data <- phorm.mat.data %>%
  filter(!(cobbleID %in% unique(replicates.to.remove$cobbleID)))

rm(replicates.to.remove)

## Remove a09_el_l DW and AFDM data because numbers are outliers and too high
## I think I mis-recorded the DW in my lab notebook,
## so I don't know what the actual DW value is
phorm.mat.data[phorm.mat.data$cobbleID == "a09_el_l", names(phorm.mat.data) %in% c("dw_g", "afdm_g")] <- NA

#### DIVIDE BY COBBLE AREA TO NORMALIZE BY CM^2
phorm.mat.data <- phorm.mat.data %>%
                    mutate(dw_mg_cm2= (dw_g / cob_area_cm2)*1000,
                           afdm_mg_cm2= (afdm_g / cob_area_cm2)*1000,
                           chla_ug_cm2= chla_ug / cob_area_cm2,
                           pe_ug_cm2= pe_ug / cob_area_cm2) %>%
                    mutate_at(vars(dw_mg_cm2:pe_ug_cm2), funs(round(., 3)))

return(phorm.mat.data)
}



