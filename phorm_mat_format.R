## Script to input and format AFDM from cyano light experiment 2017
## afdm= grams
## chla= micrograms/L
## pe= absorbance values from spectrophotometer

#### Libraries #################################################################
library(tidyverse)
################################################################################


#### File Paths ################################################################
dir_input <- file.path("/Users","KeithBG","Documents","UC Berkeley","CyanoMeta_NSF","LightExp", "Data","phorm_mat_data")
################################################################################


#### READ IN AFDM, CHLA, AND PHYCOERYTHRIN DATA
sampID <- read_tsv(file.path(dir_input, "subsamplingID.txt"))
phorm.afdm <- read_tsv(file.path(dir_input, "AFDM.txt"))
phorm.chla <- read_tsv(file.path(dir_input, "chla.txt"))
phorm.pe <- read_tsv(file.path(dir_input, "phycoerythrin.txt"))


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
                     mutate(chla_calc_ugL= (chla_raw_ugL / dilution_factor) * (Total_vol / Chla_vol)) %>%
                     select(site, rep, treat, source, id, chla_raw_ugL, chla_calc_ugL)


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
  mutate(pe_ugL= (calculate.phycoerythrin.mg_mL(.) * 1000000) * (Total_vol / PE_vol))

## Merge all data frames togeter
phorm.mat.data <- full_join(phorm.afdm.data, phorm.chla.data) %>%
            full_join(., phorm.pe.data) %>%
            mutate(site.abv= ifelse(site== "sf", "a", "b"),
                   cobbleID= str_c(str_c(site.abv, rep, sep=""), source, treat, sep= "_")) %>%
            select(cobbleID, site, rep, treat, source, id, dw_g, afdm_g, chla_calc_ugL, pe_ugL)

## Round columns
phorm.mat.data[, 7:8] <- sapply(phorm.mat.data[, 7:8], function(x) round(x, 4))
phorm.mat.data[, 9:10] <- sapply(phorm.mat.data[, 9:10], function(x) round(x, 1))


## Get the cm^2 from each rock using ImageJ

