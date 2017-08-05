## Script to format data from LiCor deployments for Cyano Light Experiment 2017

library(tidyr)
library(ggplot2)
source("/Users/KeithBG/R_Functions/licor_import.R")

## Read in data files
el1 <- licor.import(filename= "EL072617.TXT")
sf1 <- licor.import(filename= "SF080117.TXT")

## Lets try this now