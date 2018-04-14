## ggplot themes for 2017 cyano light experiment figures

## Load libraries
library(ggplot2)
library(grid)

## White background, no lines, white strip background
theme_mat <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(1, 1, 1, 1), "cm"),
                   text = element_text(size= 14),
                   plot.background = element_rect(fill = "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color="black"),
                   axis.text = element_text(colour="black"),
                   axis.title.x = element_text(margin = margin(t= 15)),
                   axis.title.y = element_text(margin = margin(r= 15)),
                   legend.background = element_rect(size=0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill= "transparent", color=NA))






## White background, no lines, white strip background, no facet labels
theme_kbg_no.facet <- theme(panel.grid = element_blank(),
                            plot.margin = unit(c(1, 1, 1, 1), "cm"),
                            text = element_text(size=22),
                            panel.background = element_rect(fill=NA, color="black"),
                            axis.text = element_text(colour="black"),
                            axis.title.x = element_text(vjust = -0.75),
                            axis.title.y = element_text(vjust = 1.5),
                            legend.background = element_rect(size=0.25, color="black"),
                            legend.key = element_blank(),
                            strip.background=element_rect(fill=NA, color=NA),
                            strip.text= element_blank())


