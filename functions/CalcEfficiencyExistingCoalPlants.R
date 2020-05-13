library(tidyverse)
library(dplyr)
library(data.table)
coal <- c('Conv. Coal', 'IGCC', 'Coal + CS')
eff<-NoPolicy$ElecEffPct
elecprod <- NoPolicy$ElecProdSpec %>% rename(energy_technology=energy_carrier)
elecprod$energy_technology <- factor(elecprod$energy_technology, levels=energy_technology_28)
                   
eff_coal <- inner_join(eff, elecprod, by=c('year', 'region', 'energy_technology')) %>%
            group_by(year, region, energy_technology) %>%
            mutate(n=ifelse(energy_technology %in% coal, value.x*value.y,0)) %>%
            mutate(m=ifelse(energy_technology %in% coal, value.y,0)) %>%
            group_by(year, region) %>%
            summarise(value=sum(n)/sum(m))
eff_coal$value <- format(eff_coal$value, scientific=FALSE)