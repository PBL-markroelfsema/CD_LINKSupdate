
# --------- 2 sensitivity runs for fuel efficiency cars and cars
#           2a different paramters (energy tax, factor)
#           2b sensivity fuel efficiency cars and cars from NoPolicy and NPi

library(dplyr)
library(tidyverse)
library(data.table)

#setwd("C:/Users/markr/OneDrive/IMAGE")
source("../TIMER_output/functions/Settings.R")
source("../TIMER_output/functions/General Functions.R")
source("../TIMER_output/functions/Import_TIMER_output.R")
source("../TIMER_output/functions/Process_TIMER_output.R")
source("../TIMER_output/functions/pbl_colors.R")
source("Settings_indicators.R")

Rundir=paste("C:/Users/markr/OneDrive/IMAGE", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = "TIMER_2015"
RDir = "6_R"

NPI_PPS_780 <- ImportTimerScenario('NPi_update_PowerPlantStandard_India_780','NPi_update', Rundir, Project, TIMERGeneration, RDir, Policy=FALSE)
NPI_PPS_800 <- ImportTimerScenario('NPi_update_PowerPlantStandard_India_800','NPi_update', Rundir, Project, TIMERGeneration, RDir, Policy=FALSE)
NPI_PPS_820 <- ImportTimerScenario('NPi_update_PowerPlantStandard_India_820','NPi_update', Rundir, Project, TIMERGeneration, RDir, Policy=FALSE)
NPI_PPS_840 <- ImportTimerScenario('NPi_update_PowerPlantStandard_India_840','NPi_update', Rundir, Project, TIMERGeneration, RDir, Policy=FALSE)
NPI_PPS_860 <- ImportTimerScenario('NPi_update_PowerPlantStandard_India_860','NPi_update', Rundir, Project, TIMERGeneration, RDir, Policy=FALSE)
NPI_PPS_880 <- ImportTimerScenario('NPi_update_PowerPlantStandard_India_880','NPi_update', Rundir, Project, TIMERGeneration, RDir, Policy=FALSE)

NPI_PPS_780_i <- ProcessTimerScenario(NPI_PPS_780, Rundir, Project, RDir, Policy=FALSE)
NPI_PPS_800_i <- ProcessTimerScenario(NPI_PPS_800, Rundir, Project, RDir, Policy=FALSE)
NPI_PPS_820_i <- ProcessTimerScenario(NPI_PPS_820, Rundir, Project, RDir, Policy=FALSE)
NPI_PPS_840_i <- ProcessTimerScenario(NPI_PPS_840, Rundir, Project, RDir, Policy=FALSE)
NPI_PPS_860_i <- ProcessTimerScenario(NPI_PPS_860, Rundir, Project, RDir, Policy=FALSE)
NPI_PPS_880_i <- ProcessTimerScenario(NPI_PPS_880, Rundir, Project, RDir, Policy=FALSE)

NPI_PPS_780_eff <- mutate(NPI_PPS_780_i$ElecEffCoalPct, scenario="efficiency 780")
NPI_PPS_800_eff <- mutate(NPI_PPS_800_i$ElecEffCoalPct, scenario="efficiency 800")
NPI_PPS_820_eff <- mutate(NPI_PPS_820_i$ElecEffCoalPct, scenario="efficiency 820")
NPI_PPS_840_eff <- mutate(NPI_PPS_840_i$ElecEffCoalPct, scenario="efficiency 840")
NPI_PPS_860_eff <- mutate(NPI_PPS_860_i$ElecEffCoalPct, scenario="efficiency 860")
NPI_PPS_880_eff <- mutate(NPI_PPS_880_i$ElecEffCoalPct, scenario="efficiency 880")
NPI_PPS <- rbind(NPI_PPS_780_eff, NPI_PPS_800_eff) %>% 
           rbind(NPI_PPS_820_eff) %>% 
           rbind(NPI_PPS_840_eff) #%>% 
           #rbind(NPI_PPS_860_eff) %>% 
           #rbind(NPI_PPS_880_eff)

ggplot(data=filter(NPI_PPS, region=="INDIA", year>=2015, year<=2050), aes(x=year, y=value, colour=scenario)) + geom_line() + theme_bw()
       
