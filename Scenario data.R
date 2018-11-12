library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)

setwd("C:/Users/mrroelfs/OneDrive/CD_LINKSupdate/6_R/CD_LINKSupdate")

# Source scripts (after setting working directory)
source('../TIMER_output/functions/Settings.R')
source('../TIMER_output/functions/General Functions.R')
source('../TIMER_output/functions/Import_TIMER_output.R')
source('../TIMER_output/functions/Process_TIMER_output.R')

# data from IMAGE
Rundir=paste("C:/Users/mrroelfs/OneDrive", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = 'TIMER_2015'

# Read no policy scenario
NoPolicy_update <- ImportTimerScenario('NoPolicy_update','NoPolicy_update', Rundir, Project, TIMERGeneration)
NoPolicy_update_ind <- ProcessTimerScenario(NoPolicy_update, Rundir, Project)
NPi_update <- ImportTimerScenario('NPi_update','NPi_update', Rundir, Project, TIMERGeneration)
NPi_update_ind <- ProcessTimerScenario(NPi_update, Rundir, Project)

write.table(NPi_update_ind$EMISCO2EQ_indicator , "data/EMISCO2EQ.csv", sep=";", row.names=FALSE)
write.table(NPi_update_ind$EMISCO2EQ_LU_indicator , "data/EMISCO2EQ_LULUCF.csv", sep=";", row.names=FALSE)
write.table(NPi_update_ind$LUEMCO2_TOT_indicator , "data/EMISCO2_LULUCF.csv", sep=";", row.names=FALSE)

# plot for one region, two scenario
ggplot() + geom_line(data=filter(NoPolicy_update_ind$EMISCO2EQ_indicator, year>=2010, year<=2030), aes(year, value, colour="NoPolicy_2018") ) + 
  geom_line(data=filter(NPi_update_ind$EMISCO2EQ_indicator, year>=2010, year<=2030), aes(year, value, colour="NPi_2018")) + 
  + facet_grid(region~.)
  scale_colour_manual(name="Scenario",values=c(NoPolicy_2018="blue", NPi_2018="green")) +
  ylim(0, NA) +
  labs(y="Total GHG Emissions transport, incl LULUCF (MtCO2eq)", x = "Year")

ggplot() + geom_line(data=filter(NPii$EMIS_ETS, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_2017") ) + 
  geom_line(data=filter(NPi_update_i$EMIS_ETS, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_update")) + 
  scale_colour_manual(name="Scenario",values=c(NPi_2017="blue", NPi_update="green")) +
  ylim(0, NA) +
  labs(y="Emissions energy/industry (MtCO2eq)", x = "Year")

ggplot() + geom_line(data=filter(NPii$EMIS_transport, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_2017") ) + 
  geom_line(data=filter(NPi_update_i$EMIS_transport, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_update")) + 
  scale_colour_manual(name="Scenario",values=c(NPi_2017="blue", NPi_update="green")) +
  ylim(0, NA) +
  labs(y="Emissions transport (MtCO2eq)", x = "Year")