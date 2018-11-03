setwd("~/disks/y/ontwapps/Timer/Users/Mark/CD_LINKSupdate/6_R")
Rundir=paste("~/disks/y/ontwapps/Timer/Users/Mark", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = 'TIMER_2015'

library(dplyr)
library(tidyverse)

source('TIMER_output/functions/Settings.R')
source('TIMER_output/functions/General Functions.R')
source('TIMER_output/functions/Import_TIMER_output.R')
source('TIMER_output/functions/Process_TIMER_output.R')
source('TIMER_output/functions/pbl_colors.R')
source('Settings_indicators.R')

# Read NPi scenario from previous round
NPi_old <- ImportTimerScenario('NPi','NPi', Rundir, Project, TIMERGeneration)
NPii_old <- ProcessTimerScenario(NPi_old, Rundir, Project)
Check_targets_NPi_old <- NULL
for (i in 1:nrow(Read_CD_LINKS))
  #for (i in 20:20)
{ print(paste(Read_CD_LINKS[i,2], "-",Read_CD_LINKS[i,3]))
  if (as.character(Read_CD_LINKS[i,3]) %in% names(NPii_old))
  {  tmp_NPi_old <- filter(NPii_old[[as.character(Read_CD_LINKS[i,3])]], region==as.character(Read_CD_LINKS[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS[i,3]) %in% names(NPi))
  { tmp_NPi_old <- filter(NPi_old[[as.character(Read_CD_LINKS[i,3])]], region==as.character(Read_CD_LINKS[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_NPi_old <- mutate(tmp_NPi_old, R_Variable = Read_CD_LINKS[i,3])
  tmp_NPi_old <- mutate(tmp_NPi_old, Policy_ID = Read_CD_LINKS[i,1])
  tmp_NPi_old <- spread(tmp_NPi_old, key=year, value=value)
  tmp_NPi_old <- data.table(tmp_NPi_old)
  Check_targets_NPi_old <- rbind(Check_targets_NPi_old, tmp_NPi_old)
  
}
Check_targets_NPi_old <- select(Check_targets_NPi_old, Policy_ID, R_Variable, region, unit, everything())

write.table(Check_targets_NPi_old , "data/IndicatorOutput_NPi_old.csv", sep=";", row.names=FALSE)