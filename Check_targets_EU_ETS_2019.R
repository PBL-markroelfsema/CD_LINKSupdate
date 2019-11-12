library(dplyr)
library(tidyverse)

setwd("~/disks/y/Kennisbasis/IMAGE/model/users/mark/timer/CD_LINKSupdate/6_R/CD_LINKSupdate")
source("../TIMER_output/functions/Settings.R")
source("../TIMER_output/functions/General Functions.R")
source("../TIMER_output/functions/Import_TIMER_output.R")
source("../TIMER_output/functions/Process_TIMER_output.R")
source("../TIMER_output/functions/pbl_colors.R")
source("Settings_indicators.R")

Rundir=paste("~/disks/y/Kennisbasis/IMAGE/model/users/mark/timer", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = "TIMER_2015"

# Read in list indicators for which output needs to be provided (from TIMER_implementation.xlsx) for check_targegs
Read_CD_LINKS_EU_ETS <- read.csv(file="data/IndicatorInput_EU_ETS.csv", header=TRUE, sep=";") #or sep=","
Read_CD_LINKS_EU_ETS <- filter(Read_CD_LINKS_EU_ETS, R_variable!="")
Read_CD_LINKS_EU_ETS <- filter(Read_CD_LINKS_EU_ETS, Region %in% regions28_EU)
Read_CD_LINKS_EU_ETS$Region <- factor(Read_CD_LINKS_EU_ETS$Region, regions28_EU)

#NoPoliy
Check_targets_NoPolicy_EU_ETS <- NULL
for (i in 1:nrow(Read_CD_LINKS_EU_ETS))
  #for (i in 20:20)
{ 
  print(paste(Read_CD_LINKS_EU_ETS[i,2], "-",Read_CD_LINKS_EU_ETS[i,3]))
  if (as.character(Read_CD_LINKS_EU_ETS[i,3]) %in% names(NoPolicy_2019_i))
  {  tmp_NoPolicy_EU_ETS <- filter(NoPolicy_2019_i[[as.character(Read_CD_LINKS_EU_ETS[i,3])]], region %in% c('WEU', 'CEU', 'EU'), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS_transport[i,3]) %in% names(NoPolicy))
  { tmp_NoPolicy_EU_ETS <- filter(NoPolicy_2019[[as.character(Read_CD_LINKS_EU_ETS[i,3])]], region %in% c('WEU', 'CEU', 'EU'), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_NoPolicy_EU_ETS <- mutate(tmp_NoPolicy_EU_ETS, R_Variable = Read_CD_LINKS_EU_ETS[i,3])
  tmp_NoPolicy_EU_ETS <- mutate(tmp_NoPolicy_EU_ETS, Policy_ID = Read_CD_LINKS_EU_ETS[i,1])
  tmp_NoPolicy_EU_ETS <- spread(tmp_NoPolicy_EU_ETS, key=year, value=value)
  tmp_NoPolicy_EU_ETS=data.table(tmp_NoPolicy_EU_ETS)
  Check_targets_NoPolicy_EU_ETS <- rbind(Check_targets_NoPolicy_EU_ETS, tmp_NoPolicy_EU_ETS)
  
}
Check_targets_NoPolicy_EU_ETS <- select(Check_targets_NoPolicy_EU_ETS, Policy_ID, R_Variable, region, unit, everything())
write.table(Check_targets_NoPolicy_EU_ETS , "data/check_targets/IndicatorOutput_NoPolicy_ETS.csv", sep=";", row.names=FALSE)

#EU_ETS
# Read current national policies scenario
#EU_ETS <- ImportTimerScenario('EU_ETS','NPi_update', Rundir, Project, TIMERGeneration)
#EU_ETSi <- ProcessTimerScenario(EU_ETS, Rundir, Project)

Check_targets_EU_ETS <- NULL
for (i in 1:nrow(Read_CD_LINKS_EU_ETS))
  #for (i in 20:20)
{ print(paste(Read_CD_LINKS_EU_ETS[i,2], "-",Read_CD_LINKS_EU_ETS[i,3]))
  if (as.character(Read_CD_LINKS_EU_ETS[i,3]) %in% names(NPi_2019_i))
  {  tmp_EU_ETS<- filter(NPi_2019_i[[as.character(Read_CD_LINKS_EU_ETS[i,3])]], region %in% c('WEU', 'CEU', 'EU'), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS[i,3]) %in% names(NPi))
  { tmp_EU_ETS <- filter(NPi_2019[[as.character(Read_CD_LINKS_EU_ETS[i,3])]], region %in% c('WEU', 'CEU', 'EU'), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_EU_ETS <- mutate(tmp_EU_ETS, R_Variable = Read_CD_LINKS_EU_ETS[i,3])
  tmp_EU_ETS <- mutate(tmp_EU_ETS, Policy_ID = Read_CD_LINKS_EU_ETS[i,1])
  tmp_EU_ETS <- spread(tmp_EU_ETS, key=year, value=value)
  tmp_EU_ETS=data.table(tmp_EU_ETS)
  Check_targets_EU_ETS <- rbind(Check_targets_EU_ETS, tmp_EU_ETS)
  
}
Check_targets_EU_ETS <- select(Check_targets_EU_ETS, Policy_ID, R_Variable, region, unit, everything())

write.table(Check_targets_EU_ETS, "data/check_targets/IndicatorOutput_EU_ETS.csv", sep=";", row.names=FALSE)

