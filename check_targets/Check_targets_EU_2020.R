#setwd("~/disks/y/ontwapps/Timer/Users/Mark/CD_LINKSupdate/6_R")

library(tidyverse)
library(dplyr)
library(data.table)

source('../TIMER_output/functions/Settings.R')
source('../TIMER_output/functions/General Functions.R')
source('../TIMER_output/functions/Import_TIMER_output.R')
source('../TIMER_output/functions/Process_TIMER_output.R')
source('../TIMER_output/functions/pbl_colors.R')
source('Settings_indicators.R')

# First read in No Policy scenario
# Export variable names to Excel (TIMER_implementation.xls) --> data.var_names.csv
# Create in excel list of variables/region combinations for which you want to TIMER output --> data/IndicatorInput.csv
# Create this output based on Import_TIMER_output and Process_TIMER_ output --> data/IndicatorOutput.csv
# This csv file can be imported in Excel

Rundir=paste("C:/Users/markr/OneDrive/IMAGE", sep="")
#Rundir=paste("C:/Users/Roelf003/OneDrive/IMAGE", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = "TIMER_2015"
RDir = "6_R"

# Read no policy scenario
NoPolicy   <- ImportTimerScenario('NoPolicy_2020','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
NoPolicyi   <- ProcessTimerScenario(NoPolicy, Rundir, Project, RDir, Policy=TRUE)

# Make file with variable names from list
var_names_bl <- as.data.frame(names(NoPolicy))
colnames(var_names_bl) <- c("variable")
var_names_ind <- as.data.frame(names(NoPolicyi))
colnames(var_names_ind) <- c("variable")
var_names <- rbind(var_names_ind,var_names_bl)
colnames(var_names) <- c("variable")
write.table(var_names, "data/var_names_NoPolicy_2020.csv", sep=";", row.names=FALSE)

# Read in list indicators for which output needs to be provided (from TIMER_implementation.xlsx) for check_targegs
Read_CD_LINKS_EU <- read.csv(file="data/IndicatorInput_EU_2020.csv", header=TRUE, sep=";") #or sep=","
Read_CD_LINKS_EU <- filter(Read_CD_LINKS_EU, R_variable!="")
Read_CD_LINKS_EU <- filter(Read_CD_LINKS_EU, Region %in% regions28_EU)
Read_CD_LINKS_EU$Region <- factor(Read_CD_LINKS_EU$Region, regions28_EU)

#NoPoliy
Check_targets_NoPolicy_EU <- NULL
for (i in 1:nrow(Read_CD_LINKS_EU))
  #for (i in 20:20)
{ 
  print(paste(Read_CD_LINKS_EU[i,2], "-",Read_CD_LINKS_EU[i,3]))
  if (as.character(Read_CD_LINKS_EU[i,3]) %in% names(NoPolicyi))
  {  tmp_NoPolicy_EU <- filter(NoPolicyi[[as.character(Read_CD_LINKS_EU[i,3])]], region %in% c('WEU', 'CEU', 'EU'), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS_transport[i,3]) %in% names(NoPolicy))
  { tmp_NoPolicy_EU <- filter(NoPolicy[[as.character(Read_CD_LINKS_EU[i,3])]], region %in% c('WEU', 'CEU', 'EU'), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_NoPolicy_EU <- mutate(tmp_NoPolicy_EU, R_Variable = Read_CD_LINKS_EU[i,3])
  tmp_NoPolicy_EU <- mutate(tmp_NoPolicy_EU, Policy_ID = Read_CD_LINKS_EU[i,1])
  tmp_NoPolicy_EU <- spread(tmp_NoPolicy_EU, key=year, value=value)
  tmp_NoPolicy_EU=data.table(tmp_NoPolicy_EU)
  Check_targets_NoPolicy_EU <- rbind(Check_targets_NoPolicy_EU, tmp_NoPolicy_EU)
  
}
Check_targets_NoPolicy_EU <- select(Check_targets_NoPolicy_EU, Policy_ID, R_Variable, region, unit, everything())
write.table(Check_targets_NoPolicy_EU , "data/IndicatorOutput_NoPolicy_EU_2020.csv", sep=";", row.names=FALSE)

NPi <- ImportTimerScenario('NPi_update_2020','NPi_update', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
NPii <- ProcessTimerScenario(NPi, Rundir, Project, RDir, Policy=TRUE)

Check_targets_EU <- NULL
for (i in 1:nrow(Read_CD_LINKS_EU))
  #for (i in 20:20)
{ print(paste(Read_CD_LINKS_EU[i,2], "-",Read_CD_LINKS_EU[i,3]))
  if (as.character(Read_CD_LINKS_EU[i,3]) %in% names(NPii))
  {  tmp_EU<- filter(NPii[[as.character(Read_CD_LINKS_EU[i,3])]], region %in% c('WEU', 'CEU', 'EU'), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS[i,3]) %in% names(NPi))
  { tmp_EU <- filter(NPi[[as.character(Read_CD_LINKS_EU[i,3])]], region %in% c('WEU', 'CEU', 'EU'), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_EU <- mutate(tmp_EU, R_Variable = Read_CD_LINKS_EU[i,3])
  tmp_EU <- mutate(tmp_EU, Policy_ID = Read_CD_LINKS_EU[i,1])
  tmp_EU <- spread(tmp_EU, key=year, value=value)
  tmp_EU=data.table(tmp_EU)
  Check_targets_EU <- rbind(Check_targets_EU, tmp_EU)
  
}
Check_targets_EU <- select(Check_targets_EU, Policy_ID, R_Variable, region, unit, everything())
write.table(Check_targets_EU, "data/IndicatorOutput_NPi_EU_2020.csv", sep=";", row.names=FALSE)

