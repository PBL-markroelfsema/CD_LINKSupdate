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

StartYear = 2005
EndYear = 2030

# Make file with variable names from list
var_names_bl <- as.data.frame(names(NoPolicy))
colnames(var_names_bl) <- c("variable")
var_names_ind <- as.data.frame(names(NoPolicyi))
colnames(var_names_ind) <- c("variable")
var_names <- rbind(var_names_ind,var_names_bl)
colnames(var_names) <- c("variable")
write.table(var_names, "data/var_names_NoPolicy_2020.csv", sep=";", row.names=FALSE)

# Read in list indicators for which output needs to be provided (from TIMER_implementation.xlsx) for check_targegs
Read_CD_LINKS_NoPolicy <- read.csv(file="data/IndicatorInput_Transport.csv", header=TRUE, sep=";") #or sep=";"
Read_CD_LINKS_NoPolicy <- filter(Read_CD_LINKS_NoPolicy, R_variable!="")
Read_CD_LINKS_NoPolicy <- filter(Read_CD_LINKS_NoPolicy, Region %in% regions28_EU)
Read_CD_LINKS_NoPolicy$Region <- factor(Read_CD_LINKS_NoPolicy$Region, regions28_EU)

Check_targets_NoPolicy <- NULL
for (i in 1:nrow(Read_CD_LINKS_NoPolicy))
#for (i in 1:5)
{ print(paste(Read_CD_LINKS_NoPolicy[i,2], "-", Read_CD_LINKS_NoPolicy[i,3]))
  if (as.character(Read_CD_LINKS_NoPolicy[i,3]) %in% names(NoPolicyi))
  {  tmp_NoPolicy <- filter(NoPolicyi[[as.character(Read_CD_LINKS_NoPolicy[i,3])]], region==as.character(Read_CD_LINKS_NoPolicy[i,2]), year >= StartYear, year <= EndYear) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS_transport[i,3]) %in% names(NoPolicy))
  { tmp_NoPolicy <- filter(NoPolicy[[as.character(Read_CD_LINKS_NoPolicy[i,3])]], region==as.character(Read_CD_LINKS_NoPolicy[i,2]), year >= StartYear, year <= EndYear) %>% select(year, region, value, unit)
  }
  tmp_NoPolicy <- mutate(tmp_NoPolicy, R_Variable = Read_CD_LINKS_NoPolicy[i,3])
  tmp_NoPolicy <- mutate(tmp_NoPolicy, Policy_ID = Read_CD_LINKS_NoPolicy[i,1])
  tmp_NoPolicy <- spread(tmp_NoPolicy, key=year, value=value)
  Check_targets_NoPolicy <- rbind(Check_targets_NoPolicy, tmp_NoPolicy)
  
}
Check_targets_NoPolicy <- select(Check_targets_NoPolicy, Policy_ID, R_Variable, region, unit, everything())
write.table(Check_targets_NoPolicy , "data/IndicatorOutput_Transport_NoPolicy.csv", sep=";", row.names=FALSE)

# Read current national policies scenario
NPi <- ImportTimerScenario('NPi_update_2020','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
NPii <- ProcessTimerScenario(NPi, Rundir, Project, RDir, Policy=TRUE)

# Make file with variable names from list
var_names_bl <- as.data.frame(names(NPi))
colnames(var_names_bl) <- c("variable")
var_names_ind <- as.data.frame(names(NPii))
colnames(var_names_ind) <- c("variable")
var_names <- rbind(var_names_ind,var_names_bl)
colnames(var_names) <- c("variable")
write.table(var_names, "data/var_names_NPi_2020.csv", sep=";", row.names=FALSE)

# Read in list indicators for which output needs to be provided (from TIMER_implementation.xlsx) for check_targegs
Read_CD_LINKS_transport <- read.csv(file="data/IndicatorInput_transport.csv", header=TRUE, sep=";") #or sep=";"
Read_CD_LINKS_transport <- filter(Read_CD_LINKS_transport, R_variable!="")
Read_CD_LINKS_transport <- filter(Read_CD_LINKS_transport, Region %in% regions28_EU)
Read_CD_LINKS_transport$Region <- factor(Read_CD_LINKS_transport$Region, regions28_EU)

Check_targets_NPi <- NULL
for (i in 1:nrow(Read_CD_LINKS_transport))
#for (i in 14:15)
#i=15
{ print(paste(Read_CD_LINKS_transport[i,2], "-",Read_CD_LINKS_transport[i,3]))
  if (as.character(Read_CD_LINKS_transport[i,3]) %in% names(NPii))
  {  tmp_NPi <- filter(NPii[[as.character(Read_CD_LINKS_transport[i,3])]], region==as.character(Read_CD_LINKS_transport[i,2]), year >= StartYear, year <= EndYear) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS_transport[i,3]) %in% names(NPi_transport))
  { tmp_NPi <- filter(NPi[[as.character(Read_CD_LINKS_transport[i,3])]], region==as.character(Read_CD_LINKS_transport[i,2]), year >= StartYear, year <= EndYear) %>% select(year, region, value, unit)
  }
  tmp_NPi <- mutate(tmp_NPi, R_Variable = Read_CD_LINKS_transport[i,3])
  tmp_NPi <- mutate(tmp_NPi, Policy_ID = Read_CD_LINKS_transport[i,1])
  tmp_NPi <- spread(tmp_NPi, key=year, value=value)
  tmp_NPi=data.table(tmp_NPi)
  Check_targets_NPi <- rbind(Check_targets_NPi, tmp_NPi)
}
Check_targets_NPi <- select(Check_targets_NPi, Policy_ID, R_Variable, region, unit, everything())
write.table(Check_targets_NPi , "data/IndicatorOutput_transport_NPi.csv", sep=";", row.names=FALSE)
