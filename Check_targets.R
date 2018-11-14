library(dplyr)
library(tidyverse)

setwd("~/disks/y/ontwapps/Timer/Users/Mark/CD_LINKSupdate/6_R/CD_LINKSupdate")
source("../TIMER_output/functions/Settings.R")
source("../TIMER_output/functions/General Functions.R")
source("../TIMER_output/functions/Import_TIMER_output.R")
source("../TIMER_output/functions/Process_TIMER_output.R")
source("../TIMER_output/functions/pbl_colors.R")
source("Settings_indicators.R")

Rundir=paste("~/disks/y/ontwapps/Timer/Users/Mark", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = "TIMER_2015"

# First read in No Policy scenario
# Export variable names to Excel (TIMER_implementation.xls) --> data.var_names.csv
# Create in excel list of variables/region combinations for which you want to TIMER output --> data/IndicatorInput.csv
# Create this output based on Import_TIMER_output and Process_TIMER_ output --> data/IndicatorOutput.csv
# This csv file can be imported in Excel
NoPolicy <- ImportTimerScenario('NoPolicy','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
NoPolicy_i <- ProcessTimerScenario(NoPolicy, Rundir, Project, Policy=TRUE)
NPi <- ImportTimerScenario('NPi','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPii <- ProcessTimerScenario(NPi, Rundir, Project, Policy=TRUE)


# Read no policy scenario
NoPolicy_update <- ImportTimerScenario('NoPolicy_update','NoPolicy_update', Rundir, Project, TIMERGeneration, Policy=TRUE)
NoPolicy_update_i <- ProcessTimerScenario(NoPolicy_update, Rundir, Project, Policy=TRUE)

# Make file with variable names from list
var_names_bl <- as.data.frame(names(NoPolicy_update))
colnames(var_names_bl) <- c("variable")
var_names_ind <- as.data.frame(names(NoPolicy_update_i))
colnames(var_names_ind) <- c("variable")
var_names <- rbind(var_names_ind,var_names_bl)
colnames(var_names) <- c("variable")
write.table(var_names, "data/var_names.csv", sep=";", row.names=FALSE)

# Read in list indicators for which output needs to be provided (from TIMER_implementation.xlsx) for check_targegs
Read_CD_LINKS <- read.csv(file="data/IndicatorInput.csv", header=TRUE, sep=";") #or sep=","
Read_CD_LINKS <- filter(Read_CD_LINKS, R_variable!="")
Read_CD_LINKS <- filter(Read_CD_LINKS, Region %in% regions28_EU)
Read_CD_LINKS$Region <- factor(Read_CD_LINKS$Region, regions28_EU)

#NoPoliy
Check_targets_NoPolicy_update <- NULL
for (i in 1:nrow(Read_CD_LINKS))
  #for (i in 20:20)
{ print(paste(Read_CD_LINKS[i,2], "-",Read_CD_LINKS[i,3]))
  if (as.character(Read_CD_LINKS[i,3]) %in% names(NoPolicy_update_i))
  {  tmp_NoPolicy_update <- filter(NoPolicy_update_i[[as.character(Read_CD_LINKS[i,3])]], region==as.character(Read_CD_LINKS[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS_transport[i,3]) %in% names(NoPolicy_update))
  { tmp_NoPolicy_update <- filter(NoPolicy_update[[as.character(Read_CD_LINKS[i,3])]], region==as.character(Read_CD_LINKS[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_NoPolicy_update <- mutate(tmp_NoPolicy_update, R_Variable = Read_CD_LINKS[i,3])
  tmp_NoPolicy_update <- mutate(tmp_NoPolicy_update, Policy_ID = Read_CD_LINKS[i,1])
  tmp_NoPolicy_update <- spread(tmp_NoPolicy_update, key=year, value=value)
  tmp_NoPolicy_update=data.table(tmp_NoPolicy_update)
  Check_targets_NoPolicy_update <- rbind(Check_targets_NoPolicy_update, tmp_NoPolicy_update)
  
}
Check_targets_NoPolicy_update <- select(Check_targets_NoPolicy_update, Policy_ID, R_Variable, region, unit, everything())
write.table(Check_targets_NoPolicy_update , "data/IndicatorOutput_NoPolicy_update.csv", sep=";", row.names=FALSE)

#NPi_update
# Read current national policies scenario
NPi_update <- ImportTimerScenario('NPi_update','NPi_update', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi_update_i <- ProcessTimerScenario(NPi_update, Rundir, Project, Policy=TRUE)

Check_targets_NPi_update <- NULL
for (i in 1:nrow(Read_CD_LINKS))
#for (i in 20:20)
{ print(paste(Read_CD_LINKS[i,2], "-",Read_CD_LINKS[i,3]))
  if (as.character(Read_CD_LINKS[i,3]) %in% names(NPi_update_i))
  {  tmp_NPi_update <- filter(NPi_update_i[[as.character(Read_CD_LINKS[i,3])]], region==as.character(Read_CD_LINKS[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS[i,3]) %in% names(NPi_update))
  { tmp_NPi_update <- filter(NPi_update[[as.character(Read_CD_LINKS[i,3])]], region==as.character(Read_CD_LINKS[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_NPi_update <- mutate(tmp_NPi_update, R_Variable = Read_CD_LINKS[i,3])
  tmp_NPi_update <- mutate(tmp_NPi_update, Policy_ID = Read_CD_LINKS[i,1])
  tmp_NPi_update <- spread(tmp_NPi_update, key=year, value=value)
  tmp_NPi_update=data.table(tmp_NPi_update)
  Check_targets_NPi_update <- rbind(Check_targets_NPi_update, tmp_NPi_update)

}
Check_targets_NPi_update <- select(Check_targets_NPi_update, Policy_ID, R_Variable, region, unit, everything())

write.table(Check_targets_NPi_update , "data/IndicatorOutput_NPi_update.csv", sep=";", row.names=FALSE)
