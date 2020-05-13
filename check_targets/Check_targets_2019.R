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

# Run scenarios at least until 2050

# First read in No Policy scenario
# Export variable names to Excel (TIMER_implementation.xls) --> data.var_names.csv
# Create in excel list of variables/region combinations for which you want to TIMER output --> data/IndicatorInput.csv
# Create this output based on Import_TIMER_output and Process_TIMER_ output --> data/IndicatorOutput.csv
# This csv file can be imported in Excel
NoPolicy_2019 <- ImportTimerScenario('NoPolicy_2019','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
NoPolicy_2019_i <- ProcessTimerScenario(NoPolicy_2019, Rundir, Project, Policy=TRUE)

# Make file with variable names from list
var_names_bl <- as.data.frame(names(NoPolicy_2019))
colnames(var_names_bl) <- c("variable")
var_names_ind <- as.data.frame(names(NoPolicy_2019_i))
colnames(var_names_ind) <- c("variable")
var_names <- rbind(var_names_ind,var_names_bl)
colnames(var_names) <- c("variable")
write.table(var_names, "data/check_targets/var_names.csv", sep=";", row.names=FALSE)

# Read in list indicators for which output needs to be provided (from TIMER_implementation.xlsx) for check_targegs
Read_CD_LINKS_2019 <- read.csv(file="data/check_targets/IndicatorInput_2019.csv", header=TRUE, sep=";") #or sep=","
Read_CD_LINKS_2019 <- filter(Read_CD_LINKS_2019, R_variable!="")
Read_CD_LINKS_2019 <- filter(Read_CD_LINKS_2019, Region %in% regions28_EU)
Read_CD_LINKS_2019$Region <- factor(Read_CD_LINKS_2019$Region, regions28_EU)

#NoPoliy
Check_targets_NoPolicy_2019 <- NULL
for (i in 1:nrow(Read_CD_LINKS_2019)) #for (i in 20:20)
{ print(paste(Read_CD_LINKS_2019[i,2], "-",Read_CD_LINKS_2019[i,3]))
  if (as.character(Read_CD_LINKS_2019[i,3]) %in% names(NoPolicy_2019_i))
  {  tmp_NoPolicy_2019 <- filter(NoPolicy_2019_i[[as.character(Read_CD_LINKS_2019[i,3])]], region==as.character(Read_CD_LINKS_2019[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  else # as.character(Read_CD_LINKS_2019_transport[i,3]) %in% names(NoPolicy))
  { tmp_NoPolicy_2019 <- filter(NoPolicy_2019[[as.character(Read_CD_LINKS_2019[i,3])]], region==as.character(Read_CD_LINKS_2019[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_NoPolicy_2019 <- mutate(tmp_NoPolicy_2019, R_Variable = Read_CD_LINKS_2019[i,3])
  tmp_NoPolicy_2019 <- mutate(tmp_NoPolicy_2019, Policy_ID = Read_CD_LINKS_2019[i,1])
  tmp_NoPolicy_2019 <- spread(tmp_NoPolicy_2019, key=year, value=value)
  tmp_NoPolicy_2019=data.table(tmp_NoPolicy_2019)
  Check_targets_NoPolicy_2019 <- rbind(Check_targets_NoPolicy_2019, tmp_NoPolicy_2019)
  
}
Check_targets_NoPolicy_2019 <- select(Check_targets_NoPolicy_2019, Policy_ID, R_Variable, region, unit, everything())
write.table(Check_targets_NoPolicy_2019, "data/check_targets/IndicatorOutput_NoPolicy_2019.csv", sep=";", row.names=FALSE)


#NPi
NPi_2019 <- ImportTimerScenario('NPi_update_2019','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi_2019_i <- ProcessTimerScenario(NPi_2019, Rundir, Project, Policy=TRUE)

Check_targets_NPi_2019 <- NULL
for (i in 1:nrow(Read_CD_LINKS_2019))
#for (i in 20:20)
{ 
  #i=8
  print(paste(Read_CD_LINKS_2019[i,2], "-",Read_CD_LINKS_2019[i,3]))
  if (as.character(Read_CD_LINKS_2019[i,3]) %in% names(NPi_2019_i))
  {  tmp_NPi_2019 <- filter(NPi_2019_i[[as.character(Read_CD_LINKS_2019[i,3])]], region==as.character(Read_CD_LINKS_2019[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  } else # as.character(Read_CD_LINKS_2019[i,3]) %in% names(NPi))
  { tmp_NPi_2019 <- filter(NPi_2019[[as.character(Read_CD_LINKS_2019[i,3])]], region==as.character(Read_CD_LINKS_2019[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  }
  tmp_NPi_2019 <- mutate(tmp_NPi_2019, R_Variable = Read_CD_LINKS_2019[i,3])
  tmp_NPi_2019 <- mutate(tmp_NPi_2019, Policy_ID = Read_CD_LINKS_2019[i,1])
  tmp_NPi_2019 <- spread(tmp_NPi_2019, key=year, value=value)
  tmp_NPi_2019=data.table(tmp_NPi_2019)
  Check_targets_NPi_2019 <- rbind(Check_targets_NPi_2019, tmp_NPi_2019)

}
Check_targets_NPi_2019 <- select(Check_targets_NPi_2019, Policy_ID, R_Variable, region, unit, everything())

write.table(Check_targets_NPi_2019, "data/check_targets/IndicatorOutput_NPi_2019.csv", sep=";", row.names=FALSE)
