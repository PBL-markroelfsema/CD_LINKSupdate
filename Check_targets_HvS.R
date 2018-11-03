library(tidyverse)
source('TIMER_output/functions/Settings.R')
source('TIMER_output/functions/General Functions.R')
source('TIMER_output/functions/Import_TIMER_output.R')
source('TIMER_output/functions/Process_TIMER_output.R')
source('TIMER_output/functions/pbl_colors.R')
source('Settings_indicators.R')

Rundir=paste("~/disks/y/ontwapps/Timer/Users/Mark", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = 'TIMER_2015'

# Read no policy scenario
NoPolicy <- ImportTimerScenario('NoPolicy_update','NoPolicy_update', Rundir, Project, TIMERGeneration)
NoPolicyi <- ProcessTimerScenario(NoPolicy, Rundir, Project)

# Read current national policies scenario
NPi_Ren <- ImportTimerScenario('NPi_update_Renewables','NoPolicy_update', Rundir, Project, TIMERGeneration)
NPii_Ren <- ProcessTimerScenario(NPi_Ren, Rundir, Project)

# Make file with variable names from list
var_names_bl <- as.data.frame(names(NPi_Ren))
colnames(var_names_bl) <- c("variable")
var_names_ind <- as.data.frame(names(NPii_Ren))
colnames(var_names_ind) <- c("variable")
var_names <- rbind(var_names_ind,var_names_bl)
colnames(var_names) <- c("variable")
write.table(var_names, "data/var_names.csv", sep=";", row.names=FALSE)

# FROM THIS POINT CHECK TARGETS
# Read in list indicators for which output needs to be provided (from TIMER_implementation.xlsx) for check_targegs
Read_CD_LINKS <- read.csv(file="data/IndicatorInput_Ren.csv", header=TRUE, sep=",") #or sep=";"
Read_CD_LINKS <- filter(Read_CD_LINKS, R_variable!="")
Read_CD_LINKS <- filter(Read_CD_LINKS, Region %in% regions28_EU)
Read_CD_LINKS$Region <- factor(Read_CD_LINKS$Region, regions28_EU)


Check_targets <- NULL
for (i in 1:nrow(Read_CD_LINKS)) 
{ print(paste(Read_CD_LINKS[i,2], "-",Read_CD_LINKS[i,3]))
  tmp <- filter(NPii_Ren[[as.character(Read_CD_LINKS[i,3])]], region==as.character(Read_CD_LINKS[i,2]), year >= 1990, year <= 2050) %>% select(year, region, value, unit)
  tmp <- mutate(tmp, R_Variable = Read_CD_LINKS[i,3])
  tmp <- mutate(tmp, Policy_ID = Read_CD_LINKS[i,1])
  tmp <- spread(tmp, key=year, value=value)
  tmp=data.table(tmp)
  Check_targets <- rbind(Check_targets, tmp)

}
Check_targets <- select(Check_targets, Policy_ID, R_Variable, region, unit, everything())

#write_csv(Check_targets , "CD_LINKS/data/indcators.csv")
write.table(Check_targets , "data/IndicatorOutput_Ren.csv", sep=";", row.names=FALSE)
