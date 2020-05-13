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
CurPol_withFlag_noTarget   <- ImportTimerScenario('NPi_update_2019_withFlag_noTarget','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
CurPol_withFlag_noTargeti   <- ProcessTimerScenario(CurPol_withFlag_noTarget, Rundir, Project, RDir, Policy=TRUE)
CurPol_noFlag_noTarget   <- ImportTimerScenario('NPi_update_2019_noFlag_noTarget','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
CurPol_noFlag_noTargeti   <- ProcessTimerScenario(CurPol_noFlag_noTarget, Rundir, Project, RDir, Policy=TRUE)
CurPol_withFlag_withTarget   <- ImportTimerScenario('NPi_update_2019_withFlag_withTarget','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
CurPol_withFlag_withTargeti   <- ProcessTimerScenario(CurPol_withFlag_withTarget, Rundir, Project, RDir, Policy=TRUE)
CurPol_noFlag_withTarget   <- ImportTimerScenario('NPi_update_2019_noFlag_withTarget','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
CurPol_noFlag_withTargeti   <- ProcessTimerScenario(CurPol_noFlag_withTarget, Rundir, Project, RDir, Policy=TRUE)

r <- "World"
np <- filter(NoPolicyi$EMISCO2EQ, region==r, GHG_Category=="EMISCO2EQ", main_sector=="Total") %>% mutate(scenario="NoPolicy")
cp_wFnT <- filter(CurPol_withFlag_noTargeti$EMISCO2EQ, region==r, GHG_Category=="EMISCO2EQ", main_sector=="Total") %>% mutate(scenario="CurPol with flag, no target (used)")
cp_nFnT<- filter(CurPol_noFlag_noTargeti$EMISCO2EQ, region==r, GHG_Category=="EMISCO2EQ", main_sector=="Total") %>% mutate(scenario="CurPol no flag, no target")
cp_nFwT<- filter(CurPol_noFlag_withTargeti$EMISCO2EQ, region==r, GHG_Category=="EMISCO2EQ", main_sector=="Total") %>% mutate(scenario="CurPol no flag, with target")
cp_wFwT<- filter(CurPol_withFlag_withTargeti$EMISCO2EQ, region==r, GHG_Category=="EMISCO2EQ", main_sector=="Total") %>% mutate(scenario="CurPol with flag, with target")
d <- rbind(np, cp_wFnT) %>% rbind(cp_nFnT) %>% rbind(cp_wFwT) %>% rbind(cp_wFwT) %>% rbind(cp_nFwT)
scens <- c('NoPolicy', 'CurPol with flag, no target (used)', 'CurPol no flag, no target', 'CurPol with flag, with target', 'CurPol no flag, with target')
d$scenario <- factor(d$scenario, levels=scens)
g<- ggplot(data=filter(d, year>=2010, year<=2050)) + 
        geom_line(aes(x=year, y=value, colour=scenario)) +
        geom_jitter(aes(x=year, y=value, colour=scenario)) +
        theme_bw()
plot(g)
