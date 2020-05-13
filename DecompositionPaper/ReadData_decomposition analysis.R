library(data.table)
library(dplyr)
library(tidyverse)

source('../TIMER_output/functions/Settings.R')
source('../TIMER_output/functions/Import_TIMER_output.R')
source('../TIMER_output/functions/Process_TIMER_output.R')
source('decomposition_functions.R')

Rundir=paste0("H:/IMAGE", sep="")
RDir="6_R"
Project=paste("CD_LINKSupdate")
TIMERGeneration = 'TIMER_2015'
TIMER_folder = paste(Rundir, Project, "2_TIMER/outputlib", TIMERGeneration, Project, sep="/")

NoPolicy <- ImportTimerScenario('NoPolicy_2019','NoPolicy_update', Rundir, Project, TIMERGeneration, RDir)
NoPolicyi <- ProcessTimerScenario(NoPolicy, Rundir, Project, RDir)

NPi <- ImportTimerScenario('NPi_update_2019','NPi_update', Rundir, Project, TIMERGeneration, RDir)
NPii <- ProcessTimerScenario(NPi, Rundir, Project, RDir)
