# 1. Check fuel efficiency cars and heavy trucks

library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)

#setwd("C:/Users/markr/OneDrive/IMAGE")
source("../TIMER_output/functions/Settings.R")
source("../TIMER_output/functions/General Functions.R")
source("../TIMER_output/functions/Import_TIMER_output.R")
source("../TIMER_output/functions/Process_TIMER_output.R")
source("../TIMER_output/functions/pbl_colors.R")
source("Settings_indicators.R")

Rundir=paste("C:/Users/markr/OneDrive/IMAGE", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = "TIMER_2015"
RDir = "6_R"

NoPolicy_2019 <- ImportTimerScenario('NoPolicy_2019','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
NPi_2019 <- ImportTimerScenario('NPi_update_2019','NPi', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)

country="CHN"
# ----------- 1a. cars
# old is based on old calculations for efficiency existing fleet, which was based on efficiency new cars (but share existing cars)
eff_cars_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="existing")
eff_cars_NPi <- filter(NPi_2019$EfficiencyFleet_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="existing")
eff_new_cars_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_new_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="new")
eff_new_cars_NPi <- filter(NPi_2019$EfficiencyFleet_new_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="new")

eff_cars_old_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_cars_old, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="existing")
eff_cars_old_NPi <- filter(NPi_2019$EfficiencyFleet_cars_old, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="existing")

eff_cars <- rbind(eff_new_cars_NoPol, eff_cars_NoPol) %>% rbind(eff_new_cars_NPi) %>% rbind(eff_cars_NPi)
eff_cars_old <- rbind(eff_cars_old_NoPol, eff_cars_old_NPi)
# compare efficiency existing and new cars (new indicator for existing cars) in NoPolicy and NPi scenario
# line also shows old method for existing cars
g<-ggplot(data=eff_cars) + geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(data=eff_cars_old, aes(x=year, y=value, colour=scenario)) +
  ggtitle(paste0("Cars ", country))+ 
  theme_bw() +
  ylim(0, NA)
f = paste0("graphs/fuel efficiency/fuel efficiency cars_", country, ".jpg")
ggsave(f,g)
# compare efficiency existing and new cars (new indicator for existing cars) in NoPolicy and NPi scenario
g<-ggplot() + geom_point(data=eff_cars, aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(data=eff_cars, aes(x=year, y=value, colour=scenario, shape=standard)) +
  ggtitle(paste0("Cars ", country)) + 
  theme_bw() +
  ylim(0, NA)
f = paste0("graphs/fuel efficiency/fuel efficiency cars_v2_", country, ".jpg")
ggsave(f,g)

# -------------- 1b. heavy trucks
eff_HvyT_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="existing")
eff_HvyT_NPi <- filter(NPi_2019$EfficiencyFleet_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="existing")

eff_new_HvyT_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_new_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="new")
eff_new_HvyT_NPi <- filter(NPi_2019$EfficiencyFleet_new_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="new")

eff_HvyT_old_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_HvyT_old, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="existing")
eff_HvyT_old_NPi <- filter(NPi_2019$EfficiencyFleet_HvyT_old, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="existing")

eff_HvyT <- rbind(eff_new_HvyT_NoPol, eff_HvyT_NoPol) %>% rbind(eff_new_HvyT_NPi) %>% rbind(eff_HvyT_NPi)
eff_HvyT_old <- rbind(eff_HvyT_old_NoPol, eff_HvyT_old_NPi)
# compare efficiency existing and new heavy trucks (new indicator for existing heavy trucks) in NoPolicy and NPi scenario
# line also shows old method for existing heavy trucks
g<-ggplot() + geom_point(data=eff_HvyT, aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(data=eff_HvyT_old, aes(x=year, y=value, colour=scenario)) +
  ggtitle(paste0("Heavy trucks ", country)) +
  ylim(0, NA) +
  theme_bw()
f = paste0("graphs/fuel efficiency/fuel efficiency heavy trucks_", country, ".jpg")
ggsave(f,g)
# compare efficiency existing and new heavy trucks (new indicator for existing heavy trucks) in NoPolicy and NPi scenario
g<-ggplot() + geom_point(data=eff_HvyT, aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(data=eff_HvyT, aes(x=year, y=value, colour=scenario, shape=standard)) +
  ggtitle(paste0("Heavy trucks ", country)) + 
  theme_bw() +
  ylim(0, NA)
tst1 <- mutate(eff_HvyT_NPi, x="current")
tst2 <- mutate(eff_HvyT_old_NPi, x="old")
tst <- rbind(tst1, tst2) %>% spread(key=x,value=value)
View(tst)
f = paste0("graphs/fuel efficiency/fuel efficiency heavy trucks_v2_", country, ".jpg")
ggsave(f, g)
