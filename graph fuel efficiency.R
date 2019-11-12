# 1. Check fuel efficiency cars and heavy trucks

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
NPi_2019 <- ImportTimerScenario('NPi_update_2019','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)

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
ggplot(data=eff_cars) + geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
                        geom_line(data=eff_cars_old, aes(x=year, y=value, colour=scenario)) +
                        ggtitle(paste0("Cars ", country)) +
                        ylim(0, NA)

# -------------- 1b. heavy trucks
eff_HvyT_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="existing")
eff_HvyT_NPi <- filter(NPi_2019$EfficiencyFleet_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="existing")

eff_new_HvyT_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_new_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="new")
eff_new_HvyT_NPi <- filter(NPi_2019$EfficiencyFleet_new_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="new")

eff_HvyT_old_NoPol <- filter(NoPolicy_2019$EfficiencyFleet_HvyT_old, year>=2015, year<=2030, region==country) %>% mutate(scenario="NoPolicy", standard="existing")
eff_HvyT_old_NPi <- filter(NPi_2019$EfficiencyFleet_HvyT_old, year>=2015, year<=2030, region==country) %>% mutate(scenario="NPi", standard="existing")

eff_HvyT <- rbind(eff_new_HvyT_NoPol, eff_HvyT_NoPol) %>% rbind(eff_new_HvyT_NPi) %>% rbind(eff_HvyT_NPi)
eff_HvyT_old <- rbind(eff_HvyT_old_NoPol, eff_HvyT_old_NPi)
ggplot() + geom_point(data=eff_HvyT, aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
                      geom_line(data=eff_HvyT_old, aes(x=year, y=value, colour=scenario)) +
                      ggtitle(paste0("Heavy trucks ", country)) +
                      ylim(0, NA)
ggplot() + geom_point(data=eff_HvyT, aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
           geom_line(data=eff_HvyT, aes(x=year, y=value, colour=scenario, shape=standard)) +
                      ggtitle(paste0("Heavy trucks ", country)) + 
                      theme_bw() +
                      ylim(0, NA)
tst1 <- mutate(eff_HvyT_NPi, x="current")
tst2 <- mutate(eff_HvyT_old_NPi, x="old")
tst <- rbind(tst1, tst2) %>% spread(key=x,value=value)
View(tst)

# --------- 2 sensitivity runs for fuel efficiency cars and heavy trucks
#           2a different paramters (energy tax, factor)
#           2b sensivity fuel efficiency cars and heavy trucks from NoPolicy and NPi

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
Test_cars_base <- ImportTimerScenario('Test_cars_base','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_1   <- ImportTimerScenario('Test_cars_1','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_2   <- ImportTimerScenario('Test_cars_2','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_3   <- ImportTimerScenario('Test_cars_3','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_5   <- ImportTimerScenario('Test_cars_5','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_10  <- ImportTimerScenario('Test_cars_10','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_15  <- ImportTimerScenario('Test_cars_15','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)

Test_cars_NPi_base <- ImportTimerScenario('Test_cars_NPi_base','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_NPi_1   <- ImportTimerScenario('Test_cars_NPi_1','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_NPi_2   <- ImportTimerScenario('Test_cars_NPi_2','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_NPi_3   <- ImportTimerScenario('Test_cars_NPi_3','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_NPi_5   <- ImportTimerScenario('Test_cars_NPi_5','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_NPi_10  <- ImportTimerScenario('Test_cars_NPi_10','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_cars_NPi_15  <- ImportTimerScenario('Test_cars_NPi_15','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)

Test_HvyT_base <- ImportTimerScenario('Test_HvyT_base','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_1   <- ImportTimerScenario('Test_HvyT_1','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_2   <- ImportTimerScenario('Test_HvyT_2','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_3   <- ImportTimerScenario('Test_HvyT_3','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_5   <- ImportTimerScenario('Test_HvyT_5','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_10  <- ImportTimerScenario('Test_HvyT_10','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_15  <- ImportTimerScenario('Test_HvyT_15','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)

Test_HvyT_NPi_base <- ImportTimerScenario('Test_HvyT_NPi_base','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_NPi_1   <- ImportTimerScenario('Test_HvyT_NPi_1','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_NPi_2   <- ImportTimerScenario('Test_HvyT_NPi_2','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_NPi_3   <- ImportTimerScenario('Test_HvyT_NPi_3','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_NPi_5   <- ImportTimerScenario('Test_HvyT_NPi_5','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_NPi_10  <- ImportTimerScenario('Test_HvyT_NPi_10','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
Test_HvyT_NPi_15  <- ImportTimerScenario('Test_HvyT_NPi_15','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)

country="USA"

# ------------- 2a test of the impact of increasing the enery tax factor, used to implement fuel efficiency standards
#               For heavy trucks, it shows that by 2030, increasing the energy tax does not have any impact beyond the factor 1

# energy tax cars
Test_cars_1_energy_tax <- filter(Test_cars_1$EnergyTax_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="1.0")
Test_cars_2_energy_tax <- filter(Test_cars_2$EnergyTax_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="2.0")
Test_cars_3_energy_tax <- filter(Test_cars_3$EnergyTax_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="3.0")
Test_cars_5_energy_tax <- filter(Test_cars_5$EnergyTax_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="5.0")
Test_cars_10_energy_tax <- filter(Test_cars_10$EnergyTax_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="10")
Test_cars_15_energy_tax <- filter(Test_cars_15$EnergyTax_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="15")
cars_energy_tax <- rbind(Test_cars_1_energy_tax,Test_cars_2_energy_tax) %>% rbind(Test_cars_3_energy_tax) %>% rbind(Test_car_5_energy_tax) %>% rbind(Test_car_10_energy_tax) %>% rbind(Test_car_15_energy_tax)
cars_energy_tax$scenario <- factor(car_energy_tax$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))
ggplot(data=filter(cars_energy_tax, year==2030)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  theme_bw() +
  ggtitle(paste0("Energy tax car fleet ", country)) + 
  scale_fill_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) + 
  #scale_fill_brewer(palette="Dark2") +
  ylim(0, NA)

# energy tax factor cars (NoPolicy)
Test_cars_1_factor<- filter(Test_cars_1$Factor_cars, year>=2015, year<=2030, region==country, travel_mode=="Car", energy_carrier=="Liquid fuel") %>% mutate(scenario="1.0")
Test_cars_2_factor <- filter(Test_cars_2$Factor_cars, year>=2015, year<=2030, region==country, travel_mode=="Car", energy_carrier=="Liquid fuel") %>% mutate(scenario="2.0")
Test_cars_3_factor <- filter(Test_cars_3$Factor_cars, year>=2015, year<=2030, region==country, travel_mode=="Car", energy_carrier=="Liquid fuel") %>% mutate(scenario="3.0")
Test_cars_5_factor <- filter(Test_cars_5$Factor_cars, year>=2015, year<=2030, region==country, travel_mode=="Car", energy_carrier=="Liquid fuel") %>% mutate(scenario="5.0")
Test_cars_10_factor <- filter(Test_cars_10$Factor_cars, year>=2015, year<=2030, region==country, travel_mode=="Car", energy_carrier=="Liquid fuel") %>% mutate(scenario="10")
Test_cars_15_factor <- filter(Test_cars_15$Factor_cars, year>=2015, year<=2030, region==country, travel_mode=="Car", energy_carrier=="Liquid fuel") %>% mutate(scenario="15")
cars_factor <- rbind(Test_cars_1_factor,Test_cars_2_factor) %>% rbind(Test_cars_3_factor) %>% rbind(Test_cars_5_factor) %>% rbind(Test_cars_10_factor) %>% rbind(Test_cars_15_factor)
ggplot(data=cars_factor) + geom_line(aes(x=year, y=value, colour=scenario), size=2) +
  ggtitle(paste0("Energy tax factor car fleet ", country)) + 
  scale_colour_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) +
  theme_bw() +
  ylim(0, NA)

# energy tax heavy trucks
Test_HvyT_1_energy_tax <- filter(Test_HvyT_1$EnergyTax_HvyT, year>=2015, year<=2030) %>% mutate(scenario="1.0")
Test_HvyT_2_energy_tax <- filter(Test_HvyT_2$EnergyTax_HvyT, year>=2015, year<=2030) %>% mutate(scenario="2.0")
Test_HvyT_3_energy_tax <- filter(Test_HvyT_3$EnergyTax_HvyT, year>=2015, year<=2030) %>% mutate(scenario="3.0")
Test_HvyT_5_energy_tax <- filter(Test_HvyT_5$EnergyTax_HvyT, year>=2015, year<=2030) %>% mutate(scenario="5.0")
Test_HvyT_10_energy_tax <- filter(Test_HvyT_10$EnergyTax_HvyT, year>=2015, year<=2030) %>% mutate(scenario="10")
Test_HvyT_15_energy_tax <- filter(Test_HvyT_15$EnergyTax_HvyT, year>=2015, year<=2030) %>% mutate(scenario="15")
HvyT_energy_tax <- rbind(Test_HvyT_1_energy_tax,Test_HvyT_2_energy_tax) %>% rbind(Test_HvyT_3_energy_tax) %>% rbind(Test_HvyT_5_energy_tax) %>% rbind(Test_HvyT_10_energy_tax) %>% rbind(Test_HvyT_15_energy_tax)
HvyT_energy_tax$scenario <- factor(HvyT_energy_tax$scenario, c("0.5","1.0","2.0", "3.0", "5.0", "10", "15"))
ggplot(data=filter(HvyT_energy_tax, year==2030, region==country)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  theme_bw() +
  ggtitle(paste0("Energy tax Hvy fleet ", country)) + 
  #scale_fill_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) + 
  scale_fill_brewer(palette="Dark2") +
  ylim(0, NA)

# energy tax factor heavy trucks (NoPolicy)
Test_HvyT_1_factor<- filter(Test_HvyT_1$Factor_HvyT, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="1.0")
Test_HvyT_2_factor <- filter(Test_HvyT_2$Factor_HvyT, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="2.0")
Test_HvyT_3_factor <- filter(Test_HvyT_3$Factor_HvyT, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="3.0")
Test_HvyT_5_factor <- filter(Test_HvyT_5$Factor_HvyT, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="5.0")
Test_HvyT_10_factor <- filter(Test_HvyT_10$Factor_HvyT, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="10")
Test_HvyT_15_factor <- filter(Test_HvyT_15$Factor_HvyT, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="15")
HvyT_factor <- rbind(Test_HvyT_1_factor,Test_HvyT_2_factor) %>% rbind(Test_HvyT_3_factor) %>% rbind(Test_HvyT_5_factor) %>% rbind(Test_HvyT_10_factor) %>% rbind(Test_HvyT_15_factor)
ggplot(data=filter(HvyT_factor, region==country)) + geom_line(aes(x=year, y=value, colour=scenario), size=2) +
  ggtitle(paste0("Energy tax factor Hvy fleet ", country)) + 
  scale_colour_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) +
  theme_bw() +
  ylim(0, NA)

# energy tax heavy trucks (NPi)
Test_HvyT_NPi_1_energy_tax <- filter(Test_HvyT_NPi_1$EnergyTax_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="1.0")
Test_HvyT_NPi_2_energy_tax <- filter(Test_HvyT_NPi_2$EnergyTax_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="2.0")
Test_HvyT_NPi_3_energy_tax <- filter(Test_HvyT_NPi_3$EnergyTax_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="3.0")
Test_HvyT_NPi_5_energy_tax <- filter(Test_HvyT_NPi_5$EnergyTax_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="5.0")
Test_HvyT_NPi_10_energy_tax <- filter(Test_HvyT_NPi_10$EnergyTax_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="10")
Test_HvyT_NPi_15_energy_tax <- filter(Test_HvyT_NPi_15$EnergyTax_HvyT, year>=2015, year<=2030, region==country) %>% mutate(scenario="15")
HvyT_NPi_energy_tax <- rbind(Test_HvyT_NPi_1_energy_tax,Test_HvyT_NPi_2_energy_tax) %>% rbind(Test_HvyT_NPi_3_energy_tax) %>% rbind(Test_HvyT_NPi_5_energy_tax) %>% rbind(Test_HvyT_NPi_10_energy_tax) %>% rbind(Test_HvyT_NPi_15_energy_tax)
HvyT_NPi_energy_tax$scenario <- factor(HvyT_NPi_energy_tax$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))
ggplot(data=filter(HvyT_NPi_energy_tax, year==2030)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  theme_bw() +
  ggtitle(paste0("Energy tax Hvy fleet ", country)) + 
  scale_fill_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) + 
  #scale_fill_brewer(palette="Dark2") +
  ylim(0, NA)

# energy tax factor heavy trucks (NPi)
Test_HvyT_NPi_1_factor<- filter(Test_HvyT_NPi_1$Factor_HvyT, year>=2015, year<=2030, region==country, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="1.0")
Test_HvyT_NPi_2_factor <- filter(Test_HvyT_NPi_2$Factor_HvyT, year>=2015, year<=2030, region==country, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="2.0")
Test_HvyT_NPi_3_factor <- filter(Test_HvyT_NPi_3$Factor_HvyT, year>=2015, year<=2030, region==country, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="3.0")
Test_HvyT_NPi_5_factor <- filter(Test_HvyT_NPi_5$Factor_HvyT, year>=2015, year<=2030, region==country, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="5.0")
Test_HvyT_NPi_10_factor <- filter(Test_HvyT_NPi_10$Factor_HvyT, year>=2015, year<=2030, region==country, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="10")
Test_HvyT_NPi_15_factor <- filter(Test_HvyT_NPi_15$Factor_HvyT, year>=2015, year<=2030, region==country, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="15")
HvyT_NPi_factor <- rbind(Test_HvyT_1_factor,Test_HvyT_2_factor) %>% rbind(Test_HvyT_3_factor) %>% rbind(Test_HvyT_5_factor) %>% rbind(Test_HvyT_10_factor) %>% rbind(Test_HvyT_15_factor)
ggplot(data=HvyT_NPi_factor) + geom_line(aes(x=year, y=value, colour=scenario), size=2) +
  ggtitle(paste0("Energy tax factor Hvy fleet ", country)) + 
  scale_colour_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) +
  theme_bw() +
  ylim(0, NA)


# 2b efficiency senstivity runs cars and heavy trucks (new and existing) from NoPolicy and NPi

# efficiency new and existing fleet (NoPolicy)
Test_cars_base_eff_new <- filter(Test_cars_base$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="base", standard="new")
Test_cars_1_eff_new <- filter(Test_cars_1$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="new")
Test_cars_2_eff_new <- filter(Test_cars_2$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="new")
Test_cars_3_eff_new <- filter(Test_cars_3$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="new")
Test_cars_5_eff_new <- filter(Test_cars_5$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="new")
Test_cars_10_eff_new <- filter(Test_cars_10$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="10", standard="new")
Test_cars_15_eff_new <- filter(Test_cars_15$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="15", standard="new")
eff_cars_new <- rbind(Test_cars_base_eff_new, Test_cars_1_eff_new) %>% rbind(Test_cars_2_eff_new) %>% rbind(Test_cars_3_eff_new) %>% rbind(Test_cars_5_eff_new) %>% rbind(Test_cars_10_eff_new) %>% rbind(Test_cars_15_eff_new)
eff_cars_new$scenario <- factor(eff_cars_new$scenario, c("base", "1.0","2.0", "3.0", "5.0", "10", "15"))

Test_cars_base_eff <- filter(Test_cars_base$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="base", standard="existing")
Test_cars_1_eff <- filter(Test_cars_1$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="existing")
Test_cars_2_eff <- filter(Test_cars_2$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="existing")
Test_cars_3_eff <- filter(Test_cars_3$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="existing")
Test_cars_5_eff <- filter(Test_cars_5$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="existing")
Test_cars_10_eff <- filter(Test_cars_10$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="10", standard="existing")
Test_cars_15_eff <- filter(Test_cars_15$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="15", standard="existing")
eff_cars_existing <- rbind(Test_cars_base_eff, Test_cars_1_eff) %>% rbind(Test_cars_2_eff) %>% rbind(Test_cars_3_eff) %>% rbind(Test_cars_5_eff) %>% rbind(Test_cars_10_eff) %>% rbind(Test_cars_15_eff)
eff_cars_existing$scenario <- factor(eff_cars_existing$scenario, c("base", "1.0","2.0", "3.0", "5.0", "10", "15"))

# graph only new vehicle efficiency (NoPolicy)
ggplot(data=filter(eff_cars_new, region==country, scenario%in%c("base", "1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario)) +
  ggtitle(paste0("Fuel efficiency new car fleet ", country))+ 
  theme_bw() +
  ylim(0, NA)

# graph ony new and existing vehicle efficiency (NoPolicy)
eff_cars <- rbind(eff_cars_new, eff_cars_existing)
ggplot(data=filter(eff_cars, region==country, scenario%in%c("base", "1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario, linetype=standard)) +
  ggtitle(paste0("Fuel efficiency new and existing car fleet (from NoPolicy) ", country))+ 
  theme_bw() +
  ylim(0, NA)

# efficiency new and existing fleet (NPi)
#Test_cars_NPi_base_eff_new <- filter(Test_cars_base_eff$EfficiencyFleet_new_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="base", standard="new")
Test_cars_NPi_1_eff_new <- filter(Test_cars_NPi_1$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="new")
Test_cars_NPi_2_eff_new <- filter(Test_cars_NPi_2$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="new")
Test_cars_NPi_3_eff_new <- filter(Test_cars_NPi_3$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="new")
Test_cars_NPi_5_eff_new <- filter(Test_cars_NPi_5$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="new")
Test_cars_NPi_10_eff_new <- filter(Test_cars_NPi_10$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="10", standard="new")
Test_cars_NPi_15_eff_new <- filter(Test_cars_NPi_15$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="15", standard="new")
#eff_cars_NPi_new <- rbind(Test_cars_base_eff_new, Test_cars_0_5_eff_new) %>% rbind(Test_cars_1_eff_new) %>% rbind(Test_cars_2_eff_new) %>% rbind(Test_cars_3_eff_new) %>% rbind(Test_cars_5_eff_new) %>% rbind(Test_cars_10_eff_new) %>% rbind(Test_cars_15_eff_new)
eff_cars_NPi_new <- rbind(Test_cars_NPi_1_eff_new,Test_cars_NPi_2_eff_new) %>% rbind(Test_cars_NPi_3_eff_new) %>% rbind(Test_cars_NPi_5_eff_new) %>% rbind(Test_cars_NPi_10_eff_new) %>% rbind(Test_cars_NPi_15_eff_new)
eff_cars_NPi_new$scenario <- factor(eff_cars_NPi_new$scenario, c("base", "0.5","1.0","2.0", "3.0", "5.0", "10", "15"))

#Test_cars_NPi_base_eff <- filter(Test_cars_NPi_base$EfficiencyFleet_cars, year>=2015, year<=2030, region==country) %>% mutate(scenario="base", standard="existing")
Test_cars_NPi_1_eff <- filter(Test_cars_NPi_1$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="existing")
Test_cars_NPi_2_eff <- filter(Test_cars_NPi_2$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="existing")
Test_cars_NPi_3_eff <- filter(Test_cars_NPi_3$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="existing")
Test_cars_NPi_5_eff <- filter(Test_cars_NPi_5$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="existing")
Test_cars_NPi_10_eff <- filter(Test_cars_NPi_10$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="10", standard="existing")
Test_cars_NPi_15_eff <- filter(Test_cars_NPi_15$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="15", standard="existing")
#eff_cars_NPi_existing <- rbind(Test_cars_NPi_base_eff, Test_cars_NPi_0_5_eff) %>% rbind(Test_cars_NPi_1_eff) %>% rbind(Test_cars_NPi_2_eff) %>% rbind(Test_cars_NPi_3_eff) %>% rbind(Test_cars_NPi_5_eff) %>% rbind(Test_cars_NPi_10_eff) %>% rbind(Test_cars_NPi_15_eff)
eff_cars_NPi_existing <- rbind(Test_cars_NPi_1_eff,Test_cars_NPi_2_eff) %>% rbind(Test_cars_NPi_3_eff) %>% rbind(Test_cars_NPi_5_eff) %>% rbind(Test_cars_NPi_10_eff) %>% rbind(Test_cars_NPi_15_eff)
eff_cars_NPi_existing$scenario <- factor(eff_cars_NPi_existing$scenario, c("base", "0.5","1.0","2.0", "3.0", "5.0", "10", "15"))

# graph only new vehicle efficiency (NPi)
ggplot(data=filter(eff_cars_NPi_existing, region==country, scenario%in%c("base", "1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario)) +
  ggtitle(paste0("Fuel efficiency new car fleet (from NPi) ", country))+ 
  theme_bw() +
  ylim(0, NA)

# graph new and existing vehicle efficiency (NPi)
eff_cars <- rbind(eff_cars_NPi_new, eff_cars_NPi_existing)
ggplot(data=filter(eff_cars, region==country, scenario%in%c("base", "1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario, linetype=standard)) +
  ggtitle(paste0("Fuel efficiency new and existing car fleet (from NPi) ", country))+ 
  theme_bw() +
  ylim(0, NA)

# ----------------- 2b2 Heavy trucks

# efficiency new fleet for heavy trucks (NoPolicy)
Test_HvyT_1_eff_new <- filter(Test_HvyT_1$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="new")
Test_HvyT_2_eff_new <- filter(Test_HvyT_2$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="new")
Test_HvyT_3_eff_new <- filter(Test_HvyT_3$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="new")
Test_HvyT_5_eff_new <- filter(Test_HvyT_5$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="new")
Test_HvyT_10_eff_new <- filter(Test_HvyT_10$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="10", standard="new")
Test_HvyT_15_eff_new <- filter(Test_HvyT_15$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="15", standard="new")
eff_HvyT_new <- rbind(Test_HvyT_1_eff_new,Test_HvyT_2_eff_new) %>% rbind(Test_HvyT_3_eff_new) %>% rbind(Test_HvyT_5_eff_new) %>% rbind(Test_HvyT_10_eff_new) %>% rbind(Test_HvyT_15_eff_new)
eff_HvyT_new$scenario <- factor(eff_HvyT_new$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

ggplot(data=filter(eff_HvyT_new, region==country)) + geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario)) +
  ggtitle(paste0("Fuel efficiency new Hvy fleet (from NoPolicy) ", country)) + 
  scale_colour_brewer(palette="Dark2") +
  theme_bw() +
  ylim(0, NA)
ggplot(data=filter(eff_HvyT_new, region==country, year==2030)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  ggtitle(paste0("Fuel efficiency Hvy new fleet (from NoPolicy) ", country)) + 
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  ylim(0, NA)

# efficiency new fleet for heavy trucks (NPi)
Test_HvyT_NPi_1_eff_new <- filter(Test_HvyT_NPi_1$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="existing")
Test_HvyT_NPi_2_eff_new <- filter(Test_HvyT_NPi_2$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="existing")
Test_HvyT_NPi_3_eff_new <- filter(Test_HvyT_NPi_3$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="existing")
Test_HvyT_NPi_5_eff_new <- filter(Test_HvyT_NPi_5$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="existing")
Test_HvyT_NPi_10_eff_new <- filter(Test_HvyT_NPi_10$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="10", standard="existing")
Test_HvyT_NPi_15_eff_new <- filter(Test_HvyT_NPi_15$EfficiencyFleet_new_HvyT, year>=2015, year<=2030) %>% mutate(scenario="15", standard="existing")
eff_HvyT_NPi_new <- rbind(Test_HvyT_NPi_1_eff_new,Test_HvyT_NPi_2_eff_new) %>% rbind(Test_HvyT_NPi_3_eff_new) %>% rbind(Test_HvyT_NPi_5_eff_new) %>% rbind(Test_HvyT_NPi_10_eff_new) %>% rbind(Test_HvyT_NPi_15_eff_new)
eff_HvyT_NPi_new$scenario <- factor(eff_HvyT_NPi_new$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

ggplot(data=filter(eff_HvyT_NPi_new, country==region)) + geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario)) +
  ggtitle(paste0("Fuel efficiency new Hvy fleet (from NPi) ", country))+ 
  scale_colour_brewer(palette="Dark2") + 
  theme_bw() +
  ylim(0, NA)
ggplot(data=filter(eff_HvyT_NPi_new, year==2030)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  ggtitle(paste0("Fuel efficiency new Hvy fleet (from NPi) ", country))+ 
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  ylim(0, NA)

# efficiency existing fleet for heavy trucks (NoPolicy)
Test_HvyT_1_eff <- filter(Test_HvyT_1$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="existing")
Test_HvyT_2_eff <- filter(Test_HvyT_2$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="existing")
Test_HvyT_3_eff <- filter(Test_HvyT_3$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="existing")
Test_HvyT_5_eff <- filter(Test_HvyT_5$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="existing")
Test_HvyT_10_eff <- filter(Test_HvyT_10$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="10", standard="existing")
Test_HvyT_15_eff <- filter(Test_HvyT_15$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="15", standard="existing")
eff_HvyT_existing <- rbind(Test_HvyT_1_eff,Test_HvyT_2_eff) %>% rbind(Test_HvyT_3_eff) %>% rbind(Test_HvyT_5_eff) %>% rbind(Test_HvyT_10_eff) %>% rbind(Test_HvyT_15_eff)
eff_HvyT_existing$scenario <- factor(eff_HvyT_existing$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

ggplot(data=filter(eff_HvyT_existing, region==country)) + geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario)) +
  ggtitle(paste0("Fuel efficiency existing Hvy fleet (from NoPolicy) ", country))+ 
  scale_colour_brewer(palette="Dark2") + 
  theme_bw() +
  ylim(0, NA)
ggplot(data=filter(eff_HvyT_existing, year==2030)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  ggtitle(paste0("Fuel efficiency existing Hvy fleet (from NoPolicy) ", country))+ 
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  ylim(0, NA)

# efficiency existing fleet for heavy trucks (NPi)
Test_HvyT_NPi_1_eff <- filter(Test_HvyT_NPi_1$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="existing")
Test_HvyT_NPi_2_eff <- filter(Test_HvyT_NPi_2$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="existing")
Test_HvyT_NPi_3_eff <- filter(Test_HvyT_NPi_3$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="existing")
Test_HvyT_NPi_5_eff <- filter(Test_HvyT_NPi_5$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="existing")
Test_HvyT_NPi_10_eff <- filter(Test_HvyT_NPi_10$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="10", standard="existing")
Test_HvyT_NPi_15_eff <- filter(Test_HvyT_NPi_15$EfficiencyFleet_HvyT, year>=2015, year<=2030) %>% mutate(scenario="15", standard="existing")
eff_HvyT_NPi_existing <- rbind(Test_HvyT_NPi_1_eff,Test_HvyT_NPi_2_eff) %>% rbind(Test_HvyT_NPi_3_eff) %>% rbind(Test_HvyT_NPi_5_eff) %>% rbind(Test_HvyT_NPi_10_eff) %>% rbind(Test_HvyT_NPi_15_eff)
eff_HvyT_NPi_existing$scenario <- factor(eff_HvyT_NPi_existing$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

ggplot(data=filter(eff_HvyT_NPi_existing, region==country)) + geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario)) +
  ggtitle(paste0("Fuel efficiency existing Hvy fleet (from NPi) ", country))+ 
  scale_colour_brewer(palette="Dark2") + 
  theme_bw() +
  ylim(0, NA)
ggplot(data=filter(eff_HvyT_NPi_existing, year==2030)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  ggtitle(paste0("Fuel efficiency existing Hvy fleet (from NPi) ", country)) + 
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  ylim(0, NA)

# COMPARE efficiency new and existing fleet (NoPolicy)
eff_HvyT <- rbind(eff_HvyT_new, eff_HvyT_existing)
ggplot(data=filter(eff_HvyT, scenario%in%c("1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario, linetype=standard))+ 
  scale_colour_brewer(palette="Dark2") +
  ggtitle(paste0("Fuel efficiency new HvyT fleet (from NoPolicy) ", country))+ 
  theme_bw() +
  ylim(0, NA)


# COMPARE efficiency new and existing fleet (NPi)
eff_HvyT_NPi <- rbind(eff_HvyT_NPi_new, eff_HvyT_NPi_existing)
ggplot(data=filter(eff_HvyT_NPi, scenario%in%c("1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=5) +
  geom_line(aes(x=year, y=value, colour=scenario, linetype=standard)) + 
  scale_colour_brewer(palette="Dark2") +
  ggtitle(paste0("Fuel efficiency existing HvyT fleet (from NPi) ", country))+ 
  theme_bw() +
  ylim(0, NA)
