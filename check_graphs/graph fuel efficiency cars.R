
# --------- 2 sensitivity runs for fuel efficiency cars and cars
#           2a different paramters (energy tax, factor)
#           2b sensivity fuel efficiency cars and cars from NoPolicy and NPi

library(dplyr)
library(tidyverse)
library(data.table)

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

#("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","World")
countries_cars = c("USA","WEU", "CHN")
country_cars = "USA"

#Test_cars_base <- ImportTimerScenario('Test_cars_base','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NoPolicy_1   <- ImportTimerScenario('Test_cars_1','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NoPolicy_2   <- ImportTimerScenario('Test_cars_2','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NoPolicy_3   <- ImportTimerScenario('Test_cars_3','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NoPolicy_5   <- ImportTimerScenario('Test_cars_5','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NoPolicy_10  <- ImportTimerScenario('Test_cars_10','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NoPolicy_15  <- ImportTimerScenario('Test_cars_15','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)

#Test_cars_NPi_base <- ImportTimerScenario('Test_cars_NPi_base','NPi', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NPi_1   <- ImportTimerScenario('Test_cars_NPi_1','NPi', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NPi_2   <- ImportTimerScenario('Test_cars_NPi_2','NPi', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NPi_3   <- ImportTimerScenario('Test_cars_NPi_3','NPi', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NPi_5   <- ImportTimerScenario('Test_cars_NPi_5','NPi', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NPi_10  <- ImportTimerScenario('Test_cars_NPi_10','NPi', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
Test_cars_NPi_15  <- ImportTimerScenario('Test_cars_NPi_15','NPi', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)

# ------------- 2a SHOW PARAMETERS
#               for testing the impact of increasing the enery tax factor, used to implement fuel efficiency standards
#               For heavy trucks, it shows that by 2030, increasing the energy tax does not have any impact beyond the factor 1

# energy tax cars
Test_cars_NoPolicy_1_energy_tax <- filter(Test_cars_NoPolicy_1$EnergyTax_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0")
Test_cars_NoPolicy_2_energy_tax <- filter(Test_cars_NoPolicy_2$EnergyTax_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0")
Test_cars_NoPolicy_3_energy_tax <- filter(Test_cars_NoPolicy_3$EnergyTax_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0")
Test_cars_NoPolicy_5_energy_tax <- filter(Test_cars_NoPolicy_5$EnergyTax_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0")
Test_cars_NoPolicy_10_energy_tax <- filter(Test_cars_NoPolicy_10$EnergyTax_cars, year>=2015, year<=2030) %>% mutate(scenario="10")
Test_cars_NoPolicy_15_energy_tax <- filter(Test_cars_NoPolicy_15$EnergyTax_cars, year>=2015, year<=2030) %>% mutate(scenario="15")
cars_NoPolicy_energy_tax <- rbind(Test_cars_NoPolicy_1_energy_tax,Test_cars_NoPolicy_2_energy_tax) %>% rbind(Test_cars_NoPolicy_3_energy_tax) %>% rbind(Test_cars_NoPolicy_5_energy_tax) %>% rbind(Test_cars_NoPolicy_10_energy_tax) %>% rbind(Test_cars_NoPolicy_15_energy_tax)
cars_NoPolicy_energy_tax$scenario <- factor(cars_NoPolicy_energy_tax$scenario, c("0.5","1.0","2.0", "3.0", "5.0", "10", "15"))
# bar chart showing energy taxes for cars with basis NoPolicy scenarios
g<-ggplot(data=filter(cars_NoPolicy_energy_tax, year==2030, region==country_cars)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  theme_bw() +
  ggtitle(paste0("Energy tax Hvy fleet ", country_cars))+ 
  ylab(Test_cars_NoPolicy_1$EnergyTax_cars$unit) + 
  #scale_fill_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) + 
  scale_fill_brewer(palette="Dark2") +
  ylim(0, NA)
f = paste0("graphs/fuel_efficiency/energy tax from NoPolicy for fuel efficiency cars_", country_cars, ".jpg")
plot(g)
ggsave(f, g)

# energy tax factor for oil cars (NoPolicy)
Test_cars_NoPolicy_1_oil_factor<- filter(Test_cars_NoPolicy_1$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="1.0")
Test_cars_NoPolicy_2_oil_factor <- filter(Test_cars_NoPolicy_2$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="2.0")
Test_cars_NoPolicy_3_oil_factor <- filter(Test_cars_NoPolicy_3$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="3.0")
Test_cars_NoPolicy_5_oil_factor <- filter(Test_cars_NoPolicy_5$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="5.0")
Test_cars_NoPolicy_10_oil_factor <- filter(Test_cars_NoPolicy_10$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="10")
Test_cars_NoPolicy_15_oil_factor <- filter(Test_cars_NoPolicy_15$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="15")
cars_NoPolicy_factor_oil <- rbind(Test_cars_NoPolicy_1_oil_factor,Test_cars_NoPolicy_2_oil_factor) %>% rbind(Test_cars_NoPolicy_3_oil_factor) %>% rbind(Test_cars_NoPolicy_5_oil_factor) %>% rbind(Test_cars_NoPolicy_10_oil_factor) %>% rbind(Test_cars_NoPolicy_15_oil_factor)
# bar chart showing energy tax factors for cars with basis NoPolicy scenario
g<-ggplot(data=filter(cars_NoPolicy_factor_oil, region==country_cars)) + geom_line(aes(x=year, y=value, colour=scenario), size=2) +
  ggtitle(paste0("Energy tax factor oil Hvy fleet ", country_cars)) + 
  scale_colour_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) +
  theme_bw() +
  ylim(0, NA)
plot(g)

# energy tax factor for gas cars (NoPolicy)
Test_cars_NoPolicy_1_gas_factor<- filter(Test_cars_NoPolicy_1$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Gaseous fuel") %>% mutate(scenario="1.0")
Test_cars_NoPolicy_2_gas_factor <- filter(Test_cars_NoPolicy_2$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Gaseous fuel") %>% mutate(scenario="2.0")
Test_cars_NoPolicy_3_gas_factor <- filter(Test_cars_NoPolicy_3$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Gaseous fuel") %>% mutate(scenario="3.0")
Test_cars_NoPolicy_5_gas_factor <- filter(Test_cars_NoPolicy_5$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Gaseous fuel") %>% mutate(scenario="5.0")
Test_cars_NoPolicy_10_gas_factor <- filter(Test_cars_NoPolicy_10$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Gaseous fuel") %>% mutate(scenario="10")
Test_cars_NoPolicy_15_gas_factor <- filter(Test_cars_NoPolicy_15$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Gaseous fuel") %>% mutate(scenario="15")
cars_NoPolicy_factor_gas <- rbind(Test_cars_NoPolicy_1_gas_factor,Test_cars_NoPolicy_2_gas_factor) %>% rbind(Test_cars_NoPolicy_3_gas_factor) %>% rbind(Test_cars_NoPolicy_5_gas_factor) %>% rbind(Test_cars_NoPolicy_10_gas_factor) %>% rbind(Test_cars_NoPolicy_15_gas_factor)
# bar chart showing energy tax factors for cars with basis NoPolicy scenario
g<-ggplot(data=filter(cars_NoPolicy_factor_gas, region==country_cars)) + geom_line(aes(x=year, y=value, colour=scenario), size=2) +
  ggtitle(paste0("Energy tax factor gas Hvy fleet ", country_cars)) + 
  scale_colour_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) +
  theme_bw() +
  ylim(0, NA)
plot(g)

# energy tax factor for biofuel cars (NoPolicy)
Test_cars_NoPolicy_1_bio_factor<- filter(Test_cars_NoPolicy_1$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Modern biofuel") %>% mutate(scenario="1.0")
Test_cars_NoPolicy_2_bio_factor <- filter(Test_cars_NoPolicy_2$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Modern biofuel") %>% mutate(scenario="2.0")
Test_cars_NoPolicy_3_bio_factor <- filter(Test_cars_NoPolicy_3$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Modern biofuel") %>% mutate(scenario="3.0")
Test_cars_NoPolicy_5_bio_factor <- filter(Test_cars_NoPolicy_5$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Modern biofuel") %>% mutate(scenario="5.0")
Test_cars_NoPolicy_10_bio_factor <- filter(Test_cars_NoPolicy_10$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Modern biofuel") %>% mutate(scenario="10")
Test_cars_NoPolicy_15_bio_factor <- filter(Test_cars_NoPolicy_15$Factor_cars, year>=2015, year<=2030, freight_mode=="Heavy truck", energy_carrier=="Modern biofuel") %>% mutate(scenario="15")
cars_NoPolicy_factor_bio <- rbind(Test_cars_NoPolicy_1_bio_factor,Test_cars_NoPolicy_2_bio_factor) %>% rbind(Test_cars_NoPolicy_3_bio_factor) %>% rbind(Test_cars_NoPolicy_5_bio_factor) %>% rbind(Test_cars_NoPolicy_10_bio_factor) %>% rbind(Test_cars_NoPolicy_15_bio_factor)
# bar chart showing energy tax factors for cars with basis NoPolicy scenario
g<-ggplot(data=filter(cars_NoPolicy_factor_bio, region==country_cars)) + geom_line(aes(x=year, y=value, colour=scenario), size=2) +
  ggtitle(paste0("Energy tax factor biofuel Hvy fleet ", country_cars)) + 
  scale_colour_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) +
  theme_bw() +
  ylim(0, NA)
plot(g)

# energy tax cars (NPi)
Test_cars_NPi_1_energy_tax <- filter(Test_cars_NPi_1$EnergyTax_cars, year>=2015, year<=2030, region==country_cars) %>% mutate(scenario="1.0")
Test_cars_NPi_2_energy_tax <- filter(Test_cars_NPi_2$EnergyTax_cars, year>=2015, year<=2030, region==country_cars) %>% mutate(scenario="2.0")
Test_cars_NPi_3_energy_tax <- filter(Test_cars_NPi_3$EnergyTax_cars, year>=2015, year<=2030, region==country_cars) %>% mutate(scenario="3.0")
Test_cars_NPi_5_energy_tax <- filter(Test_cars_NPi_5$EnergyTax_cars, year>=2015, year<=2030, region==country_cars) %>% mutate(scenario="5.0")
Test_cars_NPi_10_energy_tax <- filter(Test_cars_NPi_10$EnergyTax_cars, year>=2015, year<=2030, region==country_cars) %>% mutate(scenario="10")
Test_cars_NPi_15_energy_tax <- filter(Test_cars_NPi_15$EnergyTax_cars, year>=2015, year<=2030, region==country_cars) %>% mutate(scenario="15")
cars_NPi_energy_tax <- rbind(Test_cars_NPi_1_energy_tax,Test_cars_NPi_2_energy_tax) %>% rbind(Test_cars_NPi_3_energy_tax) %>% rbind(Test_cars_NPi_5_energy_tax) %>% rbind(Test_cars_NPi_10_energy_tax) %>% rbind(Test_cars_NPi_15_energy_tax)
cars_NPi_energy_tax$scenario <- factor(cars_NPi_energy_tax$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))
# bar chart showing energy taxes for cars with basis NPi scenario
g<-ggplot(data=filter(cars_NPi_energy_tax, year==2030)) + geom_bar(aes(x=scenario, y=value, fill=scenario), stat="identity") +
  theme_bw() +
  ggtitle(paste0("Energy tax Hvy fleet ", country_cars)) + 
  scale_fill_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) + 
  #scale_fill_brewer(palette="Dark2") +
  ylim(0, NA)
f = paste0("graphs/fuel_efficiency/Energy tax from NPi for fuel efficiency cars_", country_cars, ".jpg")
plot(g)
ggsave(f, g)

# energy tax factor cars (NPi)
Test_cars_NPi_1_factor <- filter(Test_cars_NPi_1$Factor_cars, year>=2015, year<=2030, region==country_cars, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="1.0")
Test_cars_NPi_2_factor <- filter(Test_cars_NPi_2$Factor_cars, year>=2015, year<=2030, region==country_cars, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="2.0")
Test_cars_NPi_3_factor <- filter(Test_cars_NPi_3$Factor_cars, year>=2015, year<=2030, region==country_cars, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="3.0")
Test_cars_NPi_5_factor <- filter(Test_cars_NPi_5$Factor_cars, year>=2015, year<=2030, region==country_cars, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="5.0")
Test_cars_NPi_10_factor <- filter(Test_cars_NPi_10$Factor_cars, year>=2015, year<=2030, region==country_cars, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="10")
Test_cars_NPi_15_factor <- filter(Test_cars_NPi_15$Factor_cars, year>=2015, year<=2030, region==country_cars, freight_mode=="Heavy truck", energy_carrier=="Liquid fuel") %>% mutate(scenario="15")
cars_NPi_factor <- rbind(Test_cars_NPi_1_factor,Test_cars_NPi_2_factor) %>% rbind(Test_cars_NPi_3_factor) %>% rbind(Test_cars_NPi_5_factor) %>% rbind(Test_cars_NPi_10_factor) %>% rbind(Test_cars_NPi_15_factor)
# bar chart showing energy tax factors for cars with basis NPi scenario
ggplot(data=cars_NPi_factor) + geom_line(aes(x=year, y=value, colour=scenario), size=2) +
  ggtitle(paste0("Energy tax factor liquid fuel Hvy fleet ", country_cars)) + 
  scale_colour_discrete(breaks=c("1.0","2.0", "3.0", "5.0", "10", "15")) +
  theme_bw() +
  ylim(0, NA)


# 2b efficiency senstivity runs cars and cars (new and existing) from NoPolicy and NPi

# efficiency new fleet for cars (NoPolicy)
Test_cars_NoPolicy_1_eff_new <- filter(Test_cars_NoPolicy_1$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="new")
Test_cars_NoPolicy_2_eff_new <- filter(Test_cars_NoPolicy_2$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="new")
Test_cars_NoPolicy_3_eff_new <- filter(Test_cars_NoPolicy_3$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="new")
Test_cars_NoPolicy_5_eff_new <- filter(Test_cars_NoPolicy_5$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="new")
Test_cars_NoPolicy_10_eff_new <- filter(Test_cars_NoPolicy_10$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="10", standard="new")
Test_cars_NoPolicy_15_eff_new <- filter(Test_cars_NoPolicy_15$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="15", standard="new")
eff_cars_NoPolicy_new <- rbind(Test_cars_NoPolicy_1_eff_new,Test_cars_NoPolicy_2_eff_new) %>% rbind(Test_cars_NoPolicy_3_eff_new) %>% rbind(Test_cars_NoPolicy_5_eff_new) %>% rbind(Test_cars_NoPolicy_10_eff_new) %>% rbind(Test_cars_NoPolicy_15_eff_new)
eff_cars_NoPolicy_new$scenario <- factor(eff_cars_NoPolicy_new$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

# efficiency new fleet for cars (NPi)
Test_cars_NPi_1_eff_new <- filter(Test_cars_NPi_1$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="new")
Test_cars_NPi_2_eff_new <- filter(Test_cars_NPi_2$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="new")
Test_cars_NPi_3_eff_new <- filter(Test_cars_NPi_3$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="new")
Test_cars_NPi_5_eff_new <- filter(Test_cars_NPi_5$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="new")
Test_cars_NPi_10_eff_new <- filter(Test_cars_NPi_10$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="10", standard="new")
Test_cars_NPi_15_eff_new <- filter(Test_cars_NPi_15$EfficiencyFleet_new_cars, year>=2015, year<=2030) %>% mutate(scenario="15", standard="new")
eff_cars_NPi_new <- rbind(Test_cars_NPi_1_eff_new,Test_cars_NPi_2_eff_new) %>% rbind(Test_cars_NPi_3_eff_new) %>% rbind(Test_cars_NPi_5_eff_new) %>% rbind(Test_cars_NPi_10_eff_new) %>% rbind(Test_cars_NPi_15_eff_new)
eff_cars_NPi_new$scenario <- factor(eff_cars_NPi_new$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

# efficiency existing fleet for cars (NoPolicy)
Test_cars_NoPolicy_1_eff_existing <- filter(Test_cars_NoPolicy_1$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="existing")
Test_cars_NoPolicy_2_eff_existing <- filter(Test_cars_NoPolicy_2$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="existing")
Test_cars_NoPolicy_3_eff_existing <- filter(Test_cars_NoPolicy_3$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="existing")
Test_cars_NoPolicy_5_eff_existing <- filter(Test_cars_NoPolicy_5$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="existing")
Test_cars_NoPolicy_10_eff_existing <- filter(Test_cars_NoPolicy_10$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="10", standard="existing")
Test_cars_NoPolicy_15_eff_existing <- filter(Test_cars_NoPolicy_15$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="15", standard="existing")
eff_cars_NoPolicy_existing <- rbind(Test_cars_NoPolicy_1_eff_existing,Test_cars_NoPolicy_2_eff_existing) %>% rbind(Test_cars_NoPolicy_3_eff_existing) %>% rbind(Test_cars_NoPolicy_5_eff_existing) %>% rbind(Test_cars_NoPolicy_10_eff_existing) %>% rbind(Test_cars_NoPolicy_15_eff_existing)
eff_cars_NoPolicy_existing$scenario <- factor(eff_cars_NoPolicy_existing$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

# efficiency existing fleet for cars (NPi)
Test_cars_NPi_1_eff_existing <- filter(Test_cars_NPi_1$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="1.0", standard="existing")
Test_cars_NPi_2_eff_existing <- filter(Test_cars_NPi_2$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="2.0", standard="existing")
Test_cars_NPi_3_eff_existing <- filter(Test_cars_NPi_3$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="3.0", standard="existing")
Test_cars_NPi_5_eff_existing <- filter(Test_cars_NPi_5$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="5.0", standard="existing")
Test_cars_NPi_10_eff_existing <- filter(Test_cars_NPi_10$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="10", standard="existing")
Test_cars_NPi_15_eff_existing <- filter(Test_cars_NPi_15$EfficiencyFleet_cars, year>=2015, year<=2030) %>% mutate(scenario="15", standard="existing")
eff_cars_NPi_existing <- rbind(Test_cars_NPi_1_eff_existing,Test_cars_NPi_2_eff_existing) %>% rbind(Test_cars_NPi_3_eff_existing) %>% rbind(Test_cars_NPi_5_eff_existing) %>% rbind(Test_cars_NPi_10_eff_existing) %>% rbind(Test_cars_NPi_15_eff_existing)
eff_cars_NPi_existing$scenario <- factor(eff_cars_NPi_existing$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

# emissions for cars (NoPolicy)
Test_cars_NoPolicy_1_em <- filter(Test_cars_NoPolicy_1$TransportTraveltCO2Emissions, year>=2015, year<=2030, travel_mode=="Car")  %>% mutate(scenario="1.0")
Test_cars_NoPolicy_2_em <- filter(Test_cars_NoPolicy_2$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="2.0")
Test_cars_NoPolicy_3_em <- filter(Test_cars_NoPolicy_3$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="3.0")
Test_cars_NoPolicy_5_em <- filter(Test_cars_NoPolicy_5$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="5.0")
Test_cars_NoPolicy_10_em <- filter(Test_cars_NoPolicy_10$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="10")
Test_cars_NoPolicy_15_em <- filter(Test_cars_NoPolicy_15$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="15")
em_cars_NoPolicy <- rbind(Test_cars_NoPolicy_1_em, Test_cars_NoPolicy_2_em) %>% rbind(Test_cars_NoPolicy_3_em) %>% rbind(Test_cars_NoPolicy_5_em) %>% rbind(Test_cars_NoPolicy_10_em) %>% rbind(Test_cars_NoPolicy_15_em)
em_cars_NoPolicy$scenario <- factor(em_cars_NoPolicy$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))

# emissions for cars (NPi)
Test_cars_NPi_1_em <- filter(Test_cars_NPi_1$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car")  %>% mutate(scenario="1.0")
Test_cars_NPi_2_em <- filter(Test_cars_NPi_2$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="2.0")
Test_cars_NPi_3_em <- filter(Test_cars_NPi_3$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="3.0")
Test_cars_NPi_5_em <- filter(Test_cars_NPi_5$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="5.0")
Test_cars_NPi_10_em <- filter(Test_cars_NPi_10$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="10")
Test_cars_NPi_15_em <- filter(Test_cars_NPi_15$TransportTravelCO2Emissions, year>=2015, year<=2030, travel_mode=="Car") %>% mutate(scenario="15")
em_cars_NPi <- rbind(Test_cars_NPi_1_em, Test_cars_NPi_2_em) %>% rbind(Test_cars_NPi_3_em) %>% rbind(Test_cars_NPi_5_em) %>% rbind(Test_cars_NPi_10_em) %>% rbind(Test_cars_NPi_15_em)
em_cars_NPi$scenario <- factor(em_cars_NPi$scenario, c("1.0","2.0", "3.0", "5.0", "10", "15"))


########################################################
# COMPARE
# cars
TIMER_model_folder = paste("C:/Users/markr/OneDrive/IMAGE/CD_LINKSupdate/2_TIMER/TIMER_2015_SSPs_ClimatePolicies/ENERGY/ENDATREG/data/endem",sep="/")
Travel_included = read.mym2r.nice(mym.folder=TIMER_model_folder, scen.econ="transport",   
                               filename='trvl_PremFac', varname=NULL, 
                               collist=list(regions26, travel_mode_travel_excl_total, car_type), 
                               namecols=c('region', 'mode', 'travel_type'), novarname = TRUE)

# show heavy truck efficiency per truck type
year_eff_car=2030
d1 <- filter(Test_cars_NoPolicy_1$EfficiencyTravel, region==country_cars, mode=="Car", value<=5) %>% mutate(scenario="NoPolicy")
d2 <- filter(Test_cars_NPi_1$EfficiencyTravel, region==country_cars, mode=="Car", value<=5) %>% mutate(scenario="NPi")
d3 <- rbind(d1,d2)
d4 <- filter(Travel_included, mode=="Car") %>%
      mutate(label=ifelse(value==1000&year==year_eff_car,"excluded", ""))
g<-ggplot() +
  geom_line(data=d3, aes(x=year, y=value, linetype=scenario)) +
  ##geom_point(data=d4, aes(x=year, y=value/1000)) +
  geom_text(data=d4, aes(x=year, y=value/1000,label=label)) +
  facet_wrap(~travel_type) +
  theme_bw()
plot(g)
ggsave(file=paste('graphs/fuel_efficiency/fuel eff cars', country_cars, '.jpg', sep=""), width=20, height=10, dpi=400)

# Visualise efficiency for new cars for different regions (in order)
year_eff_cars=2015
e1 <- filter(Test_cars_NoPolicy_1$EfficiencyFreight, region%in%countries_cars, mode=="Heavy truck", year==year_eff_cars) %>% select(-year)
e2 <- filter(Freight_included, mode=="Heavy truck") %>% select(-year)
e3 <- inner_join(e1,e2, by=c('region', 'travel_type', 'mode')) %>% 
      mutate(efficiency=ifelse(value.y==1000,0,value.x)) %>%
      select(-value.x, -value.y) %>%
      arrange(region, efficiency)
g<-ggplot(data=e3) + 
   geom_bar(aes(x=reorder(travel_type, -efficiency), y=efficiency, fill=region), stat="identity", position="dodge") +  
   facet_wrap(~region) +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90)) +
   ggtitle(paste0("new cars fuel efficiency for ", year_eff_cars))
plot(g)
ggsave(file=paste('graphs/fuel_efficiency/fuel eff cars.jpg', sep=""), width=20, height=10, dpi=400)

# COMPARE efficiency new and existing fleet (NoPolicy)
#eff_cars <- rbind(eff_cars_NoPolicy_new, eff_cars_existing); output = "compare"; scenario = "NoPolicy"
eff_cars <- eff_cars_NoPolicy_new; output = "new"; scenario = "NoPolicy"
#eff_cars <- eff_cars_NoPolicy_existing; output = "existing"; scenario = "NoPolicy"
#eff_cars <- rbind(eff_cars_NPi_new, eff_cars_NPi_existing); output = "compare"; scenario = "NPi"
#eff_cars <- eff_cars_NPi_new; output = "new"; scenario = "NPi"
#eff_cars <- eff_cars_NPi_existing; output = "existing"; scenario = "NPi"

g<-ggplot(data=filter(eff_cars, region%in%countries_cars, scenario%in%c("1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=1) +
  geom_line(aes(x=year, y=value, colour=scenario, linetype=standard))+ 
  facet_wrap(~region, nrow=2)+
  scale_colour_brewer(palette="Dark2") +
  ggtitle(paste0("Fuel efficiency\n",output, " cars fleet (from ", scenario, ")")) +
  ylab(eff_cars$unit) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(0, NA)
f = paste0("graphs/fuel_efficiency/fuel efficiency different countries ", output, " cars ", scenario, ".jpg")
plot(g)
ggsave(f, g)


# COMPARE emission levels cars
em_cars <- em_cars_NoPolicy; scenario="NoPolicy"
scenario = "NoPolicy"
#em_cars <- em_cars_NPi; scenario="NPi"
g<-ggplot(data=filter(em_cars, region%in%countries_cars, scenario%in%c("1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario), size=1) +
  geom_line(aes(x=year, y=value, colour=scenario))+ 
  facet_wrap(~region)+
  scale_colour_brewer(palette="Dark2") +
  ggtitle(paste0("Fuel efficiency\ncars fleet (from ", scenario,")")) +
  ylab(em_cars$unit) +
  theme_bw() +
  ylim(0, NA)
f = paste0("graphs/fuel_efficiency/GHG emissions cars ", scenario, ".jpg")
plot(g)
ggsave(f, g)

# COMPARE efficiency new and existing fleet (NPi)
#eff_cars_NPi <- rbind(eff_cars_NPi_new, eff_cars_NPi_existing)
eff_cars_NPi <- eff_cars_NPi_new
scenario = "NPi"
#eff_cars_NPi <- eff_cars_NPi_existing
g<-ggplot(data=filter(eff_cars_NPi, region%in%countries_cars, scenario%in%c("1.0","2.0", "3.0", "5.0", "10", "15"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=1) +
  geom_line(aes(x=year, y=value, colour=scenario, linetype=standard))+ 
  facet_wrap(~region)+
  scale_colour_brewer(palette="Dark2") +
  ggtitle(paste0("Fuel efficiency new and existing cars fleet (from NPi) ", country_cars))+ 
  theme_bw() +
  ylim(0, NA)
f = paste0("graphs/fuel_efficiency/GHG emissions cars ", scenario, ".jpg")
plot(g)
ggsave(f, g)

