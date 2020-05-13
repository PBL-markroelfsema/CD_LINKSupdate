
# --------- 2 sensitivity runs for fuel efficiency cars and heavy trucks
#           2a different paramters (energy tax, factor)
#           2b sensivity fuel efficiency cars and heavy trucks from NoPolicy and NPi

library(dplyr)
library(tidyverse)
library(data.table)

#setwd("C:/Users/markr/OneDrive/IMAGE")
source("../TIMER_output/functions/Settings.R")
source("../TIMER_output/functions/General Functions.R")
source("../TIMER_output/functions/Import_TIMER_output.R")
source("../TIMER_output/functions/Process_TIMER_output.R")
source("../TIMER_output/functions/pbl_colors.R")
#source("Settings_indicators.R")


Rundir=paste("C:/Users/markr/OneDrive/IMAGE", sep="")
#Rundir=paste("C:/Users/Roelf003/OneDrive/IMAGE", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = "TIMER_2015"
RDir = "6_R"

NoPolicy   <- ImportTimerScenario('NoPolicy_2020','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
NoPolicyi   <- ProcessTimerScenario(NoPolicy, Rundir, Project, RDir, Policy=TRUE)
NPi        <- ImportTimerScenario('NPi_update_2020','NoPolicy', Rundir, Project, TIMERGeneration, RDir, Policy=TRUE)
NPii        <- ProcessTimerScenario(NPi, Rundir, Project, RDir, Policy=TRUE)


# CAFE STANDARDS

# Show eff existing cars, eff new cars, co2 intensity and emissions for cars
# for one country
cntr_cars = "EU"
v = c("Efficiency new cars", "Efficiency new cars (excl EV)", "Efficiency existing cars", "CO2 intensity cars", "CO2 emissions cars")
x1 <- mutate(NoPolicy_eff_cars_existing, variable="Efficiency existing cars")
x2 <- mutate(NoPolicy_eff_cars_new, variable="Efficiency new cars")
x3 <- mutate(NoPolicy_eff_cars_new_excl_EV, variable="Efficiency new cars (excl EV)")
x4 <- mutate(NoPolicy_emint_cars, variable="CO2 intensity cars")
x5 <- mutate(NoPolicy_em_cars, variable="CO2 emissions cars")
x6 <- mutate(NPi_eff_cars_existing, variable="Efficiency existing cars")
x7 <- mutate(NPi_eff_cars_new, variable="Efficiency new cars")
x8 <- mutate(NPi_eff_cars_new_excl_EV, variable="Efficiency new cars (excl EV)")
x9 <- mutate(NPi_emint_cars, variable="CO2 intensity cars")
x10 <- mutate(NPi_em_cars, variable="CO2 emissions cars")
data_graph_cars <- rbind(x1, x2) %>% rbind(x3) %>% rbind(x4) %>% rbind(x5) %>% rbind(x6) %>% rbind(x7) %>% rbind(x8) %>% rbind(x9) %>% rbind(x10)
data_graph_cars$variable <- factor(data_graph_cars$variable, levels=v)
data_graph_cars <- arrange(data_graph_cars, variable, scenario)
g<-ggplot(data=filter(data_graph_cars, region%in%cntr_cars, scenario%in%c("NoPolicy", "NPi"))) +
  geom_point(aes(x=year, y=value, colour=scenario), size=1) +
  geom_line(aes(x=year, y=value, colour=scenario))+ 
  facet_wrap(~variable, nrow=2, scale="free")+
  scale_colour_brewer(palette="Dark2") +
  ggtitle(paste0("Cars ", cntr_cars)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(0, NA)
plot(g)
f = paste0("graphs/fuel_efficiency/cars ", cntr_cars)
ggsave(f, g)

#__________________________________________________________________

# HEAVY TRUCKS
#("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","World")
countries_eff_HvyT = c("CAN","USA","EU", "CHN")

# efficiency existing fleet for heavy trucks
NoPolicy_eff_HvyT_existing <- filter(NoPolicy$EfficiencyFleet_HvyT, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NoPolicy", standard="existing")
NPi_eff_HvyT_existing <- filter(NPi$EfficiencyFleet_HvyT, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NPi", standard="existing")

# emissions for heavy trucks
NoPolicy_em_HvyT <- filter(NoPolicy$TransportFreightCO2Emissions, year>=start_year_eff_cars, year<=2030, travel_mode=="Heavy truck")  %>% mutate(scenario="NoPolicy")
NPi_em_HvyT <- filter(NPi$TransportFreightCO2Emissions, year>=start_year_eff_cars, year<=2030, travel_mode=="Heavy truck")  %>% mutate(scenario="NPi")

# COMPARE efficiency existing HvyT fleet
eff_HvyT_scenarios <- rbind(NoPolicy_eff_HvyT_existing, NPi_eff_HvyT_existing); output = "compare"
eff_HvyT_2020_2030 <- filter(eff_HvyT_scenarios, year>=2020, year<=2030, region%in%countries_eff_HvyT)
g<-ggplot(data=filter(eff_HvyT_scenarios, region%in%countries_eff_HvyT, scenario%in%c("NoPolicy", "NPi"))) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=standard), size=1) +
  geom_line(aes(x=year, y=value, colour=scenario, linetype=standard))+ 
  scale_colour_brewer(palette="Dark2") +
  ggtitle(paste0("Fuel efficiency\n",output, " HvyT fleet (from ", scenario, ")")) +
  ylab(eff_HvyT$unit) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(0, NA)
f = paste0("graphs/fuel_efficiency/fuel efficiency different countries and scenarios ", output, " heavy trucks ", scenario, ".jpg")
plot(g)
ggsave(f, g)

# BIOFUEL TARGETS
start_year_bio_cars = 2010
# CARS
#("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","World")
countries_bio_cars = c("USA","EU", "MEX")

# biofuel share for cars
NoPolicy_biofuelshare_cars_existing <- filter(NoPolicy$BiofuelShare_existing_cars, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NoPolicy")
NPi_eff_biofuelshare_cars_existing <- filter(NPi$BiofuelShare_existing_cars, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NPi")

# biofuel share for new cars
NoPolicy_biofuelshare_cars_new <- filter(NoPolicy$BiofuelShare_new_cars, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NoPolicy")
NPi_eff_biofuelshare_cars_new <- filter(NPi$BiofuelShare_new_cars, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NPi")

# blending share for cars
NoPolicy_blendingshare_cars_existing <- filter(NoPolicy$BlendingShareBio_cars_pkm, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NoPolicy")
NPi_eff_blendingshare_cars_existing <- filter(NPi$BlendingShareBio_cars_pkm, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NPi")

# blending share for new cars
NoPolicy_blendinghare_cars_new <- filter(NoPolicy$BlendingShareBio_new_cars_pkm, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NoPolicy")
NPi_eff_blendingshare_cars_new <- filter(NPi$BlendingShareBio_new_cars_pkm, year>=start_year_eff_cars, year<=2030) %>% mutate(scenario="NPi")

# COMPARE biofuels targets for new and existing cars
bio_cars_existing_scenarios <- rbind(NoPolicy_biofuelshare_cars_existing, NPi_eff_biofuelshare_cars_existing) %>% mutate(type='existing')
bio_cars_new_scenarios <- rbind(NoPolicy_biofuelshare_cars_new, NPi_eff_biofuelshare_cars_new) %>% mutate(type='new')
blending_cars_existing_scenarios <- rbind(NoPolicy_blendingshare_cars_existing, NPi_eff_blendingshare_cars_existing) %>% mutate(type='existing')
blending_cars_new_scenarios <- rbind(NoPolicy_blendinghare_cars_new, NPi_eff_blendingshare_cars_new) %>% mutate(type='new')

#data_bio_scenarios <- rbind(bio_cars_existing_scenarios, bio_cars_new_scenarios); variable = "biofuel standard"
data_bio_scenarios <- rbind(blending_cars_existing_scenarios, blending_cars_new_scenarios); variable = "biofuel blending share"
d1=filter(data_bio_scenarios, region%in%countries_bio_cars, scenario%in%c("NoPolicy", "NPi"))
d2=filter(data_bio_scenarios, region%in%countries_bio_cars, scenario%in%c("NoPolicy", "NPi"), type=='existing')
g<-ggplot() +
  geom_point(data=d1, aes(x=year, y=value, colour=type, shape=scenario), size=2) +
  #geom_line(data=d2, aes(x=year, y=value, shape=scenario))+ 
  facet_wrap(~region, nrow=2)+
  scale_colour_brewer(palette="Dark2") +
  ggtitle(paste0(variable, "\ncar Biofuel standards")) +
  ylab(data_bio_scenarios$unit) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(type~region, ncol=3) +
  ylim(0, NA)
f = paste0("graphs/fuel_efficiency/", variable, " different countries and scenarios ", output, " cars.jpg")
plot(g)
ggsave(f, g)
