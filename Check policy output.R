library(ggplot2)
source('TIMER_output/functions/mym2r.R')
source('TIMER_output/functions/Settings.R')
source('TIMER_output/functions/General Functions.R')
source('TIMER_output/functions/Import_TIMER_output.R')
source('TIMER_output/functions/Process_TIMER_output.R')

Rundir=paste("~/disks/y/ontwapps/Timer/Users/Mark", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = 'TIMER_2015'
TIMER_folder = paste(Rundir, Project, "2_TIMER/outputlib", TIMERGeneration, Project, sep="/")

# NoPolicy
TIMER_Scenario = "NoPolicy_update"

# Transport
# Travel
eff1_cars_nopol = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                            filename='EnEff1_cars.dat', varname=NULL, 
                            collist=list(regions26,travel_mode_excl_total, car_type), 
                            namecols=c('region','travel_mode_freight', 'car_type'), novarname = TRUE)
eff1_cars_nopol <- mutate(eff1_nopol, unit="T pkm")
eff1_cars_nopol <- mutate(eff1_nopol, var="No Policy")

# Freight
eff1_trucks_nopol = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                                  filename='EnEff1_trucks.dat', varname=NULL, 
                                  collist=list(regions26,travel_mode_freight_excl_total, car_type), 
                                  namecols=c('region','travel_mode_freight', 'car_type'), novarname = TRUE)
eff1_trucks_nopol <- mutate(eff1_trucks_nopol, unit="T pkm")
eff1_trucks_nopol <- mutate(eff1_trucks_nopol, var="No Policy")

# NPi_update_CAFEStandards_cars
TIMER_Scenario <- "NPi_update_CAFEStandards_trucks"

# Travel
# Freight
eff1_trucks_CPS <- read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                             filename='EnEff1_trucks.dat', varname=NULL, 
                             collist=list(regions26,travel_mode_freight_excl_total, car_type), 
                             namecols=c('region','travel_mode_freight', 'car_type'), novarname = TRUE)
eff1_trucks_CPS <- mutate(eff1_trucks_CPS, unit="T pkm")
eff1_trucks_CPS <- mutate(eff1_trucks_CPS, var="CPS")


eff1_trucks_base = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                                   filename='eff1_base_trucks.dat', varname=NULL, 
                                   collist=list(regions26,travel_mode_freight_excl_total, car_type), 
                                   namecols=c('region','travel_mode_freight', 'car_type'), novarname = TRUE)
eff1_trucks_base <- mutate(eff1_trucks_base, unit="T pkm")
eff1_trucks_base <- mutate(eff1_trucks_base, var="base")

eff1_trucks_pol_ST = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                              filename='eff1_pol_ST_trucks.dat', varname=NULL, 
                              collist=list(regions26,travel_mode_excl_total, car_type), 
                              namecols=c('region','travel_mode_freight', 'car_type'), novarname = TRUE)
eff1_trucks_pol_ST <- mutate(eff1_trucks_pol_ST, unit="T pkm")
eff1_trucks_pol_ST <- mutate(eff1_trucks_pol_ST, var="ST")

eff1_trucks_pol_LT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                            filename='eff1_pol_LT_trucks.dat', varname=NULL, 
                            collist=list(regions26,travel_mode_freight_excl_total, car_type), 
                            namecols=c('region','travel_mode_freight', 'car_type'), novarname = TRUE)
eff1_trucks_pol_LT <- mutate(eff1_trucks_pol_LT, unit="T pkm")
eff1_trucks_pol_LT <- mutate(eff1_trucks_pol_LT, var="LT")

eff1_trucks_compare <- rbind(eff1_trucks_nopol, eff1_trucks_CPS)
eff1_trucks_compare2 <- inner_join(eff1_trucks_nopol, eff1_trucks_CPS, by=c('year', 'region', 'travel_mode_freight', 'car_type'))
eff1_trucks_CAFE <- rbind(eff1_trucks_base, eff1_trucks_pol_ST) %>% rbind(eff1_trucks_pol_LT)

region_selec = "USA"

eff1_trucks_compare_plot1 <- filter(eff1_trucks_compare, region==region_selec, travel_mode_freight=="Medium truck", car_type==car_type[1], year<=2050)
ggplot() + geom_point(data=eff1_trucks_compare_plot1, mapping=aes(x=year, y=value, colour=var, shape=var))
ggplot() + geom_point(data=eff1_trucks_compare_plot1, mapping=aes(x=year, y=value)) + facet_wrap(~var)

eff1_trucks_compare_plot2 <- filter(eff1_trucks_compare, region==region_selec, travel_mode_freight=="Medium truck", year<=2050)
ggplot() + geom_point(data=eff1_trucks_compare_plot2, mapping=aes(x=year, y=value, colour=var, shape=var))
ggplot() + geom_point(data=eff1_trucks_compare_plot2, mapping=aes(x=year, y=value, colour=var)) + facet_wrap(~car_type)

eff1_trucks_CPS_plot <- filter(eff1_trucks_CPS, region==region_selec, travel_mode=="Car", car_type=="ICE2000", year<=2050)
ggplot(data=eff1_trucks_CPS_plot, aes(year, value, colour=var, shape=var)) + 
      geom_point(alpha=1/3) + 
      geom_line()
ggplot() + geom_point(data=eff1_trucks_CPS_plot, mapping=aes(x=year, y=value, colour=var)) + facet_wrap(~var, nrow=3)

# NPi_update_CAFEStandards_trucks
TIMER_Scenario = "NPi_update_CAFEStandards_trucks"
eff1_trucks_pol_ST = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                              filename='eff1_pol_ST_trucks.dat', varname=NULL, 
                              collist=list(regions26,travel_mode_freight_excl_total, car_type), 
                              namecols=c('region','travel_mode_freight', 'car_type'), novarname = TRUE)
eff1_trucks_pol_ST <- mutate(eff1_trucks_pol_ST, unit="T pkm")
eff1_trucks_pol_ST <- mutate(eff1_trucks_pol_ST, var="ST")

eff1_trucks_pol_LT = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                              filename='eff1_pol_LT_trucks.dat', varname=NULL, 
                              collist=list(regions26,travel_mode_freight_excl_total, car_type), 
                              namecols=c('region','travel_mode_freight', 'car_type'), novarname = TRUE)
eff1_trucks_pol_LT <- mutate(eff1_trucks_pol_LT, unit="T pkm")
eff1_trucks_pol_LT <- mutate(eff1_trucks_pol_LT, var="LT")

eff1_trucks_compare <- rbind(eff1_trucks_nopol, eff1_trucks_CPS)
eff1_trucks_CAFE <- rbind(eff1_trucks_base, eff1_trucks_pol_ST) #%>% rbind(eff1_trucks_pol_LT)

region_selec = "USA"

eff1_trucks_compare_plot <- filter(eff1_trucks_compare, region==region_selec, travel_mode_freight=="Heavy truck", car_type=="ICE2010", year<=2050)
ggplot() + geom_point(data=eff1_trucks_compare_plot, mapping=aes(x=year, y=value, colour=var, shape=var))
ggplot() + geom_point(data=eff1_trucks_compare_plot, mapping=aes(x=year, y=value)) + facet_wrap(~var)

eff1_trucks_CAFE_plot <- filter(eff1_trucks_CAFE, region==region_selec, travel_mode_freight=="Heavy truck", car_type==car_type[12], year<=2050)
ggplot() + geom_point(data=eff1_trucks_CAFE_plot, mapping=aes(x=year, y=value, colour=var)) + facet_wrap(~var, nrow=3)
ggplot() + geom_point(data=eff1_trucks_CAFE_plot, mapping=aes(x=year, y=value, colour=var, shape=var, size=var))

eff1_trucks_CAFE_plot <- filter(eff1_trucks_CAFE, region==region_selec, travel_mode_freight=="Heavy truck", year>=2010, year<=2050)
ggplot() + geom_point(data=eff1_trucks_CAFE_plot, mapping=aes(x=year, y=value, colour=var)) + facet_wrap(~car_type, nrow=3)


# TRAVEL
# Efficiency cars
TIMER_Scenario <- "NoPolicy_update"
eff1_new_cars_nopol = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                                  filename='Eff_cars.dat', varname=NULL, 
                                  collist=list(regions26), 
                                  namecols=c('region'), novarname = TRUE)
eff1_new_cars_nopol <- mutate(eff1_new_cars_nopol, unit="T pkm")
eff1_new_cars_nopol <- mutate(eff1_new_cars_nopol, var="No Policy")
TIMER_Scenario <- "NPi_update_CAFEStandards_cars"
eff1_new_cars_CAFE = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                                      filename='Eff_cars.dat', varname=NULL, 
                                      collist=list(regions26), 
                                      namecols=c('region'), novarname = TRUE)
eff1_new_cars_CAFE <- mutate(eff1_new_cars_CAFE, unit="T pkm")
eff1_new_cars_CAFE <- mutate(eff1_new_cars_CAFE, var="CAFE")
TIMER_Scenario <- "NPi_update_CAFEStandards_cars_alt"
eff1_new_cars_CAFE_alt = read.mym2r.nice(mym.folder=TIMER_folder, scen.econ=paste(TIMER_Scenario, "/policy", sep=""), 
                                    filename='Eff_cars.dat', varname=NULL, 
                                    collist=list(regions26), 
                                    namecols=c('region'), novarname = TRUE)
eff1_new_cars_CAFE_alt <- mutate(eff1_new_cars_CAFE_alt, unit="T pkm")
eff1_new_cars_CAFE_alt <- mutate(eff1_new_cars_CAFE_alt, var="CAFE_alt")

eff1_new_cars_compare <- rbind(eff1_new_cars_nopol, eff1_new_cars_CAFE, eff1_new_cars_CAFE_alt)
eff1_new_cars_join <- inner_join(eff1_new_cars_nopol, eff1_new_cars_CAFE, by=c('year', 'region'))
eff1_new_cars_join <- inner_join(eff1_new_cars_join, eff1_new_cars_CAFE_alt, by=c('year', 'region'))
eff1_new_cars_CAFE_join <- inner_join(eff1_new_cars_CAFE, eff1_new_cars_CAFE_alt, by=c('year', 'region'))

select_regions = c("CAN", "USA","MEX", "BRA","EU","TUR", "UKR", "RUS","INDIA","KOR", "CHN","INDO", "JAP","World")
eff1_new_cars_compare_plot <- filter(eff1_new_cars_compare, year>=2010, year<=2050, region %in% select_regions)
ggplot() + geom_line(data=eff1_new_cars_compare_plot, mapping=aes(x=year, y=value, colour=var)) +
           facet_wrap(~region) +
           ylim(0,2.5)
ggplot() + geom_line(data=filter(eff1_new_cars_compare_plot, region=="USA"), mapping=aes(x=year, y=value, colour=var)) +
  facet_wrap(~region) +
  ylim(0,2.5)
