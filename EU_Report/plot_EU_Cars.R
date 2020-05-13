# history
# EEA
# https://www.eea.europa.eu/data-and-maps/daviz/average-emissions-for-new-cars-5#tab-chart_1
EU_hist_cars_CO2_pkm_new_EEA <- read.table("../data/average-emissions-for-new-cars-5.csv", sep=";", header=TRUE) %>% as.data.frame()
EU_hist_cars_CO2_pkm_new_EEA <- select(EU_hist_cars_CO2_pkm_new_EEA, year.year,Average.CO2.emissions.from.new.passenger.cars.number) %>%
                                rename(Year=year.year, Value=Average.CO2.emissions.from.new.passenger.cars.number) %>%
                                mutate(Source="EEA")

# Odysee-MURE database
# https://www.indicators.odyssee-mure.eu/energy-indicators/road-transport-new-cars.html
EU_hist_cars_l_100km_Odys <- read_excel(path="../data/Specific-consumption-of-new-cars-test-value.xlsx", sheet="Worksheet", range="A2:U34", col_names=TRUE) %>% as.data.frame()
colnames(EU_hist_cars_l_100km_Odys)[1] <- c('country_region') 
EU_hist_cars_l_100km_Odys <- gather(EU_hist_cars_l_100km_Odys, 4:ncol(EU_hist_cars_l_100km_Odys), key="Year", value="Value")
EU_hist_cars_l_100km_Odys$Value <- as.numeric(EU_hist_cars_l_100km_Odys$Value)
EU_hist_cars_l_100km_Odys$Year <- as.integer(EU_hist_cars_l_100km_Odys$Year)
EU_hist_cars_CO2_pkm_new_Oday <- filter(EU_hist_cars_l_100km_Odys, country_region=="European Union") %>%
                                 select(-country_region, -Unit, -Source) %>%
                                  mutate(Value=replace(Value, Year>1850, (co2_intensity_fuel*10^3)/(100/Value)), Source="Odyssee")

EU_hist_cars_CO2_pkm_new <- rbind(EU_hist_cars_CO2_pkm_new_EEA,EU_hist_cars_CO2_pkm_new_Oday)

# Targets
EU_CO2_cars_Target_year = c(2021,2025,2030)
EU_CO2_cars_Target_value = c(95,81,59)
EU_CO2_cars_target <- data.frame(EU_CO2_cars_Target_year, EU_CO2_cars_Target_value) %>% 
  rename(year=EU_CO2_cars_Target_year, value=EU_CO2_cars_Target_value)


# TIMER scenarios
EU_Transport_cars_gCO2_km_NoPolicy <- filter(NoPolicyi$CO2_intensity_fleet_new_cars_tailpipe, carrier=="total", region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy")
EU_Transport_cars_gCO2_km_NPi <- filter(NPii$CO2_intensity_fleet_new_cars_tailpipe, carrier=="total", region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NPi")
EU_Transport_cars_gCO2_km <- rbind(EU_Transport_cars_gCO2_km_NoPolicy, EU_Transport_cars_gCO2_km_NPi) %>% select(-carrier) #%>%

# TIMER scenarios only in target years
EU_Transport_cars_target_years_gCO2_km <- filter(EU_Transport_cars_gCO2_km, year==2021 | year==2025 | year==2030)


# Efficiency for individual cars in TIMER --> translated to gCO2/km
EU_Transport_cars_efficiency_TIMER_MJ_pkm <- filter(NoPolicy$EfficiencyTravel, region%in%c('WEU', 'CEU'), mode=="Car", year>=2000, year<=2030) %>% 
  cbind(as.data.frame(car_type_included)) %>%
  filter(car_type_included==TRUE)
EU_Transport_cars_efficiency_TIMER_km_l <- EU_Transport_cars_efficiency_TIMER_MJ_pkm %>%
  mutate(value=replace(value, value >0, energy_intensity_fuel/(value*Load_car))) %>%
  mutate(unit=replace(unit, mode=="Car", "km/l"))
EU_Transport_cars_efficiency_TIMER_gCO2_km <- EU_Transport_cars_efficiency_TIMER_km_l %>%
  mutate(value=replace(value, value >0, co2_intensity_fuel*10^3/value)) %>%
  mutate(unit=replace(unit, mode=="Car", "gCO2/km"))

g_co2_new_cars <- ggplot() + 
  geom_line(data=EU_Transport_cars_gCO2_km, aes(x=year, y=value, colour=Policy)) + 
  geom_point(data=EU_Transport_cars_target_years_gCO2_km, aes(x=year, y=value, colour=Policy), size=2) + 
  geom_point(data=filter(EU_hist_cars_CO2_pkm_new, Year<=2018), aes(x=Year, y=Value, colour=Source)) +
  geom_point(data=filter(EU_Transport_cars_efficiency_TIMER_gCO2_km, region=="WEU", travel_type%in%c("ICE2000", "ICE Diesel Oil")), aes(x=year, y=value, shape=travel_type), alpha=0.5, size=1,                  colour="cornflowerblue") +
  geom_point(data=EU_CO2_cars_target, aes(x=year, y=value, size=year), colour="brown4") +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("EEA"="black", "Odyssee"="grey", "NoPolicy"="darkgreen", "NPi"="blue"), 
                      breaks=c("EEA", "Odyssee", "NoPolicy", "NPi"),
                      labels=c("EEA"="History EU28 (EEA)", "Odyssee"="History E28 (Odyssee-MURE)", "NoPolicy"="No policy baseline", "NPi"="Current policies")) +
  scale_shape_discrete(name="TIMER Car type", labels=c("ICE2000"="gasoline", "ICE Diesel Oil"="diesel")) +
  scale_size_continuous(name="CO2 performance standard", labels=EU_CO2_cars_Target_year, breaks=EU_CO2_cars_Target_year, range = c(2,3)) +
  ylim(0, NA) +
  ylab(EU_Transport_cars_gCO2_km$unit) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(reverse=TRUE))
