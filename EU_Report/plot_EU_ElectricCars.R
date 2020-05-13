# history
#https://www.eea.europa.eu/data-and-maps/daviz/new-electric-vehicles-in-eu-28#tab-chart_1
EU_hist_cars_electric_new <- read.table("../data/new-electric-vehicles-in-eu-28.csv", sep=",", header=TRUE) %>% as.data.frame()

# Targets
EU_elec_cars_Target_year = c(2025,2030)
EU_elec_cars_Target_value = c(15,35)
EU_elec_cars_target <- data.frame(EU_elec_cars_Target_year, EU_elec_cars_Target_value) %>% 
  rename(year=EU_elec_cars_Target_year, value=EU_elec_cars_Target_value)
pander(EU_CO2_cars_target)

# TIMER scenarios
EU_Transport_cars_elec_share_NoPolicy <- filter(NoPolicy$ElectricShare_new_cars, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy")
EU_Transport_cars_elec_share_NPi <- filter(NPi$ElectricShare_new_cars, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NPi")
EU_Transport_cars_elec_share <- rbind(EU_Transport_cars_elec_share_NoPolicy, EU_Transport_cars_elec_share_NPi) %>% mutate(value=100*value)

# TIMER scenarios only in target years
EU_Transport_cars_target_years_elec_share <- filter(EU_Transport_cars_elec_share, year==2021 | year==2025 | year==2030)

g_electric_new_cars <- ggplot() + 
  geom_line(data=EU_Transport_cars_elec_share, aes(x=year, y=value, colour=Policy)) + 
  geom_point(data=EU_Transport_cars_target_years_elec_share, aes(x=year, y=value, colour=Policy), size=2) + 
  geom_point(data=filter(EU_hist_cars_electric_new, year.year<=2018), aes(x=year.year, y=`Share.of.electric.vehicles.....number`, colour="History")) +
  geom_point(data=EU_elec_cars_target, aes(x=year, y=value, size=year), colour="brown4") +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy"="darkgreen", "NPi"="blue"), 
                      labels=c("History"="History EU28 (EEA)", "NoPolicy"="No policy baseline", "NPi"="Current policies")) +
  #scale_shape_discrete(name="TIMER Car type") +
  scale_size_continuous(name="Electric share target", labels=EU_elec_cars_Target_year, breaks=EU_elec_cars_Target_year, range = c(2,3)) +
  ylim(0, 40) +
  #ylab(EU_Transport_cars_gCO2_km$unit) +
  #ggtitle("EU Share new registration\n electric cars\n in total car fleet") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(reverse=TRUE))