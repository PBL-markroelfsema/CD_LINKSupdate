
# Targets
EU_CO2_HvyT_Target_base = filter(NoPolicyi$CO2_intensity_fleet_new_HvyT_tailpipe_total, region=="EU", year==2021)
EU_CO2_HvyT_Target_year = c(2025,2030)
EU_CO2_HvyT_Target_value = c((1-0.15)*EU_CO2_HvyT_Target_base$value, (1-0.3)*EU_CO2_HvyT_Target_base$value)
EU_CO2_HvyT_target <- data.frame(EU_CO2_HvyT_Target_year, EU_CO2_HvyT_Target_value) %>% 
  rename(year=EU_CO2_HvyT_Target_year, value=EU_CO2_HvyT_Target_value)

# TIMER
EU_TIMER_Transport_HvyT_Eff_new_NoPolicy <- filter(NoPolicyi$CO2_intensity_fleet_new_HvyT_tailpipe_total,region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy")
EU_TIMER_Transport_HvyT_Eff_new_NPi <- filter(NPii$CO2_intensity_fleet_new_HvyT_tailpipe_total, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NPi")
EU_TIMER_Transport_HvyT_Eff_new <- rbind(EU_TIMER_Transport_HvyT_Eff_new_NoPolicy, EU_TIMER_Transport_HvyT_Eff_new_NPi)

# TIMER scenarios only in target years
EU_TIMER_Transport_HvyT_target_years <- filter(EU_TIMER_Transport_HvyT_Eff_new, year==2025 | year==2030)

g_HvyT <- ggplot(data=EU_TIMER_Transport_HvyT_Eff_new) + geom_line(aes(x=year, y=value, colour=Policy)) +
  geom_point(data=EU_TIMER_Transport_HvyT_target_years, aes(x=year, y=value, colour=Policy), size=2) + 
  geom_point(data=EU_CO2_HvyT_target, aes(x=year, y=value, size=year), colour="brown4") +
  scale_colour_manual(name="Scenario", values=c("NoPolicy"="darkgreen", "NPi"="blue"), 
                      labels=c("NoPolicy"="No policy baseline", "NPi"="Current policies")) +
  scale_size_continuous(name="CO2 performance standard", labels=EU_CO2_HvyT_Target_year, breaks=EU_CO2_HvyT_Target_year, range = c(2,3)) +
  theme_bw()