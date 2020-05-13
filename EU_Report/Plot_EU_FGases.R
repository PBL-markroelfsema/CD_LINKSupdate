d_EU_FGas_EU28_hist <- filter(GHG_emissions_UNFCCC_TIMER, Region%in%c(11,12), Gas=="Aggregate F-gases", Sector=="Total") %>% select(-Sector) %>%
  spread(key=Region, value=Value) %>%
  mutate(Value=`11` +`12`) %>%
  select(-`11`,-`12`)

# Targets
EU_FGas_Target_base_2020 = filter(d_EU_FGas_EU28_hist, Year==2015)$Value
EU_FGas_Target_base_2030 = filter(d_EU_FGas_EU28_hist, Year==2014)$Value
EU_FGas_Target_year = c(2020,2030)
EU_FGas_Target_value = c((1-37/100)*EU_FGas_Target_base_2020,(1-2/3)*EU_FGas_Target_base_2030)
EU_FGas_Target <- data.frame(EU_FGas_Target_year, EU_FGas_Target_value) %>% 
  rename(year=EU_FGas_Target_year, value=EU_FGas_Target_value)
#pander(EU_FGas_target)

# TIMER scenarios
EU_TIMER_FGas_NoPolicy <- filter(NoPolicyi$FGases, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy (v2015)")
EU_TIMER_FGas_NoPolicy_3_2 <- filter(NoPolicy_3_2i$FGases, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy (v3_2)")
EU_TIMER_FGas_NPi <- filter(NPii$FGases, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NPi")
EU_TIMER_FGas <- rbind(EU_TIMER_FGas_NoPolicy, EU_TIMER_FGas_NPi) %>% rbind(EU_TIMER_FGas_NoPolicy_3_2) %>% select(-region, -unit)

# add history as scenario
d_tmp <- select(d_EU_FGas_EU28_hist, Year, Value) %>% 
  rename(value=Value, year=Year) %>% 
  mutate(Policy="History")
EU_TIMER_FGas <- rbind(EU_TIMER_FGas, d_tmp)

# TIMER scenarios only in target years
EU_FGas_target_years <- filter(EU_TIMER_FGas, year==2020 | year==2030)

g_FGas_EU <- ggplot() + 
  geom_line(data=EU_TIMER_FGas, aes(x=year, y=value, colour=Policy)) + 
  geom_point(data=EU_FGas_target_years, aes(x=year, y=value, colour=Policy), size=2) + 
  #geom_point(data=d_EU_FGas_EU28_hist, aes(x=Year, y=Value, linetype=Source)) +
  geom_point(data=EU_FGas_Target, aes(x=year, y=value, size=year), colour="brown4") +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy (v2015)"="darkgreen", "NoPolicy (v3_2)"="chartreuse", "NPi"="blue"), 
                      labels=c("History"="History Eureope (UNFCCC)", "NoPolicy"="No policy baseline (v2015)", "NPi"="CurFGast policies (v3_2)")) +
  scale_size_continuous(name="FGas targets", labels=EU_FGas_Target_year, breaks=EU_FGas_Target_year, range = c(2,3)) +
  ylim(0, NA) +
  ylab(EU_TIMER_FGas$unit[1]) +
  #ggtitle("Fluorinated gases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(reverse=TRUE))