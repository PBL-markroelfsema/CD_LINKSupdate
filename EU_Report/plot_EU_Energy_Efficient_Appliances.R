#https://www.eea.europa.eu/data-and-maps/daviz/energy-consumption-by-end-uses-3#tab-chart_1
#EU_hist_Appliances <- read.table("data/energy-consumption-by-end-uses-3.csv", sep=",", header=TRUE) %>% as.data.frame()
#EU_hist_Appliances <- gather(2:ncol(EU_hist_Appliances), key="Country", value=value)
#EU_hist_Appliances <- filter(EU_hist_Appliances, region%in%c("EU"), variable %in%c('Final Energy|Residential and Commercial')) %>%
#  spread(key=region, value=value) %>%
# mutate(value=WEU+CEU) %>%
#  select(-WEU, -CEU) %>%
#  mutate(source="History Europe (IEA)") %>%
# select(period, value) %>%
# mutate(value=10^3*value, sector="Buildings", Policy="History") %>%
#  rename(year=period)



# TIMER scenarios
EU_TIMER_Appliances_NoPolicy <- filter(NoPolicy$FinalEnergy_Residential_Appliances, region=="EU", year>=2000, year<=2030, population_group=="Total" , appliance=="Total") %>% mutate(Policy="NoPolicy (v2015)")
EU_TIMER_Appliances_NoPolicy_3_2 <- filter(NoPolicy_3_2$FinalEnergy_Residential_Appliances, region=="EU", year>=2000, year<=2030, population_group=="Total", appliance=="Total") %>% mutate(Policy="NoPolicy (v3_2)")
EU_TIMER_Appliances_NPi <- filter(NPi$FinalEnergy_Residential_Appliances, region=="EU", year>=2000, year<=2030, population_group=="Total", appliance=="Total") %>% mutate(Policy="NPi")
EU_TIMER_Appliances <- rbind(EU_TIMER_Appliances_NoPolicy, EU_TIMER_Appliances_NPi) %>% rbind(EU_TIMER_Appliances_NoPolicy_3_2) %>% select(-region, -unit, -appliance)

# TIMER scenarios only in target years
EU_Appliances_target_years <- filter(EU_TIMER_Appliances, year==2020 | year==2030)

g_Appliances_EU <- ggplot() + 
  geom_line(data=EU_TIMER_Appliances, aes(x=year, y=value, colour=Policy)) + 
  geom_point(data=EU_Appliances_target_years, aes(x=year, y=value, colour=Policy), size=2) + 
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy (v2015)"="darkgreen", "NoPolicy (v3_2)"="chartreuse", "NPi"="blue"), 
                      labels=c("History"="History IEA", "NoPolicy"="No policy baseline (v2015)", "NPi"="Current policies (v3_2)")) +
  ylim(0, NA) +
  ylab(EU_TIMER_Appliances$unit[1]) +
  #ggtitle("Buildings") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(reverse=TRUE))
