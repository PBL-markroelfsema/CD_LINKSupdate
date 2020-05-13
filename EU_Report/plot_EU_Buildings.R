EU_hist_Buildings <- read.table("../data/IEA_IAMregions.csv", sep=";", header=TRUE) %>% as.data.frame()
EU_hist_Buildings <- filter(EU_hist_Buildings, region%in%c("WEU", "CEU"), variable %in%c('Final Energy|Residential and Commercial')) %>%
  spread(key=region, value=value) %>%
  mutate(value=WEU+CEU) %>%
  select(-WEU, -CEU) %>%
  mutate(source="History Europe (IEA)") %>%
  select(period, value) %>%
  mutate(value=10^3*value, sector="Buildings", Policy="History") %>%
  rename(year=period)


# TIMER scenarios
EU_TIMER_Building_NoPolicy <- filter(NoPolicy$FinalEnergy, region=="EU", year>=2000, year<=2030, sector%in%c('Residential', 'Service'), energy_carrier=="Total") %>% mutate(Policy="NoPolicy (v2015)")
EU_TIMER_Building_NoPolicy_3_2 <- filter(NoPolicy_3_2$FinalEnergy, region=="EU", year>=2000, year<=2030, sector%in%c('Residential', 'Service'), energy_carrier=="Total") %>% mutate(Policy="NoPolicy (v3_2)")
EU_TIMER_Building_NPi <- filter(NPi$FinalEnergy, region=="EU", year>=2000, year<=2030, sector%in%c('Residential', 'Service'), energy_carrier=="Total") %>% mutate(Policy="NPi")
EU_TIMER_Building <- rbind(EU_TIMER_Building_NoPolicy, EU_TIMER_Building_NPi) %>% rbind(EU_TIMER_Building_NoPolicy_3_2) %>% select(-region, -unit, -energy_carrier)
# Add total Buildings sectora and history as scenarios
d_tmp <- group_by(EU_TIMER_Building, year, Policy) %>%
  summarise(value=sum(value)) %>%
  mutate(sector="Buildings") %>% as.data.frame()

EU_TIMER_Building <- rbind(EU_TIMER_Building, d_tmp) %>% rbind(EU_hist_Buildings)

reduction_NoPolicy_NPi_residential <- filter(EU_TIMER_Building, sector=="Residential", year==2030, Policy%in%c('NoPolicy (v2015)', 'NPi')) %>%
  spread(key=Policy, value=value) %>%
  mutate(reduction=100*(`NPi`/`NoPolicy (v2015)`-1))
reduction_NoPolicy_NPi_service <- filter(EU_TIMER_Building, sector=="Service", year==2030, Policy%in%c('NoPolicy (v2015)', 'NPi')) %>%
  spread(key=Policy, value=value) %>%
  mutate(reduction=100*(`NPi`/`NoPolicy (v2015)`-1))

# TIMER scenarios only in target years
EU_Building_target_years <- filter(EU_TIMER_Building, year==2020 | year==2030) %>% 
  mutate(label=ifelse(sector=="Residential" & year==2030 & Policy=="NPi", paste0(round(reduction_NoPolicy_NPi_residential$reduction, 1),"%"), ""))

g_Building_EU <- ggplot() + 
  geom_line(data=EU_TIMER_Building, aes(x=year, y=value, colour=Policy)) + 
  geom_point(data=EU_Building_target_years, aes(x=year, y=value, colour=Policy), size=2) + 
  geom_text(data=EU_Building_target_years, aes(x=year, y=value, label=label), nudge_x=-5, nudge_y=-800) +
  #geom_point(data=EU_Building_Target, aes(x=year, y=value, size=year), colour="brown4") +
  facet_wrap(~sector) +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy (v2015)"="darkgreen", "NoPolicy (v3_2)"="chartreuse", "NPi"="blue"), 
                      labels=c("History"="History IEA", "NoPolicy"="No policy baseline (v2015)", "NPi"="Current policies (v3_2)")) +
  ylim(0, NA) +
  ylab(EU_TIMER_Building$unit[1]) +
  #ggtitle("Buildings") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(reverse=TRUE))