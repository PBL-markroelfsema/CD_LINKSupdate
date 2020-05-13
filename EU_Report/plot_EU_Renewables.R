# Is there a difference in (gross) final energy between EEA and IEA?
# https://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Final_energy_consumption

#https://www.eea.europa.eu/data-and-maps/data/approximated-estimates-for-the-share-1
EU_hist_REN_EEA <- read.table("../data/nrg_ind_ren_1_Data_final.csv", sep=",", header=TRUE) %>% as.data.frame()
EU_hist_REN_final_EEA <- filter(EU_hist_REN_EEA, GEO=="European Union - 27 countries (from 2020)") %>% mutate(Source="EEA", Unit="%") %>%
                         select(TIME, Value, Unit, Source) %>%
                         rename(Year=TIME) %>%
                         mutate(Variable="Final Energy")
EU_hist_REN_TPES_EEA <- filter(EU_hist_REN_EEA, GEO=="European Union - 27 countries (from 2020)") %>% mutate(Source="EEA", Unit="%") %>%
                         select(TIME, Value, Unit, Source) %>%
                         rename(Year=TIME) %>%
                         mutate(Variable="TPES")

data_IEA <- read.table("../data/IEA_IAMregions.csv", sep=";", header=TRUE) %>% as.data.frame()
EU_hist_REN_final_IEA <- filter(data_IEA, variable=="Final Energy|Renewable share", region=="EU") %>% #EU28, Europe is not available, must be calculated together with final energy
                         select(period, value, unit) %>%
                         rename(Year=period, Value=value, Unit=unit) %>%
                         mutate(Source="IEA", Variable="Final Energy")
EU_hist_REN_TPES_IEA <- filter(data_IEA, variable=="Primary Energy|Renewable share", region=="EU") %>% #EU28, Europe is not available, must be calculated together with final energy
                         select(period, value, unit) %>%
                         rename(Year=period, Value=value, Unit=unit) %>%
                         mutate(Source="IEA", Variable="TPES")
EU_hist_REN <- rbind(EU_hist_REN_final_EEA,EU_hist_REN_TPES_EEA) %>% rbind(EU_hist_REN_final_IEA) %>% rbind(EU_hist_REN_TPES_IEA)

# Targets
EU_REN_Target_year = c(2020,2030)
EU_REN_Target_value = c(20,32)
EU_REN_Target <- data.frame(EU_REN_Target_year, EU_REN_Target_value) %>% 
                       rename(year=EU_REN_Target_year, value=EU_REN_Target_value)
#pander(EU_REN_final_target)

# TIMER scenarios
EU_TIMER_REN_TPES_NoPolicy <- filter(NoPolicyi$RenTPESShare, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy (v2015)") %>% mutate(variable="TPES")
EU_TIMER_REN_TPES_NoPolicy_3_2 <- filter(NoPolicy_3_2i$RenTPESShare, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy (v3_2)") %>% mutate(variable="TPES")
EU_TIMER_REN_TPES_NPi <- filter(NPii$RenTPESShare, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NPi") %>% mutate(variable="TPES")
EU_TIMER_REN_TPES <- rbind(EU_TIMER_REN_TPES_NoPolicy, EU_TIMER_REN_TPES_NPi) %>% rbind(EU_TIMER_REN_TPES_NoPolicy_3_2) %>% select(-region) %>% mutate(source="TIMER model")

EU_TIMER_REN_final_NoPolicy <- filter(NoPolicyi$RENfinalenergyshare, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy (v2015)") %>% mutate(variable="Final Energy")
EU_TIMER_REN_final_NoPolicy_3_2 <- filter(NoPolicy_3_2i$RENfinalenergyshare, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy (v3_2)") %>% mutate(variable="Final Energy")
EU_TIMER_REN_final_NPi <- filter(NPii$RENfinalenergyshare, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NPi") %>% mutate(variable="Final Energy")
EU_TIMER_REN_final <- rbind(EU_TIMER_REN_final_NoPolicy, EU_TIMER_REN_final_NPi) %>% rbind(EU_TIMER_REN_final_NoPolicy_3_2) %>% select(-region) %>% mutate(source="TIMER model")

EU_TIMER_REN <- rbind(EU_TIMER_REN_final, EU_TIMER_REN_TPES)
EU_TIMER_REN$year <- as.integer(EU_TIMER_REN$year)

# add history as scenario
d_tmp <- select(EU_hist_REN, Year, Value, Unit, Source, Variable) %>% rename(year=Year, value=Value, unit=Unit, source=Source, variable=Variable) %>% mutate(Policy="History")
EU_TIMER_REN <- rbind(EU_TIMER_REN, d_tmp)

# TIMER scenarios only in target years
EU_REN_target_years <- filter(EU_TIMER_REN, year==2020 | year==2030)

g_REN_final_EU <- ggplot() + 
  geom_line(data=filter(EU_TIMER_REN,variable=="Final Energy"), aes(x=year, y=value, colour=Policy, linetype=source)) +
  #geom_line(data=EU_hist_REN_final, aes(x=Year, y=Value, linetype=Source)) + 
  geom_point(data=filter(EU_REN_target_years,variable=="Final Energy"), aes(x=year, y=value, colour=Policy), size=2) + 
  #geom_point(data=filter(EU_hist_REN_final, TIME<=2018), aes(x=TIME, y=Value, linetype=Source)) +
  geom_point(data=EU_REN_Target, aes(x=year, y=value, size=year), colour="brown4") +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy (v2015)"="darkgreen", "NoPolicy (v3_2)"="chartreuse", "NPi"="blue"), 
                      labels=c("History"="History EU28 (EEA)", "NoPolicy"="No policy baseline (v2015)", "NPi"="Current policies")) +
  scale_size_continuous(name="Renewable energy target", labels=EU_REN_Target_year, breaks=EU_REN_Target_year, range = c(2,3)) +
  scale_linetype(name="Source") +
  ylim(0, 40) +
  ylab(EU_TIMER_REN$unit[1]) +
  ggtitle("Renewable share\nfinal energy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(reverse=TRUE))
g_REN_TPES_EU <- ggplot() + 
  geom_line(data=filter(EU_TIMER_REN,variable=="TPES"), aes(x=year, y=value, colour=Policy, linetype=source)) +
  #geom_line(data=EU_hist_REN_final, aes(x=Year, y=Value, linetype=Source)) + 
  geom_point(data=filter(EU_REN_target_years, variable=="TPES"), aes(x=year, y=value, colour=Policy), size=2) + 
  #geom_point(data=filter(EU_hist_REN_final, TIME<=2018), aes(x=TIME, y=Value, linetype=Source)) +
  geom_point(data=EU_REN_Target, aes(x=year, y=value, size=year), colour="brown4") +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy (v2015)"="darkgreen", "NoPolicy (v3_2)"="chartreuse", "NPi"="blue"), 
                      labels=c("History"="History EU28 (EEA)", "NoPolicy"="No policy baseline (v2015)", "NPi"="Current policies")) +
  scale_size_continuous(name="Renewable energy target", labels=EU_REN_Target_year, breaks=EU_REN_Target_year, range = c(2,3)) +
  scale_linetype(name="Source") +
  ylim(0, 40) +
  ylab(EU_TIMER_REN$unit[1]) +
  ggtitle("Renewable share\nTPES energy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none")
g_REN_TPES_EU + g_REN_final_EU 

