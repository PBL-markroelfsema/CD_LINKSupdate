MToeToTJ =4.1868*10^(4)

# IEA Energy Balance
EU_hist_EE <- read.table("../data/IEA_IAMregions.csv", sep=";", header=TRUE) %>% as.data.frame()
#EU_hist_EE <- filter(EU_hist_EE, region=="EU", variable %in%c('Final Energy', 'Primary Energy')) %>%
#              mutate(value=10^3*value, source="IEA")
EU_hist_EE <- filter(EU_hist_EE, region%in%c("WEU","CEU"), variable %in%c('Final Energy', 'Primary Energy')) %>%
  mutate(value=10^3*value, source="History Europe (IEA)") %>% 
  spread(key=region, value=value) %>%
  mutate(value=WEU+CEU, region="Europe") %>%
  select(-WEU, -CEU)

# EU Reference scenario
# https://ec.europa.eu/energy/data-analysis/energy-modelling/eu-reference-scenario-2016_en?redir=1
EU_hist_Energy_PRIMES <- read_excel(path="../data/AppendixRefSce.xls", sheet="EU28-B", range="A30:L31", col_names=FALSE) %>% as.data.frame()
colnames(EU_hist_Energy_PRIMES) <- c('variable', '2000', '2005','2010','2015','2020','2025','2030','2035','2040','2045','2050') 
EU_hist_Energy_PRIMES <- filter(EU_hist_Energy_PRIMES, variable%in%c('Primary energy consumption', 'Final Energy Demand')) 
EU_hist_Energy_PRIMES <- gather(EU_hist_Energy_PRIMES, 2:ncol(EU_hist_Energy_PRIMES), key=year, value=value) %>%
  mutate(unit="MToe", source="EU reference scenario 2016\n (PRIMES)") %>%
  mutate(variable=replace(variable, variable=="Primary energy consumption", "Primary Energy")) %>%
  mutate(variable=replace(variable, variable=="Final Energy Demand", "Final Energy")) %>%
  mutate(value=MToeToTJ*value*10^(-6), unit="PJ", source="EU28 Reference scenario 2016")
EU_hist_Energy_PRIMES$year <- as.numeric(EU_hist_Energy_PRIMES$year)


# Absolute targets (in PJ)
EU_EE_TPES_Target_abs_year = c(2020,2030)
t1 = MToeToTJ*1483*10^(3)*10^(-6)
t2 = MToeToTJ*1273*10^3*10^(-6)
EU_EE_TPES_Target_abs_value = c(t1,t2)
EU_EE_TPES_Target_abs <- data.frame(EU_EE_TPES_Target_abs_year, EU_EE_TPES_Target_abs_value) %>% 
                         rename(year=EU_EE_TPES_Target_abs_year, value=EU_EE_TPES_Target_abs_value) %>%
                         mutate(variable="Primary Energy")
EU_EE_FE_Target_abs_year = c(2020,2030)
t1 = MToeToTJ*1086*10^(3)*10^(-6)
t2 = MToeToTJ*956*10^3*10^(-6)
EU_EE_FE_Target_abs_value = c(t1,t2)
EU_EE_FE_Target_abs <- data.frame(EU_EE_FE_Target_abs_year, EU_EE_FE_Target_abs_value) %>% 
                       rename(year=EU_EE_FE_Target_abs_year, value=EU_EE_FE_Target_abs_value) %>%
                       mutate(variable="Final Energy")
EU_EE_Target_abs <- rbind(EU_EE_TPES_Target_abs, EU_EE_FE_Target_abs) %>% mutate(unit="PJ", type="Absolute")

v2010_TPES=filter(EU_hist_Energy_PRIMES, variable=="Primary Energy", year==2010)$value
v2010_FE=filter(EU_hist_Energy_PRIMES, variable=="Final Energy", year==2010)$value 
Perc_Target <- filter(EU_hist_Energy_PRIMES, year%in%c(2020, 2030)) %>% mutate(v2010=ifelse(variable=="Primary Energy",v2010_TPES, v2010_FE)) %>%
               mutate(reduction=100*(value/v2010-1)) %>%
               select(-value, -v2010) %>%
               rename(value=reduction)

# TIMER scenarios
EU_TIMER_EE_TPES_NoPolicy <- filter(NoPolicy$TPES, region=="EU", year>=2000, year<=2030, energy_carrier=="Total") %>% mutate(Policy="NoPolicy (v2015)")
EU_TIMER_EE_TPES_NoPolicy_3_2 <- filter(NoPolicy_3_2$TPES, region=="EU", year>=2000, year<=2030, energy_carrier=="Total") %>% mutate(Policy="NoPolicy (v3_2)")
EU_TIMER_EE_TPES_NPi <- filter(NPi$TPES, region=="EU", year>=2000, year<=2030, energy_carrier=="Total") %>% mutate(Policy="NPi")
EU_TIMER_EE_TPES <- rbind(EU_TIMER_EE_TPES_NoPolicy, EU_TIMER_EE_TPES_NPi) %>% rbind(EU_TIMER_EE_TPES_NoPolicy_3_2) %>% select(-energy_carrier) %>% mutate(variable="Primary Energy")

# TIMER scenarios
EU_TIMER_EE_FE_NoPolicy <- filter(NoPolicy$FinalEnergy, region=="EU", year>=2000, year<=2030, energy_carrier=="Total", sector=="Total") %>% mutate(Policy="NoPolicy (v2015)")
EU_TIMER_EE_FE_NoPolicy_3_2 <- filter(NoPolicy_3_2$FinalEnergy, region=="EU", year>=2000, year<=2030, energy_carrier=="Total", sector=="Total") %>% mutate(Policy="NoPolicy (v3_2)")
EU_TIMER_EE_FE_NPi <- filter(NPi$FinalEnergy, region=="EU", year>=2000, year<=2030, energy_carrier=="Total", sector=="Total") %>% mutate(Policy="NPi")
EU_TIMER_EE_FE <- rbind(EU_TIMER_EE_FE_NoPolicy, EU_TIMER_EE_FE_NPi) %>% rbind(EU_TIMER_EE_FE_NoPolicy_3_2) %>% select(-energy_carrier, -sector) %>% mutate(variable="Final Energy")
EU_TIMER_EE <- rbind(EU_TIMER_EE_TPES,EU_TIMER_EE_FE) %>% mutate(source="TIMER")

# Tercentage target (%)
EU_EE_Target_perc <- filter(EU_TIMER_EE, year==2010, Policy=="NoPolicy (v2015)") %>% 
  inner_join(Perc_Target, by=c('variable')) %>%
  mutate(target=value.x*(1+value.y/100)) %>%
  select(-year.x, -value.x, -Policy, -source.x, -source.y, -value.y, -unit.y, -region) %>%
  rename(unit=unit.x, year=year.y, value=target) %>%
  mutate(type="Percentage")
EU_EE_Target <- rbind(EU_EE_Target_abs, EU_EE_Target_perc)

# TIMER scenarios only in target years
EU_EE_target_years <- filter(EU_TIMER_EE, year==2020 | year==2030)

g_EE_EU <- ggplot() + 
  geom_line(data=EU_TIMER_EE, aes(x=year, y=value, colour=Policy)) + 
  geom_line(data=filter(EU_hist_EE), aes(x=period, y=value, linetype=source)) +
  geom_line(data=filter(EU_hist_Energy_PRIMES, year<=2030), aes(x=year, y=value, linetype=source)) +
  geom_point(data=EU_EE_target_years, aes(x=year, y=value, colour=Policy), size=2) + 
  geom_point(data=EU_EE_Target, aes(x=year, y=value, shape=interaction(year, type)), colour="brown4") +
  facet_wrap(~variable) +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy (v2015)"="darkgreen", "NoPolicy (v3_2)"="chartreuse", "NPi"="blue"), 
                      labels=c("History"="History EEA", "NoPolicy"="No policy baseline (v2015)", "NPi"="Current policies (v3_2)")) +
  scale_shape((name="Energy efficiency goal"), 
              labels=c("2020.Absolute"="2020\n(absolute from EE Directive)", 
                       "2030.Absolute"="2030\n(absolute from EE Directive)", 
                       "2020.Percentage"="2020\n(percentage from PRIMES scenario)", 
                       "2030.Percentage"="2030\n(percentage rel. to 2010 from PRIMES scenario)")) +
  scale_linetype(name="Source", breaks=c("History Europe (IEA)", "EU28 Reference scenario 2016")) +
  ylim(0, NA) +
  ylab(EU_TIMER_EE$unit[1]) +
  #ggtitle("Energy efficiency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(reverse=TRUE))