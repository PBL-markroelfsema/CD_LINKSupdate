# UNFCCC history
d_EU_GHG_EU28_UNFCCC <- filter(GHG_emissions_UNFCCC_TIMER, Region%in%c(11,12), Gas=="Total", Sector=="Total excl. LULUCF") %>% select(-Sector) %>%
  spread(key=Region, value=Value) %>%
  mutate(Value=`11` +`12`) %>%
  select(-`11`,-`12`) %>% as.data.frame()
d_EU_GHG_EU28_UNFCCC$Source <- as.character(d_EU_GHG_EU28_UNFCCC$Source)
d_EU_GHG_EU28_UNFCCC <- mutate(d_EU_GHG_EU28_UNFCCC, Source=replace(Source, Year>=1850, "UNFCCC (Europe)"))

# https://ec.europa.eu/energy/data-analysis/energy-modelling/eu-reference-scenario-2016_en?redir=1
EU_hist_GHG_PRIMES <- read_excel(path="../data/AppendixRefSce.xls", sheet="EU28-B", range="A56:L58", col_names=FALSE) %>% as.data.frame()
#EU_hist_GHG_PRIMES <- read_excel(path="data/AppendixRefSce.xls", sheet="EU28-B", range="A56:L58", col_names=FALSE) %>% as.data.frame()
colnames(EU_hist_GHG_PRIMES) <- c('variable', '2000', '2005','2010','2015','2020','2025','2030','2035','2040','2045','2050') 
EU_hist_GHG_PRIMES <- filter(EU_hist_GHG_PRIMES, variable=="TOTAL GHG emissions (Mt of CO2 eq.)") 
EU_hist_GHG_PRIMES$`variable` <- NULL
EU_hist_GHG_PRIMES$`2000` <- NULL
EU_hist_GHG_PRIMES <- gather(EU_hist_GHG_PRIMES, 1:ncol(EU_hist_GHG_PRIMES), key=year, value=value) %>%
                      mutate(unit="MtCO2eq", source="EU28 reference scenario 2016\n (PRIMES)", gas="Total", GWP="AR4")
EU_hist_GHG_PRIMES <- filter(EU_hist_GHG_PRIMES, year<=2030) %>%
                      rename(Year=year, Value=value, Unit=unit, Source=source, Gas=gas)

d_EU_GHG_EU28_hist <- rbind(d_EU_GHG_EU28_UNFCCC, EU_hist_GHG_PRIMES)
d_EU_GHG_EU28_hist$Year <- as.integer(d_EU_GHG_EU28_hist$Year)

# TIMER scenarios
EU_TIMER_GHG_NoPolicy <- filter(NoPolicyi$EMISCO2EQexcl_LULUCF_indicator, region=="EU", year>=1990, year<=2030) %>% mutate(Policy="NoPolicy (v2015)")
EU_TIMER_GHG_NoPolicy_3_2 <- filter(NoPolicy_3_2i$EMISCO2EQexcl_LULUCF_indicator, region=="EU", year>=1990, year<=2030) %>% mutate(Policy="NoPolicy (v3_2)")
EU_TIMER_GHG_NPi <- filter(NPii$EMISCO2EQexcl_LULUCF_indicator, region=="EU", year>=1990, year<=2030) %>% mutate(Policy="NPi")
EU_TIMER_GHG <- rbind(EU_TIMER_GHG_NoPolicy, EU_TIMER_GHG_NPi) %>% rbind(EU_TIMER_GHG_NoPolicy_3_2) %>% select(-region)

# Targets
# Absolute emission target based on UNFCCC historical data
EU_GHG_Target_base_2020 = filter(d_EU_GHG_EU28_hist, Year==1990)$Value
EU_GHG_Target_base_2030 = filter(d_EU_GHG_EU28_hist, Year==1990)$Value
EU_GHG_Target_year = c(2020,2030)
EU_GHG_Target_value = c((1-20/100)*EU_GHG_Target_base_2020,(1-40/100)*EU_GHG_Target_base_2030)
EU_GHG_Target_perc <- data.frame(EU_GHG_Target_year, EU_GHG_Target_value) %>% 
                      rename(year=EU_GHG_Target_year, value=EU_GHG_Target_value) %>% 
                      mutate(type="Absolute")
# Percentage relative to TIMER GHG emissions
EU_GHG_Target_base_2020 = filter(EU_TIMER_GHG_NoPolicy, year==1990)$value
EU_GHG_Target_base_2030 = filter(EU_TIMER_GHG_NoPolicy, year==1990)$value
EU_GHG_Target_year = c(2020,2030)
EU_GHG_Target_value = c((1-20/100)*EU_GHG_Target_base_2020,(1-40/100)*EU_GHG_Target_base_2030)
EU_GHG_Target_abs <- data.frame(EU_GHG_Target_year, EU_GHG_Target_value) %>% 
                     rename(year=EU_GHG_Target_year, value=EU_GHG_Target_value) %>% 
                     mutate(type="Percentage")
EU_GHG_Target <- rbind(EU_GHG_Target_perc, EU_GHG_Target_abs)

# add history as scenario
#d_tmp <- select(d_EU_GHG_EU28_hist, Year, Value) %>% 
#         rename(value=Value, year=Year) %>% 
#         mutate(Policy="History")
#EU_TIMER_GHG <- rbind(EU_TIMER_GHG, d_tmp)

# TIMER scenarios only in target years
EU_GHG_target_years <- filter(EU_TIMER_GHG, year==2020 | year==2030)


g_GHG_Target_EU <- ggplot() + 
  geom_line(data=EU_TIMER_GHG, aes(x=year, y=value, colour=Policy)) + 
  geom_line(data=d_EU_GHG_EU28_hist, aes(x=Year, y=Value, linetype=Source)) + 
  geom_point(data=EU_GHG_target_years, aes(x=year, y=value, colour=Policy), size=2) + 
  geom_point(data=EU_GHG_Target, aes(x=year, y=value, shape=interaction(year, type))) +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy (v2015)"="darkgreen", "NoPolicy (v3_2)"="chartreuse", "NPi"="blue"), 
                      labels=c("NoPolicy"="No policy baseline (v2015)", "NPi"="Curent policies (v3_2)")) +
  scale_linetype_discrete(name="Source history") +
  scale_size_continuous(name="GHG targets", labels=EU_GHG_Target_year, breaks=EU_GHG_Target_year, range = c(2,5)) +
  scale_shape(name="GHG targets", labels=c("2020.Absolute"="2020 (rel. to 1990 UNFCCC)", 
                                           "2030.Absolute"="2030 (rel. to 1990 UNFCCC)", 
                                           "2020.Percentage"="2020 (rel. to 1990 IMAGE)", 
                                           "2030.Percentage"="2030 (rel. to 1990 IMAGE)")) +
  ylim(0, NA) +
  ylab(EU_TIMER_GHG$unit[1]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(reverse=TRUE))

