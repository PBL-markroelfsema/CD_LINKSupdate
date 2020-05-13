# https://ec.europa.eu/energy/data-analysis/energy-modelling/eu-reference-scenario-2016_en?redir=1
EU_hist_ETS_PRIMES <- read_excel(path="../data/AppendixRefSce.xls", sheet="EU28-B", range="A56:L58", col_names=FALSE) %>% as.data.frame()
#EU_hist_ETS_PRIMES <- read_excel(path="data/AppendixRefSce.xls", sheet="EU28-B", range="A56:L58", col_names=FALSE) %>% as.data.frame()
colnames(EU_hist_ETS_PRIMES) <- c('variable', '2000', '2005','2010','2015','2020','2025','2030','2035','2040','2045','2050') 
EU_hist_ETS_PRIMES <- filter(EU_hist_ETS_PRIMES, variable=="of which ETS sectors (2013 scope) GHG emissions") 
EU_hist_ETS_PRIMES$`variable` <- NULL
EU_hist_ETS_PRIMES$`2000` <- NULL
EU_hist_ETS_PRIMES <- gather(EU_hist_ETS_PRIMES, 1:ncol(EU_hist_ETS_PRIMES), key=year, value=value) %>%
  mutate(unit="MtCO2eq", source="EU28 reference scenario 2016\n (PRIMES)")

#https://www.eea.europa.eu/data-and-maps/dashboards/emissions-trading-viewer-1
EU_hist_ETS_Registry <- read.table('../data/ETS_Database_v34.csv', sep="\t", header=TRUE)
#EU_hist_ETS_Registry <- read.table('data/ETS_Database_v34.csv', sep="\t", header=TRUE)
EU_hist_ETS_Registry <- filter(EU_hist_ETS_Registry, `ETS.information`%in%c("2. Verified emissions", "Estimate to reflect current ETS scope for allowances and emissions"),
                               `main.activity.sector.name`=="20-99 All stationary installations") %>%
                        group_by(year) %>%
                        summarise(value=sum(value)) %>%
                        filter(!str_detect(year, "Total")) %>%
                        mutate(value=replace(value, value>0, value/10^6)) %>%
                        mutate(unit="MtCO2eq", source="ETS Registry")
EU_hist_ETS <- rbind(EU_hist_ETS_PRIMES, EU_hist_ETS_Registry)
EU_hist_ETS$year <- as.integer(EU_hist_ETS$year)

# Targets (cap on GHG emissions)
# https://ec.europa.eu/clima/policies/ets/cap_en

EU_ETS_Target <- read.table('../data/EU_ETS_cap.csv', sep=";", header=TRUE)
EU_ETS_Target <- mutate(EU_ETS_Target, value=allowances/10^6, unit="MtCO2eq") %>% select(-allowances)

# 80% of energy supply and industry sector is covered by ETS
EU_ETS_Target_TIMER_year = c(2020,2030)
perc_NoPolicy_2020 <- filter(NoPolicyi$EMIS_ETS, region=="EU", year%in%c(2005, 2020)) %>% 
  spread(key='year', value=value) %>%
  mutate(perc = `2020`/`2005`-1)
perc_NoPolicy_2020 <- perc_NoPolicy_2020$perc
perc_NoPolicy_2030 <- filter(NoPolicyi$EMIS_ETS, region=="EU", year%in%c(2005, 2030)) %>% 
  spread(key='year', value=value) %>%
  mutate(perc = `2030`/`2005`-1)
perc_NoPolicy_2030 <- perc_NoPolicy_2030$perc
NoPolicy_2005 <- filter(NoPolicyi$EMIS_ETS, region=="EU", year%in%c(2005))$value
EU_ETS_Target_2020 = 0.8*NoPolicy_2005*(1-0.21) + 0.2*NoPolicy_2005*(1+perc_NoPolicy_2020)
EU_ETS_Target_2030 = 0.8*NoPolicy_2005*(1-0.43) + 0.2*NoPolicy_2005*(1+perc_NoPolicy_2030)
EU_ETS_Target_TIMER_value = c(EU_ETS_Target_2020,EU_ETS_Target_2030) 
EU_ETS_Target_TIMER <- data.frame(EU_ETS_Target_TIMER_year, EU_ETS_Target_TIMER_value) %>% 
  rename(year=EU_ETS_Target_TIMER_year, value=EU_ETS_Target_TIMER_value)
#pander(EU_REN_final_target)

# TIMER scenarios
EU_ETS_TIMER_NoPolicy <- filter(NoPolicyi$EMIS_ETS, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NoPolicy")
EU_ETS_TIMER_NPi <- filter(NPii$EMIS_ETS, region=="EU", year>=2000, year<=2030) %>% mutate(Policy="NPi")
EU_ETS_TIMER <- rbind(EU_ETS_TIMER_NoPolicy, EU_ETS_TIMER_NPi) #%>% mutate(value=100*value)

# TIMER scenarios only in target years
EU_ETS_target_years <- filter(EU_ETS_TIMER, year==2020 | year==2030)

grob <- grobTree(textGrob("ETS covers 80% of energy supply\n and industry emissions", x=0.4, y=0.35, hjust=0,
                          gp=gpar(col="black", fontsize=10, fontface="italic")))

g_EU_ETS <- ggplot() + 
  geom_line(data=EU_ETS_TIMER, aes(x=year, y=value, colour=Policy)) + 
  geom_point(data=EU_ETS_target_years, aes(x=year, y=value, colour=Policy), size=2) + 
  geom_line(data=filter(EU_hist_ETS, year<=2018), aes(x=year, y=value, linetype=source)) +
  geom_point(data=EU_ETS_Target, aes(x=year, y=value, size=year), colour="darkyellow") +
  geom_point(data=EU_ETS_Target_TIMER, aes(x=year, y=value, size=year), colour="brown4") +
  #geom_text(x=0.6, y=0.5, label="ETS covers 80% of energy supply\n and industry emissions") +
  annotation_custom(grob) +
  theme_bw() +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy"="darkgreen", "NPi"="blue"), 
                      labels=c("History"="History EEA", "NoPolicy"="No policy baseline", "NPi"="Current policies")) +
  scale_linetype(name="Source") +
  scale_size_continuous(name="ETS target", labels=EU_ETS_Target_TIMER_year, breaks=EU_ETS_Target_TIMER_year, range = c(2,3)) +
  ylab(EU_ETS_TIMER$unit[1]) +
  ylim(0, 3500) +
 # ggtitle("Emission trading system") +
  theme(legend.title=element_text(size=8), legend.text=element_text(size=6), legend.spacing.y = unit(0.1, 'cm')) +
  guides(fill=guide_legend(reverse=TRUE))
