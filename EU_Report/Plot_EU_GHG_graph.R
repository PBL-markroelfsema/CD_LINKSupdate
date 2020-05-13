sector_order <- c('Total', 'Total excl. LULUCF', 'Energy supply', 'Industry', 'Transport', 'Buildings', 'LULUCF', 'Agriculture', 'Waste', 'Other', 'Bunkers')
d_EU_GHG_EU28_hist <- filter(GHG_emissions_UNFCCC, Region=="EU28", Gas=="Total") %>% select(-Country, -SubSector)
d_EU_GHG_WEU_CEU_hist <- filter(GHG_emissions_UNFCCC_TIMER, Region%in%c(11,12), Gas=="Total") %>%
  group_by(Source, Gas, Sector, Year, Unit, GWP) %>%
  summarise(Value=sum(Value)) %>%
  mutate(Region="Europe") %>% as.data.frame()
d_EU_GHG_hist <- rbind(d_EU_GHG_EU28_hist, d_EU_GHG_WEU_CEU_hist) 
d_EU_GHG_hist <- spread(d_EU_GHG_hist, key=Region, value=Value) %>%
  mutate(`Europe excl. EU28` = Europe-EU28) %>%
  gather(EU28, Europe, `Europe excl. EU28`, key="Region", value="Value") %>%
  filter(Region!='Europe')
d_EU_GHG_hist$Region <- factor(d_EU_GHG_hist$Region, levels=c("Europe", "Europe excl. EU28", "EU28"))

TIMER_NoPolicy <- filter(NoPolicyi$EMISCO2EQ, region%in%c('WEU', 'CEU'), year<=2030, GHG_Category=='EMISCO2EQ') %>% mutate(Scenario="No new policies (v2015)")
TIMER_NoPolicy_3_2 <- filter(NoPolicy_3_2i$EMISCO2EQ, region%in%c('WEU', 'CEU'), year<=2030, GHG_Category=='EMISCO2EQ') %>% mutate(Scenario="No new policies (v3_2)")
#TIMER_NPi <- filter(NPii$EMISCO2EQ, region%in%c('WEU', 'CEU'), year<=2030, GHG_Category=='EMISCO2EQ') %>% mutate(Scenario="Current policies")
TIMER_NPi <- NULL
d_TIMER_total <- rbind(TIMER_NoPolicy, TIMER_NPi) %>% rbind(TIMER_NoPolicy_3_2)

d_EU_GHG_TIMER_total <- d_TIMER_total %>%
  select(-GHG_Category) %>%
  rename(Sector=main_sector, Region=region, Unit=unit, Year=year, Value=value) %>%
  group_by(Scenario, Sector, Year, Unit) %>%
  summarise(Value=sum(Value)) %>%
  mutate(Region="Europe")

TIMER_total_excl_lulucf_NoPolicy <- filter(NoPolicyi$EMISCO2EQexcl_LULUCF_indicator, region%in%c('WEU', 'CEU'), year<=2030) %>% mutate(Scenario="No new policies (v2015)")
TIMER_total_excl_lulucf_NoPolicy_3_2 <- filter(NoPolicy_3_2i$EMISCO2EQexcl_LULUCF_indicator, region%in%c('WEU', 'CEU'), year<=2030) %>% mutate(Scenario="No new policies (v3_2)")
#TIMER_total_excl_lulucf_NPi <- filter(NPii$EMISCO2EQexcl_LULUCF_indicator, region%in%c('WEU', 'CEU'), year<=2030) %>% mutate(Scenario="Current policies")
TIMER_total_excl_lulucf_NPi <- NULL
d_TIMER_total_excl_lulucf <- rbind(TIMER_total_excl_lulucf_NoPolicy, TIMER_total_excl_lulucf_NPi) %>% rbind(TIMER_total_excl_lulucf_NoPolicy_3_2)

d_EU_GHG_TIMER_total_excl_lulucf <- d_TIMER_total_excl_lulucf %>%
  rename(Region=region, Unit=unit, Year=year, Value=value) %>%
  group_by(Scenario, Year, Unit) %>%
  summarise(Value=sum(Value)) %>%
  mutate(Region="Europe") %>%
  mutate(Sector='Total excl. LULUCF')

d_EU_GHG_TIMER <- rbind(d_EU_GHG_TIMER_total, d_EU_GHG_TIMER_total_excl_lulucf) %>% as.data.frame()
#d_EU_GHG_TIMER$Scenario <- factor(d_EU_GHG_TIMER$Scenario, levels=c("No new policies (v2015)", "Current policies (v2015)", "No new policies (v3_2)"))
d_EU_GHG_TIMER$Scenario <- factor(d_EU_GHG_TIMER$Scenario, levels=c("No new policies (v2015)", "No new policies (v3_2)"))

d_EU_GHG_hist$Region <- factor(d_EU_GHG_hist$Region, levels=c("Europe", "Europe excl. EU28", "EU28"))
d_EU_GHG_hist$Sector <- factor(d_EU_GHG_hist$Sector, levels=sector_order)
d_EU_GHG_TIMER$Sector <- factor(d_EU_GHG_TIMER$Sector, levels=sector_order)

g_GHG_EU <- ggplot() +
  geom_bar(data=d_EU_GHG_hist, aes(x=Year, y=Value, fill=Region), stat="identity") +
  geom_line(data=d_EU_GHG_TIMER, aes(x=Year, y=Value, linetype=Scenario)) +
  facet_wrap(~Sector, scales='free_y') +
  scale_fill_brewer(name="Historical data", palette="Set1") +
  scale_linetype(name="IMAGE Scenario") +
  ylab(d_EU_GHG_hist$Unit[1]) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))