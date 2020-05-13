renewable_energy_transport_hist <- read.table("../data/nrg_ind_ren_1_Data_transport.csv", sep=",", header=TRUE)
renewable_energy_transport_hist_EU <- filter(renewable_energy_transport_hist, GEO=='European Union - 28 countries (2013-2020)')

# Policy target
EU_Bio_Road_Transport_Target_year = c(2020,2030)
EU_Bio_Road_Transport_value = c(10,14)
EU_Bio_Road_Transport_Target <- data.frame(EU_Bio_Road_Transport_Target_year, EU_Bio_Road_Transport_value) %>% 
  rename(year=EU_Bio_Road_Transport_Target_year, value=EU_Bio_Road_Transport_value)

# travel
trvl_bio_NoPolicy <- filter(NoPolicyi$BlendingShareBio_energy_trvl_alt, travel_mode=="Road", region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Bio", Policy="NoPolicy") %>% 
  mutate(value, value=replace(value, year>=1971, 100*value)) %>%
  select(-travel_mode)
trvl_ren_NoPolicy <- filter(NoPolicyi$RenTransportShare_Road_trvl, region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Renewable", Policy="NoPolicy")
trvl_bio_NPi <- filter(NPii$BlendingShareBio_energy_trvl_alt, travel_mode=="Road", region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Bio", Policy="NPi") %>% 
  mutate(value, value=replace(value, year>=1971, 100*value) )%>%
  select(-travel_mode)
trvl_ren_NPi <- filter(NPii$RenTransportShare_Road_trvl, region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Renewable", Policy="NPi")
d_travel <- rbind(trvl_bio_NoPolicy, trvl_ren_NoPolicy) %>% rbind(trvl_bio_NPi) %>% rbind(trvl_ren_NPi)

# freight
frgt_bio_NoPolicy <- filter(NoPolicyi$BlendingShareBio_energy_frgt_alt, travel_mode=="Road", region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Bio", Policy="NoPolicy") %>% 
  mutate(value, value=replace(value, year>=1971, 100*value)) %>%
  select(-travel_mode)
frgt_ren_NoPolicy <- filter(NoPolicyi$RenTransportShare_Road_frgt, region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Renewable", Policy="NoPolicy")
frgt_bio_NPi <- filter(NPii$BlendingShareBio_energy_frgt_alt, travel_mode=="Road", region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Bio", Policy="NPi") %>% 
  mutate(value, value=replace(value, year>=1971, 100*value) )%>%
  select(-travel_mode)
frgt_ren_NPi <- filter(NPii$RenTransportShare_Road_frgt, region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Renewable", Policy="NPi")
d_freight <- rbind(frgt_bio_NoPolicy, frgt_ren_NoPolicy) %>% rbind(frgt_bio_NPi) %>% rbind(frgt_ren_NPi)

# total transport
trp_bio_NoPolicy <- filter(NoPolicyi$BlendingShareBio_energy_alt, travel_mode=="Road", region=="EU", year>=2010, year<=2030) %>%
  mutate(Variable="Bio", Policy="NoPolicy") %>% 
  mutate(value, value=replace(value, year>=1971, 100*value)) %>% 
  select(-travel_mode) 
trp_ren_NoPolicy <- filter(NoPolicyi$RenTransportShare_Road, travel_mode=="Total", type=="Total", region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Renewable", Policy="NoPolicy") %>% 
  select(-travel_mode, -type) 
trp_bio_NPi <- filter(NPii$BlendingShareBio_energy_alt, travel_mode=="Road", region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Bio", Policy="NPi") %>% 
  mutate(value, value=replace(value, year>=1971, 100*value) )%>%
  select(-travel_mode)
trp_ren_NPi <- filter(NPii$RenTransportShare_Road, travel_mode=="Total", type=="Total", region=="EU", year>=2010, year<=2030) %>% mutate(Variable="Renewable", Policy="NPi")%>% 
  select(-travel_mode, -type) 
d_trp <- rbind(trp_bio_NoPolicy, trp_ren_NoPolicy) %>% rbind(trp_bio_NPi) %>% rbind(trp_ren_NPi)


g_ren_travel <- ggplot() + 
  geom_line(data=d_travel, aes(x=year, y=value, linetype=Variable, colour=Policy)) +
  geom_point(data=renewable_energy_transport_hist_EU, aes(x=TIME, y=Value, colour="History")) +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy"="darkgreen", "NPi"="blue"), 
                      labels=c("History"="History Eurostat", "NoPolicy"="No policy baseline", "NPi"="Current policies"), guide=FALSE) +
  scale_linetype(guide=FALSE) +
  ylim(0,35) +
  ylab(d_trp$unit[1]) + 
  ggtitle('Travel') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g_ren_freight <- ggplot() + 
  geom_line(data=d_freight, aes(x=year, y=value, linetype=Variable, colour=Policy)) +
  geom_point(data=renewable_energy_transport_hist_EU, aes(x=TIME, y=Value, colour="History")) +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy"="darkgreen", "NPi"="blue"), 
                      labels=c("History"="History Eurostat", "NoPolicy"="No policy baseline", "NPi"="Current policies"), guide=FALSE) +
  scale_linetype(guide=FALSE) +
  ylim(0,35) +
  ylab(d_trp$unit[1]) + 
  ggtitle('Freight') +                   
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g_ren_trp <- ggplot() + 
  geom_line(data=d_trp, aes(x=year, y=value, linetype=Variable, colour=Policy)) +
  geom_point(data=renewable_energy_transport_hist_EU, aes(x=TIME, y=Value, colour="History")) +
  geom_point(data=EU_Bio_Road_Transport_Target, aes(x=year, y=value, size=year), colour="brown4") +
  scale_colour_manual(name="Scenario", values=c("History"="black", "NoPolicy"="darkgreen", "NPi"="blue"), 
                      labels=c("History"="History EU28 (Eurostat)", "NoPolicy"="No policy baseline", "NPi"="Current policies")) +
  scale_size_continuous(name="Biofuel standard", labels=EU_Bio_Road_Transport_Target_year, breaks=EU_Bio_Road_Transport_Target_year, range = c(2,3)) +
  ylim(0,35) +
  ylab(d_trp$unit[1]) +                   
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.title=element_text(size=6), legend.text=element_text(size=6), legend.spacing.y = unit(0.1, "mm"), legend.key.height=unit(0.5,"line"))