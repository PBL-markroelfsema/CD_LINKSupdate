d_travel <- filter(NoPolicy$TransportTravelCO2Emissions, !travel_mode%in%c("Walking", "Biking", "-", "Total"), region=="EU", year>=2010, year<=2030) %>% mutate(sub_sector="Travel")
d_freight <- filter(NoPolicy$TransportFreightCO2Emissions, !travel_mode%in%c("-", "Total"), region=="EU", year>=2010, year<=2030) %>% mutate(sub_sector="Freigth")
d_transport <- rbind(d_travel, d_freight)
#pander(d_transport)

# https://colorbrewer2.org/?type=qualitative&scheme=Paired&n=11
colors_transport <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99")
colourCount = length(unique(d_transport$travel_mode))
getPalette = colorRampPalette(brewer.pal(5, "PuBuGn"))

g_transport_EU <- ggplot(data=d_transport, aes(x=year, y=value, fill=travel_mode)) +
  geom_area(stat="identity") +
  ylab(d_transport$unit) +
  #scale_fill_brewer(name="Travel mode", palette="Paired", values = getPalette(13)) +
  #scale_fill_manual(name="Travel mode", values = getPalette(colourCount)) +
  scale_fill_manual(name="Travel mode", values = colors_transport) +
  guides(fill=guide_legend(ncol=2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))