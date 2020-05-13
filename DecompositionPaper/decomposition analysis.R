GHG_AFOLU_NoPolicy <- NoPolicyi$EMIS_AFOLU %>% mutate(scenario="No new policies")
GHG_AFOLU_NPi <- NPii$EMIS_AFOLU %>% mutate(scenario="National policies")
GHG_AFOLU <- rbind(GHG_AFOLU_NoPolicy, GHG_AFOLU_NPi)

reg<-"USA"
g<-ggplot(data=filter(GHG_AFOLU, region==reg, year>=2015, year<=2030)) + 
          geom_line(aes(x=year, y=value, colour=scenario)) +
          theme_bw() +
          ylim(0, NA)
plot(g)
