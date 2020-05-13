library(patchwork)

#co2_intens = NPii$CO2_intensity_fleet_new_cars; type="incl_elec"
co2_intens = NPii$CO2_intensity_fleet_new_cars_tailpipe; type="tailpipe"
# show co2 intensity new fleet and electric share new cars
r = c('EU', 'USA', 'CHN', 'INDIA', 'BRA', 'JPN')
d1 <- filter(co2_intens, region %in% r, year>=2010, year<=2030) %>% 
      mutate(value=replace(value, value==0, NA)) %>%
      mutate(variable="CO2 intensity")  %>% as.data.frame()
d2 <- filter(NPi$ElectricShare_new_cars, region %in% r, year>=2010, year<=2030) %>%
      mutate(value=replace(value, year>=1971, 100*value)) %>%
      mutate(variable="share electric cars", carrier="electricity")  %>% as.data.frame()
d <- rbind(d1,d2) %>% 
     mutate(min=ifelse(variable=="CO2 intensity", 0, 0)) %>% 
     mutate(max=ifelse(variable=="CO2 intensity", 250, 15)) %>% 
     as.data.frame() 
g1<- ggplot(data=d) + geom_line(aes(x=year, y=value, colour=carrier)) + 
                      geom_blank(aes(x=year, y=min)) +
                      geom_blank(aes(x=year, y=max)) +
                      facet_grid(variable~region, scales="free_y") +
                      ggtitle(paste0('CO2 intensity new car fleet (',type,')')) +
                      theme_bw() +
                      theme(axis.text.x = element_text(angle = 90))
ggsave(paste0('graphs/co2-intensity_',type,'.jpg'))

co2_intens = NPii$CO2_intensity_fleet_new_cars; type="incl_elec"
# show co2 intensity new fleet and electric share new cars
r = c('EU', 'USA', 'CHN', 'INDIA', 'BRA', 'JPN')
d1 <- filter(co2_intens, region %in% r, year>=2010, year<=2030) %>% 
      mutate(value=replace(value, value==0, NA)) %>%
      mutate(variable="CO2 intensity")  %>% as.data.frame()
d2 <- filter(NPi$ElectricShare_new_cars, region %in% r, year>=2010, year<=2030) %>%
      mutate(value=replace(value, year>=1971, 100*value)) %>%
      mutate(variable="share electric cars", carrier="electricity")  %>% as.data.frame()
d <- rbind(d1,d2) %>% 
    mutate(min=ifelse(variable=="CO2 intensity", 0, 0)) %>% 
    mutate(max=ifelse(variable=="CO2 intensity", 250, 15)) %>% 
    as.data.frame() 
g2<- ggplot(data=d) + geom_line(aes(x=year, y=value, colour=carrier)) +
                      geom_blank(aes(x=year, y=min)) +
                      geom_blank(aes(x=year, y=max)) +
                      facet_grid(variable~region, scales="free_y") +
                      ggtitle(paste0('CO2 intensity new car fleet (',type,')')) +
                      theme_bw() +
                      theme(axis.text.x = element_text(angle = 90))
ggsave(paste0('graphs/co2-intensity_',type,'.jpg'))

g1 + g2

# compare electric share cars and new cars
r = c('CHN', 'EU')
d1 <- filter(NPi$ElectricShare_cars, region%in%r, year>=2010, year<=2030) %>% mutate(vintage='existing')
d2 <- filter(NPi$ElectricShare_new_cars, region%in%r, year>=2010, year<=2030) %>% mutate(vintage='new')
d <- rbind(d1,d2) %>% mutate(value=replace(value, year>=1971, 100*value))
ggplot(data=d) + geom_line(aes(x=year, y=value, colour=vintage)) +
                 facet_wrap(~region) +
                 theme_bw()

