library(tidyverse)
iea_data <- read.table('data/iea-tj-1990-2015-v2.csv', header=T, sep=',')
iea_data <- as.data.frame(iea_data)
IEA_biomass_product <- c("PRIMSBIO", "CHARCOAL", "BIOGASES", "BIODIESEL", "BIOGASOL", 
                         "BIOJETKERO","OBIOLIQ", "INDWASTE", "MUNWASTEN", "MUNWASTER")
IEA_crude_oil_product <- c("CRNGFEED","CRUDEOIL","NGL",  "REFFEEDS","ADDITIVE","NONCRUDE")
IEA_light_oil_product <- c("REFINGAS", "ETHANE","LPG","NONBIOGASO", "AVGAS","JETGAS","JETKERO","OTHKERO",     
                           "NONBIODIES", "RESFUEL","NAPHTHA","WHITESP","LUBRIC","BITUMEN","PARWAX","PETCOKE",       
                           "ONONSPEC",    "NONBIOJETK")
IEA_oil <- c(IEA_crude_oil_product, IEA_light_oil_product)

RestOfSouthAmerica_ISO <- c("ARG", "BOL", "CHL", "COL", "ECU", "FLK", "GUF", "GUY", "PRY", "PER", "SUR", "URY", "VEN")

biofuels_road_RoSA <- filter(iea_data, PRODUCT %in% IEA_biomass_product,
                        ISO3 %in% RestOfSouthAmerica, FLOW=='ROAD')
biofuels_road_RoSA <- spread(biofuels_road_RoSA, key=PRODUCT, value=VALUE) %>%
                 replace(is.na(.), 0) %>%
                 #mutate(biofuels=rowSums(.[8:9]))
                 mutate(biofuels=rowSums(select(., one_of(IEA_biomass_product)))) %>%
                 select(-TIMER_CARRIER)
#write.table(biofuels_road_RoSA, file="data/biofuels_history_RoSA.csv", sep=";", row.names=F)
oil_road_RoSA <- filter(iea_data, PRODUCT %in% IEA_oil,
                        ISO3 %in% RestOfSouthAmerica, FLOW=='ROAD')  %>%
                 group_by(COUNTRY, FLOW, PRODUCT, Year, UNIT, ISO3, IMAGE) %>%
                 summarise(VALUE=sum(VALUE)) %>% as.data.frame()
oil_road_RoSA <- spread(oil_road_RoSA, key=PRODUCT, value=VALUE)  %>%
                 replace(is.na(.), 0.0) %>%
                 mutate(oil=rowSums(select(.,one_of(IEA_oil))))
                 
#write.table(biofuels_road_RoSA, file="data/oil_history_RoSA.csv", sep=";", row.names=F)

total_road_RoSA <- filter(iea_data, PRODUCT =="TOTAL",
                             ISO3 %in% RestOfSouthAmerica, FLOW=='ROAD')
total_road_RoSA <- spread(total_road_RoSA, key=PRODUCT, value=VALUE) %>%
  replace(is.na(.), 0) %>%
  #mutate(total=rowSums(.[8:9]))
  mutate(total=rowSums(select(., one_of('TOTAL')))) %>%
  select(-TIMER_CARRIER)
#write.table(total_road_RoSA, file="data/total_history_RoSA.csv", sep=";", row.names=F)

biofuels_transport_tmp <- inner_join(biofuels_road_RoSA, oil_road_RoSA, 
                                 by=c('COUNTRY', 'FLOW', 'Year', 'UNIT', 'ISO3', 'IMAGE'))
biofuels_transport <- inner_join(biofuels_transport_tmp, total_road_RoSA, by=c('COUNTRY', 'FLOW', 'Year', 'UNIT', 'ISO3', 'IMAGE')) %>%
                      select(COUNTRY, FLOW, Year, UNIT, ISO3, IMAGE, biofuels, oil, total)
write.table(biofuels_transport, file="data/biofuels_road_transport_history_RoSA.csv", sep=";", row.names=F)

# calculate total Rest of South America
biofuels_transport_total <- group_by(biofuels_transport, FLOW, Year, UNIT, ISO3, IMAGE) %>%
                            summarise(biofuels=sum(biofuels), oil=sum(oil), total=sum(total)) %>%
                            mutate(COUNTRY="Rest of South America") %>%
                            select(COUNTRY, everything()) %>%
                            as.data.frame() %>%
                            rbind(biofuels_transport)
write.table(biofuels_transport_total, file="data/biofuels_road_transport_history_RoSA.csv", sep=";", row.names=F)
