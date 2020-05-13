source('CD_LINKS/Init_indicators.R')

Innovation_Electricity <-  mutate(NPi_ind$NonFossilElecShare, scenario="National policies")
Innovation_Electricity <-  mutate(NPi2020_1000_ind$NonFossilElecShare, scenario="2C") %>% rbind(Innovation_Electricity)
Innovation_Electricity <-  mutate(NPi2020_400_ind$NonFossilElecShare, scenario="1.5C") %>% rbind(Innovation_Electricity)

PersonKm <-  mutate(NPi$PersonKilometers, scenario="National policies")
PersonKm <-  mutate(NPi2020_1000$PersonKilometers, scenario="2C") %>% rbind(PersonKm)
PersonKm <-  mutate(NPi2020_400$PersonKilometers, scenario="1.5C") %>% rbind(PersonKm)