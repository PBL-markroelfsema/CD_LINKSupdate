library(dplyr)
library(tidyverse)

#NoPolicy_2019
#NoPolicy_2019_i
#NPi_2019
#NPi_2019_i

# check differences with last year
#regions_graph = c("CAN","USA","MEX","RCAM","BRA", "SAF", "TUR","RUS","INDIA","KOR","CHN","INDO","JAP","OCE", "World")
regions_graph = c("CAN","USA", "EU", "World")
d1_2018 <- filter(NoPolicy_2018_i$EMISCO2EQ, GHG_Category=="EMISCO2EQ") %>% mutate(scenario="NoPolicy", AnalysisYear="2018") %>% select(-GHG_Category)
d2_2018 <- filter(NPi_2018_i$EMISCO2EQ, GHG_Category=="EMISCO2EQ") %>% mutate(scenario="NPi", AnalysisYear="2018") %>% select(-GHG_Category)
d1_2019 <- filter(NoPolicy_2019_i$EMISCO2EQ, GHG_Category=="EMISCO2EQ") %>% mutate(scenario="NoPolicy", AnalysisYear="2019") %>% select(-GHG_Category)
d2_2019 <- filter(NPi_2019_i$EMISCO2EQ, GHG_Category=="EMISCO2EQ") %>% mutate(scenario="NPi", AnalysisYear="2019") %>% select(-GHG_Category)
d_2018 <- rbind(d1_2018, d2_2018)
d_2019 <- rbind(d1_2019, d2_2019)
d <- rbind(d1_2018, d2_2018) %>% rbind(d1_2019) %>% rbind(d2_2019)

ggplot(data=filter(d, region %in% regions_graph, year>=2015, year<=2030)) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=AnalysisYear)) +  
  facet_grid(region~main_sector, scales="free_y") +
  ylim(0, NA) +
  theme_bw()

ggplot(data=filter(d, region %in% regions_graph, year>=2015, year<=2030)) +
  geom_point(aes(x=year, y=value, colour=scenario, shape=AnalysisYear)) +  
  facet_grid(region~main_sector, scales="free_y") +
  ylim(0, NA) +
  theme_bw()

ggplot() +
  geom_line(data=filter(d_2019, region %in% regions_graph, year>=2015, year<=2030), 
            aes(x=year, y=value, colour=scenario, linetype=AnalysisYear)) +  
  geom_point(data=filter(d_2018, region %in% regions_graph, year==2030), 
             aes(x=year, y=value, shape=scenario),size=3) +
  facet_grid(region~main_sector, scales="free_y") +
  ylim(0, NA) +
  theme_bw()

# Check EU ETS target
d1_2018 <- mutate(filter(NoPolicy_2018_i$EMISCO2EQ, main_sector=="Total", GHG_Category=="EMISCO2EQ"), sector="Total", scenario="NoPolicy", AnalysisYear="2018")%>% select(-main_sector, -GHG_Category)
d2_2018 <- mutate(NoPolicy_2018_i$EMIS_power, sector="Power", scenario="NoPolicy", AnalysisYear="2018")
d3_2018 <- mutate(NoPolicy_2018_i$EMIS_industry, sector="Industry", scenario="NoPolicy", AnalysisYear="2018")
d4_2018 <- mutate(NoPolicy_2018_i$EMIS_ETS, sector="ETS", scenario="NoPolicy", AnalysisYear="2018")

d5_2018 <- mutate(NPi_2018_i$EMIS_power, sector="Power", scenario="NPi", AnalysisYear="2018")
d6_2018 <- mutate(NPi_2018_i$EMIS_industry, sector="Industry", scenario="NPi", AnalysisYear="2018")
d7_2018 <- mutate(NPi_2018_i$EMIS_ETS, sector="ETS", scenario="NPi", AnalysisYear="2018")

d1_2019 <- mutate(filter(NoPolicy_2019_i$EMISCO2EQ, main_sector=="Total", GHG_Category=="EMISCO2EQ"), sector="Total", scenario="NoPolicy", AnalysisYear="2019")%>% select(-main_sector, -GHG_Category)
d2_2019 <- mutate(NoPolicy_2019_i$EMIS_power, sector="Power", scenario="NoPolicy", AnalysisYear="2019")
d3_2019 <- mutate(NoPolicy_2019_i$EMIS_industry, sector="Industry", scenario="NoPolicy", AnalysisYear="2019")
d4_2019 <- mutate(NoPolicy_2019_i$EMIS_ETS, sector="ETS", scenario="NoPolicy", AnalysisYear="2019")

d5_2019 <- mutate(NPi_2019_i$EMIS_power, sector="Power", scenario="NPi", AnalysisYear="2019")
d6_2019 <- mutate(NPi_2019_i$EMIS_industry, sector="Industry", scenario="NPi", AnalysisYear="2019")
d7_2019 <- mutate(NPi_2019_i$EMIS_ETS, sector="ETS", scenario="NPi", AnalysisYear="2019")

d_2018<- rbind(d1_2018, d2_2018) %>% rbind(d3_2018) %>% rbind(d4_2018) %>% rbind(d5_2018) %>% rbind(d6_2018) %>% rbind(d7_2018)
d_2019<- rbind(d1_2019, d2_2019) %>% rbind(d3_2019) %>% rbind(d4_2019) %>% rbind(d5_2019) %>% rbind(d6_2019) %>% rbind(d7_2019)
d <- rbind(d_2018, d_2019)

ggplot(data=filter(d, region=="EU", year>=2015, year<=2030)) +
                   geom_point(aes(x=year, y=value, colour=AnalysisYear, shape=scenario)) +  
                   #geom_line(aes(x=year, y=value, colour=AnalysisYear))
                   facet_wrap(~sector, nrow=1, scales="free_y") +
                   ylim(0, NA) +
                   theme_bw()
