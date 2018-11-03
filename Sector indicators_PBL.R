source('CD_LINKS/ReadTimerData_indicators.R')
source('CD_LINKS/Settings_indicators.R')

# Create data for sector indicators graph
d_REN_electricity <- mutate(NoPolicy_ind$RenElecShare, scenario="No policy")
d_REN_electricity <- mutate(NPi_ind$RenElecShare, scenario="National policies") %>% rbind(d_REN_electricity)
d_REN_electricity <- mutate(INDCi_ind$RenElecShare, scenario="NDC") %>% rbind(d_REN_electricity)
d_REN_electricity <- mutate(NPi2020_1000_ind$RenElecShare, scenario="2C") %>% rbind(d_REN_electricity)
d_REN_electricity <- mutate(INDCi2030_1000_ind$RenElecShare, scenario="2C delay") %>% rbind(d_REN_electricity)
d_REN_electricity <- mutate(NPi2020_400_ind$RenElecShare, scenario="1.5C") %>% rbind(d_REN_electricity)
d_REN_electricity$scenario <- factor(d_REN_electricity$scenario, levels=Scenarios_fig)

DataIndicators_PBL <- mutate(d_REN_electricity, variable="Share of Renewable Electricity")

d_CO2_intensity_cars <- mutate(NoPolicy_ind$CO2_km_cars, scenario="No policy")
d_CO2_intensity_cars <- mutate(NPi_ind$CO2_km_cars, scenario="National policies") %>% rbind(d_CO2_intensity_cars)
d_CO2_intensity_cars <- mutate(INDCi_ind$CO2_km_cars, scenario="NDC") %>% rbind(d_CO2_intensity_cars)
d_CO2_intensity_cars <- mutate(NPi2020_1000_ind$CO2_km_cars, scenario="2C") %>% rbind(d_CO2_intensity_cars)
d_CO2_intensity_cars <- mutate(INDCi2030_1000_ind$CO2_km_cars, scenario="2C delay") %>% rbind(d_CO2_intensity_cars)
d_CO2_intensity_cars <- mutate(NPi2020_400_ind$CO2_km_cars, scenario="1.5C") %>% rbind(d_CO2_intensity_cars)
d_CO2_intensity_cars$scenario <- factor(d_CO2_intensity_cars$scenario, levels=Scenarios_fig)

DataIndicators_PBL <- mutate(d_CO2_intensity_cars, variable="CO2 intensity cars") %>% rbind(DataIndicators_PBL) 

d_Energy_intensity_residential_buildings <- mutate(NoPolicy_ind$Residential_FinalEnergy_m2, scenario="No policy")
d_Energy_intensity_residential_buildings <- mutate(NPi_ind$Residential_FinalEnergy_m2, scenario="National policies") %>% rbind(d_Energy_intensity_residential_buildings)
d_Energy_intensity_residential_buildings <- mutate(INDCi_ind$Residential_FinalEnergy_m2, scenario="NDC") %>% rbind(d_Energy_intensity_residential_buildings)
d_Energy_intensity_residential_buildings <- mutate(NPi2020_1000_ind$Residential_FinalEnergy_m2, scenario="2C") %>% rbind(d_Energy_intensity_residential_buildings)
d_Energy_intensity_residential_buildings <- mutate(INDCi2030_1000_ind$Residential_FinalEnergy_m2, scenario="2C delay") %>% rbind(d_Energy_intensity_residential_buildings)
d_Energy_intensity_residential_buildings <- mutate(NPi2020_400_ind$Residential_FinalEnergy_m2, scenario="1.5C") %>% rbind(d_Energy_intensity_residential_buildings)
d_Energy_intensity_residential_buildings$scenario <- factor(d_Energy_intensity_residential_buildings$scenario, levels=Scenarios_fig)

DataIndicators_PBL <- mutate(d_Energy_intensity_residential_buildings, variable="Energy intensity residential buildings") %>% rbind(DataIndicators_PBL)

d_Energy_intensity_industry <- mutate(NoPolicy_ind$Industry_Energy_IVA, scenario="No policy")
d_Energy_intensity_industry <- mutate(NPi_ind$Industry_Energy_IVA, scenario="National policies") %>% rbind(d_Energy_intensity_industry)
d_Energy_intensity_industry <- mutate(INDCi_ind$Industry_Energy_IVA, scenario="NDC") %>% rbind(d_Energy_intensity_industry)
d_Energy_intensity_industry <- mutate(NPi2020_1000_ind$Industry_Energy_IVA, scenario="2C") %>% rbind(d_Energy_intensity_industry)
d_Energy_intensity_industry <- mutate(INDCi2030_1000_ind$Industry_Energy_IVA, scenario="2C delay") %>% rbind(d_Energy_intensity_industry)
d_Energy_intensity_industry <- mutate(NPi2020_400_ind$Industry_Energy_IVA, scenario="1.5C") %>% rbind(d_Energy_intensity_industry)
d_Energy_intensity_industry$scenario <- factor(d_Energy_intensity_industry$scenario, levels=Scenarios_fig)

DataIndicators_PBL <- mutate(d_Energy_intensity_industry, variable="Energy intensity industry") %>% rbind(DataIndicators_PBL)

DI_PBL <- spread(DataIndicators_PBL, key=year, value=value)
DI_PBL <- select(DI_PBL, variable, scenario, region, unit, everything())
write.table(DI_PBL, file="Indicators_PBL.csv", sep=";", row.names = FALSE)

# Create sector indicators graphs
figure_d_Energy_intensity_residential_buildings <- ggplot(data=filter(d_Energy_intensity_residential_buildings, region=="World", year>=2010, year<=2050)) +
  geom_line(aes(x=year, y=value, color=scenario)) +
  labs(title="Buildings sector", subtitle="residential energy use per m2") +
  labs(x = "year", y = "GJ/m2") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_manual(values = rhg_cols)

figure_d_REN_electricity <- ggplot(data=filter(d_REN_electricity, region=="World", year>=2010, year<=2050)) +
  geom_line(aes(x=year, y=value, color=scenario)) +
  labs(title="Electricity sector", subtitle="share of renewable electricity") +
  labs(x = "year", y = "%") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_manual(values = rhg_cols)

figure_d_CO2_intensity_cars <- ggplot(data=filter(d_CO2_intensity_cars, region=="World", year>=2010, year<=2050)) +
  geom_line(aes(x=year, y=value, color=scenario)) +
  labs(title="Transport sector", subtitle="CO2-intensity cars") +
  labs(x = "year", y = "gCO2/km") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_manual(values = rhg_cols)

figure_d_Energy_intensity_industry <- ggplot(data=filter(d_Energy_intensity_industry, region=="World", year>=2010, year<=2050)) +
  geom_line(aes(x=year, y=value, color=scenario)) +
  labs(title="Industry sector", subtitle="energy use per industry value added") +
  labs(x = "year", y = "PJ/million US$(2005)") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
  scale_color_manual(values = rhg_cols)

g1 <- grid.arrange(figure_d_REN_electricity, figure_d_CO2_intensity_cars, figure_d_Energy_intensity_residential_buildings, figure_d_Energy_intensity_industry, ncol=2, nrow=2)
g2 <- grid_arrange_shared_legend(figure_d_REN_electricity, figure_d_CO2_intensity_cars, figure_d_Energy_intensity_residential_buildings, figure_d_Energy_intensity_industry)
