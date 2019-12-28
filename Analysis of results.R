library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("~/disks/y/ontwapps/Timer/Users/Mark/CD_LINKSupdate/6_R/CD_LINKSupdate")
source("../TIMER_output/functions/Settings.R")
source("../TIMER_output/functions/General Functions.R")
source("../TIMER_output/functions/Import_TIMER_output.R")
source("../TIMER_output/functions/Process_TIMER_output.R")
source("../TIMER_output/functions/pbl_colors.R")
source("Settings_indicators.R")

Rundir=paste("~/disks/y/ontwapps/Timer/Users/Mark", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = "TIMER_2015"

reg <- unique(NPi$ENEMISCO2$region)

# First read in No Policy scenario
# Export variable names to Excel (TIMER_implementation.xls) --> data.var_names.csv
# Create in excel list of variables/region combinations for which you want to TIMER output --> data/IndicatorInput.csv
# Create this output based on Import_TIMER_output and Process_TIMER_ output --> data/IndicatorOutput.csv
# This csv file can be imported in Excel
NoPolicy <- ImportTimerScenario('NoPolicy','NoPolicy', Rundir, Project, TIMERGeneration)
NoPolicy_i <- ProcessTimerScenario(NoPolicy, Rundir, Project)
NPi <- ImportTimerScenario('NPi','NPi', Rundir, Project, TIMERGeneration)
NPii <- ProcessTimerScenario(NPi, Rundir, Project)
NoPolicy_update <- ImportTimerScenario('NoPolicy_update','NoPolicy_update', Rundir, Project, TIMERGeneration)
NoPolicy_update_i <- ProcessTimerScenario(NoPolicy_update, Rundir, Project)
NPi_update <- ImportTimerScenario('NPi_update','NPi_update', Rundir, Project, TIMERGeneration)
NPi_update_i <- ProcessTimerScenario(NPi_update, Rundir, Project)

INDCi <- ImportTimerScenario('INDCi','INDCi', Rundir, Project, TIMERGeneration, Policy=TRUE)
INDC2030i_1000 <- ImportTimerScenario('INDC2030i_1000','INDC2030i_1000', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_1600 <- ImportTimerScenario('NPi2020_1600','NPi2020_1600', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_1000 <- ImportTimerScenario('NPi2020_1000','NPi2020_1000', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_400 <- ImportTimerScenario('NPi2020_400','NPi2020_400', Rundir, Project, TIMERGeneration, Policy=TRUE)
INDCi_i <- ProcessTimerScenario(INDCi, Rundir, Project, Policy=TRUE)
INDC2030i_1000_i <- ProcessTimerScenario(INDC2030i_1000, Rundir, Project, Policy=TRUE)
NPi2020_1600_i <- ProcessTimerScenario(NPi2020_1600, Rundir, Project, Policy=TRUE)
NPi2020_1000_i <- ProcessTimerScenario(NPi2020_1000, Rundir, Project, Policy=TRUE)
NPi2020_400_i <- ProcessTimerScenario(NPi2020_400, Rundir, Project, Policy=TRUE)

NPi_update_tmp <- ImportTimerScenario('NPi_update_tmp','NPi_update', Rundir, Project, TIMERGeneration)
NPi_update_i_tmp <- ProcessTimerScenario(NPi_update_tmp, Rundir, Project)

NPi_update_Renewables <- ImportTimerScenario('NPi_update_Renewables','NPi_update', Rundir, Project, TIMERGeneration)
NPi_update_Renewables_i <- ProcessTimerScenario(NPi_update_Renewables, Rundir, Project)
NPi_update_PowerPlantStandard <- ImportTimerScenario('NPi_update_PowerPlantStandard','NPi_update', Rundir, Project, TIMERGeneration)
NPi_update_PowerPlantStandard_i <- ProcessTimerScenario(NPi_update_PowerPlantStandard, Rundir, Project)
NPi_update_CarbonTax <- ImportTimerScenario('NPi_update_CarbonTax','NPi_update', Rundir, Project, TIMERGeneration)
NPi_update_CarbonTax_i <- ProcessTimerScenario(NPi_update_CarbonTax, Rundir, Project)

# general variables
reg <- unique(NPii$EMISCO2EQ$region)

# plot facet to compare three scenarios
NoPol2017 <- mutate(NoPolicy_i$EMISCO2EQ, scenario="NoPolicy 2017")
NoPol2018 <- mutate(NoPolicy_update_i$EMISCO2EQ, scenario="NoPolicy 2018")
NPi2017 <- mutate(NPii$EMISCO2EQ, scenario="NPi 2017")
NPi2018 <- mutate(NPi_update_i$EMISCO2EQ, scenario="NPi 2018")
Graph_compare <- rbind(NoPol2018, NoPol2017) %>% rbind(NPi2017) %>% rbind(NPi2018)

# check no policy baselines version 2017 and 2018
NoPol2017_check <- mutate(NoPol2017, scenario="NoPolicy 2017")
NoPol2018_check <- mutate(NoPol2017, scenario="NoPolicy 2018") 
NoPol2018_check$value <- jitter(NoPol2018_check$value, factor = 1, amount = NULL)
NoPol_check <- rbind(NoPol2017_check, NoPol2018_check)
ggplot(data=filter(NoPol_check, year>=2010, year<=2030, region=="JAP", main_sector=="Total", GHG_Category=="EMISCO2EQ")) + geom_line(aes(year, value))

# check reductions relative to NoPolicy (2018)
NPi_reductions <- inner_join(NoPol2018, NPi2018, by=c('year', 'region', 'main_sector', 'GHG_Category'))
NPi_reductions <- mutate(NPi_reductions, value=value.x-value.y)
NPi_reductions <- select(NPi_reductions, year, region, main_sector, GHG_Category, value)
NPi_reductions <- mutate(NPi_reductions, policy="All")

# show differences between 2017 and 2018 for each sector
for (i in 1:length(reg)) {
#for (i in 1:2) {
  ggplot() + geom_line(data=filter(Graph_compare, GHG_Category=="EMISCO2EQ", year>=2010, year<=2030, region==reg[i]), aes(year, value, colour=scenario) ) +
    facet_wrap(~main_sector) +
    ylim(-200, NA) +
    labs(y="GHG Emissions", x = "Year") + 
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle(paste(reg[i], ": NoPolicy and NPi scenario, 2017 and 2018 versions", sep=""))
  ggsave(file=paste('graphs/Fig_compare_scen_', reg[i], '.jpg', sep=""), width=20, height=10, dpi=400)
}

# plot EU results for update scenarios
ggplot() + geom_line(data=filter(NoPolicy_update_i$EMISCO2EQ_indicator, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NoPolicy_2018") ) + 
  geom_line(data=filter(NPi_update_i$EMISCO2EQ_indicator, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_2018")) + 
scale_colour_manual(name="Scenario",values=c(NoPolicy_2018="blue", NPi_2018="green")) +
  ylim(0, NA) +
  labs(y="Total GHG Emissionsincl LULUCF (MtCO2eq)", x = "Year")  + 
  theme_bw()
ggsave(file=paste('graph/Fig_compare_EU_Total.jpg'), width=20, height=40, dpi=200)

ggplot() + geom_line(data=filter(NPii$EMIS_ETS, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_2017") ) + 
  geom_line(data=filter(NPi_update_i$EMIS_ETS, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_update")) + 
  scale_colour_manual(name="Scenario",values=c(NPi_2017="blue", NPi_update="green")) +
  ylim(0, NA) +
  labs(y="Emissions energy/industry (MtCO2eq)", x = "Year") + 
  theme_bw()

ggplot() + geom_line(data=filter(NPii$EMIS_transport, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_2017") ) + 
  geom_line(data=filter(NPi_update_i$EMIS_transport, year>=2010, year<=2030, region=="EU"), aes(year, value, colour="NPi_update")) + 
  scale_colour_manual(name="Scenario",values=c(NPi_2017="blue", NPi_update="green")) +
  ylim(0, NA) +
  labs(y="Emissions transport (MtCO2eq)", x = "Year") + 
  theme_bw()

EU_Total_NoPolicy <- filter(NoPolicy_update_i$EMISCO2EQ, year>=1990, year<=2030, region=="EU") %>% mutate(scenario="NoPolicy")
EU_Total_NPi <- filter(NPi_update_i$EMISCO2EQ, year>=1990, year<=2030, region=="EU") %>% mutate(scenario="NPi")
EU_Total <- rbind(EU_Total_NoPolicy, EU_Total_NPi)
write.table(EU_Total, "data/NPi_update_EU_EMISCO2EQ.csv", sep=";", row.names=FALSE)

EU_Aviation_NoPolicy <- filter(NPi_update$TransportCO2Emissions, year>=1990, year<=2030, region=="EU", travel_mode=="Air") %>% mutate(scenario="NoPolicy")
EU_Aviation_NPi <- filter(NPi_update$TransportCO2Emissions, year>=1990, year<=2030, region=="EU", travel_mode=="Air") %>% mutate(scenario="NPi")
EU_Aviation <- rbind(EU_Aviation_NoPolicy, EU_Aviation_NPi)
write.table(EU_Aviation, "data/NPi_update_EU_Aviation.csv", sep=";", row.names=FALSE)

EU_LULUCF_NoPolicy <- filter(NoPolicy_update_i$EMISCO2EQ, year>=1990, year<=2030, region=="EU") %>% mutate(scenario="NoPolicy")
EU_LULUCF_NPi <- filter(NPi_update_i$EMISCO2EQ, year>=1990, year<=2030, region=="EU") %>% mutate(scenario="NPi")
EU_LULUCF <- rbind(EU_LULUCF_NoPolicy, EU_LULUCF_NPi)
write.table(EU_LULUCF, "data/NPi_update_EU_LULUCF.csv", sep=";", row.names=FALSE)

# India/China renewable target and Power plant standard
# 1. Emissions
NoPol2018_CO2EQ <- mutate(NoPolicy_update_i$EMISCO2EQ, scenario="NoPolicy 2018")
NPi2017_CO2EQ <- mutate(NPii$EMISCO2EQ, scenario="NPi 2017")
NPi2018_Renewables_CO2EQ <- mutate(NPi_update_Renewables_i$EMISCO2EQ, scenario="NPi 2018 Renewable policy")
NPi2018_PowerPlantStandard_CO2EQ <- mutate(NPi_update_PowerPlantStandard_i$EMISCO2EQ, scenario="NPi 2018 Power plant standard")
NPi2018_CarbonTax_CO2EQ <- mutate(NPi_update_CarbonTax_i$EMISCO2EQ, scenario="NPi 2018 Carbon tax")
NPi2018_CO2EQ <- mutate(NPi_update_i$EMISCO2EQ, scenario="NPi 2018")
Graph_compare_REN_PPS_CO2EQ <- rbind(NoPol2018_CO2EQ, NPi2017_CO2EQ) %>% 
                               rbind(NPi2018_Renewables_CO2EQ) %>% 
                               rbind(NPi2018_PowerPlantStandard_CO2EQ) %>%
                               rbind(NPi2018_CarbonTax_CO2EQ) %>% 
                               rbind(NPi2018_CO2EQ)

ggplot() + geom_line(data=filter(Graph_compare_REN_PPS_CO2EQ, GHG_Category=="EMISCO2EQ", main_sector=="Total", year>=2010, year<=2030, region=="INDIA"), aes(year, value, colour=scenario) ) +
  ylim(0, NA) +
  labs(y="GHG Emissions", x = "Year") + 
  ggtitle("India") +
  theme_bw()

ggplot() + geom_line(data=filter(Graph_compare_REN_PPS_CO2EQ, GHG_Category=="EMISCO2EQ", main_sector=="Total", year>=2010, year<=2030, region=="CHN"), aes(year, value, colour=scenario) ) +
  ylim(0, NA) +
  labs(y="GHG Emissions", x = "Year") + 
  ggtitle("China") +
  theme_bw()

ggplot() + geom_jitter(data=filter(Graph_compare_REN_PPS_CO2EQ, GHG_Category=="EMISCO2EQ", main_sector=="Total", year>=2010, year<=2030, region=="CHN"), aes(year, value, colour=scenario) ) +
  ylim(0, NA) +
  labs(y="GHG Emissions", x = "Year") + 
  ggtitle("China") +
  theme_bw()

# 1b. Emission reductions
REN_reductions <- inner_join(NoPol2018_CO2EQ, NPi2018_Renewables_CO2EQ, by=c('year', 'region', 'main_sector', 'GHG_Category'))
REN_reductions <- mutate(REN_reductions, value=value.x-value.y)
REN_reductions <- select(REN_reductions, year, region, main_sector, GHG_Category, value)
REN_reductions <- mutate(REN_reductions, policy="Renewables")

PowerPlantStandard_reductions <- inner_join(NoPol2018_CO2EQ, NPi2018_PowerPlantStandard_CO2EQ, by=c('year', 'region', 'main_sector', 'GHG_Category'))
PowerPlantStandard_reductions <- mutate(PowerPlantStandard_reductions, value=value.x-value.y)
PowerPlantStandard_reductions <- select(PowerPlantStandard_reductions, year, region, main_sector, GHG_Category, value)
PowerPlantStandard_reductions <- mutate(PowerPlantStandard_reductions, policy="New power plants standard")

CarbonTax_reductions <- inner_join(NoPol2018_CO2EQ, NPi2018_CarbonTax_CO2EQ, by=c('year', 'region', 'main_sector', 'GHG_Category'))
CarbonTax_reductions <- mutate(CarbonTax_reductions, value=value.x-value.y)
CarbonTax_reductions <- select(CarbonTax_reductions, year, region, main_sector, GHG_Category, value)
CarbonTax_reductions <- mutate(CarbonTax_reductions, policy="PAT scheme")

NPi_reductions <- inner_join(NoPol2018_CO2EQ, NPi2018_CO2EQ, by=c('year', 'region', 'main_sector', 'GHG_Category'))
NPi_reductions <- mutate(NPi_reductions, value=value.x-value.y)
NPi_reductions <- select(NPi_reductions, year, region, main_sector, GHG_Category, value)
NPi_reductions <- mutate(NPi_reductions, policy="All")

Reductions <- rbind(REN_reductions, PowerPlantStandard_reductions) %>% rbind(CarbonTax_reductions) %>% rbind(NPi_reductions)
Reductions_India_2030 <- filter(Reductions, year==2030, region=="INDIA")

NoPol2018_eff <- mutate(NoPolicy_update_i$ElecEffCoalNewPct, scenario="NoPolicy 2018")
NPI_PPS_800_eff <- mutate(NPi2018_PowerPlantStandard800$ElecEffCoalNewPct, scenario="efficiency 800/360")
NPI_PPS_820_eff <- mutate(NPi2018_PowerPlantStandard820$ElecEffCoalNewPct, scenario="efficiency 820/380")
NPI_PPS_840_eff <- mutate(NPi2018_PowerPlantStandard840$ElecEffCoalNewPct, scenario="efficiency 840/400")
NPI_PPS_860_eff <- mutate(NPi2018_PowerPlantStandard860$ElecEffCoalNewPct, scenario="efficiency 860/420")
NPI_PPS_880_eff <- mutate(NPi2018_PowerPlantStandard880$ElecEffCoalNewPct, scenario="efficiency 880/440")

#2. Efficiency
NoPol2018_ElecEff <- mutate(NoPolicy_i$ElecEffNewPct, scenario="NoPolicy 2018")
NPi2017_ElecEff <- mutate(NPii$ElecEffNewPct, scenario="NPi 2017")
NPi2018_Renewables_ElecEff <- mutate(NPi_update_Renewables_i$ElecEffNewPct, scenario="NPi 2018 Renewable policy")
NPi2018_PowerPlantStandard_ElecEff<- mutate(NPi_update_PowerPlantStandard_i$ElecEffNewPct, scenario="NPi 2018 Power plant standard")
NPi2018_ElecEff <- mutate(NPi_update_i$ElecEffNewPct, scenario="NPi 2018")
Graph_compare_India_ElecEff <- rbind(NoPol2018_ElecEff, NPi2017_ElecEff) %>% rbind(NPi2018_Renewables_ElecEff) %>% rbind(NPi2018_PowerPlantStandard_ElecEff) %>% rbind(NPi2018_ElecEFf)
ggplot() + geom_line(data=filter(Graph_compare_India_ElecEff, year>=2010, year<=2030, region=="INDIA"), aes(year, value, colour=scenario) ) +
  ylim(0, NA) +
  labs(y="GHG Emissions", x = "Year")

NoPol2018_CO2_KWh <- mutate(NoPolicy_i$CO2_KWh, scenario="NoPolicy 2018")
NPi2017_CO2_KWh <- mutate(NPii$CO2_KWh, scenario="NPi 2017")
NPi2018_PowerPlantStandard_CO2_KWh <- mutate(NPi_update_PowerPlantStandard_i$CO2_KWh, scenario="NPi 2018 Power plant standard")
NPi2018_CO2_KWh_tmp <- mutate(NPi_update_i_tmp$CO2_KWh, scenario="NPi 2018 without PPS")
NPi2018_CO2_KWh <- mutate(NPi_update_i$CO2_KWh, scenario="NPi 2018")
Graph_compare_India_CO2_KWh <- rbind(NoPol2018_CO2_KWh, NPi2017_CO2_KWh) %>% rbind(NPi2018_PowerPlantStandard_CO2_KWh) %>% rbind(NPi2018_CO2_KWh_tmp) %>% rbind(NPi2018_CO2_KWh)
ggplot() + geom_line(data=filter(Graph_compare_India_CO2_KWh, year>=2010, year<=2030, region=="INDIA"), aes(year, value, colour=scenario) ) +
  ylim(0, NA) +
  labs(y="gCO2/KWh", x = "Year") +
  ggtitle("CO2 intensity of power sector")

write.table(NPi_update_i$EMISCO2EQ_indicator , "data/NPi_update_EMISCO2EQ.csv", sep=";", row.names=FALSE)
write.table(NPi_update_i$EMISCO2EQexcl , "data/NPi_update_EMISCO2EQ_exclLULUCF.csv", sep=";", row.names=FALSE)
write.table(NPi_update_i$LUEMCO2_TOT_indicator , "data/NPi_update_EMISCO2_LULUCF.csv", sep=";", row.names=FALSE)

# check nuclear capacity for India
India_NPi_NuclearCapacity=filter(NPi$ElecCap, region=="INDIA", energy_technology=="Nuclear") %>% mutate(scenario="NPi") %>% mutate(value=value/1000)
India_INDCi_NuclearCapacity=filter(INDCi$ElecCap, region=="INDIA", energy_technology=="Nuclear") %>% mutate(scenario="INDCi") %>% mutate(value=value/1000)
India_NPi2020_1000_NuclearCapacity=filter(NPi2020_400$ElecCap, region=="INDIA", energy_technology=="Nuclear") %>% mutate(scenario="NPi2020_1000") %>% mutate(value=value/1000)
India_INDCi2030_1000_NuclearCapacity=filter(NPi2020_1000$ElecCap, region=="INDIA", energy_technology=="Nuclear") %>% mutate(scenario="INDCi2030_1000") %>% mutate(value=value/1000)
India_nuclear <- rbind(India_NPi_NuclearCapacity, India_INDCi_NuclearCapacity) %>% 
                 rbind(India_NPi2020_1000_NuclearCapacity) %>%
                 rbind(India_INDCi2030_1000_NuclearCapacity)
MaxNuclear <- as.numeric(filter(India_NPi2020_400_NuclearCapacity, year==2030) %>% select(value))
ggplot(data=India_nuclear) + geom_line(aes(x=year, y=value, colour=scenario)) +
                             labs(y=India_NPi_NuclearCapacity$unit, x = "Year") +
                             ggtitle("Nuclear capacity India") +
                             xlim(2010, 2030) + 
                             ylim(0, MaxNuclear) + 
                             theme_bw()


